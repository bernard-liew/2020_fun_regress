library (keras)
library (imager)

# Helper
library (tidyverse)
library (flextable)
library (officer)
library (cowplot)

# Measures
source("code/measures_johnson.R")
source("code/helper_func.R")

# Create labels for plots
ynames <- c("AP ankle (Nm/kg)",
            "ML ankle (Nm/kg)",
            "Vt ankle (Nm/kg)",
            "AP GRF (N/kg)",
            "ML GRF (N/kg)",
            "Vt GRF (N/kg)",
            "AP hip (Nm/kg)",
            "ML hip (Nm/kg)",
            "Vt hip (Nm/kg)",
            "AP knee (Nm/kg)",
            "ML knee (Nm/kg)",
            "Vt knee (Nm/kg)")

varLvls <- c("ankle_moment_ap",
             "ankle_moment_ml",
             "ankle_moment_vt",
             "knee_moment_ap",
             "knee_moment_ml",
             "knee_moment_vt",
             "hip_moment_ap",
             "hip_moment_ml",
             "hip_moment_vt",
             "com_grf_ap",
             "com_grf_ml",
             "com_grf_vt")

yNewNames <- c("Ankle Inv(+)/Eve(-)",
               "Ankle Df(+)/Pf(-)",
               "Ankle Add(+)/Abd(-)",
               "Knee Add(+)/Abd(-)",
               "Knee E(+)/F(-)",
               "Knee IR(+)/ER(-)",
               "Hip Add(+)/Abd(-)",
               "Hip F(+)/E(-)",
               "Hip IR(+)/ER(-)",
               "GRF AP",
               "GRF ML",
               "GRF VT")

# PFFR -------------------------------------------------------------------------
train <- readRDS("output/train_data.RDS")
test <- readRDS("output/test_data.RDS")

response_vars <- names(train)[grep("grf|hip_moment|knee_moment|ankle_moment", names(train))]
pred_vars <- names(train)[grep("angle|vel|accl", names(train))]

# Load models

pred_list <- list()
err_list <- list()

for(n in seq_along (response_vars)){

  pred_list[[n]] <- readRDS(file.path ("output", paste0("PredictionFx_",response_vars[n],".RDS")))
  err_list[[n]] <- readRDS(file.path ("output", paste0("MeasuresFx_",response_vars[n],".RDS")))

}

# Plot and get error

names (pred_list) <- response_vars
names (err_list) <- response_vars

obs.df.mean <- test[response_vars] %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

obs.df.sd <- test[response_vars] %>%
  map (get_meanSD, func = sd) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

obs.df <- obs.df.mean %>%
  inner_join(obs.df.sd, by = c("cycle", "var")) %>%
  rename (obs_Mean = Mean,
          obs_Sd = Sd)


pred.df.mean <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

pred.df.sd <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

pffr.df <- pred.df.mean %>%
  inner_join(pred.df.sd, by = c("cycle", "var")) %>%
  inner_join(obs.df, by = c("cycle", "var")) %>%
  mutate (method = "pffr")


pffr.err <- err_list %>%
  map (~ map (., mean)) %>%
  map (bind_cols) %>%
  bind_rows(.id = "Outcomes") %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate (`relRMSE(%)` = relRMSE * 100) %>%
  select (Outcomes, RMSE, `relRMSE(%)`, cor) %>%
  mutate (method = "pffr")

pffr.err$Outcomes <- c("Ankle Inv(+)/Eve(-)",
                       "Ankle Df(+)/Pf(-)",
                       "Ankle Add(+)/Abd(-)",
                       "GRF AP",
                       "GRF ML",
                       "GRF VT",
                       "Hip Add(+)/Abd(-)",
                       "Hip F(+)/E(-)",
                       "Hip IR(+)/ER(-)",
                       "Knee Add(+)/Abd(-)",
                       "Knee E(+)/F(-)",
                       "Knee IR(+)/ER(-)")

# Deep scratch -----------------------------------------------------------------

###################### Import data ###########################################
load ("output/deep_data.RData")

out_vars <- dimnames(train_y_array)[[3]]
out_axes_vars <- dimnames(train_y_array)[[4]]

# Load pretrained image model ---------------------------------------------------------------
wid <- 150
ht <- 150


# Resize  input using cubic interpolation [5]

train_x <- resize(train_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()
test_x <- resize(test_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()


pred_list <- list()
err_list <- list()

fileNames <- list.files("output/", "kerasScra")

for (n in seq_along (fileNames)) {

  input_path <- file.path ("output", fileNames[n])
  model <- load_model_hdf5(input_path)

  pred_list[[n]] <- model %>% predict(test_x)

}

obs_list <- list()
for (out in out_vars) {

  for (oaxes in out_axes_vars) {

    outnames <- paste0(out, "_", oaxes)

    obs_list[[outnames]] <- test_y_array[,,out, oaxes]

  }

}

for (n in seq_along (fileNames)) {

  err_list[[n]] <- all_measures(obs_list[[n]], pred_list[[n]])

}

# Plot and get error

varNames <- fileNames %>%
  str_remove("kerasScra_") %>%
  str_remove(".h5")

names (obs_list) <- names (pred_list) <- varNames

obs.df.mean <- obs_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

obs.df.sd <- obs_list %>%
  map (get_meanSD, func = sd) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

obs.df <- obs.df.mean %>%
  inner_join(obs.df.sd, by = c("cycle", "var"))%>%
  rename (obs_Mean = Mean,
          obs_Sd = Sd)


pred.df.mean <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

pred.df.sd <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

cnn.df <- pred.df.mean %>%
  inner_join(pred.df.sd, by = c("cycle", "var")) %>%
  inner_join(obs.df, by = c("cycle", "var")) %>%
  mutate (method = "deep_scratch")


cnn.err <- err_list %>%
  map (~ map (., mean)) %>%
  map (bind_cols) %>%
  bind_rows(.id = "Outcomes") %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate (`relRMSE(%)` = relRMSE * 100) %>%
  select (Outcomes, RMSE, `relRMSE(%)`, cor) %>%
  mutate (method = "deep_scratch")

cnn.err$Outcomes <- c("Ankle Inv(+)/Eve(-)",
                      "Ankle Df(+)/Pf(-)",
                      "Ankle Add(+)/Abd(-)",
                      "GRF AP",
                      "GRF ML",
                      "GRF VT",
                      "Hip Add(+)/Abd(-)",
                      "Hip F(+)/E(-)",
                      "Hip IR(+)/ER(-)",
                      "Knee Add(+)/Abd(-)",
                      "Knee E(+)/F(-)",
                      "Knee IR(+)/ER(-)")

# Deep transfer -----------------------------------------------------------------

###################### Import data ###########################################


pred_list <- list()
err_list <- list()

fileNames <- list.files("output/", "kerasTL")

for (n in seq_along (fileNames)) {

  input_path <- file.path ("output", fileNames[n])
  model <- load_model_hdf5(input_path)

  pred_list[[n]] <- model %>% predict(test_x)

}

obs_list <- list()
for (out in out_vars) {

  for (oaxes in out_axes_vars) {

    outnames <- paste0(out, "_", oaxes)

    obs_list[[outnames]] <- test_y_array[,,out, oaxes]

  }

}

for (n in seq_along (fileNames)) {

  err_list[[n]] <- all_measures(obs_list[[n]], pred_list[[n]])

}

# Plot and get error

varNames <- fileNames %>%
  str_remove("kerasTL_") %>%
  str_remove(".h5")

names (obs_list) <- names (pred_list) <- varNames

obs.df.mean <- obs_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

obs.df.sd <- obs_list %>%
  map (get_meanSD, func = sd) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

obs.df <- obs.df.mean %>%
  inner_join(obs.df.sd, by = c("cycle", "var"))%>%
  rename (obs_Mean = Mean,
          obs_Sd = Sd)


pred.df.mean <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Mean")

pred.df.sd <- pred_list %>%
  map (get_meanSD, func = mean) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -c (cycle),
               names_to = "var",
               values_to = "Sd")

TL.df <- pred.df.mean %>%
  inner_join(pred.df.sd, by = c("cycle", "var")) %>%
  inner_join(obs.df, by = c("cycle", "var")) %>%
  mutate (method = "deep_TL")


TL.err <- err_list %>%
  map (~ map (., mean)) %>%
  map (bind_cols) %>%
  bind_rows(.id = "Outcomes") %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate (`relRMSE(%)` = relRMSE * 100) %>%
  select (Outcomes, RMSE, `relRMSE(%)`, cor) %>%
  mutate (method = "deep_TL")

TL.err$Outcomes <-c("Ankle Inv(+)/Eve(-)",
                    "Ankle Df(+)/Pf(-)",
                    "Ankle Add(+)/Abd(-)",
                    "GRF AP",
                    "GRF ML",
                    "GRF VT",
                    "Hip Add(+)/Abd(-)",
                    "Hip F(+)/E(-)",
                    "Hip IR(+)/ER(-)",
                    "Knee Add(+)/Abd(-)",
                    "Knee E(+)/F(-)",
                    "Knee IR(+)/ER(-)")
# Complot plots

df.plot <- bind_rows(pffr.df,
                     cnn.df,
                     TL.df) %>%
  mutate (var = factor (var, levels = varLvls, labels = yNewNames),
          Method = method) %>%
  filter (!grepl ("GRF", var)) %>%
  select (-method)

temp1 <- df.plot %>%
  select (cycle, var, obs_Mean, obs_Sd) %>%
  rename (Mean = obs_Mean,
          Sd = obs_Sd) %>%
  mutate (Method = "obs")

df.plot <- df.plot %>%
  select (!matches ("obs")) %>%
  bind_rows(temp1) %>%
  mutate (Method = factor (Method, levels = c("obs", "pffr", "deep_scratch", "deep_TL")))


p <- ggplot (df.plot) +
  geom_line(aes(x = cycle, y = Mean, colour = Method), size = 1) +
   #geom_ribbon(aes(x = cycle, ymin = Mean - Sd, ymax = Mean + Sd, fill = method), alpha = 0.1) +
  facet_wrap(~ var, scales = "free", ncol = 3) +
  scale_color_manual(values = c("black", "red", "blue", "green")) +
  scale_fill_manual(values = c("black", "red", "blue", "green")) +
  guides(fill = "none") +
  ylab ("Joint moment (Nm/kg)") +
  xlab ("Stance phase (100%)") +
  theme_cowplot() +
  theme (text = element_text(size=16))

pdf ("manuscript/fig1.pdf", width = 20, height = 10)
p
dev.off ()

# Table
err.df <- bind_rows(pffr.err,
                    cnn.err,
                    TL.err) %>%
  mutate (Outcomes = factor (Outcomes, levels = yNewNames),
          Method = factor (method, levels = c("pffr", "deep_scratch", "deep_TL"))) %>%
  arrange (Outcomes, Method) %>%
  filter (!grepl ("GRF", Outcomes)) %>%
  select (Outcomes, Method,  everything(), -method)

my_path <- paste0("manuscript/table2",
                  "perf",
                  ".docx")


ft <- flextable(err.df) %>%
  set_caption(caption = "Table 2. Prediction performance") %>%
  autofit()

my_doc <- read_docx()  %>%
  body_add_flextable(ft)

print (my_doc, target = my_path)
