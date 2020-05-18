rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)

# modelling
library (refund)

# parallel
library (doParallel)
library (foreach)

# plotting
library (cowplot)
library (ggpubr)

# measures
source("code/measures_johnson.R")

# Split data---------------------------------------------------------------

train <- readRDS("output/train_data.RDS")
test <- readRDS("output/test_data.RDS")

# Get variables---------------------------------------------------------------
response_vars <- names(train)[grep("grf|knee_moment", names(train))]
pred_vars <- names(train)[grep("angle|vel|accl", names(train))]


# Modelling---------------------------------------------------------------


doParallel::registerDoParallel(7)
model <- foreach (n = 1: length (response_vars), .packages = "refund") %dopar%{

  response <- response_vars[n]

  # Create formula
  form <- paste(response, " ~ 1 + ", paste(
    paste("ff(", pred_vars,
          ", yind=cycle, xind=cycle)"),
    collapse = " + "), " + age",
    "+ ht + wt",
    " + sex + s(id, bs = 're') + s(cond, bs = 're')")

  # initialize the model
  m <- pffr(as.formula(form),
            yind = cycle,
            algorithm = "bam",
            data = train)


  # plot(m)

  # Save model and predictions

  saveRDS(m, file = file.path ("output", paste0("Model_",response,".RDS")))

  pp <- predict(m, newdata = test[-which(names(test)=="cycle")])

  saveRDS(pp, file = file.path ("output", paste0("Prediction_",response,".RDS")))

  measures <- all_measures(test[[response]], pp)

  saveRDS(measures, file = file.path ("output", paste0("Measures_",response,".RDS")))

  rm(m)
  gc()

}

# Load models-------------------------------------------------------

mod_list <- list()
pred_list <- list()
err_list <- list()

for(n in seq_along (response_vars)){

  #mod_list[[n]] <- readRDS(file.path ("output", paste0("Model_",response_vars[n],".RDS")))
  pred_list[[n]] <- readRDS(file.path ("output", paste0("Prediction_",response_vars[n],".RDS")))
  err_list[[n]] <- readRDS(file.path ("output", paste0("Measures_",response_vars[n],".RDS")))

}

names (pred_list) <- response_vars
names (err_list) <- response_vars

# Plot predicted vs observed ------------------------------------------

ynames <- c("AP GRF (N/kg)",
            "ML GRF (N/kg)",
            "Vert GRF (N/kg)",
            "Frontal knee moment (Nm/kg)",
            "Sagittal knee moment (Nm/kg)",
            "Axial knee moment (Nm/kg))")

results_plot <- function (test, pred, response, ynames) {

  pred_m <- apply (pred, 2, mean)
  pred_s <- apply (pred, 2, sd)

  obs_m <- apply (test, 2, mean)
  obs_s <- apply (test, 2, sd)

  df_plot <- data.frame (var = rep (c("pred", "obs"), each = 101),
                         cycle = rep (c(1:101), times = 2),
                         Mean = c(pred_m, obs_m),
                         Sd = c(pred_s, obs_s))

  ggplot (df_plot) +
    geom_line(aes(x = cycle, y = Mean, colour = var), size = 2) +
    geom_ribbon(aes(x = cycle, ymin = Mean - Sd, ymax = Mean + Sd, fill = var), alpha = 0.4) +
    scale_color_manual(values = c("black", "red")) +
    scale_fill_manual(values = c("black", "red")) +
    guides(fill = "none") +
    ylab (ynames) +
    xlab ("Cycle (100%)") +
    theme_bw() +
    theme (text = element_text(size=16))



}

f_list <- list()

for(n in seq_along (response_vars)){

  resp <- response_vars[n]

  f_list[[n]] <- results_plot(test = test[[resp]], pred = pred_list[[resp]], response = resp, ynames = ynames[n])

}


err <- err_list %>%
  map (~ map (., mean)) %>%
  map (bind_cols) %>%
  bind_rows(.id = "Outcomes") %>%
  mutate_if(is.numeric, round, 2)

err$Outcomes <- ynames

table_f <- ggtexttable(err, theme = ttheme("mOrange"))


legend <- get_legend(
  # create some space to the left of the legend
  f_list[[1]] +
    guides(color = guide_legend(title="Predicted vs. \nObserved"), nrow = 1) +
    theme(legend.position = "right")
)

f <- plot_grid(plotlist = f_list %>% map (~. + theme(legend.position="none")), ncol = 3)

f1 <- plot_grid(table_f,
                legend,
                ncol = 1,
                rel_widths = c(0.5, .5))

f2 <- plot_grid(f,
                f1,
                nrow = 1,
                rel_widths = c(0.7, .3))
f2
