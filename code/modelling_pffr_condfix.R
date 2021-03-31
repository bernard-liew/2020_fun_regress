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

response_vars <- names(train)[grep("grf|hip_moment|knee_moment|ankle_moment", names(train))]
pred_vars <- names(train)[grep("angle|vel|accl", names(train))]


# Modelling---------------------------------------------------------------


for (n in seq_along(response_vars)) {

  response <- response_vars[n]

  # Create formula
  form <- paste(response, " ~ 1 + ", paste(
    paste("sff(", pred_vars,
          ", yind=cycle, xind=cycle)"),
    collapse = " + "),
    " + age",
    " + ht",
    " + wt",
    " + sex",
    " + side",
    " + cond",
    "+ s(id, bs = 're')"
    )

  # initialize the model
  m <- pffr(as.formula(form),
            yind = cycle,
            algorithm = "bam",
            data = train)


  # plot(m)

  # Save model and predictions

  saveRDS(m, file = file.path ("output", paste0("ModelFix_",response,".RDS")))

  pp <- predict(m, newdata = test[-which(names(test)=="cycle")])

  saveRDS(pp, file = file.path ("output", paste0("PredictionFix_",response,".RDS")))

  measures <- all_measures(test[[response]], pp)

  saveRDS(measures, file = file.path ("output", paste0("MeasuresFix_",response,".RDS")))

  rm(m)
  gc()

}

# Load models-------------------------------------------------------

mod_list <- list()
pred_list <- list()
err_list <- list()

for(n in seq_along (response_vars)){

  #mod_list[[n]] <- readRDS(file.path ("output", paste0("Model_",response_vars[n],".RDS")))
  pred_list[[n]] <- readRDS(file.path ("output", paste0("PredictionFix_",response_vars[n],".RDS")))
  err_list[[n]] <- readRDS(file.path ("output", paste0("MeasuresFix_",response_vars[n],".RDS")))

}

names (pred_list) <- response_vars
names (err_list) <- response_vars

# Plot predicted vs observed ------------------------------------------

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
  mutate_if(is.numeric, round, 2) %>%
  mutate (`relRMSE(%)` = relRMSE * 100) %>%
  select (Outcomes, RMSE, `relRMSE(%)`, cor)

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

pdf ("output/predictCondfix.pdf", width = 20, height = 10)
f2
dev.off ()
