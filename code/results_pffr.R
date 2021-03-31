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

test <- readRDS("output/pffr_test_data.RDS")

# Get variables---------------------------------------------------------------
response_vars <- names(testVirtual())[grep("grf|knee_moment", names(test))]
pred_vars <- names(test)[grep("angle|vel|accl", names(test))]


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
