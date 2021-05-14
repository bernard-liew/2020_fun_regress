rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)
library (reticulate)

# modelling
library (keras)
library (tensorflow)

# image
library (imager)

# plotting
library (cowplot)
library (ggpubr)

# set python environment
use_condaenv("tf-gpu")

# measures
source("code/measures_johnson.R")
source("code/helper_func.R")

# Load data---------------------------------------------------------------

load ("output/deep_data.RData")

# Get variables---------------------------------------------------------------

out_vars <- dimnames(train_y_array)[[3]]
out_axes_vars <- dimnames(train_y_array)[[4]]



# Load pretrained image model ---------------------------------------------------------------
wid <- 150
ht <- 150


conv_base <- application_vgg16(weights = 'imagenet', include_top = FALSE, input_shape = c(wid,ht,3))

# Resize  input using cubic interpolation [5]

train_x <- resize(train_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()
test_x <- resize(test_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()

# Split train set into validate and train, as needed modelling
set.seed (2839)
val_id <- sample (unique (dimnames(train_x_array)[[1]]), size = 10)
val_id <- dimnames(train_x_array)[[1]] %in% val_id

val_x <- train_x[val_id,,,]
train_x <- train_x[!val_id,,,]

for (out in out_vars) {

  for (oaxes in out_axes_vars) {

    # Select outcomes ---------------------------------------------------------
    train_y <- train_y_array[!val_id,,out, oaxes]
    val_y <- train_y_array[val_id,,out, oaxes]
    test_y <- test_y_array[,,out, oaxes]

    # Create model ------------------------------------------------------------
    model <- keras_model_sequential() %>%
      conv_base %>%
      layer_flatten() %>%
      layer_dense (units = 256, activation = "relu") %>%
      layer_activation(activation = "relu") %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense (units = 101, activation = "linear")

    # Consider freezing bottom weights to prevent overfit
    freeze_weights(conv_base)

    # run model ------------------------------------------------------------
    model %>% compile(
      optimizer = "rmsprop",
      #optimizer = optimizer_adam(lr = 0.0001),
      loss = "mse",
      metrics = c("mae")
    )

    history <- model %>% fit(
      train_x, train_y,
      epochs = 200,
      batch_size = 16,
      validation_data = list(val_x, val_y),
      callbacks =   list(
        callback_learning_rate_scheduler(
          tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .7)
        )
      )
    )

    output_path <- file.path ("output", paste0("kerasTL_", out, "_",oaxes, ".h5"))

    save_model_hdf5(model, output_path)

  }

}

# Predict ------------------------------------------------------------
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

# Remove GRF

var_rm <- c(4:6)
pred_list [var_rm] <- NULL
obs_list [var_rm] <- NULL
err_list [var_rm] <- NULL

# Plot performance -------------------------------------------------------------

# Plot predicted vs observed ------------------------------------------

ynames <- c("AP ankle (Nm/kg)",
            "ML ankle (Nm/kg)",
            "Vt ankle (Nm/kg)",
            # "AP GRF (N/kg)",
            # "ML GRF (N/kg)",
            # "Vt GRF (N/kg)",
            "AP hip (Nm/kg)",
            "ML hip (Nm/kg)",
            "Vt hip (Nm/kg)",
            "AP knee (Nm/kg)",
            "ML knee (Nm/kg)",
            "Vt knee (Nm/kg)")

f_list <- list()

for(n in seq_along (ynames)){

  f_list[[n]] <- results_plot(test = obs_list[[n]], pred = pred_list[[n]], ynames = ynames[n])

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
    guides(color = guide_legend(title="Performance"), nrow = 1) +
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

pdf ("output/deepTL.pdf", width = 20, height = 10)
f2
dev.off ()
