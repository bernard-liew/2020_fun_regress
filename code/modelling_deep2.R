rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)
library (reticulate)

# modelling
library (keras)

# image
library (imager)

# set python environment
use_condaenv("tf-gpu")

# measures
source("code/measures_johnson.R")
source("code/helper_func.R")

# Load data---------------------------------------------------------------

load ("output/deep_data.RData")

# Load pretrained image model ---------------------------------------------------------------
wid <- 150
ht <- 150


conv_base <- application_densenet121(
  weights = "imagenet", # "none" to use architecture only
  include_top = FALSE,
  input_shape = c(wid, ht, 3)
)

# Consider freezing bottom weights to prevent overfit
#freeze_weights(conv_base, from = "conv5_block10_0_relu ")

# Resize  input using cubic interpolation [5]

train_x <- resize(train_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()
test_x <- resize(test_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()

# Split train set into validate and train, as needed modelling

val_id <- sample (1:dim (train_x)[1], size = 30)
val_x <- train_x[val_id,,,]
train_x <- train_x[-val_id,,,]


# Select outcomes ---------------------------------------------------------

outname <- "knee_moment"
axes <- "ml"
train_y <- train_y_array[-val_id,,outname, axes]
val_y <- train_y_array[val_id,,outname, axes]
test_y <- test_y_array[,,outname, axes]

# Create model --------------------------------------------------------------

model <- keras_model_sequential() %>%
  conv_base %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense (units = 1024, activation = "linear") %>%
  layer_dense (units = 1024, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense (units = 1024, activation = "linear") %>%
  layer_dense (units = 1024, activation = "relu") %>%
  layer_dense (units = 101, activation = "linear")

# freeze_weights(conv_base)


model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history <- model %>% fit(
  train_x, train_y,
  epochs = 20,
  batch_size = 16,
  validation_data = list(val_x, val_y)
)


test_y_pred <- model %>% predict(test_x)

# Plot performance -------------------------------------------------------------
plot (history)

par(mfrow=c(1,2))
matplot (t(test_y))
matplot (t(test_y_pred))

results_plot(test_y,
             test_y_pred,
             ynames = "Knee abd moment")


err <- all_measures(test_y, test_y_pred) %>%
  summarize_all (mean)
