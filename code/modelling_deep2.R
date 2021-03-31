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

# Load data---------------------------------------------------------------

load ("output/deep_data.RData")

# Load pretrained image model ---------------------------------------------------------------
wid <- 150
ht <- 150


conv_base <- application_densenet121(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(wid, ht, 3)
)

# Resize  input using cubic interpolation [5]

train_x <- resize(train_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()
test_x <- resize(test_x_array, size_y = wid, size_z = ht, interpolation_type = 5) %>% as.array()

# Select outcomes ---------------------------------------------------------

outname <- "knee_moment"
axes <- "ap"
train_y <- train_y_array[,,outname, axes]
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
  epochs = 100,
  batch_size = 8,
  validation_data = list(test_x, test_y)
)


test_y_pred <- model %>% predict(test_x)

# Plot performance -------------------------------------------------------------
plot (history)

par(mfrow=c(1,2))
matplot (t(test_y))
matplot (t(test_y_pred))
