rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)
library (reticulate)

# modelling
library (keras)

# image
library (EBImage)

# set python environment
use_condaenv("tf-gpu")

# Load data---------------------------------------------------------------

load ("output/deep_data.RData")

# Load pretrained image model ---------------------------------------------------------------
wid <- 224
ht <- 224


conv_base <- application_densenet121(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(wid, ht, 3)
)

# Resize  input

train_x <- array (dim = c(nrow (train_x_array), wid, ht, 3))
test_x<- array (dim = c(nrow (test_x_array), wid, ht, 3))

for (n in 1: nrow (train_x_array)) {

  train_x[n, , , ] <- resize (train_x_array[n, , , ], w = wid, h = ht)

}

for (n in 1: nrow (test_x_array)) {

  test_x[n, , , ] <- resize (test_x_array[n, , , ], w = wid, h = ht)

}

# Select outcomes ---------------------------------------------------------

outname <- "com_grf"
axes <- "vt"
train_y <- train_y_array[,,outname, axes]
test_y <- test_y_array[,,outname, axes]

# Create model --------------------------------------------------------------

model <- keras_model_sequential() %>%
  conv_base %>%
  layer_max_pooling_2d(pool_size = c(6, 6)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense (units = 1024, activation = "linear") %>%
  layer_dense (units =1024, activation = "relu") %>%
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
