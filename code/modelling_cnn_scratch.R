rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)
library (reticulate)

# modelling
library (keras)
library(tensorflow)

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

cnn1 <- cnn_block(filters = 16, kernel_size = c(3,3), pool_size = c(3,3), rate = 0.25,
                  shape(wid, ht, 3))
cnn2 <- cnn_block(filters = 32, kernel_size = c(3,3), pool_size = c(2,2), rate = 0.25)
cnn3 <- cnn_block(filters = 32, kernel_size = c(3,3), pool_size = c(2,2), rate = 0.25)

model <- keras_model_sequential() %>% 
  cnn1() %>% 
  cnn2() %>% 
  cnn3() %>% 
  # branch end
  layer_flatten() %>% 
  layer_dense(256) %>% 
  layer_activation(activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(101)


model %>% compile(
  optimizer = optimizer_adam(lr = 0.0001),
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
    ),
    callback_early_stopping(patience = 15)
  )
)


test_y_pred <- model %>% predict(test_x)

# Plot performance -------------------------------------------------------------
# plot (history)

par(mfrow=c(1,2))
matplot (t(test_y), type = "l")
matplot (t(test_y_pred), type = "l")

results_plot(test_y,
             test_y_pred,
             ynames = "Knee abd moment")


err <- all_measures(test_y, test_y_pred) %>%
  summarize_all (mean)
