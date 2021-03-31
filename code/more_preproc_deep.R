rm(list=ls())

#Load packages------------------------------------------------------------
# helper
library(tidyverse)

# Split data---------------------------------------------------------------

train <- readRDS("output/train_data.RDS")
test <- readRDS("output/test_data.RDS")

# Get variables---------------------------------------------------------------

response_vars <- names(train)[grep("grf|knee_moment", names(train))]
pred_vars <- names(train)[grep("angle|vel|accl", names(train))]

# Create arrays --------------------------------------------------------------

## Restructure train data

train_mat <- train[map_lgl (train, is.matrix)]
train_x <- train_mat[grepl ("accl|vel|angle", names (train_mat))]
train_y <- train_mat[grepl ("grf|knee_moment", names (train_mat))]
axes <- c("ap", "ml", "vt")
prednames <-unique (str_remove_all(pred_vars, "_ap|_ml|_vt"))
outnames <-unique (str_remove_all(response_vars, "_ap|_ml|_vt"))

### Restructure predictors

temp_array <- array (dim = c(nrow (train_x[[1]]), ncol (train_x[[1]]), 9))
train_x_array <- array (dim = c(nrow (train_x[[1]]), ncol (train_x[[1]]), 9, 3))

for (n in seq_along (axes)) {

  temp <- train_x[grepl (axes[n], names (train_x))]

  for (m in seq_along (temp)) {

    temp_array[, , m] <- temp[[m]]

  }

  train_x_array[, , , n] <- temp_array

}


### Restructure outcomes

temp_array <- array (dim = c(nrow (train_y[[1]]), ncol (train_y[[1]]), 2))
train_y_array <- array (dim = c(nrow (train_y[[1]]), ncol (train_y[[1]]), 2, 3))

for (n in seq_along (axes)) {

  temp <- train_y[grepl (axes[n], names (train_y))]

  for (m in seq_along (temp)) {

    temp_array[, , m] <- temp[[m]]

  }

  train_y_array[, , , n] <- temp_array

}

## Restructure test data

test_mat <- test[map_lgl (test, is.matrix)]
test_x <- test_mat[grepl ("accl|vel|angle", names (test_mat))]
test_y <- test_mat[grepl ("grf|knee_moment", names (test_mat))]

### Restructure predictors

temp_array <- array (dim = c(nrow (test_x[[1]]), ncol (test_x[[1]]), 9))
test_x_array <- array (dim = c(nrow (test_x[[1]]), ncol (test_x[[1]]), 9, 3))

axes <- c("ap", "ml", "vt")

for (n in seq_along (axes)) {

  temp <- test_x[grepl (axes[n], names (test_x))]

  for (m in seq_along (temp)) {

    temp_array[, , m] <- temp[[m]]

  }

  test_x_array[, , , n] <- temp_array

}

### Restructure outcomes

temp_array <- array (dim = c(nrow (test_y[[1]]), ncol (test_y[[1]]), 2))
test_y_array <- array (dim = c(nrow (test_y[[1]]), ncol (test_y[[1]]), 2, 3))

for (n in seq_along (axes)) {

  temp <- test_y[grepl (axes[n], names (test_y))]

  for (m in seq_along (temp)) {

    temp_array[, , m] <- temp[[m]]

  }

  test_y_array[, , , n] <- temp_array

}

# Rename dimensions of array

dimnames(train_x_array)[[3]] <- dimnames(test_x_array)[[3]] <- prednames
dimnames(train_y_array)[[3]] <- dimnames(test_y_array)[[3]] <- outnames

dimnames(train_x_array)[[4]] <-
  dimnames(test_x_array)[[4]] <-
  dimnames(train_y_array)[[4]] <-
  dimnames(test_y_array)[[4]] <- axes


save(test_x_array,
     test_y_array,
     train_x_array,
     train_y_array,
     file = "output/deep_data.RData")
