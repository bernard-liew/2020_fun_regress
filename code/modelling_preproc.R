# data preproc
library(tidyverse)
library(purrr)

# load data
rm (list = ls())

df <- readRDS("output/data_4sets.RDS")

## -------------------------------------------------------------------------------------

## CHECK: df$cycle has time point 43 at the end??
cycle <- 1:101

rm_obs <- which (df$cond == "(.10)50")

df <- df %>%
  map_if (is.matrix, ~.[-rm_obs,]) %>%
  map_if (is.vector, ~.[-rm_obs])


df$cond[df$cond == "runt30"] <- "runt35"

df_mean <- df[-which(names(df)=="cycle")] %>% lapply(., function(x){

  ret <- aggregate(as.data.frame(x), by = list(df$subj, df$cond),
                   FUN = function(x) if(is.numeric(x)) mean(x) else x[1])[,-1*(1:2)]
  if(NCOL(ret)>1)
    ret <- as.matrix(ret)
  return(ret)

})

df_mean$cycle <- cycle
names(df_mean)[1] <- "id"
df_mean$study <- as.factor(gsub("(.*)_(.*)","\\1",df_mean$id))

df_id_study <- df_mean[c("id","study")] %>%
  as.data.frame()%>%
  mutate (id = factor (id))

df_id_study %>%
  group_by(study) %>%
  summarize(number_subjects = length(unique(id)))

## Define train and test (80/20 as in Johnson) -------------------------------------------

set.seed(2020-5-4)

train_ids <- df_id_study %>%
  mutate_all (as.character) %>%
  unique %>%
  group_by(study) %>%
  sample_frac(0.8) %>%
  ungroup() %>%
  purrr::pluck("id")

test_ids <- setdiff(levels(df_id_study$id), train_ids)

ind_train <- sapply(df_id_study$id, function(id) id %in% train_ids)
ind_test <- sapply(df_id_study$id, function(id) id %in% test_ids)

train_dat <-  df_mean %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_train]) %>%
  map_if (is.matrix,  ~ as.matrix(.[ind_train,]))

test_dat <-  df_mean %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_test]) %>%
  map_if (is.matrix,  ~ as.matrix(.[ind_test,]))

split_set <- ifelse (df_mean$id %in% train_ids, "train", "test")

df_mean$split_set <- split_set


saveRDS(df_mean, file="output/data_comb.RDS")

## Scale for pffr and deep learning-------------------------------------------------------------------------

get_mean <- train_dat %>%
  map_if (grepl("ankle|knee|hip", names (train_dat)) & !grepl("grf|moment", names (train_dat)),
          apply, 2, mean) %>%
  map_if (grepl("ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)), mean)

get_sd <- train_dat %>%
  map_if (grepl("ankle|knee|hip", names (train_dat)) & !grepl("grf|moment", names (train_dat)),
          apply, 2, sd) %>%
  map_if (grepl("ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)), sd)


for (n in which (grepl("ankle|knee|hip|ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)))) {

  train_dat[[n]] <- scale (train_dat[[n]], center = get_mean[[n]], scale = get_sd[[n]])
  test_dat[[n]] <- scale (test_dat[[n]], center = get_mean[[n]], scale = get_sd[[n]])

}

for (n in which (grepl("ht|wt|age", names (train_dat)))) {

  train_dat[[n]] <- as.numeric (train_dat[[n]])
  test_dat[[n]] <- as.numeric (test_dat[[n]])

}

# Ensure levels of factors same across datasets
levels (train_dat$id) <- levels (test_dat$id) <- levels (df_mean$id)
levels (train_dat$sex) <- levels (test_dat$sex) <- levels (df_mean$sex)
levels (train_dat$cond) <- levels (test_dat$cond) <- levels (df_mean$cond)
levels (train_dat$study) <- levels (test_dat$study) <- levels (df_mean$study)
train_dat$cycle <- test_dat$cycle <- 1:101 # just in case


saveRDS(train_dat, file="output/train_data.RDS")
saveRDS(test_dat, file="output/test_data.RDS")


## Define train and test (80/20), lag split -------------------------------------------

set.seed(2020-5-4)
df_id_study <- df_id_study %>%
  mutate (id = as.character(id)) %>%
  rowid_to_column()

train <- df_id_study %>%
  group_by(id) %>%
  sample_frac (0.8, replace = FALSE)

test <- df_id_study %>%
  anti_join(train)


ind_train <- train$rowid
ind_test <- test$rowid

train_dat <-  df_mean %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_train]) %>%
  map_if (is.matrix,  ~ as.matrix(.[ind_train,]))

test_dat <-  df_mean %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_test]) %>%
  map_if (is.matrix,  ~ as.matrix(.[ind_test,]))

## Scale for pffr and deep learning-------------------------------------------------------------------------

get_mean <- train_dat %>%
  map_if (grepl("ankle|knee|hip", names (train_dat)) & !grepl("grf|moment", names (train_dat)),
          apply, 2, mean) %>%
  map_if (grepl("ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)), mean)

get_sd <- train_dat %>%
  map_if (grepl("ankle|knee|hip", names (train_dat)) & !grepl("grf|moment", names (train_dat)),
          apply, 2, sd) %>%
  map_if (grepl("ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)), sd)


for (n in which (grepl("ankle|knee|hip|ht|wt|age", names (train_dat)) & !grepl("grf|moment", names (train_dat)))) {

  train_dat[[n]] <- scale (train_dat[[n]], center = get_mean[[n]], scale = get_sd[[n]])
  test_dat[[n]] <- scale (test_dat[[n]], center = get_mean[[n]], scale = get_sd[[n]])

}

for (n in which (grepl("ht|wt|age", names (train_dat)))) {

  train_dat[[n]] <- as.numeric (train_dat[[n]])
  test_dat[[n]] <- as.numeric (test_dat[[n]])

}

# Ensure levels of factors same across datasets
df_mean <- df_mean %>%
  map_if (is.character, factor)

train_dat$id <- factor (train_dat$id, levels = levels (df_mean$id))
train_dat$cond <- factor (train_dat$cond, levels = levels (df_mean$cond))
train_dat$sex <- factor (train_dat$sex, levels = levels (df_mean$sex))
train_dat$side <- factor (train_dat$side, levels = levels (df_mean$side))
train_dat$study <- factor (train_dat$study, levels = levels (df_mean$study))

test_dat$id <- factor (test_dat$id, levels = levels (df_mean$id))
test_dat$cond <- factor (test_dat$cond, levels = levels (df_mean$cond))
test_dat$sex <- factor (test_dat$sex, levels = levels (df_mean$sex))
test_dat$side <- factor (test_dat$side, levels = levels (df_mean$side))
test_dat$study <- factor (test_dat$study, levels = levels (df_mean$study))

train_dat$cycle <- test_dat$cycle <- 1:101 # just in case



saveRDS(train_dat, file="output/train_lagdata.RDS")
saveRDS(test_dat, file="output/test_lagdata.RDS")
