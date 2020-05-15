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
  as.data.frame()

df_id_study %>%
  group_by(study) %>%
  summarize(number_subjects = length(unique(id)))

## Define train and test (80/20 as in Johnson)

set.seed(2020-5-4)

train_ids <- df_id_study %>%
  unique %>%
  group_by(study) %>%
  sample_frac(0.8) %>%
  ungroup() %>%
  dplyr::select(id)

test_ids <- setdiff(levels(df_id_study$id), train_ids$id)

ind_train <- sapply(df_id_study$id, function(id) id %in% train_ids$id)
ind_test <- sapply(df_id_study$id, function(id) id %in% test_ids)

df.scale <- df_mean %>%
  map_if (grepl("ankle|knee|hip", names (df_mean)) & !grepl("grf|moment", names (df_mean)),
          ~ scale(.[ind_train,], center = TRUE, scale = FALSE)) %>%
  map_if (grepl("grf|moment", names (df_mean)), ~ as.matrix(.[ind_train,])) %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_train])

df.scale_test <- df_mean %>%
  map_if (grepl("ankle|knee|hip", names (df_mean)) & !grepl("grf|moment", names (df_mean)),
          ~ scale(.[ind_test,], center = TRUE, scale = FALSE)) %>%
  map_if (grepl("grf|moment", names (df_mean)), ~ as.matrix(.[ind_test])) %>%
  map_if (~is.null(dim(.)) & length(.) != 101, ~ .[ind_test])

rm(df)
rm(df_mean)
gc()

df.scale$id <- as.factor(df.scale$id)
df.scale_test$id <- as.factor(df.scale_test$id)
df.scale$cond <- as.factor(df.scale$cond)
df.scale_test$cond <- as.factor(df.scale_test$cond)
df.scale$sex <- as.factor(df.scale$sex)
df.scale_test$sex <- as.factor(df.scale_test$sex)

train_cen <- df.scale %>% map (attr, "scaled:center")
test_cen <- df.scale_test %>% map (attr, "scaled:center")

scales <- list (train_cen = train_cen,
                test_cen = test_cen)

saveRDS(df.scale, file="output/train_data.RDS")
saveRDS(df.scale, file="output/test_data.RDS")
saveRDS(scales, file="output/scales.RDS")
