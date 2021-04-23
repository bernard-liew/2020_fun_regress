wide_2_long <- function (x) {

  x %>%
    pivot_longer(-ITEM,
                 names_to = c(".value", "step"),
                 names_pattern = "(.).(.*)",
                 values_drop_na = TRUE) %>%
    pivot_longer(cols = X:Z,
                 names_to = "axis",
                 values_to = "value") %>%
    rename_all (~tolower (.)) %>%
    mutate (axis = ifelse (axis == "X", "ap",
                           ifelse (axis == "Y", "vt", "ml")))
}

keep_cols <- function (x) {

  x[,names (x) %in% c(1:101)] %>%
    as.data.frame() %>%
    as.matrix()
}

return_na_rows <- function (x) {

  which (!complete.cases(x) == TRUE)

}

my_outlier <- function (x) {

  Data <- t(x)
  Data = array (Data, dim = c(nrow (Data), ncol(Data), 1))
  Result <- fOutl(Data, type = "fDO", diagnostic = TRUE)
  out <- Result$locOutlX == 1
  outData <- x
  outData[out] <- NA

  return(outData)

}

wide_2_longer <- function (x) {

  x %>%
    pivot_longer(cols = -ITEM,
                 names_to = "axis2",
                 values_to = "value") %>%
    separate(col = "axis2",
             into = c("axis", "step"),
             sep = "_", fill = "right") %>%
    mutate (step = replace_na(step, 0) %>% as.numeric) %>%
    rename_all (~tolower (.))
}

empty_2_long <- function (x) {

  x <-  data.frame (item = c (1:101),
                           axis = NA,
                           step = NA,
                           value = NA)

  return (x)

}

modify_step <- function (subj, cond) {

  temp <- data.frame(subj = subj,
                     cond = cond,
                     step2 = 1) %>%
    group_by(subj, cond) %>%
    mutate(step = cumsum(step2))

  step <- as.vector(temp$step)

  return (step)

}

# Plot predicted vs observed ------------------------------------------

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
    geom_ribbon(aes(x = cycle, ymin = Mean - Sd, ymax = Mean + Sd, fill = var), alpha = 0.2) +
    scale_color_manual(values = c("black", "red")) +
    scale_fill_manual(values = c("black", "red")) +
    guides(fill = "none") +
    ylab (ynames) +
    xlab ("Cycle (100%)") +
    theme_bw() +
    theme (text = element_text(size=16))



}
