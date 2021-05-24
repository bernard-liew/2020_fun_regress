# load data


rm (list = ls())
library (tidyverse)
library (officer)
library (flextable)
library (qwraps2)
library (cowplot)

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


df_demo <- df_mean [1:8] %>%
  bind_cols() %>%
  mutate (study = ifelse (grepl("fukuchi", id), "fukuchi",
                          ifelse (grepl ("study1", id), "study1",
                                  ifelse (grepl ("study2_pre", id), "study2pre", "study2post")))) %>%
  distinct(study, id, age, ht, wt, sex)


study_id <- df_demo %>%
  #distinct (study, id) %>%
  group_by (study, id) %>%
  tally ()



our_summary1 <-
  list("Age (yo)" =
         list("mean (sd)" = ~ qwraps2::mean_sd(age, denote_sd = "paren")),
       "Height (m)" =
         list("mean (sd)" = ~ qwraps2::mean_sd(ht, denote_sd = "paren")),
       "Mass (kg)" =
         list("mean (sd)" = ~ qwraps2::mean_sd(wt, denote_sd = "paren")),
       "Sex" =
         list("Female" = ~ qwraps2::n_perc0(sex == "f"),
              "Male"  = ~ qwraps2::n_perc0(sex == "m"))
       )

by_cyl <- summary_table(df_demo %>%
                          group_by(study), our_summary1) %>%
  as.data.frame()

by_cyl$Variables <-  c("Age (years)",
                       "Height (m)",
                       "Mass (kg)",
                       "Sex-F",
                       "Sex-M")

by_cyl <- dplyr::select (by_cyl, Variables, everything ())


my_path <- paste0("manuscript/table_1",
                  "summary",
                  ".docx")

ft <- flextable(by_cyl ) %>%
  autofit()

my_doc <- read_docx()  %>%
  body_add_flextable(ft) %>%
  body_end_section_landscape()

print (my_doc, target = my_path)

# Plot

var_plot <- grepl ("ankle|knee|hip", names (df_mean))

df_plot_mean <- df_mean [var_plot] %>%
  map (~apply (.x, 2, mean)) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -cycle,
               names_to = "var",
               values_to = "Mean")

df_plot_sd <- df_mean [var_plot] %>%
  map (~apply (.x, 2, sd)) %>%
  bind_cols() %>%
  mutate (cycle = 1:101) %>%
  pivot_longer(cols = -cycle,
               names_to = "var",
               values_to = "Sd")

df_plot <- df_plot_mean %>%
  inner_join(df_plot_sd, by = c("cycle", "var"))

# Relvel and rename

newLvls <- c("ankle_angle_ap", "ankle_angle_ml",  "ankle_angle_vt",
             "ankle_vel_ap",    "ankle_vel_ml",    "ankle_vel_vt",
             "ankle_accl_ap","ankle_accl_ml","ankle_accl_vt",
             "ankle_moment_ap", "ankle_moment_ml", "ankle_moment_vt",
             # knee
             "knee_angle_ap", "knee_angle_ml",  "knee_angle_vt",
             "knee_vel_ap",    "knee_vel_ml",    "knee_vel_vt",
             "knee_accl_ap","knee_accl_ml","knee_accl_vt",
             "knee_moment_ap", "knee_moment_ml", "knee_moment_vt",
             # hip
             "hip_angle_ap", "hip_angle_ml",  "hip_angle_vt",
             "hip_vel_ap",    "hip_vel_ml",    "hip_vel_vt",
             "hip_accl_ap","hip_accl_ml","hip_accl_vt",
             "hip_moment_ap", "hip_moment_ml", "hip_moment_vt")

newLbls <- c("Ankle Inv(+)/Eve(-) angle (deg)", "Ankle DF(+)/PF(-) angle (deg)",  "Ankle Add(+)/Abd(-) angle (deg)",
             "Ankle Inv(+)/Eve(-) vel (deg/s)", "Ankle DF(+)/PF(-) vel (deg/s)",  "Ankle Add(+)/Abd(-) vel (deg/s)",
             "Ankle Inv(+)/Eve(-) acc (deg/s2)", "Ankle DF(+)/PF(-) acc (deg/s2)",  "Ankle Add(+)/Abd(-) acc (deg/s2)",
             "Ankle Inv(+)/Eve(-) moment (Nm/kg)", "Ankle DF(+)/PF(-) moment (Nm/kg)",  "Ankle Add(+)/Abd(-) moment (Nm/kg)",
             # Knee
             "Knee Inv(+)/Eve(-) angle (deg)", "Knee DF(+)/PF(-) angle (deg)",  "Knee Add(+)/Abd(-) angle (deg)",
             "Knee Inv(+)/Eve(-) vel (deg/s)", "Knee DF(+)/PF(-) vel (deg/s)",  "Knee Add(+)/Abd(-) vel (deg/s)",
             "Knee Inv(+)/Eve(-) acc (deg/s2)", "Knee DF(+)/PF(-) acc (deg/s2)",  "Knee Add(+)/Abd(-) acc (deg/s2)",
             "Knee Inv(+)/Eve(-) moment (Nm/kg)", "Knee DF(+)/PF(-) moment (Nm/kg)",  "Knee Add(+)/Abd(-) moment (Nm/kg)",
             # hip
             "Hip Inv(+)/Eve(-) angle (deg)", "Hip DF(+)/PF(-) angle (deg)",  "Hip Add(+)/Abd(-) angle (deg)",
             "Hip Inv(+)/Eve(-) vel (deg/s)", "Hip DF(+)/PF(-) vel (deg/s)",  "Hip Add(+)/Abd(-) vel (deg/s)",
             "Hip Inv(+)/Eve(-) acc (deg/s2)", "Hip DF(+)/PF(-) acc (deg/s2)",  "Hip Add(+)/Abd(-) acc (deg/s2)",
             "Hip Inv(+)/Eve(-) moment (Nm/kg)", "Hip DF(+)/PF(-) moment (Nm/kg)",  "Hip Add(+)/Abd(-) moment (Nm/kg)")

p <- df_plot %>%
  mutate (var = factor (var, levels = newLvls, labels = newLbls)) %>%
  ggplot () +
  geom_line(aes(x = cycle, y = Mean), size = 1) +
  geom_ribbon(aes(x = cycle, ymin = Mean - Sd, ymax = Mean + Sd), alpha = 0.2) +
  facet_wrap(~ var, ncol = 3, scales = "free") +
  ylab ("Variables") +
  xlab ("Cycle (100%)") +
  theme_cowplot() +
  theme (text = element_text(size=16))

pdf ("manuscript/sm_fig1.pdf", width = 15, height = 20)
p
dev.off ()
