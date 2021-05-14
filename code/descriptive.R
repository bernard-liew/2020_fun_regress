library (tidyverse)
library (officer)
library (flextable)
library (qwraps2)

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
