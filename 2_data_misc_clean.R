
dat <- readRDS("data/sensitivity_data_prepped.rds")

dat$Common_Name <- str_trim(dat$Common_Name)
dat$combo_name <- str_c(dat$Common_Name, " (", dat$Scientific_Name, ")")
dat$Sens_Disc <- ifelse(is.na(dat$Sens_Disc), "y", dat$Sens_Disc)
dat$State <- str_replace_all(dat$State, "Ak", "AK")
species <- unique(dat$combo_name)

n_sens_fact <- function(df) {
  hits <- lapply(species, function(x) {
    sub <- filter(df, combo_name == x)
    return(sum(sub$Sens_Disc == "y"))
  }) %>% unlist()
  n_sens <- tibble(species = species, n_sens_factors = hits)
  new_join <- left_join(n_sens, dat, by = c("species" = "combo_name"))
  return(new_join)
}

new_dat <- n_sens_fact(dat)

saveRDS(new_dat, file = "data/sensitivity_data_prepped_v2.rds")

dis <- readRDS("data/discussion_data_prepped.rds")
dis$Common_Name <- str_trim(dis$Common_Name)
dis$combo_name <- str_c(dis$Common_Name, " (", dis$Scientific_Name, ")")
dis$Discuss_Level <- as.character(dis$Discuss_Level)
#dis$Discuss_Level <- if_else(
#  dis$Discuss_Level == "Not expected (newly listed)",
#  "Excluded from analysis",
#  dis$Discuss_Level
#)
#dis$Discuss_Level <- as.factor(dis$Discuss_Level)
dis$Discuss_Level <- if_else(
  dis$Discuss_Level == "No threat, no action needed",
  "No threat,\nno action needed",
  dis$Discuss_Level
)
dis$State <- str_replace_all(dis$State, "Ak", "AK")


saveRDS(dis, file = "data/discussion_data_prepped_v2.rds")

