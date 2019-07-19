library(rio)
library(stringr)
library(tidyverse)

dat <- import("ESA_climate_sensitivity_2018_ad.xlsx")
names(dat) <- str_replace_all(names(dat), " |-", "_")
names(dat)[7] <- "Q_n"
names(dat)[9] <- "Q_response"
dat <- select(dat, -c(13, 14))
names(dat)

unique(dat$Scientific_Name) %>% length()

table(dat$Taxon)
dat$Taxon_orig <- dat$Taxon
dat$Taxon <- if_else(dat$Taxon == "Fish", "Vertebrate", dat$Taxon)
table(dat$Taxon)

table(dat$Subtaxon)
dat$Subtaxon <- if_else(dat$Subtaxon == "fish", "Fish", dat$Subtaxon)
dat$Subtaxon <- tolower(dat$Subtaxon)
table(dat$Subtaxon)

# WHAT REGION IS THIS??? It's not FWS regions...
table(dat$Lead_Region)
dat$Lead_Region <- ifelse(dat$Lead_Region == "N/A", NA, dat$Lead_Region)

state_exp <- dat$State %>% str_replace_all("\r\n", "") %>%
  str_replace_all("OH WI", "OH, WI") %>%
  str_replace_all("IN IA", "IN, IA") %>%
  str_replace_all("Ak", "AK") %>%
  str_replace_all("AL ", "AL") %>%
  str_split(",|, ") %>%
  unlist() %>%
  str_trim("both")
table(state_exp)
dat$state_2 <- lapply(dat$State, function(x) {
  x %>% str_replace_all("\r\n", "") %>%
    str_replace_all("OH WI", "OH, WI") %>%
    str_replace_all("IN IA", "IN, IA") %>%
    str_replace_all("Ak", "AK") %>%
    str_replace_all("AL ", "AL") %>%
    str_split(",|, ") %>%
    unlist() %>%
    str_trim("both")
})

table(dat$Q_n)
table(dat$Q_Text)

table(dat$Q_response)

# remove NA species
data <- filter(dat, !is.na(dat$Q_n))

# Split q11
nq_11 <- filter(data, Q_n != 11)
q11 <- filter(data, Q_n == 11)
q11_ans <- lapply(q11$Q_response, function(x) {
  case_when(
    x == "?" ~ "?",
    x == "n" ~ "n",
    x == "n/y" ~ "n",
    x == "y/n" ~ "y",
    TRUE ~ "y"
  )
}) %>% unlist()
q12_ans <- lapply(q11$Q_response, function(x) {
  case_when(
    x == "?" ~ "?",
    x == "n" ~ "n",
    x == "n/y" ~ "y",
    x == "y/n" ~ "n",
    TRUE ~ "y"
  )
}) %>% unlist()

q12_pre <- select(q11, c(1:6))
q12_pre$Q_n <- 12
q12_pre$Q_Text <- "Is climate change addressed with concrete actions in recovery materials?"
q12_pre$Q_response <- q12_ans
q12_pre$Explanation <- q11$Explanation
q12_pre$Source <- q11$Source
q12_pre$Other_info <- q11$Other_info
q12_pre$state_2 <- q11$state_2

q11_pre <- select(q11, c(1:6))
q11_pre$Q_n <- 11
q11_pre$Q_Text <- "Is climate change discussed as a threat?"
q11_pre$Q_response <- q11_ans
q11_pre$Explanation <- q11$Explanation
q11_pre$Source <- q11$Source
q11_pre$Other_info <- q11$Other_info
q11_pre$state_2 <- q11$state_2

q11_12 <- bind_rows(q11_pre, q12_pre)

final <- bind_rows(nq_11, q11_12)
table(final$Q_n)
final$combo_name <- paste0(final$Common_Name, " (", final$Scientific_Name, ")")
saveRDS(final, "data_clean.rds")
