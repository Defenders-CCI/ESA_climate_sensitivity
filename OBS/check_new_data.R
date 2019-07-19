library(dplyr)
library(stringr)

dat <- readRDS("climate_sensitivity_2018-11-6.rds")
names(dat)
hd <- head(dat, 12)

old <- readRDS("data_clean.rds")
names(old)
hdo <- head(old, 12)

names(dat)[1:12] <- names(old)[1:12]

# Set aside new cols to re-bind later
new_col <- select(dat, Factor, Sens.Discuss)
dat_bak <- dat
dat <- select(dat, -Factor, -Sens.Discuss)

######################
## clean up variables, including factor --> char conversion

# taxa
table(dat$Taxon)
table(old$Taxon)
is.factor(dat$Taxon)
dat$Taxon <- as.character(dat$Taxon)
table(dat$Taxon)

# subtaxa
table(dat$Subtaxon)
is.factor(dat$Subtaxon)
dat$Subtaxon <- as.character(dat$Subtaxon)

# regions
table(dat$Lead_Region)
is.factor(dat$Lead_Region)
dat$Lead_Region <- as.character(dat$Lead_Region)
dat$Lead_Region <- gsub(dat$Lead_Region,
                        pattern = "NMFS: Ocean",
                        replacement = "NMFS: Marine")
dat$Lead_Region <- gsub(dat$Lead_Region,
                        pattern = "FWS Region 6: Mountain Prairie",
                        replacement = "FWS Region 6: Mountain-Prairie")

# states
is.factor(dat$State)
table(dat$State)  # 153 levels, spaces at end of str, etc.
dat$State <- as.character(dat$State)
dat$State <- str_trim(dat$State)   # Now 142
dat$State_ls <- lapply(
  dat$State,
  function(x) {
    str_split(x, ", |,|\\ ")   # mix of sep
  }
)
unlist(dat$State_ls) %>% table()
unlist(dat$State_ls) %>% table() %>% length()

# combo name
dat$Common_Name <- str_trim(dat$Common_Name)
dat$Scientific_Name <- str_trim(dat$Scientific_Name)
dat$combo_name <- str_c(dat$Common_Name, " (", dat$Scientific_Name, ")")
head(dat$combo_name, 30)

###################
# re-bind cols and save
dat_clean <- dat
dat <- bind_cols(dat, new_col)
names(dat)[16] <- "Sens_Discuss"
dat$Sens_Discuss <- as.character(dat$Sens_Discuss)

saveRDS(dat, "final_data_2018-11-24.rds")
