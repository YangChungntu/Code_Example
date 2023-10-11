###############################################################################
# Summary Statistics for CP Patients' Siblings                                #
# And Control Groups                                                          #
#                                                                             #
# Last Modified: 2023-Oct-6                                                   #
###############################################################################
rm(list = ls()); gc()

library(data.table)
library(arrow)
library(MatchIt)
library(dplyr)
library(ggplot2)
###############################################################################
### PREAMBLE ##################################################################
###############################################################################
setwd("E:/H112057/yangc/CP")

source("E:/H112057/arrow/arrow_20230831/arrow_helpers.R")

Disk <- "E"
output.dir <- paste0(Disk, ":/H112057/yangc/CP/processed_data")
par.dir <- paste0(Disk, ":/H112057/parquet")
result.dir <- paste0(Disk, ":/H112057/yangc/CP/result")
CleanData.dir <- paste0(Disk, ":/H112057/CleanData/processed_data")
figure.dir <- paste0(result.dir, "/figure")
CPRelatedICD <- c(343, 3430, 3431, 3432, 3433, 3434, 3438, 3439, "G800", "G801", "G802", "G804", "G808", "G809")

### Simple Functions ##########################################################
print.time.diff <- function(now) { cat("\t",'time:',round(difftime(Sys.time(),now,units='mins'),2),"mins\n") }
start_timer <- function() { start_time <<- Sys.time() }
end_timer <- function() { end_time <<- Sys.time(); print(end_time - start_time) }


###############################################################################
### FUNCTIONS #################################################################
###############################################################################
read_AMT <- function(year) {
  ## Read the employment and income data of one year
  AMT <- open_dataset(paste0(CleanData.dir, "/annualFile_AMT", year, ".parquet")) %>% 
    collect() %>% 
    as.data.table()
  AMT[, year := year]
  return(AMT)
}

read_educ <- function(year) {
  ## Read the educational data of one year
  educ <- open_dataset(paste0(CleanData.dir, "/educ", year, ".parquet")) %>% 
    filter(valid) %>% 
    select(ID, education_year) %>% 
    collect() %>% 
    as.data.table()
  educ[, year := year]
  return(educ)
}


get_annual_employment_and_income <- function(y) {
  ## Generate summary statistics of employment and income data of one year
  message(paste("start year", y))
  AMT <- read_AMT(y)
  AMT_m <- merge(older_siblings[is.na(D_DATE) | death_year > y + 1911], AMT, all.x = TRUE, by = "ID")
  
  AMT_m[, FPAMT := FTAMT + PTAMT]
  AMT_m[, FPAMT := ifelse(is.na(FPAMT), 0, FPAMT)]
  AMT_m[, work := ifelse(FPAMT == 0, 0, 1)]
  AMT_m[, group := ifelse(CP == 1, "CP patients' siblings", "matched sample's siblings")]
  # employment rate by group
  ub <- y + 1911 - 24
  lb <- y + 1911 - 65
  annual_work <- AMT_m[birth_year < ub & birth_year > lb,
                       .(mean_work = mean(work), sd_work = sd(work), N = .N), by = c("CP", "ID_S")]
  annual_work[, se_work := sd_work / sqrt(N)]
  annual_work[, year := y]
  print(annual_work)
  
  annual_income <- AMT_m[birth_year < ub & birth_year > lb,
                         .(mean_income = mean(FPAMT), sd_income = sd(FPAMT), N = .N), by = c("CP", "ID_S")]
  annual_income[, se_income := sd_income / sqrt(N)]
  annual_income[, year := y]
  print(annual_income)
  
  return(list(annual_work, annual_income))
}

get_annual_marr <- function(y) {
  ## Generate summary statistics of marriage status of one year
  message(paste("start year", y))
  marriage_data <- unique(marr[year == y])
  marr_m <- merge(older_siblings[is.na(D_DATE) | death_year > year + 1911], marriage_data, all.x = TRUE, by = "ID")
  
  marr_m <- marr_m[!is.na(MARR)]
  marr_m[, married := ifelse(MARR == 2, 1, 0)]
  marr_m[, group := ifelse(CP == 1, "CP patients' siblings", "matched sample's siblings")]
  # marriage rate by group
  ub <- y + 1911 - 20
  annual_marr_rate <- marr_m[birth_year < ub,
                             .(mean_marr = mean(married), sd_marr = sd(married), N = .N),
                             by = c("CP", "ID_S")]
  annual_marr_rate[, se_marr := sd_marr / sqrt(N)]
  annual_marr_rate[, year := y]
  print(annual_marr_rate)
  
  return(annual_marr_rate)
}

get_annual_educ <- function(y) {
  ## Generate summary statistics of education level of one year
  message(paste("start year", y))
  educ_data <- read_educ(y)
  educ_m <- merge(older_siblings[is.na(D_DATE) | death_year > year + 1911], educ_data, all.x = TRUE, by = "ID")
  
  educ_m <- educ_m[!is.na(education_year)]
  educ_m[, group := ifelse(CP == 1, "CP patients' siblings", "matched sample's siblings")]
  # marriage rate by group
  ub <- y + 1911 - 25
  annual_educ <- educ_m[birth_year < ub,
                        .(mean_educ = mean(education_year), sd_educ = sd(education_year), N = .N),
                        by = c("CP", "ID_S")]
  annual_educ[, se_educ := sd_educ / sqrt(N)]
  annual_educ[, year := y]
  print(annual_educ)
  
  return(annual_educ)
}

###############################################################################
### MAIN ######################################################################
###############################################################################

## Read Samples
older_siblings <- open_dataset(paste0(output.dir, "/older_siblings.parquet")) %>% 
  collect() %>% 
  as.data.table()

##==========================================
## 1. birth year distribution ==============
##==========================================
older_siblings[, `:=`(birth_year = ID_BIRTHDAY %/% 10000,
                      patient_birth_year = patient_BD %/% 10000,
                      death_year = D_DATE %/% 10000)]
older_siblings[, age_diff := patient_birth_year - birth_year]

# Bar chart of patients' birth year distribution
patients_b_yr_distribution <- ggplot(older_siblings[patient_birth_year > 1949, .N, by = patient_birth_year],
                                     aes(x = patient_birth_year, y = N)) +
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Birth Year") +
  ylab("Count") +
  ggtitle("Distribution of CP patients' Birth Year") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  theme_bw()

patients_b_yr_distribution
ggsave(file.path(figure.dir, "patients_b_yr_distribution.png"), patients_b_yr_distribution, width = 6, height = 4, dpi = 300)

# Bar chart of patients' siblings' birth year distribution
b_yr_distribution <- ggplot(older_siblings[birth_year > 1945, .N, by = birth_year],
                            aes(x = birth_year, y = N)) +
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Birth Year") +
  ylab("Count") +
  ggtitle("Distribution of Older Siblings Birth Year") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  theme_bw()

b_yr_distribution
ggsave(file.path(figure.dir, "b_yr_distribution.png"), b_yr_distribution, width = 6, height = 4, dpi = 300)


# The difference in the birth year of each patient and their siblings
age_diff_distribution <- ggplot(older_siblings[age_diff <= 20, .N, by = age_diff],
                                aes(x = age_diff, y = N)) +
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Difference in Birth Year") +
  ylab("Count") +
  ggtitle("Distribution of Difference in Birth Year") +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  theme_bw()

age_diff_distribution
ggsave(file.path(figure.dir, "age_diff_distribution.png"), age_diff_distribution, width = 6, height = 4, dpi = 300)


##==========================================
## 2. education ============================
##==========================================
highest_educ <- open_dataset(paste0(CleanData.dir, "/highest_educ.parquet")) %>% 
  filter(!is.na(ID) & ID != "") %>% 
  collect() %>%
  as.data.table()

# Generate a file recording the summary statistics each year
educ_annual_list <- lapply((89: 110), get_annual_educ)
annual_educ <- rbindlist(educ_annual_list)
fwrite(annual_educ, paste0(result.dir, "/annual_educ.csv"))


##==========================================
## 3. employment ===========================
##==========================================
employment_annual_list <- list()
income_annual_list <- list()

for (year in 89: 110) {
  message(paste("start year", year))
  result <- get_annual_employment_and_income(year)
  employment_annual_list <- c(employment_annual_list, list(result[[1]]))
  income_annual_list <- c(income_annual_list, list(result[[2]]))
}

annual_employment <- rbindlist(employment_annual_list)
annual_income <- rbindlist(income_annual_list)

fwrite(annual_employment, paste0(result.dir, "/annual_employment.csv"))
fwrite(annual_income, paste0(result.dir, "/annual_income.csv"))

##==========================================
## 4. marriage =============================
##==========================================
marr <- open_dataset(paste0(CleanData.dir, "/marriage_long.parquet")) %>% 
  collect() %>% 
  as.data.table()

marriage_annual_list <- lapply((89: 110), get_annual_marr)
annual_marriage <- rbindlist(marriage_annual_list)
fwrite(annual_marriage, paste0(result.dir, "/annual_marriage.csv"))
