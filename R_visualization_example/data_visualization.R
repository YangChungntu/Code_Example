###############################################################################
# Generate Plots for Summary Statistics for CP Patients' Siblings             #
# And Control Groups                                                          #
#                                                                             #
# Last Modified: 2023-Oct-6                                                   #
###############################################################################
library(data.table)
library(ggplot2)

###############################################################################
### PREAMBLE ##################################################################
###############################################################################
rm(); gc()
setwd("~/Desktop/CCL/CP")

results.dir <- "results/figures"

###############################################################################
### FUNCTIONS #################################################################
###############################################################################
add_group_names <- function(dt) {
  ## Add group names
  dt <- dt %>%
    mutate(group = case_when(CP == 1 & ID_S == 1 ~ "Sample (Male)",
                             CP == 1 & ID_S == 2 ~ "Sample (Female)",
                             CP == 0 & ID_S == 1 ~ "Control (Male)",
                             CP == 0 & ID_S == 2 ~ "Control (Female)"))
  return(dt)
}

rename_cols <- function(dt) {
  ## Rename columns
  names(dt) <- c("CP", "ID_S", "mean", "sd", "N", "se", "year")
  return(dt)
}

combine_gender <- function(dt) {
  ## Calculate the summary statistics of the whole group with
  ## the summary statistics of male and female
  processed_dt <- dt %>%
    group_by(year, CP) %>%
    summarize(
      TotalSampleSize = sum(N),
      WeightedMean = sum(mean * N) / sum(N)) %>% 
    merge(dt, by = c("year", "CP")) %>% 
    group_by(year, CP) %>%
    summarise(
      sd = sqrt(sum((N - 1)*sd^2 + N*(mean - WeightedMean)^2) / (TotalSampleSize - 1)),
      TotalSampleSize = sum(N),
      WeightedMean = sum(mean * N) / sum(N)
    ) %>% 
    distinct() %>% 
    mutate(se = sd / sqrt(TotalSampleSize),
           group = ifelse(CP == 1, "Sample", "Control")) %>% 
    rename(mean = WeightedMean,
           N = TotalSampleSize)
  # processed_dt <- rbind(dt, processed_dt, fill = TRUE)
  return(processed_dt)
}

draw_trend_four_groups <- function(dt, y_name) {
  ## Draw line plot showing the trend of an interested variable.
  ## The plots contains four lines:
  ## Sample (Male)
  ## Sample (Female)
  ## Control (Male)
  ## Control (Female)
  figure <- ggplot(dt, aes(x = year, y = mean, color = group)) +
    geom_line() +  # Add lines
    geom_point() +  # Add points
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.1) +  # Add error bars
    labs(x = "Year", y = y_name, title = paste("trend of", y_name)) +
    scale_color_manual(values = c("Sample (Male)" = "#00ABFD",
                                  "Sample (Female)" = "blue4",
                                  "Control (Male)" = "#F8766D",
                                  "Control (Female)" = "darkred")) +
    theme_bw()
  
  return(figure)
}

draw_trend_two_groups <- function(dt, y_name) {
  ## Draw line plot showing the trend of an interested variable.
  ## The plots contains two lines:
  ## Sample
  ## Control
  figure <- ggplot(dt, aes(x = year, y = mean, color = group)) +
    geom_line() +  # Add lines
    geom_point() +  # Add points
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.1) +  # Add error bars
    labs(x = "Year", y = y_name, title = paste("trend of", y_name)) +
    scale_color_manual(values = c("Sample" = "blue",
                                  "Control" = "red")) +
    theme_bw()
  
  return(figure)
}

draw_trend_N <- function(dt) {
  ## Draw sample size over years
  figure <- ggplot(dt, aes(x = year, y = N, color = group)) +
    geom_line() +  # Add lines
    geom_point() +  # Add points
    labs(x = "Year", y = "N", title = "Trend of N by group") +
    scale_color_manual(values = c("Sample (Male)" = "#00ABFD",
                                  "Sample (Female)" = "blue4",
                                  "Control (Male)" = "#F8766D",
                                  "Control (Female)" = "darkred")) +
    theme_bw()
  
  return(figure)
}
###############################################################################
### Input #####################################################################
###############################################################################
annual_employment <- fread("NHI/annual_employment.csv")
annual_income <- fread("NHI/annual_income.csv")
annual_educ <- fread("NHI/annual_educ.csv")
annual_marriage <- fread("NHI/annual_marriage.csv")
annual_depression_all <- fread("NHI/annual_depression.csv")
annual_depression <- annual_depression_all %>% select(CP, ID_S, mean_depression, sd_depression, N, se_depression, year)
annual_depression_count <- annual_depression_all %>% select(CP, ID_S, mean_depression_count, sd_depression_count, N, se_depression_count, year)


annual_employment <- rename_cols(annual_employment) %>% add_group_names()
annual_income <- rename_cols(annual_income) %>% add_group_names()
annual_educ <- rename_cols(annual_educ) %>% add_group_names()
annual_marriage <- rename_cols(annual_marriage) %>% add_group_names()
annual_depression <- rename_cols(annual_depression) %>% add_group_names()
annual_depression_count <- rename_cols(annual_depression_count) %>% add_group_names()

###############################################################################
### GENERATING PLOTS ##########################################################
###############################################################################

##=============================================================================
## Four Groups ================================================================
##=============================================================================

## Employment =================================================================
figure <- draw_trend_four_groups(annual_employment, "employment")
figure
ggsave(file.path(results.dir, "employment_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_employment)

## Income =====================================================================
figure <- draw_trend_four_groups(annual_income, "income")
figure
ggsave(file.path(results.dir, "income_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_income)
## Education ==================================================================
figure <- draw_trend_four_groups(annual_educ, "year of education")
figure
ggsave(file.path(results.dir, "educ_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_educ)
## Marriage rate ==============================================================
figure <- draw_trend_four_groups(annual_marriage, "marriage rate")
figure
ggsave(file.path(results.dir, "marr_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_marriage)
## Depression rate ============================================================
figure <- draw_trend_four_groups(annual_depression, "depression rate")
figure
ggsave(file.path(results.dir, "depression_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_depression)
## Depression count ===========================================================
figure <- draw_trend_four_groups(annual_depression_count, "depression count")
figure
ggsave(file.path(results.dir, "depression_rate_trend_four_groups.png"), figure, width = 6, height = 4, dpi = 300)

draw_trend_N(annual_depression_count)

##=============================================================================
## Two Groups =================================================================
##=============================================================================

### Combine (Ignore) gender ===================================================
annual_employment <- annual_employment %>% combine_gender()
annual_income <- annual_income %>% combine_gender()
annual_educ <- annual_educ %>% combine_gender()
annual_marriage <- annual_marriage %>% combine_gender()
annual_depression <- annual_depression %>% combine_gender()
annual_depression_count <- annual_depression_count %>% combine_gender()


## Employment =================================================================
figure <- draw_trend_two_groups(annual_employment, "employment")
figure
ggsave(file.path(results.dir, "employment_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)

## Income =====================================================================
figure <- draw_trend_two_groups(annual_income, "income")
figure
ggsave(file.path(results.dir, "income_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)

## Education ==================================================================
figure <- draw_trend_two_groups(annual_educ, "year of education")
figure
ggsave(file.path(results.dir, "educ_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)

## Marriage rate ==============================================================
figure <- draw_trend_two_groups(annual_marriage, "marriage rate")
figure
ggsave(file.path(results.dir, "marr_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)

## Depression rate ============================================================
figure <- draw_trend_two_groups(annual_depression, "depression rate")
figure
ggsave(file.path(results.dir, "depression_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)

## Depression count ===========================================================
figure <- draw_trend_two_groups(annual_depression_count, "depression count")
figure
ggsave(file.path(results.dir, "depression_rate_trend_two_groups.png"), figure, width = 6, height = 4, dpi = 300)
