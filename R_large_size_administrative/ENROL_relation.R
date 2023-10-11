###############################################################################
# Identify Family Structure Using Health Insurance Enrollment Data            #
#                                                                             #
# Last Modified: 2023-Sep-15                                                  #
###############################################################################
library(data.table)
library(arrow)
library(dplyr)
###############################################################################
### README ####################################################################
###############################################################################
# This file outputs ID to father's, mother's, and spouse's ID
# OUTPUT FIELDS:
#   - `ID`:      (chars) individual's ID.
#   (1)
#   - `F_ID`:    (char)  father's ID.
#   - `F_ID_S`:  (int)   father's gender. 1: male; 2: female.
#   - `M_ID`:    (char)  mother's ID
#   - `M_ID_S`:  (int)   mother's gender. 1: male; 2: female.
#   - `S_ID`:    (char)  Spouse's ID
#   - `S_ID_S`:  (int)   spouse's gender. 1: male; 2: female.

###############################################################################
### PREAMBLE ##################################################################
###############################################################################

rm(list=ls()); gc()
source("E:/H112057/arrow/arrow_20230831/arrow_helpers.R")

### Variables #################################################################
DISK        <- "E"
READ.DIR    <- paste0(DISK, ":/H112057/parquet")
SAVE.DIR    <- paste0(DISK, ":/H112057/CleanData")
LOGFILE     <- "ENROL_relation.log" %>% paste0(SAVE.DIR,.)
YEARS       <- (89L:110L) %>% as.character()
MONTHS      <- c(1:12) %>% as.character() %>% str_pad(width=2,side='left',pad='0')

### Useful Independent Functions ##############################################
date.time       <- function ()       { return( format(Sys.time(),"(%Y/%b/%d) %X") ) }
start_timer <- function() { start_time <<- Sys.time() }
end_timer <- function() { end_time <<- Sys.time(); print(end_time - start_time) }

###############################################################################
### FUNCTIONS #################################################################
###############################################################################

load_enrol_data <- function(year, month) {
  ## Read enrollment data for one year, selecting only ID, ID_S, and ID_BIRTHDAY
  message(paste("Start to load:", year, month))
  start_timer()
  dt_enrol <- open_nhi("ENROL", year, month) %>%
    select(ID, ID_S, ID1, ID1_S, ID_RELATION, ID_ROC, ID_STATUS) %>% 
    mutate(ID_STATUS := as.character(ID_STATUS),
           ID_ROC := as.character(ID_ROC)) %>% 
    filter(ID_STATUS == "1" & ID_ROC == "0") %>% # Only keep those who are currently in NHI and pass error detection
    collect() %>%
    as.data.table()
  end_timer()
  
  cat('Finished reading at', date.time(), "\n", file=LOGFILE, append=TRUE) # <log>
  return(dt_enrol)
}

identify_relatioin <- function(dt) {
  ## Identify husband-wife, father-children, and mother-children relation.
  ## Then bind the data
  message("Identifying Relations")
  start_timer()
  dt <- dt[ID != ID1][, .(ID, ID_S, ID1, ID1_S, ID_RELATION)] # If ID_RELATION is 3, it means ID is ID1's child
  
  # identifying husband-wife relation ==========================================
  dt_husband_wife <- dt[ID_RELATION == 1][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_husband_wife, c("ID1", "ID1_S"), c("S_ID", "S_ID_S"))
  
  dt_husband_wife_reverse <- dt[ID_RELATION == 1][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_husband_wife_reverse, c("ID1", "ID1_S", "ID", "ID_S"), c("ID", "ID_S", "S_ID", "S_ID_S"))
  
  dt_h_w <- rbind(dt_husband_wife, dt_husband_wife_reverse)
  rm(dt_husband_wife, dt_husband_wife_reverse)
  
  # identifying father-children relation ==========================================
  dt_father_children <- dt[ID_RELATION == 2 & ID_S == 1][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_father_children, c("ID1", "ID1_S", "ID", "ID_S"), c("ID", "ID_S", "F_ID", "F_ID_S"))
  
  dt_children_father <- dt[ID_RELATION == 3 & ID1_S == 1][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_children_father, c("ID1", "ID1_S"), c("F_ID", "F_ID_S"))
  
  dt_f_c <- rbind(dt_father_children, dt_children_father)
  rm(dt_father_children, dt_children_father)
  # identifying mother-children relation ==========================================
  dt_mother_children <- dt[ID_RELATION == 2 & ID_S == 2][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_mother_children, c("ID1", "ID1_S", "ID", "ID_S"), c("ID", "ID_S", "M_ID", "M_ID_S"))
  
  dt_children_mother <- dt[ID_RELATION == 3 & ID1_S == 2][, .(ID, ID_S, ID1, ID1_S)]
  setnames(dt_children_mother, c("ID1", "ID1_S"), c("M_ID", "M_ID_S"))
  
  dt_m_c <- rbind(dt_mother_children, dt_children_mother)
  rm(dt_mother_children, dt_children_mother)
  
  
  result <- rbind(dt_h_w, dt_f_c, dt_m_c, fill = TRUE)
  rm(dt_h_w, dt_f_c, dt_m_c)
  end_timer()
  return(result)
}

remove_na_and_check_consistency <- function(vec) {
  ## Remove NA
  ## Also, if individuals' parents is not consistent through out years,
  ## mark it
  non_na_values <- unique(na.omit(vec))
  if (length(non_na_values) > 1) {
    if (is.numeric(non_na_values[1])) {
      return(c(999L))
    }
    return(c("inconsistent"))
  }
  return(non_na_values)
}

###############################################################################
### MAIN ######################################################################
###############################################################################
enrol_relation_data_list <- list()
for (year in 103:110) {
  year_data <- list()
  for (month in 1:12) {
    monthly_dt <- load_enrol_data(year, month)
    monthly_relation <- identify_relatioin(monthly_dt)
    year_data <- c(year_data, list(monthly_relation))
  }
  data <- rbindlist(year_data) %>% distinct()
  enrol_relation_data_list <- c(enrol_relation_data_list, list(data))
  
  message("write")
  start_timer()
  write_parquet(data, paste0(SAVE.DIR, "/temp_data/yearly_enrol_relation", year, ".parquet"))
  rm(data, monthly_dt, monthly_relation)
  gc()
  
  end_timer()
  cat(paste0("Done: ", year, " // Current Time: ", Sys.time(), "\n"))
}
rm(year_data)


enrol_relation_data_list <- list()
for (year in 89:110) {
  message(paste("start to read:", year))
  start_timer()
  data <- open_dataset(paste0(SAVE.DIR, "/temp_data/yearly_enrol_relation", year, ".parquet")) %>%
    select(ID, F_ID, F_ID_S, M_ID, M_ID_S, S_ID, S_ID_S) %>% 
    filter(!is.na(ID)) %>% 
    collect() %>%
    as.data.table()
  enrol_relation_data_list <- c(enrol_relation_data_list, list(data))
  end_timer()
}
rm(data)
gc()

data <- rbindlist(enrol_relation_data_list, fill = TRUE) %>% distinct()
data <- data[!is.na(ID)]

start_timer()
combine_rows <- data[, .(F_ID = remove_na_and_check_consistency(F_ID),
                         F_ID_S = remove_na_and_check_consistency(F_ID_S),
                         M_ID = remove_na_and_check_consistency(M_ID),
                         M_ID_S = remove_na_and_check_consistency(M_ID_S),
                         S_ID = remove_na_and_check_consistency(S_ID),
                         S_ID_S = remove_na_and_check_consistency(S_ID_S)), by = ID]

end_timer()

write_parquet(combine_rows, paste0(SAVE.DIR, "/processed_data/enrol_relation.parquet"))
persinfo <- open_dataset(paste0(SAVE.DIR, "/processed_data/PersInfo.parquet")) %>% collect() %>% as.data.table()











