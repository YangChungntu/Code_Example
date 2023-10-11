###############################################################################
# Identify Family Structure Using Registration Data                           #
#                                                                             #
# Last Modified: 2023-Sep-15                                                  #
###############################################################################
library(data.table)
library(arrow)
library(lubridate)
library(stringr)
###############################################################################
### README ####################################################################
###############################################################################
# This file outputs ID to father's, mother's, and spouse's ID
# OUTPUT FIELDS:
#   - `ID`:           (chars) individual's ID.
#   (1)
#   - `ID_S`:         (int)   individual's gender. 1: male; 2: female.
#   - `ID_BIRTHDAY`:  (int)   individual's birthday in the form of yyyymmdd.


###############################################################################
### PREAMBLE ##################################################################
###############################################################################
rm(list = ls()); gc()
source("E:/H112057/arrow/arrow_20230831/arrow_helpers.R")

### Variables #################################################################
DISK        <- "E"
READ.DIR    <- paste0(DISK, ":/H112057/parquet")
SAVE.DIR    <- paste0(DISK, ":/H112057/CleanData")
LOGFILE     <- "hhreg_relation.log" %>% paste0(SAVE.DIR,.)
HHREG.DIR   <- paste0(DISK, ":/H112057/yangc/hhreg_data")

# categorize popular relation (open and check)
list_h <- c("戶長", "尸長", "－戶長", "＋戶長", "０戶長", "１戶長", "３戶長", "戶長１", "戶長人", "戶長之", "戶長周", "戶長長", "戶長熊", "戶長謝")
list_m <- c("生母", "母", "養母", "繼母", "母親")
list_f <- c("父親", "生父", "養父", "繼父", "父")
list_s <- c("配偶", "夫妻", "妻", "妻子", "夫")
list_c <- c("１子", "１長子", "５子", "七子", "九子", "二子", "八子", "十一子", "十七子", "十九子", "十二子", "十八子", "十三子", "十子", "十六子", "十四子",
            "三子", "子", "子女", "子次", "子長", "五子", "六子", "兄子", "四子", "伍子", "玖子", "男子", "兒子", "長子", "長子１", "長子??", "長子女", "拾子",
            "拾玖子", "拾柒子", "拾捌子", "拾參子", "拾陸子", "拾壹子", "拾貳子", "拾肆子", "柒子", "捌子", "參子", "陸子", "貳子", "肆子", "養子", "七女", "九女",
            "二女", "八女", "十一女", "十二女", "十三女", "十女", "十六女", "三女", "女", "女兒", "五女", "六女", "夫女", "四女", "外女", "伍女", "次女", "玖女",
            "長女", "拾女", "拾參女", "拾陸女", "拾壹女", "拾貳女", "柒女", "捌女", "參女", "陸女", "貳女", "肆女", "養女", "繼女",
            "次子", "長男", "男", "次男", "三男", "參男", "肆男")
list_r <- c("七兄", "三兄", "兄長", "四兄", "本生兄", "長兄", "柒兄", "參兄", "二哥", "三哥", "大哥", "參哥", "貳哥", "七弟", "二弟", "八弟", "十弟", "三弟",
            "五弟", "六弟", "四弟", "伍弟", "次弟", "弟子", "弟弟", "弟男", "拾弟", "柒弟", "胞弟", "捌弟", "參弟", "陸弟", "貳弟", "肆弟", "養弟",
            "二姊", "二姐", "三姊", "三姐", "大姊", "大姐", "五姊", "五姐", "兄姊", "四姊", "本生姊", "伍姊", "伍姐", "姊女", "姊妹", "姊姊", "姐妹", "姐姐", "長姊",
            "胞姊", "參姊", "參姐", "貳姊", "肆姊", "肆姐", "養姊", "二妹", "三妹", "四妹", "次妹", "妹女", "妹妹", "胞妹", "參妹", "貳妹", "肆妹", "養妹",
            "弟", "妹", "姊", "兄", "姐")


### Useful Independent Functions ##############################################
start_timer <- function() { start_time <<- Sys.time() }
end_timer <- function() { end_time <<- Sys.time(); print(end_time - start_time) }

###############################################################################
### FUNCTIONS #################################################################
###############################################################################
clean_special_chars <- function(dt) {
  ## Remove special chars from the RELATION column
  for(i in c("?@", " ", "?I", "?D", "?B", "??", "?F", "??", "??", "??", "??", "??", "?f", "??", "??", "??", "")) {
    dt[ , RELATION: gsub(i, "", RELATION)]
  }
  return(dt)
}

categorize_relation <- function(relation) {
  # Define your categorization logic here
  # Return the category code (0, 1, 2, 3, 4, or NA)
  ifelse(relation %in% list_h, 0, 
         ifelse(relation %in% list_f, 1,
                ifelse(relation %in% list_m, 2,
                       ifelse(relation %in% list_s, 3,
                              ifelse(relation %in% list_c, 4,
                                     ifelse(relation %in% list_r, 5,
                                            NA))))))
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

##=============================================================================
## Section 1: get target relation =============================================
##=============================================================================

# Create a list of relation and find out the popular relations
list_relation <- c() # list of relation
reg_relation_data_list <- list() # a list storing the RELATION cols from S_MOI_HHREG
for(year in c(100:102, 104:110)) {
  reg_dt <- open_dataset(paste0(HHREG.DIR, "/S_MOI_HHREG", year, ".parquet")) %>% 
    select(RELATION) %>% 
    collect() %>% 
    as.data.table()
  reg_dt <- clean_special_chars(reg_dt)
  reg_relation_data_list <- c(reg_relation_data_list, list(reg_dt))
  list_relation <- sort(unique(c(list_relation, reg_dt$RELATION)))
  cat(paste0("Done: ", year, " // Current Time: ", Sys.time(), "\n"))
}

# Count the # of times each relation appears and order them by the #
reg_relation <- rbindlist(reg_relation_data_list, fill = TRUE)[, .(count = .N), by = RELATION][order(-count)]
popular_relation <- reg_relation[count >= 10]

# Save
fwrite(popular_relation, paste0(SAVE.DIR, "/relation_types.csv"))

##=============================================================================
## Section 2: Creating annual relation data ===================================
##=============================================================================
for(year in c(100:102, 104:110)) {
  message(paste("Start Year:", year))
  start_timer()
  dt <- open_dataset(paste0(HHREG.DIR, "/S_MOI_HHREG", year, ".parquet")) %>% 
    select(ID, ID_S, BIRTH_YM, HOST, RELATION) %>% 
    collect() %>% 
    as.data.table()
  
  # the final data
  dt_all <- data.table(ID = dt$ID,
                       ID_S = dt$ID_S,
                       BIRTH_YM = dt$BIRTH_YM)
  # Clean special characters
  dt <- clean_special_chars(dt)
  
  # create RELATION_2, which is the categorized RELATION 
  dt[, RELATION_2 := categorize_relation(RELATION)]
  end_timer()
  
  # find the host and merge back ==============================================
  message("Finding host and merging back")
  start_timer()
  dt_host <- dt[RELATION_2 == 0] # subset
  dt_host <- distinct(dt_host)
  dt_host[, count := .N, by = c("ID", "ID_S")]
  dt_host[, num_host_in_one_fam := .N, by = HOST]
  dt_host <- dt_host[count == 1 & num_host_in_one_fam == 1]
  
  # dt_host <- dt_host[, .SD[.N == 1], by = HOST] # remove duplicate hosts in the same HOST(family)
  dt_host <- dt_host[, c("HOST_ID", "HOST_ID_S", "HOST") := .(ID, ID_S, HOST)][, .(HOST_ID, HOST_ID_S, HOST)]
  dt <- merge(dt, dt_host, by = "HOST")
  dt <- dt[!is.na(HOST_ID) & !is.na(RELATION_2)]
  rm(dt_host)
  end_timer()
  
  # identifying father-child relation =========================================
  message("F-C relationship")
  start_timer()
  # the host's father
  dt_1 <- dt[RELATION_2 == 1, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_1) <- c("F_ID", "F_ID_S", "ID", "ID_S")
  # male host's children
  dt_2 <- dt[RELATION_2 == 4 & HOST_ID_S == 1, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_2) <- c("ID", "ID_S", "F_ID", "F_ID_S")
  # Female host's spouse's child
  dt_f_hosts_spouse <- setNames(dt[HOST_ID_S == 2 & RELATION_2 == 3, c("ID", "ID_S", "HOST")], c("F_ID", "F_ID_S", "HOST"))
  dt_f_hosts_child <- setNames(dt[HOST_ID_S == 2 & RELATION_2 == 4, c("ID", "ID_S", "HOST")], c("ID", "ID_S", "HOST"))
  dt_3 <- merge(dt_f_hosts_spouse, dt_f_hosts_child, by = "HOST")[, .(ID, ID_S, F_ID, F_ID_S)]
  rm(dt_f_hosts_spouse, dt_f_hosts_child)
  # Host's siblings' father
  dt_siblings <- setNames(dt[RELATION_2 == 5, c("ID", "ID_S", "HOST")], c("ID", "ID_S", "HOST"))
  dt_father <- setNames(dt[RELATION_2 == 1, c("ID", "ID_S", "HOST")], c("F_ID", "F_ID_S", "HOST"))
  dt_4 <- merge(dt_siblings, dt_father, by = "HOST")[ , .(ID, ID_S, F_ID, F_ID_S)]
  rm(dt_siblings, dt_father)
  # bind
  dt_father_children <- rbind(dt_1, dt_2, dt_3, dt_4)
  dt_father_children <- distinct(dt_father_children)
  dt_father_children <- dt_father_children[complete.cases(dt_father_children)]
  dt_father_children[, count := .N, by = ID]
  dt_father_children <- dt_father_children[count == 1][, .(ID, ID_S, F_ID, F_ID_S)]
  rm(dt_1, dt_2, dt_3, dt_4)
  end_timer()
  
  # identifying mother-child relation =========================================
  message("M-C relationship")
  start_timer()
  # the host's mother
  dt_1 <- dt[RELATION_2 == 2, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_1) <- c("M_ID", "M_ID_S", "ID", "ID_S")
  # female host's children
  dt_2 <- dt[RELATION_2 == 4 & HOST_ID_S == 2, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_2) <- c("ID", "ID_S", "M_ID", "M_ID_S")
  # male host's spouse's child
  dt_m_hosts_spouse <- setNames(dt[HOST_ID_S == 1 & RELATION_2 == 3, c("ID", "ID_S", "HOST")], c("M_ID", "M_ID_S", "HOST"))
  dt_m_hosts_child <- setNames(dt[HOST_ID_S == 1 & RELATION_2 == 4, c("ID", "ID_S", "HOST")], c("ID", "ID_S", "HOST"))
  dt_3 <- merge(dt_m_hosts_spouse, dt_m_hosts_child, by = "HOST")[, .(ID, ID_S, M_ID, M_ID_S)]
  rm(dt_m_hosts_spouse, dt_m_hosts_child)
  # Host's siblings' mother
  dt_siblings <- setNames(dt[RELATION_2 == 5, c("ID", "ID_S", "HOST")], c("ID", "ID_S", "HOST"))
  dt_mother <- setNames(dt[RELATION_2 == 2, c("ID", "ID_S", "HOST")], c("M_ID", "M_ID_S", "HOST"))
  dt_4 <- merge(dt_siblings, dt_mother, by = "HOST")[ , .(ID, ID_S, M_ID, M_ID_S)]
  rm(dt_siblings, dt_mother)
  # bind
  dt_mother_children <- rbind(dt_1, dt_2, dt_3, dt_4)
  dt_mother_children <- distinct(dt_mother_children)
  dt_mother_children <- dt_mother_children[complete.cases(dt_mother_children)]
  dt_mother_children[, count := .N, by = ID]
  dt_mother_children <- dt_mother_children[count == 1][, .(ID, ID_S, M_ID, M_ID_S)]
  rm(dt_1, dt_2, dt_3, dt_4)
  end_timer()
  
  # identifying husband-wife relation =========================================
  message("H-W relationship")
  start_timer()
  # Host's spouse
  dt_1 <- dt[RELATION_2 == 3, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_1) <- c("S_ID", "S_ID_S", "ID", "ID_S")
  # Host's spouse (reverse)
  dt_2 <- dt[RELATION_2 == 3, .(ID, ID_S, HOST_ID, HOST_ID_S)]
  colnames(dt_2) <- c("ID", "ID_S", "S_ID", "S_ID_S")
  # Host's parents
  dt_father <- setNames(dt[RELATION_2 == 1, .(ID, ID_S, HOST)], c("ID", "ID_S", "HOST"))
  dt_mother <- setNames(dt[RELATION_2 == 2, .(ID, ID_S, HOST)], c("S_ID", "S_ID_S", "HOST"))
  dt_3 <- merge(dt_father, dt_mother, by = "HOST")[, .(ID, ID_S, S_ID, S_ID_S)]
  # Host's parents (reverse)
  dt_mother <- setNames(dt[RELATION_2 == 2, .(ID, ID_S, HOST)], c("ID", "ID_S", "HOST"))
  dt_father <- setNames(dt[RELATION_2 == 1, .(ID, ID_S, HOST)], c("S_ID", "S_ID_S", "HOST"))
  dt_4 <- merge(dt_mother, dt_father, by = "HOST")[, .(ID, ID_S, S_ID, S_ID_S)]
  rm(dt_father, dt_mother)
  # bind
  dt_husband_wife <- rbind(dt_1, dt_2, dt_3, dt_4)
  dt_husband_wife <- distinct(dt_husband_wife)
  dt_husband_wife <- dt_husband_wife[complete.cases(dt_husband_wife)]
  dt_husband_wife[, count := .N, by = ID]
  dt_husband_wife <- dt_husband_wife[count == 1][, .(ID, ID_S, S_ID, S_ID_S)]
  rm(dt_1, dt_2, dt_3, dt_4)
  end_timer()
  
  # merge
  message("Merge and write")
  start_timer()
  dt_all <- dt_all %>%
    merge(dt_father_children, by = c("ID", "ID_S"), all = TRUE) %>%
    merge(dt_mother_children, by = c("ID", "ID_S"), all = TRUE) %>%
    merge(dt_husband_wife, by = c("ID", "ID_S"), all = TRUE)
  
  write_parquet(dt_all, paste0(SAVE.DIR, "/temp_data/yearly_hhreg_relation", year, ".parquet"))
  rm(dt, dt_all, dt_father_children, dt_mother_children, dt_husband_wife)
  gc()
  end_timer()
  cat(paste0("Done: ", year, " // Current Time: ", Sys.time(), "\n"))
}

##=============================================================================
## Section 3: Merging Yearly Relation Data ====================================
##=============================================================================
data_list <- data.table()
for(year in c(100:102, 104:110)) {
  dt <- open_dataset(paste0(SAVE.DIR, "/temp_data/yearly_hhreg_relation", year, ".parquet")) %>% 
    select(ID, F_ID, F_ID_S, M_ID, M_ID_S, S_ID, S_ID_S) %>% 
    collect() %>% 
    as.data.table()
  data_list <- c(data_list, list(dt))
  cat(paste0("Done: ", year, " // Current Time: ", Sys.time(), "\n"))
}
rm(dt)

data <- rbindlist(data_list, fill = TRUE) %>% distinct()
data <- data[order(ID)]
data <- data[!is.na(ID)]

start_timer()
combine_rows <- data[, .(F_ID = remove_na_and_check_consistency(F_ID),
                         F_ID_S = remove_na_and_check_consistency(F_ID_S),
                         M_ID = remove_na_and_check_consistency(M_ID),
                         M_ID_S = remove_na_and_check_consistency(M_ID_S),
                         S_ID = remove_na_and_check_consistency(S_ID),
                         S_ID_S = remove_na_and_check_consistency(S_ID_S)), by = ID]

end_timer()

# Store the processed data
write_parquet(combine_rows, paste0(SAVE.DIR, "/processed_data/hhreg_relation.parquet"))

