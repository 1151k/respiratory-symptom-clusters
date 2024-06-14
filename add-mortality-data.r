# add mortality data



# load external packages
packages_all = c("data.table", "haven", "dplyr", "flextable", "gtsummary")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# load all data
load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled--comorbidity', '.Rda'))
dt <- copy(dt_working_comorbidity)



# load mortality data
# WSAS
# fetch and format/define columns
WSAS_register_data <- haven::read_sav(paste0('input/sav/', 'WSAS-mortality', '.sav'))
print('loaded WSAS mortality data')
# select columns of interest
WSAS_register_data <- WSAS_register_data %>% select(c("LopNr", "DODSDAT", "ULORSAK"))
WSAS_register_data <- WSAS_register_data %>% rename(person_id = LopNr, mortality_cause = ULORSAK, mortality_date = DODSDAT)
# any mortality
WSAS_register_data$mortality_date <- as.Date(paste(substr(WSAS_register_data$mortality_date, 1, 4), substr(WSAS_register_data$mortality_date, 5, 6), substr(WSAS_register_data$mortality_date, 7, 8), sep = "-"), format = "%Y-%m-%d")
WSAS_register_data$mortality_date_cause <- WSAS_register_data$mortality_cause %>% as.character()
# cardiovascular mortality
WSAS_register_data$mortality_date_cardiovascular <- as.Date(NA)
WSAS_register_data$mortality_date_cardiovascular[startsWith(WSAS_register_data$mortality_cause, 'I') == TRUE] <- WSAS_register_data$mortality_date[startsWith(WSAS_register_data$mortality_cause, 'I') == TRUE]
# respiratory mortality
WSAS_register_data$mortality_date_respiratory <- as.Date(NA)
WSAS_register_data$mortality_date_respiratory[startsWith(WSAS_register_data$mortality_cause, 'J') == TRUE] <- WSAS_register_data$mortality_date[startsWith(WSAS_register_data$mortality_cause, 'J') == TRUE]
# lung cancer mortality
WSAS_register_data$mortality_date_lung_cancer <- as.Date(NA)
WSAS_register_data$mortality_date_lung_cancer[startsWith(WSAS_register_data$mortality_cause, 'C33') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'C34') == TRUE] <- WSAS_register_data$mortality_date[startsWith(WSAS_register_data$mortality_cause, 'C33') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'C34') == TRUE]
# drop mortality cause column
WSAS_register_data <- WSAS_register_data %>% select(-mortality_cause)
# load a copy of all WSAS participants and match with register data
WSAS_register_data$person_id <- as.numeric(WSAS_register_data$person_id)
dt_WSAS <- copy(dt[dt$cohort == 'WSAS-I-2008' | dt$cohort == 'WSAS-II-2016'])
dt_WSAS <- dt_WSAS %>% select(-starts_with('mortality_date'))
dt_WSAS <- merge(dt_WSAS, WSAS_register_data, by = 'person_id', all.x = TRUE)

# OLIN
# fetch and format/define columns
OLIN_register_data <- haven::read_sav(paste0('input/sav/',  'OLIN-mortality.sav'))
print('loaded OLIN mortality data')
# select columns of interest
OLIN_register_data <- OLIN_register_data %>% select(c("LopNr", "DODSDAT", "ULORSAK"))
OLIN_register_data <- OLIN_register_data %>% rename(person_id = LopNr, mortality_date = DODSDAT, mortality_cause = ULORSAK)
# any mortality
OLIN_register_data$mortality_date <- as.Date(paste(substr(OLIN_register_data$mortality_date, 1, 4), substr(OLIN_register_data$mortality_date, 5, 6), substr(OLIN_register_data$mortality_date, 7, 8), sep = "-"), format = "%Y-%m-%d")
OLIN_register_data$mortality_date_cause <- OLIN_register_data$mortality_cause %>% as.character()
# cardiovascular mortality
OLIN_register_data$mortality_date_cardiovascular <- as.Date(NA)
OLIN_register_data$mortality_date_cardiovascular[startsWith(OLIN_register_data$mortality_cause, 'I') == TRUE] <- OLIN_register_data$mortality_date[startsWith(OLIN_register_data$mortality_cause, 'I') == TRUE]
# respiratory mortality
OLIN_register_data$mortality_date_respiratory <- as.Date(NA)
OLIN_register_data$mortality_date_respiratory[startsWith(OLIN_register_data$mortality_cause, 'J') == TRUE] <- OLIN_register_data$mortality_date[startsWith(OLIN_register_data$mortality_cause, 'J') == TRUE]
# lung cancer mortality
OLIN_register_data$mortality_date_lung_cancer <- as.Date(NA)
OLIN_register_data$mortality_date_lung_cancer[startsWith(OLIN_register_data$mortality_cause, 'C33') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'C34') == TRUE] <- OLIN_register_data$mortality_date[startsWith(OLIN_register_data$mortality_cause, 'C33') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'C34') == TRUE]
# drop mortality cause column
OLIN_register_data <- OLIN_register_data %>% select(-mortality_cause)
# load a copy of all OLIN participants and match with register data
OLIN_register_data$person_id <- as.numeric(OLIN_register_data$person_id)
dt_OLIN <- copy(dt[dt$cohort == 'OLIN-IV-1996' | dt$cohort == 'OLIN-VI-2006' | dt$cohort == 'OLIN-VII-2016'])
dt_OLIN <- dt_OLIN %>% select(-starts_with('mortality_date'))
dt_OLIN <- merge(dt_OLIN, OLIN_register_data, by = 'person_id', all.x = TRUE)

# merge all data
dt <- rbind(dt_WSAS, dt_OLIN)

# print the number of rows with mortality_date not NA
print(paste0('Number of rows with mortality_date not NA: ', nrow(dt[!is.na(mortality_date)])))
print(paste0('Number of rows with mortality_date_cardiovascular not NA: ', nrow(dt[!is.na(mortality_date_cardiovascular)])))
print(paste0('Number of rows with mortality_date_respiratory not NA: ', nrow(dt[!is.na(mortality_date_respiratory)])))

# print the 100 earliest (lowest) values of mortality_date together with mortality_date_cause
print(dt[!is.na(mortality_date)][order(mortality_date)][1:100, c('mortality_date', 'mortality_date_cause')])

# save data
save(dt, file = paste0(folder_path, 'output/rda/', 'imputed-data--pooled--comorbidity--mortality', '.Rda'))
