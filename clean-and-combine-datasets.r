# combine data from all cohorts into one file and do some cleaning/preprocessing



# load external packages
packages_all = c("data.table", "foreign", "haven")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# file/cohort namings
output_name <- 'raw-data'
input_names <- c(
    'OLIN-IV-1996',
    'OLIN-VI-2006',
    'OLIN-VII-2016',
    'WSAS-I-2008',
    'WSAS-II-2016'
)
# the standardized name for each variable to be included
names_standardized <- VARIABLES$name
# set data.table that all cohorts will be merged to
merged_data <- data.table()
# debug mode (if TRUE, print more info)
debug <- TRUE



# loop cohorts, rename accordingly and update coding (cohort-specific preprocessing), and lastly add to merged data.table
for (cohort in input_names) {
    # read data
    data <- setDT(read_sav(paste0(folder_path, 'input/sav/', cohort, ".sav")))
    if (debug) { print(paste('opened', cohort)) }
    # loop standardized variable names
    for (name_standardized in names_standardized) {
        if (name_standardized == 'cohort') {
            data[, 'cohort' := cohort]
        } else {
            # get variable cohort-specific variable name
            name_cohort_specific <- as.character(VARIABLES[name == name_standardized, cohort, with = FALSE])
            # update variable name in data to standardized name
            data.table::setnames(data, name_cohort_specific, name_standardized, skip_absent = TRUE)
            if (!any(names(data) == name_standardized)) {
                data[, (name_standardized) := NA]
            }
        }
    }
    if (debug) { print('updated variable names') }
    # select only standardized variables (relevant for study)
    data <- data[, ..names_standardized]
    if (debug) { print('selected standardized variables') }
    # cohort-specific variable preprocessing
    if (cohort == 'OLIN-IV-1996') {
        data$gender <- data$gender - 1 # men = 0, women = 1, as currently they're 1 and 2, respectively
        data$asthma_family_history[data$asthma_family_history == 2] <- NA
        data$asthma_self_reported[data$asthma_self_reported == 2] <- NA
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 74] <- NA # impossible given the max age at baseline in this cohort
        data$asthma_medication_use[data$asthma_medication_use == 2] <- NA
        data$copd_family_history[data$copd_family_history == 2] <- NA
        data$copd_self_reported[data$copd_self_reported == 2] <- NA
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- NA
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- NA
        data$smoking_currently[data$smoking_currently == 3] <- NA # one person had a typo value here
        data$smoking_previously[data$smoking_previously == 3] <- 2 # three persons had a typo value here
        data$smoking_previously[data$smoking_previously == 2] <- NA
        data$cigarettes_per_day[data$cigarettes_per_day == 5] <- NA
        data$age_start_smoking[data$age_start_smoking > 74] <- NA # impossible given the max age at baseline in this cohort
        data$age_quit_smoking[data$age_quit_smoking > 74] <- NA # impossible given the max age at baseline in this cohort
        data$attack_10y[data$attack_10y == 2] <- NA
        data$attack_12m[data$attack_12m == 2] <- NA
        data$attack_exercise[data$attack_exercise == 2] <- NA
        data$attack_cold[data$attack_cold == 2] <- NA
        data$attack_dust[data$attack_dust == 2] <- NA
        data$attack_tobacco[data$attack_tobacco == 2] <- NA
        data$attack_fume[data$attack_fume == 2] <- NA
        data$attack_dust_tobacco_fume <- ifelse(data$attack_dust == 1 | data$attack_tobacco == 1 | data$attack_fume == 1, 1, 0)
        data$attack_pollen[data$attack_pollen == 2] <- NA
        data$attack_fur[data$attack_fur == 2] <- NA
        data$wheezing_12m[data$wheezing_12m == 2] <- NA
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- NA
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- NA
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- NA
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- NA
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- NA
        data$cough_productive_3m[data$cough_productive_3m == 2] <- NA
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- NA
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- NA
        data$woken_by_chest_tightness[data$woken_by_chest_tightness == 2] <- NA
        data$rhinitis[data$rhinitis == 2] <- NA
    } else if (cohort == 'OLIN-VI-2006') {
        data$gender <- data$gender - 1 # men = 0, women = 1, as the coding is 1 and 2, respectively
        data$asthma_family_history[data$asthma_family_history == 2] <- NA
        data$asthma_self_reported[data$asthma_self_reported == 2] <- NA
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 69] <- NA
        data$asthma_medication_use[data$asthma_medication_use == 2] <- NA
        data$copd_family_history[data$copd_family_history == 2] <- NA
        data$copd_self_reported[data$copd_self_reported == 2] <- NA
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- NA
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- NA
        data$smoking_currently[data$smoking_currently == 2] <- NA
        data$smoking_previously[data$smoking_previously == 2] <- NA
        data$age_start_smoking[data$age_start_smoking > 69] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 69] <- NA
        data$rural_childhood[data$rural_childhood == 2] <- NA
        data$farm_childhood[data$farm_childhood == 2] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 2] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 7] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week >= 4 & data$times_exercise_per_week < 7] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week >= 2 & data$times_exercise_per_week < 4] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$attack_10y[data$attack_10y == 2] <- NA
        data$attack_12m[data$attack_12m == 2] <- NA
        data$attack_exercise[data$attack_exercise == 2] <- NA
        data$attack_cold[data$attack_cold == 2] <- NA
        data$attack_dust[data$attack_dust == 2] <- NA
        data$attack_tobacco[data$attack_tobacco == 2] <- NA
        data$attack_fume[data$attack_fume == 2] <- NA
        data$attack_dust_tobacco_fume <- ifelse(data$attack_dust == 1 | data$attack_tobacco == 1 | data$attack_fume == 1, 1, 0)
        data$attack_pollen[data$attack_pollen == 2] <- NA
        data$attack_fur[data$attack_fur == 2] <- NA
        data$dyspnea_painkiller[data$dyspnea_painkiller == 2] <- NA
        data$wheezing_12m[data$wheezing_12m == 2] <- NA
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- NA
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- NA
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- NA
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- NA
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- NA
        data$cough_productive_3m[data$cough_productive_3m == 2] <- NA
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- NA
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- NA
        data$woken_by_chest_tightness[data$woken_by_chest_tightness == 2] <- NA
        data$rhinitis[data$rhinitis == 2] <- NA
        data$nasal_obstruction_recurrent[data$nasal_obstruction_recurrent == 2] <- NA
        data$rhinorrhea_recurrent[data$rhinorrhea_recurrent == 2] <- NA
    } else if (cohort == 'OLIN-VII-2016') {
        data$gender[data$gender == 0] <- 100
        data$gender[data$gender == 1] <- 0
        data$gender[data$gender == 100] <- 1
        data$asthma_family_history[data$asthma_family_history == 2] <- 0
        data$asthma_self_reported[data$asthma_self_reported == 2] <- 0
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- 0
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 79] <- NA
        data$asthma_medication_use[data$asthma_medication_use == 2] <- 0
        data$copd_family_history[data$copd_family_history == 2] <- 0
        data$copd_self_reported[data$copd_self_reported == 2] <- 0
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- 0
        data$copd_medication_use[data$copd_medication_use == 2] <- 0
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- 0
        data$rash_6m[data$rash_6m == 2] <- 0
        data$rash_6m_12m[data$rash_6m_12m == 2] <- 0
        data$rash_only_hands[data$rash_only_hands == 2] <- 0
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- 0
        data$smoking_currently[data$smoking_currently == 2] <- 0
        data$smoking_previously[data$smoking_previously == 2] <- 0
        data$age_start_smoking[data$age_start_smoking > 79] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 79] <- NA
        data$rural_childhood[data$rural_childhood == 2] <- 0
        data$farm_childhood[data$farm_childhood == 2] <- 0
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 2] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week > 4] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
        data$attack_10y[data$attack_10y == 2] <- 0
        data$attack_12m[data$attack_12m == 2] <- 0
        data$attack_exercise[data$attack_exercise == 2] <- 0
        data$attack_cold[data$attack_cold == 2] <- 0
        data$attack_dust_tobacco_fume[data$attack_dust_tobacco_fume == 2] <- 0
        data$attack_pollen[data$attack_pollen == 2] <- 0
        data$attack_fur[data$attack_fur == 2] <- 0
        data$wheezing_12m[data$wheezing_12m == 2] <- 0
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- 0
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- 0
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- 0
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- 0
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- 0
        data$cough_productive_3m[data$cough_productive_3m == 2] <- 0
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- 0
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- 0
        data$rhinitis[data$rhinitis == 2] <- 0
        data$nasal_obstruction_recurrent[data$nasal_obstruction_recurrent == 2] <- 0
        data$rhinorrhea_recurrent[data$rhinorrhea_recurrent == 2] <- 0
    } else if (cohort == 'WSAS-I-2008') {
        data$age <- 2008 - as.numeric(data$age)
        data$gender <- data$gender - 1 # 0 = man, 1 = woman, while it was 1 and 2, initially, respectively
        data$asthma_family_history[data$asthma_family_history == 99] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 75] <- NA
        data$copd_family_history[data$copd_family_history == 99] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 99] <- NA
        data$cigarettes_per_day <- data$cigarettes_per_day + 1
        data$age_start_smoking[data$age_start_smoking > 75] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 75] <- NA
        data$rural_childhood[data$rural_childhood == 99] <- NA
        data$farm_childhood[data$farm_childhood == 99] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 99] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 5] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
    } else if (cohort == 'WSAS-II-2016') {
        data$age <- 2016 - data$age
        data$gender <- data$gender - 1 # 0 = man, 1 = woman, while it was 1 and 2, initially, respectively
        data$asthma_family_history[data$asthma_family_history == 99] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 75] <- NA
        data$copd_family_history[data$copd_family_history == 99] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 99] <- NA
        data$smoking_currently[data$smoking_currently == 99] <- NA
        data$smoking_previously[data$smoking_previously == 99] <- NA
        data$cigarettes_per_day <- data$cigarettes_per_day + 1
        data$age_start_smoking[data$age_start_smoking > 75] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 75] <- NA
        data$rural_childhood[data$rural_childhood == 99] <- NA
        data$farm_childhood[data$farm_childhood == 99] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 99] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 5] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
    }
    if (debug) { print('performed cohort-specific data cleaning') }
    # update data type where needed
    data$age <- as.numeric(data$age) # background variables
    data$gender <- as.factor(data$gender)
    data$height <- as.numeric(data$height)
    data$weight <- as.numeric(data$weight)
    data$bmi <- as.numeric(data$bmi)
    data$asthma_family_history <- as.factor(data$asthma_family_history)
    data$asthma_self_reported <- as.factor(data$asthma_self_reported)
    data$asthma_physician_diagnosed <- as.factor(data$asthma_physician_diagnosed)
    data$asthma_physician_diagnosed_age <- as.numeric(data$asthma_physician_diagnosed_age)
    data$asthma_medication_use <- as.factor(data$asthma_medication_use)
    data$asthma_hospitalization <- as.factor(data$asthma_hospitalization)
    data$copd_family_history <- as.factor(data$copd_family_history)
    data$copd_self_reported <- as.factor(data$copd_self_reported)
    data$copd_physician_diagnosed <- as.factor(data$copd_physician_diagnosed)
    data$copd_medication_use <- as.factor(data$copd_medication_use)
    data$rhinitis_conjunctivitis_family_history <- as.factor(data$rhinitis_conjunctivitis_family_history)
    data$chronic_sinusitis_physician_diagnosed <- as.factor(data$chronic_sinusitis_physician_diagnosed)
    data$rash_6m <- as.factor(data$rash_6m)
    data$rash_6m_12m <- as.factor(data$rash_6m_12m)
    data$eczema_skin_allergy <- as.factor(data$eczema_skin_allergy)
    data$rash_only_hands <- as.factor(data$rash_only_hands)
    data$snoring_loudly <- as.factor(data$snoring_loudly)
    data$difficulty_falling_asleep <- as.factor(data$difficulty_falling_asleep)
    data$waking_up_night <- as.factor(data$waking_up_night)
    data$sleepy_during_day <- as.factor(data$sleepy_during_day)
    data$waking_up_unable_fall_asleep <- as.factor(data$waking_up_unable_fall_asleep)
    data$sleep_medication_use <- as.factor(data$sleep_medication_use)
    data$other_lung_disease_self_reported <- as.factor(data$other_lung_disease_self_reported)
    data$antihypertensive_medication_use <- as.factor(data$antihypertensive_medication_use)
    data$diabetes_medication_use <- as.factor(data$diabetes_medication_use)
    data$smoking_currently <- as.factor(data$smoking_currently)
    levels(data$smoking_currently) <- c(0, 1, 9)
    data$smoking_previously <- as.factor(data$smoking_previously)
    data$smoking_status <- as.factor(data$smoking_status)
    data$cigarettes_per_day <- as.factor(data$cigarettes_per_day)
    data$age_start_smoking <- as.numeric(data$age_start_smoking)
    data$age_quit_smoking <- as.numeric(data$age_quit_smoking)
    data$daily_snuff_6m <- as.factor(data$daily_snuff_6m)
    data$daily_snuff_6m_currently <- as.factor(data$daily_snuff_6m_currently)
    data$snuff_status <- as.factor(data$snuff_status)
    data$rural_childhood <- as.factor(data$rural_childhood)
    data$farm_childhood <- as.factor(data$farm_childhood)
    data$occupational_vgdf_exposure <- as.factor(data$occupational_vgdf_exposure)
    data$highest_academic_degree <- as.factor(data$highest_academic_degree)
    levels(data$highest_academic_degree) <- c(1, 2, 3, 4, 5, 6, 7, 100, 200, 300)
    data$SEI <- as.numeric(data$SEI)
    data$times_exercise_per_week <- as.factor(data$times_exercise_per_week)
    data$respiratory_symptoms <- as.factor(data$respiratory_symptoms) # aggregated details on respiratory symptoms
    data$respiratory_symptoms_n <- as.factor(data$respiratory_symptoms_n)
    data$attack_10y <- as.factor(data$attack_10y) # respiratory symptom variables
    data$attack_12m <- as.factor(data$attack_12m)
    data$attack_exercise <- as.factor(data$attack_exercise)
    data$attack_cold <- as.factor(data$attack_cold)
    data$attack_dust <- as.factor(data$attack_dust)
    data$attack_tobacco <- as.factor(data$attack_tobacco)
    data$attack_fume <- as.factor(data$attack_fume)
    data$attack_dust_tobacco_fume <- as.factor(data$attack_dust_tobacco_fume)
    data$attack_cold_dust_tobacco_fume <- as.factor(data$attack_cold_dust_tobacco_fume)
    data$attack_pollen <- as.factor(data$attack_pollen)
    data$attack_fur <- as.factor(data$attack_fur)
    data$attack_pollen_fur <- as.factor(data$attack_pollen_fur)
    data$dyspnea_painkiller <- as.factor(data$dyspnea_painkiller)
    data$wheezing_12m <- as.factor(data$wheezing_12m)
    data$wheezing_sob_12m <- as.factor(data$wheezing_sob_12m)
    data$wheezing_wo_cold_12m <- as.factor(data$wheezing_wo_cold_12m)
    data$wheezing_recurrent <- as.factor(data$wheezing_recurrent)
    data$cough_longstanding_12m <- as.factor(data$cough_longstanding_12m)
    data$cough_productive_recurrent <- as.factor(data$cough_productive_recurrent)
    data$cough_productive_3m <- as.factor(data$cough_productive_3m)
    data$cough_productive_3m_2y <- as.factor(data$cough_productive_3m_2y)
    data$dyspnea_ground_level_walking <- as.factor(data$dyspnea_ground_level_walking)
    data$woken_by_sob <- as.factor(data$woken_by_sob)
    data$woken_by_cough <- as.factor(data$woken_by_cough)
    data$woken_by_chest_tightness <- as.factor(data$woken_by_chest_tightness)
    data$woken_by_sob_cough_chest_tightness <- as.factor(data$woken_by_sob_cough_chest_tightness)
    data$rhinitis <- as.factor(data$rhinitis)
    data$rhinitis_currently <- as.factor(data$rhinitis_currently)
    data$rhinitis_12m <- as.factor(data$rhinitis_12m)
    data$rhinitis_5d <- as.factor(data$rhinitis_5d)
    data$rhinitis_5d_5w <- as.factor(data$rhinitis_5d_5w)
    data$rhinitis_conjunctivitis <- as.factor(data$rhinitis_conjunctivitis)
    data$nasal_obstruction_recurrent <- as.factor(data$nasal_obstruction_recurrent)
    data$rhinorrhea_recurrent <- as.factor(data$rhinorrhea_recurrent)
    data$nasal_obstruction_13w <- as.factor(data$nasal_obstruction_13w)
    data$aching_sinus_13w <- as.factor(data$aching_sinus_13w)
    data$nasal_secretion_13w <- as.factor(data$nasal_secretion_13w)
    data$reduced_smell_13w <- as.factor(data$reduced_smell_13w)
    if (debug) print('updated data types')
    # add cohort to merged data
    merged_data <- rbind(merged_data, data)
    if (debug) { print(paste('added', cohort, 'to merged data')) }
    if (debug) { print('----------')}
}



# function to remove outliers according to 1.5 IQR rule
remove_outliers <- function(feature) {
    if (debug) print(paste('removing outliers for feature', feature))
    feature_data <- as.numeric(unlist(merged_data[, feature, with = FALSE]))
    min_value <- min(feature_data, na.rm = TRUE)
    max_value <- max(feature_data, na.rm = TRUE)
    iqr_value <- IQR(feature_data, na.rm = TRUE)
    threshold_value <- 1.5 * iqr_value
    lower_threshold <- quantile(feature_data, 0.25, na.rm = TRUE) - threshold_value
    upper_threshold <- quantile(feature_data, 0.75, na.rm = TRUE) + threshold_value
    below_threshold_n <- nrow(merged_data[merged_data[[feature]] < lower_threshold, ])
    above_threshold_n <- nrow(merged_data[merged_data[[feature]] > upper_threshold, ])
    below_threshold_percentage <- round(below_threshold_n / nrow(merged_data) * 100, digits = 2)
    above_threshold_percentage <- round(above_threshold_n / nrow(merged_data) * 100, digits = 2)
    if (debug) print(paste('prior to removal: min =', min_value, ', max =', max_value))
    if (debug) print(paste0(below_threshold_n, ' (', below_threshold_percentage, '%) observations below threshold and ', above_threshold_n, ' (', above_threshold_percentage, '%) observations above threshold'))
    merged_data[merged_data[[feature]] < lower_threshold] <- NA
    merged_data[merged_data[[feature]] > upper_threshold] <- NA
    feature_data <- as.numeric(unlist(merged_data[, feature, with = FALSE]))
    min_value <- min(feature_data, na.rm = TRUE)
    max_value <- max(feature_data, na.rm = TRUE)
    if (debug) print(paste('after removal: min =', min_value, ', max =', max_value))
    return(merged_data[, feature, with = FALSE])
}

# remove outliers
merged_data$height <- remove_outliers('height')
merged_data$weight <- remove_outliers('weight')
# asthma
merged_data$asthma_self_reported[(merged_data$asthma_self_reported == 0 & merged_data$asthma_physician_diagnosed == 1)] <- NA
merged_data$asthma_physician_diagnosed[(merged_data$asthma_self_reported == 0 | is.na(merged_data$asthma_self_reported)) & merged_data$asthma_physician_diagnosed == 1] <- NA
merged_data$asthma_physician_diagnosed_age[merged_data$asthma_physician_diagnosed != 1] <- NA
merged_data$asthma_hospitalization[merged_data$asthma_self_reported == 0 & is.na(merged_data$asthma_hospitalization)] <- 0
# copd
merged_data$copd_self_reported[(merged_data$copd_self_reported == 0 & merged_data$copd_physician_diagnosed == 1)] <- NA
merged_data$copd_physician_diagnosed[(merged_data$copd_self_reported == 0 | is.na(merged_data$copd_self_reported)) & merged_data$copd_physician_diagnosed == 1] <- NA
# rash
merged_data$rash_6m_12m[merged_data$rash_6m == 0 & is.na(merged_data$rash_6m_12m)] <- 0
merged_data$rash_only_hands[merged_data$rash_6m == 0 & is.na(merged_data$rash_only_hands)] <- 0
# smoking and snuff
merged_data$smoking_currently[merged_data$smoking_currently == 1 & merged_data$smoking_previously == 1] <- 9 # replace with 9 as a "mockup" value to remove nonsensical smoking_previosly values
merged_data$smoking_previously[merged_data$smoking_currently == 9 & merged_data$smoking_previously == 1] <- NA
merged_data$smoking_currently[merged_data$smoking_currently == 9] <- NA
merged_data$smoking_previously[merged_data$smoking_currently == 1 & is.na(merged_data$smoking_previously)] <- 0
merged_data$cigarettes_per_day[merged_data$smoking_currently != 1] <- NA
merged_data$cigarettes_per_day <- as.factor(merged_data$cigarettes_per_day)
merged_data$age_start_smoking[merged_data$smoking_currently != 1 & merged_data$smoking_previously != 1] <- NA
merged_data$age_quit_smoking[merged_data$smoking_previously != 1] <- NA
merged_data$daily_snuff_6m_currently[merged_data$daily_snuff_6m == 0 & is.na(merged_data$daily_snuff_6m_currently)] <- 0
# attacks
merged_data$attack_12m[merged_data$attack_10y == 0 & is.na(merged_data$attack_12m)] <- 0
# wheezing
merged_data$wheezing_sob_12m[merged_data$wheezing_12m == 0 & is.na(merged_data$wheezing_sob_12m)] <- 0
merged_data$wheezing_wo_cold_12m[merged_data$wheezing_12m == 0 & is.na(merged_data$wheezing_wo_cold_12m)] <- 0
# coughing
merged_data$cough_productive_3m[merged_data$cough_productive_recurrent == 0 & is.na(merged_data$cough_productive_3m)] <- 0
merged_data$cough_productive_3m_2y[((merged_data$cough_productive_recurrent == 1 & merged_data$cough_productive_3m == 0) | (merged_data$cough_productive_recurrent == 0 & (merged_data$cough_productive_3m == 0 | is.nan(merged_data$cough_productive_3m)))) & is.na(merged_data$cough_productive_3m_2y)] <- 0
# rhinitis
merged_data$rhinitis_currently[merged_data$rhinitis == 0 & is.na(merged_data$rhinitis_currently)] <- 0
merged_data$rhinitis_12m[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_12m)] <- 0
merged_data$rhinitis_5d[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_5d)] <- 0
merged_data$rhinitis_5d_5w[((merged_data$rhinitis_currently == 0 & (merged_data$rhinitis_5d == 0 | is.na(merged_data$rhinitis_5d))) | (merged_data$rhinitis_currently == 1 & merged_data$rhinitis_5d == 0)) & is.na(merged_data$rhinitis_5d_5w)] <- 0
merged_data$rhinitis_conjunctivitis[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_conjunctivitis)] <- 0
# highest academic degree
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 6 | merged_data$highest_academic_degree == 7] <- 300
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 3 | merged_data$highest_academic_degree == 4 | merged_data$highest_academic_degree == 5] <- 200
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 1 | merged_data$highest_academic_degree == 2] <- 100
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 300] <- 3
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 200] <- 2
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 100] <- 1
merged_data[, highest_academic_degree := droplevels(highest_academic_degree)]
# SEI
merged_data$SEI <- merged_data$SEI %% 100
merged_data$SEI <- ifelse(
    merged_data$SEI == 1 | merged_data$SEI == 2 | merged_data$SEI == 3,
    7,
    ifelse(
        merged_data$SEI == 11 | merged_data$SEI == 12,
        1,
        ifelse(
            merged_data$SEI == 21 | merged_data$SEI == 22,
            2,
            ifelse(
                merged_data$SEI == 33 | merged_data$SEI == 34 | merged_data$SEI == 35 | merged_data$SEI == 36,
                3,
                ifelse(
                    merged_data$SEI == 44 | merged_data$SEI == 45 | merged_data$SEI == 46,
                    4,
                    ifelse(
                        merged_data$SEI == 54 | merged_data$SEI == 55 | merged_data$SEI == 56 | merged_data$SEI == 57 | merged_data$SEI == 60,
                        5,
                        ifelse(
                            merged_data$SEI == 76 | merged_data$SEI == 77 | merged_data$SEI == 78 | merged_data$SEI == 79 | merged_data$SEI == 86 | merged_data$SEI == 87 | merged_data$SEI == 89,
                            6,
                            8
                        )
                    )
                )
            )
        )
    )
)
merged_data$SEI <- as.factor(merged_data$SEI)
if (debug) print('performed universal data cleaning and preprocessing')
if (debug) print('----------')
# set unique_id (incremental integer)
merged_data[, unique_id := 1:.N]
if (debug) print('set unique id to each subject')
if (debug) print('----------')



# save merged data
dt <- merged_data
if (debug) print(paste0('dimensions of merged data: ', dim(dt)[1], ', ', dim(dt)[2]))
save(dt, file = paste0(folder_path, 'output/rda/', output_name, '.Rda'))
if (debug) print('saved merged data')