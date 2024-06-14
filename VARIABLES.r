# define variables across all included cohorts, as well as their characteristics (e.g., which variables to use for cluster analysis)



# load packages
packages_to_load = c("data.table")
packages_installed = packages_to_load %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_to_load[!packages_installed])
}
invisible(lapply(packages_to_load, library, character.only = TRUE))



# define variables names
name <- c(
    'unique_id', # identifier variables
    'person_id',
    'cohort',
    'age', # background variables
    'gender',
    'height',
    'weight',
    'bmi',
    'asthma_family_history',
    'asthma_self_reported',
    'asthma_physician_diagnosed',
    'asthma_physician_diagnosed_age',
    'asthma_medication_use',
    'asthma_hospitalization',
    'copd_family_history',
    'copd_self_reported',
    'copd_physician_diagnosed',
    'copd_medication_use',
    'rhinitis_conjunctivitis_family_history',
    'chronic_sinusitis_physician_diagnosed',
    'rash_6m',
    'rash_6m_12m',
    'rash_only_hands',
    'eczema_skin_allergy',
    'snoring_loudly',
    'difficulty_falling_asleep',
    'waking_up_night',
    'sleepy_during_day',
    'waking_up_unable_fall_asleep',
    'sleep_medication_use',
    'other_lung_disease_self_reported',
    'antihypertensive_medication_use',
    'diabetes_medication_use',
    'smoking_currently',
    'smoking_previously',
    'smoking_status', 
    'cigarettes_per_day',
    'age_start_smoking',
    'age_quit_smoking',
    'daily_snuff_6m',
    'daily_snuff_6m_currently',
    'snuff_status',
    'rural_childhood',
    'farm_childhood',
    'occupational_vgdf_exposure',
    'highest_academic_degree',
    'SEI',
    'times_exercise_per_week',
    'attack_10y', # respiratory symptom variables
    'attack_12m',
    'attack_exercise',
    'attack_cold',
    'attack_dust',
    'attack_tobacco',
    'attack_fume',
    'attack_dust_tobacco_fume',
    'attack_cold_dust_tobacco_fume', 
    'attack_pollen',
    'attack_fur',
    'attack_pollen_fur', 
    'dyspnea_painkiller',
    'wheezing_12m',
    'wheezing_sob_12m',
    'wheezing_wo_cold_12m',
    'wheezing_recurrent',
    'cough_longstanding_12m',
    'cough_productive_recurrent',
    'cough_productive_3m',
    'cough_productive_3m_2y',
    'dyspnea_ground_level_walking',
    'woken_by_sob',
    'woken_by_cough',
    'woken_by_chest_tightness',
    'woken_by_sob_cough_chest_tightness', 
    'rhinitis',
    'rhinitis_currently',
    'rhinitis_12m',
    'rhinitis_5d',
    'rhinitis_5d_5w',
    'rhinitis_conjunctivitis',
    'nasal_obstruction_recurrent',
    'rhinorrhea_recurrent',
    'nasal_obstruction_13w',
    'aching_sinus_13w',
    'nasal_secretion_13w',
    'reduced_smell_13w'
)
# print(length(name))

# define variable codes for each cohort
code_OLIN_IV_1996 <- c(
    '', # unique_id --- identifier variables
    'LopNr', # person_id
    '', # cohort
    'A1_alder', # age --- background variables
    'A1_sex', # gender
    '', # height
    '', # weight
    '', # bmi
    'A1_f1a', # asthma_family_history
    'A1_f2a', # asthma_self_reported
    'A1_f3', # asthma_physician_diagnosed
    '', # asthma_physician_diagnosed_age
    'A1_f5', # asthma_medication_use
    '', # asthma_hospitalization
    'A1_f1c', # copd_family_history
    'A1_f2c', # copd_self_reported
    'A1_f4', # copd_physician_diagnosed
    '', # copd_medication_use
    'A1_f1b', # rhinitis_conjunctivitis_family_history
    '', # chronic_sinusitis_physician_diagnosed
    '', # rash_6m
    '', # rash_6m_12m
    '', # rash_only_hands
    '', # eczema_skin_allergy
    '', # snoring_loudly
    '', # difficulty_falling_asleep
    '', # waking_up_night
    '', # sleepy_during_day
    '', # waking_up_unable_fall_asleep
    '', # sleep_medication_use
    'A1_f2d', # other_lung_disease_self_reported
    '', # antihypertensive_medication_use
    '', # diabetes_medication_use
    'A1_f14', # smoking_currently
    'A1_f14b', # smoking_previously
    '', # smoking_status
    'A1_f14a', # cigarettes_per_day
    '', # age_start_smoking
    '', # age_quit_smoking
    '', # daily_snuff_6m
    '', # daily_snuff_6m_currently
    '', # snuff_status
    '', # rural_childhood
    '', # farm_childhood
    '', # occupational_vgdf_exposure
    '', # highest_academic_degree
    'A1_SEI_CEApek', # SEI
    '', # times_exercise_per_week
    'A1_f6', # attack_10y --- respiratory symptom variables
    'A1_f6a', # attack_12m
    'A1_f13a', # attack_exercise
    'A1_f13b', # attack_cold
    'A1_f13d', # attack_dust
    'A1_f13e', # attack_tobacco
    'A1_f13f', # attack_fume
    '', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    'A1_f13h', # attack_pollen
    'A1_f13i', # attack_fur
    '', # attack_pollen_fur
    '', # dyspnea_painkiller
    'A1_f10', # wheezing_12m
    'A1_f10a', # wheezing_sob_12m
    'A1_f10b', # wheezing_wo_cold_12m
    'A1_f9', # wheezing_recurrent
    'A1_f7', # cough_longstanding_12m
    'A1_f8', # cough_productive_recurrent
    'A1_f8a', # cough_productive_3m
    'A1_f8b', # cough_productive_3m_2y
    'A1_f12', # dyspnea_ground_level_walking
    '', # woken_by_sob
    '', # woken_by_cough
    'A1_f11', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    'A1_f2b', # rhinitis
    '', # rhinitis_currently
    '', # rhinitis_12m
    '', # rhinitis_5d
    '', # rhinitis_5d_5w
    '', # rhinitis_conjunctivitis
    '', # nasal_obstruction_recurrent
    '', # rhinorrhea_recurrent
    '', # nasal_obstruction_13w
    '', # aching_sinus_13w
    '', # nasal_secretion_13w
    '' # reduced_smell_13w
)
# print(length(code_OLIN_IV_1996))
code_OLIN_VI_2006 <- c(
    '', # unique_id --- identifier variables
    'LopNr', # person_id
    '', # cohort
    'B1_alder', # age --- background variables
    'B1_sex', # gender
    '', # height
    '', # weight
    '', # bmi
    'B1_f1a', # asthma_family_history
    'B1_f2a', # asthma_self_reported
    'B1_f3', # asthma_physician_diagnosed
    'B1_f3a', # asthma_physician_diagnosed_age
    'B1_f5', # asthma_medication_use
    '', # asthma_hospitalization
    'B1_f1c', # copd_family_history
    'B1_f2c', # copd_self_reported
    'B1_f4', # copd_physician_diagnosed
    '', # copd_medication_use
    'B1_f1b', # rhinitis_conjunctivitis_family_history
    '', # chronic_sinusitis_physician_diagnosed
    '', # rash_6m
    '', # rash_6m_12m
    '', # rash_only_hands
    '', # eczema_skin_allergy
    '', # snoring_loudly
    '', # difficulty_falling_asleep
    '', # waking_up_night
    '', # sleepy_during_day
    '', # waking_up_unable_fall_asleep
    '', # sleep_medication_use
    'B1_f2d', # other_lung_disease_self_reported
    '', # antihypertensive_medication_use
    '', # diabetes_medication_use
    'B1_f16', # smoking_currently
    'B1_f16b', # smoking_previously
    '', # smoking_status
    'B1_f16a', # cigarettes_per_day
    'B1_f16c', # age_start_smoking
    '', # age_quit_smoking
    '', # daily_snuff_6m
    '', # daily_snuff_6m_currently
    '', # snuff_status
    'B1_f22', # rural_childhood
    'B1_f22a', # farm_childhood
    'B1_f19', # occupational_vgdf_exposure
    '', # highest_academic_degree
    'B1_SEI_CEApek', # SEI
    'B1_f21', # times_exercise_per_week
    'B1_f6', # attack_10y --- respiratory symptom variables
    'B1_f6a', # attack_12m
    'B1_f13a', # attack_exercise
    'B1_f13b', # attack_cold
    'B1_f13d', # attack_dust
    'B1_f13e', # attack_tobacco
    'B1_f13f', # attack_fume
    '', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    'B1_f13h', # attack_pollen
    'B1_f13i', # attack_fur
    '', # attack_pollen_fur
    'B1_f14', # dyspnea_painkiller
    'B1_f10', # wheezing_12m
    'B1_f10a', # wheezing_sob_12m
    'B1_f10b', # wheezing_wo_cold_12m
    'B1_f9', # wheezing_recurrent
    'B1_f7', # cough_longstanding_12m
    'B1_f8', # cough_productive_recurrent
    'B1_f8a', # cough_productive_3m
    'B1_f8b', # cough_productive_3m_2y
    'B1_f12', # dyspnea_ground_level_walking
    '', # woken_by_sob
    '', # woken_by_cough
    'B1_f11', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    'B1_f2b', # rhinitis
    '', # rhinitis_currently
    '', # rhinitis_12m
    '', # rhinitis_5d
    '', # rhinitis_5d_5w
    '', # rhinitis_conjunctivitis
    'B1_f15', # nasal_obstruction_recurrent
    'B1_f15a', # rhinorrhea_recurrent
    '', # nasal_obstruction_13w
    '', # aching_sinus_13w
    '', # nasal_secretion_13w
    '' # reduced_smell_13w
)
# print(length(code_OLIN_VI_2006))
code_OLIN_VII_2016 <- c(
    '', # unique_id --- identifier variables
    'LopNr', # person_id
    '', # cohort
    'C1_alder', # age --- background variables
    'C1_kon', # gender
    'g25', # height
    'g26', # weight
    '', # bmi
    'C1_Q1a', # asthma_family_history
    'C1_Q2a', # asthma_self_reported
    'C1_Q3a', # asthma_physician_diagnosed
    'C1_Q3b', # asthma_physician_diagnosed_age
    'C1_Q5', # asthma_medication_use
    'g6c', # asthma_hospitalization
    'C1_Q1c', # copd_family_history
    'C1_Q2c', # copd_self_reported
    'C1_Q4', # copd_physician_diagnosed
    'g22b', # copd_medication_use
    'C1_Q1b', # rhinitis_conjunctivitis_family_history
    'g12', # chronic_sinusitis_physician_diagnosed
    'g13', # rash_6m
    'g13b', # rash_6m_12m
    'g13c', # rash_only_hands
    'g14', # eczema_skin_allergy
    'g24a', # snoring_loudly
    'g24b', # difficulty_falling_asleep
    'g24c', # waking_up_night
    'g24d', # sleepy_during_day
    'g24e', # waking_up_unable_fall_asleep
    'g22d', # sleep_medication_use
    'C1_Q2d', # other_lung_disease_self_reported
    'g22a', # antihypertensive_medication_use
    'g22c', # diabetes_medication_use
    'C1_Q16a', # smoking_currently
    'C1_Q16c', # smoking_previously
    '', # smoking_status
    'C1_Q16b', # cigarettes_per_day
    'C1_Q16d', # age_start_smoking
    'C1_Q16e', # age_quit_smoking
    '', # daily_snuff_6m
    '', # daily_snuff_6m_currently
    '', # snuff_status
    'C1_Q22a', # rural_childhood
    'C1_Q22b', # farm_childhood
    'C1_Q20', # occupational_vgdf_exposure
    'g21', # highest_academic_degree
    'C1_SEI', # SEI
    'g19', # times_exercise_per_week
    'C1_Q6a', # attack_10y --- respiratory symptom variables
    'C1_Q6b', # attack_12m
    'C1_Q13a', # attack_exercise
    'C1_Q13b', # attack_cold
    '', # attack_dust
    '', # attack_tobacco
    '', # attack_fume
    'C1_Q13c', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    'C1_Q13d', # attack_pollen
    'C1_Q13e', # attack_fur
    '', # attack_pollen_fur
    'g15', # dyspnea_painkiller
    'C1_Q10a', # wheezing_12m
    'C1_Q10b', # wheezing_sob_12m
    'C1_Q10c', # wheezing_wo_cold_12m
    'C1_Q9', # wheezing_recurrent
    'C1_Q7', # cough_longstanding_12m
    'C1_Q8a', # cough_productive_recurrent
    'C1_Q8b', # cough_productive_3m
    'C1_Q8c', # cough_productive_3m_2y
    'C1_Q12', # dyspnea_ground_level_walking
    'g3', # woken_by_sob
    'g4', # woken_by_cough
    'g2', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    'C1_Q2b', # rhinitis
    'g7', # rhinitis_currently
    'g7b', # rhinitis_12m
    'g7c', # rhinitis_5d
    'g7d', # rhinitis_5d_5w
    'g7e', # rhinitis_conjunctivitis
    'C1_Q14', # nasal_obstruction_recurrent
    'C1_Q15', # rhinorrhea_recurrent
    'g8', # nasal_obstruction_13w
    'g9', # aching_sinus_13w
    'g10', # nasal_secretion_13w
    'g11' # reduced_smell_13w
)
# print(length(code_OLIN_VII_2016))
code_WSAS_I_2008 <- c(
    '', # unique_id --- identifier variables
    'LopNr', # person_id
    '', # cohort
    'Födelseår_2008', # age --- background variables
    'kon_2008', # gender
    'Q26_2008', # height
    'Q27_2008', # weight
    '', # bmi
    'F1A_2008', # asthma_family_history
    'F2A_2008', # asthma_self_reported
    'F3A_2008', # asthma_physician_diagnosed
    'F3B_2008', # asthma_physician_diagnosed_age
    'F5_2008', # asthma_medication_use
    'Q6P2_2008', # asthma_hospitalization
    'F1C_2008', # copd_family_history
    'F2C_2008', # copd_self_reported
    'F4_2008', # copd_physician_diagnosed
    'Q23B_2008', # copd_medication_use
    'F1B_2008', # rhinitis_conjunctivitis_family_history
    'Q12_2008', # chronic_sinusitis_physician_diagnosed
    'Q13_2008', # rash_6m
    'Q13P1_2008', # rash_6m_12m
    'Q13P2_2008', # rash_only_hands
    'Q14_2008', # eczema_skin_allergy
    'Q22A_2008', # snoring_loudly
    'Q22B_2008', # difficulty_falling_asleep
    'Q22C_2008', # waking_up_night
    'Q22D_2008', # sleepy_during_day
    'Q22E_2008', # waking_up_unable_fall_asleep
    'Q23D_2008', # sleep_medication_use
    'F2D_2008', # other_lung_disease_self_reported
    'Q23A_2008', # antihypertensive_medication_use
    'Q23C_2008', # diabetes_medication_use
    'F16A_2008', # smoking_currently
    'F16C_2008', # smoking_previously
    '', # smoking_status
    'F16B_2008', # cigarettes_per_day
    'F16D_2008', # age_start_smoking
    'Q15P2P1_2008', # age_quit_smoking
    '', # daily_snuff_6m
    '', # daily_snuff_6m_currently
    '', # snuff_status
    'F23A_2008', # rural_childhood
    'F23B_2008', # farm_childhood
    'F22_2008', # occupational_vgdf_exposure
    'Q17_2008', # highest_academic_degree
    'F18_SEI_2008_2008', # SEI
    'Q25_2008', # times_exercise_per_week
    'F6A_2008', # attack_10y --- respiratory symptom variables
    'F6B_2008', # attack_12m
    'F13A_2008', # attack_exercise
    'F13B_2008', # attack_cold
    '', # attack_dust
    '', # attack_tobacco
    '', # attack_fume
    'F13C_2008', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    'F13D_2008', # attack_pollen
    'F13E_2008', # attack_fur
    '', # attack_pollen_fur
    'F14A_2008', # dyspnea_painkiller
    'F10A_2008', # wheezing_12m
    'F10B_2008', # wheezing_sob_12m
    'F10C_2008', # wheezing_wo_cold_12m
    'F9_2008', # wheezing_recurrent
    'F7_2008', # cough_longstanding_12m
    'F8A_2008', # cough_productive_recurrent
    'F8B_2008', # cough_productive_3m
    'F8C_2008', # cough_productive_3m_2y
    'F12_2008', # dyspnea_ground_level_walking
    'Q3_2008', # woken_by_sob
    'Q4_2008', # woken_by_cough
    'Q2_2008', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    'F2B_2008', # rhinitis
    'Q7_2008', # rhinitis_currently
    'Q7P1_2008', # rhinitis_12m
    'Q7P2_2008', # rhinitis_5d
    'Q7P3_2008', # rhinitis_5d_5w
    'Q7P4_2008', # rhinitis_conjunctivitis
    'F15A_2008', # nasal_obstruction_recurrent
    'F15B_2008', # rhinorrhea_recurrent
    'Q8_2008', # nasal_obstruction_13w
    'Q9_2008', # aching_sinus_13w
    'Q10_2008', # nasal_secretion_13w
    'Q11_2008' # reduced_smell_13w
)
# print(length(code_WSAS_I_2008))
code_WSAS_II_2016 <- c(
    '', # unique_id --- identifier variables
    'LopNr', # person_id
    '', # cohort
    'Födelseår_2016', # age --- background variables
    'kon_2016', # gender
    'Q26_2016', # height
    'Q27_2016', # weight
    '', # bmi
    'F1A_2016', # asthma_family_history
    'F2A_2016', # asthma_self_reported
    'F3A_2016', # asthma_physician_diagnosed
    'F3B_2016', # asthma_physician_diagnosed_age
    'F5_2016', # asthma_medication_use
    'Q6P2_2016', # asthma_hospitalization
    'F1C_2016', # copd_family_history
    'F2C_2016', # copd_self_reported
    'F4_2016', # copd_physician_diagnosed
    'Q23B_2016', # copd_medication_use
    'F1B_2016', # rhinitis_conjunctivitis_family_history
    'Q12_2016', # chronic_sinusitis_physician_diagnosed
    'Q13_2016', # rash_6m
    'Q13P1_2016', # rash_6m_12m
    'Q13P2_2016', # rash_only_hands
    'Q14_2016', # eczema_skin_allergy
    'Q22A_2016', # snoring_loudly
    'Q22B_2016', # difficulty_falling_asleep
    'Q22C_2016', # waking_up_night
    'Q22D_2016', # sleepy_during_day
    'Q22E_2016', # waking_up_unable_fall_asleep
    'Q23D_2016', # sleep_medication_use
    'F2D_2016', # other_lung_disease_self_reported
    'Q23A_2016', # antihypertensive_medication_use
    'Q23C_2016', # diabetes_medication_use
    'F16A_2016', # smoking_currently
    'F16C_2016', # smoking_previously
    '', # smoking_status
    'F16B_2016', # cigarettes_per_day
    'F16D_2016', # age_start_smoking
    'Q15P2P1_2016', # age_quit_smoking
    'Q24_2016', # daily_snuff_6m
    'Q24P1_2016', # daily_snuff_6m_currently
    '', # snuff_status
    'F23A_2016', # rural_childhood
    'F23B_2016', # farm_childhood
    'F22_2016', # occupational_vgdf_exposure
    'Q17_2016', # highest_academic_degree
    'SEIkod_2016', # SEI
    'Q25_2016', # times_exercise_per_week
    'F6A_2016', # attack_10y --- respiratory symptom variables
    'F6B_2016', # attack_12m
    'F13A_2016', # attack_exercise
    'F13B_2016', # attack_cold
    '', # attack_dust
    '', # attack_tobacco
    '', # attack_fume
    'F13C_2016', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    'F13D_2016', # attack_pollen
    'F13E_2016', # attack_fur
    '', # attack_pollen_fur
    'F14A_2016', # dyspnea_painkiller
    'F10A_2016', # wheezing_12m
    'F10B_2016', # wheezing_sob_12m
    'F10C_2016', # wheezing_wo_cold_12m
    'F9_2016', # wheezing_recurrent
    'F7_2016', # cough_longstanding_12m
    'F8A_2016', # cough_productive_recurrent
    'F8B_2016', # cough_productive_3m
    'F8C_2016', # cough_productive_3m_2y
    'F12_2016', # dyspnea_ground_level_walking
    'Q3_2016', # woken_by_sob
    'Q4_2016', # woken_by_cough
    'Q2_2016', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    'F2B_2016', # rhinitis
    'Q7_2016', # rhinitis_currently
    'Q7P1_2016', # rhinitis_12m
    'Q7P2_2016', # rhinitis_5d
    'Q7P3_2016', # rhinitis_5d_5w
    'Q7P4_2016', # rhinitis_conjunctivitis
    'F15A_2016', # nasal_obstruction_recurrent
    'F15B_2016', # rhinorrhea_recurrent
    'Q8_2016', # nasal_obstruction_13w
    'Q9_2016', # aching_sinus_13w
    'Q10_2016', # nasal_secretion_13w
    'Q11_2016' # reduced_smell_13w
)
# print(length(code_WSAS_II_2016))

# define variables to be used in the clustering
cluster_variable <- c(
    FALSE, # unique_id --- identifier variables
    FALSE, # person_id
    FALSE, # cohort
    FALSE, # age --- background variables
    FALSE, # gender
    FALSE, # height
    FALSE, # weight
    FALSE, # bmi
    FALSE, # asthma_family_history
    FALSE, # asthma_self_reported
    FALSE, # asthma_physician_diagnosed
    FALSE, # asthma_physician_diagnosed_age
    FALSE, # asthma_medication_use
    FALSE, # asthma_hospitalization
    FALSE, # copd_family_history
    FALSE, # copd_self_reported
    FALSE, # copd_physician_diagnosed
    FALSE, # copd_medication_use
    FALSE, # rhinitis_conjunctivitis_family_history
    FALSE, # chronic_sinusitis_physician_diagnosed
    FALSE, # rash_6m
    FALSE, # rash_6m_12m
    FALSE, # rash_only_hands
    FALSE, # eczema_skin_allergy
    FALSE, # snoring_loudly
    FALSE, # difficulty_falling_asleep
    FALSE, # waking_up_night
    FALSE, # sleepy_during_day
    FALSE, # waking_up_unable_fall_asleep
    FALSE, # sleep_medication_use
    FALSE, # other_lung_disease_self_reported
    FALSE, # antihypertensive_medication_use
    FALSE, # diabetes_medication_use
    FALSE, # smoking_currently
    FALSE, # smoking_previously
    FALSE, # smoking_status
    FALSE, # cigarettes_per_day
    FALSE, # age_start_smoking
    FALSE, # age_quit_smoking
    FALSE, # daily_snuff_6m
    FALSE, # daily_snuff_6m_currently
    FALSE, # snuff_status
    FALSE, # rural_childhood
    FALSE, # farm_childhood
    FALSE, # occupational_vgdf_exposure
    FALSE, # highest_academic_degree
    FALSE, # SEI
    FALSE, # times_exercise_per_week
    TRUE, # attack_10y --- respiratory symptom variables
    TRUE, # attack_12m
    TRUE, # attack_exercise
    TRUE, # attack_cold
    FALSE, # attack_dust
    FALSE, # attack_tobacco
    FALSE, # attack_fume
    TRUE, # attack_dust_tobacco_fume
    FALSE, # attack_cold_dust_tobacco_fume
    TRUE, # attack_pollen
    TRUE, # attack_fur
    FALSE, # attack_pollen_fur
    TRUE, # dyspnea_painkiller
    TRUE, # wheezing_12m
    TRUE, # wheezing_sob_12m
    TRUE, # wheezing_wo_cold_12m
    TRUE, # wheezing_recurrent
    TRUE, # cough_longstanding_12m
    TRUE, # cough_productive_recurrent
    TRUE, # cough_productive_3m
    TRUE, # cough_productive_3m_2y
    TRUE, # dyspnea_ground_level_walking
    TRUE, # woken_by_sob
    TRUE, # woken_by_cough
    TRUE, # woken_by_chest_tightness
    FALSE, # woken_by_sob_cough_chest_tightness
    TRUE, # rhinitis
    FALSE, # rhinitis_currently
    TRUE, # rhinitis_12m
    TRUE, # rhinitis_5d
    TRUE, # rhinitis_5d_5w
    TRUE, # rhinitis_conjunctivitis
    TRUE, # nasal_obstruction_recurrent
    TRUE, # rhinorrhea_recurrent
    TRUE, # nasal_obstruction_13w
    TRUE, # aching_sinus_13w
    TRUE, # nasal_secretion_13w
    TRUE # reduced_smell_13w
)
# print(length(cluster_variable))

# define the order that the variables should be imputed
imputation_order <- c(
    'N/A', # unique_id --- identifier variables
    'N/A', # person_id
    'N/A', # cohort
    'independent', # age --- background variables
    'independent', # gender
    'independent', # height
    'independent', # weight
    'N/A', # bmi
    'independent', # asthma_family_history
    'independent', # asthma_self_reported
    'dependent-1', # asthma_physician_diagnosed
    'dependent-asthma', # asthma_physician_diagnosed_age
    'independent', # asthma_medication_use
    'dependent-1', # asthma_hospitalization
    'independent', # copd_family_history
    'independent', # copd_self_reported
    'dependent-1', # copd_physician_diagnosed
    'independent', # copd_medication_use
    'independent', # rhinitis_conjunctivitis_family_history
    'independent', # chronic_sinusitis_physician_diagnosed
    'independent', # rash_6m
    'dependent-1', # rash_6m_12m
    'dependent-1', # rash_only_hands
    'independent', # eczema_skin_allergy
    'independent', # snoring_loudly
    'independent', # difficulty_falling_asleep
    'independent', # waking_up_night
    'independent', # sleepy_during_day
    'independent', # waking_up_unable_fall_asleep
    'independent', # sleep_medication_use
    'independent', # other_lung_disease_self_reported
    'independent', # antihypertensive_medication_use
    'independent', # diabetes_medication_use
    'independent', # smoking_currently
    'dependent-1', # smoking_previously
    'N/A', # smoking_status
    'dependent-smoker', # cigarettes_per_day
    'dependent-ever-smoker', # age_start_smoking
    'dependent-ex-smoker', # age_quit_smoking
    'N/A', # daily_snuff_6m
    'N/A', # daily_snuff_6m_currently
    'N/A', # snuff_status
    'independent', # rural_childhood
    'independent', # farm_childhood
    'independent', # occupational_vgdf_exposure
    'independent', # highest_academic_degree
    'independent', # SEI
    'independent', # times_exercise_per_week
    'independent', # attack_10y --- respiratory symptom variables
    'dependent-1', # attack_12m
    'independent', # attack_exercise
    'independent', # attack_cold
    'N/A', # attack_dust
    'N/A', # attack_tobacco
    'N/A', # attack_fume
    'independent', # attack_dust_tobacco_fume
    'N/A', # attack_cold_dust_tobacco_fume
    'independent', # attack_pollen
    'independent', # attack_fur
    'N/A', # attack_pollen_fur
    'independent', # dyspnea_painkiller
    'independent', # wheezing_12m
    'dependent-1', # wheezing_sob_12m
    'dependent-1', # wheezing_wo_cold_12m
    'independent', # wheezing_recurrent
    'independent', # cough_longstanding_12m
    'independent', # cough_productive_recurrent
    'dependent-1', # cough_productive_3m
    'dependent-2', # cough_productive_3m_2y
    'independent', # dyspnea_ground_level_walking
    'independent', # woken_by_sob
    'independent', # woken_by_cough
    'independent', # woken_by_chest_tightness
    'N/A', # woken_by_sob_cough_chest_tightness
    'independent', # rhinitis
    'N/A', # rhinitis_currently
    'dependent-1', # rhinitis_12m
    'dependent-1', # rhinitis_5d
    'dependent-2', # rhinitis_5d_5w
    'dependent-1', # rhinitis_conjunctivitis
    'independent', # nasal_obstruction_recurrent
    'independent', # rhinorrhea_recurrent
    'independent', # nasal_obstruction_13w
    'independent', # aching_sinus_13w
    'independent', # nasal_secretion_13w
    'independent' # reduced_smell_13w
)
# print(length(imputation_order))

# define the variable types
variable_type <- c(
    'numeric', # unique_id --- identifier variables
    'numeric', # person_id
    'character', # cohort
    'numeric', # age --- background variables
    'factor', # gender
    'numeric', # height
    'numeric', # weight
    'numeric', # bmi
    'factor', # asthma_family_history
    'factor', # asthma_self_reported
    'factor', # asthma_physician_diagnosed
    'numeric', # asthma_physician_diagnosed_age
    'factor', # asthma_medication_use
    'factor', # asthma_hospitalization
    'factor', # copd_family_history
    'factor', # copd_self_reported
    'factor', # copd_physician_diagnosed
    'factor', # copd_medication_use
    'factor', # rhinitis_conjunctivitis_family_history
    'factor', # chronic_sinusitis_physician_diagnosed
    'factor', # rash_6m
    'factor', # rash_6m_12m
    'factor', # rash_only_hands
    'factor', # eczema_skin_allergy
    'factor', # snoring_loudly
    'factor', # difficulty_falling_asleep
    'factor', # waking_up_night
    'factor', # sleepy_during_day
    'factor', # waking_up_unable_fall_asleep
    'factor', # sleep_medication_use
    'factor', # other_lung_disease_self_reported
    'factor', # antihypertensive_medication_use
    'factor', # diabetes_medication_use
    'factor', # smoking_currently
    'factor', # smoking_previously
    'factor', # smoking_status
    'factor', # cigarettes_per_day
    'numeric', # age_start_smoking
    'numeric', # age_quit_smoking
    'factor', # daily_snuff_6m
    'factor', # daily_snuff_6m_currently
    'factor', # snuff_status
    'factor', # rural_childhood
    'factor', # farm_childhood
    'factor', # occupational_vgdf_exposure
    'factor', # highest_academic_degree
    'factor', # SEI
    'factor', # times_exercise_per_week
    'factor', # attack_10y --- respiratory symptom variables
    'factor', # attack_12m
    'factor', # attack_exercise
    'factor', # attack_cold
    'factor', # attack_dust
    'factor', # attack_tobacco
    'factor', # attack_fume
    'factor', # attack_dust_tobacco_fume
    'factor', # attack_cold_dust_tobacco_fume
    'factor', # attack_pollen
    'factor', # attack_fur
    'factor', # attack_pollen_fur
    'factor', # dyspnea_painkiller
    'factor', # wheezing_12m
    'factor', # wheezing_sob_12m
    'factor', # wheezing_wo_cold_12m
    'factor', # wheezing_recurrent
    'factor', # cough_longstanding_12m
    'factor', # cough_productive_recurrent
    'factor', # cough_productive_3m
    'factor', # cough_productive_3m_2y
    'factor', # dyspnea_ground_level_walking
    'factor', # woken_by_sob
    'factor', # woken_by_cough
    'factor', # woken_by_chest_tightness
    'factor', # woken_by_sob_cough_chest_tightness
    'factor', # rhinitis
    'factor', # rhinitis_currently
    'factor', # rhinitis_12m
    'factor', # rhinitis_5d
    'factor', # rhinitis_5d_5w
    'factor', # rhinitis_conjunctivitis
    'factor', # nasal_obstruction_recurrent
    'factor', # rhinorrhea_recurrent
    'factor', # nasal_obstruction_13w
    'factor', # aching_sinus_13w
    'factor', # nasal_secretion_13w
    'factor' # reduced_smell_13w
)
# print(length(variable_type))

# define which variables should be in the characteristics plot
characteristics_plot <- c(
    FALSE, # unique_id --- identifier variables
    FALSE, # person_id
    FALSE, # cohort
    TRUE, # age --- background variables
    TRUE, # gender
    FALSE, # height
    FALSE, # weight
    TRUE, # bmi
    TRUE, # asthma_family_history
    TRUE, # asthma_self_reported
    TRUE, # asthma_physician_diagnosed
    TRUE, # asthma_physician_diagnosed_age
    TRUE, # asthma_medication_use
    TRUE, # asthma_hospitalization
    TRUE, # copd_family_history
    TRUE, # copd_self_reported
    TRUE, # copd_physician_diagnosed
    TRUE, # copd_medication_use
    TRUE, # rhinitis_conjunctivitis_family_history
    TRUE, # chronic_sinusitis_physician_diagnosed
    TRUE, # rash_6m
    TRUE, # rash_6m_12m
    TRUE, # rash_only_hands
    TRUE, # eczema_skin_allergy
    TRUE, # snoring_loudly
    TRUE, # difficulty_falling_asleep
    TRUE, # waking_up_night
    TRUE, # sleepy_during_day
    TRUE, # waking_up_unable_fall_asleep
    TRUE, # sleep_medication_use
    TRUE, # other_lung_disease_self_reported
    TRUE, # antihypertensive_medication_use
    TRUE, # diabetes_medication_use
    FALSE, # smoking_currently
    FALSE, # smoking_previously
    TRUE, # smoking_status
    TRUE, # cigarettes_per_day
    TRUE, # age_start_smoking
    TRUE, # age_quit_smoking
    FALSE, # daily_snuff_6m
    FALSE, # daily_snuff_6m_currently
    FALSE, # snuff_status
    TRUE, # rural_childhood
    TRUE, # farm_childhood
    TRUE, # occupational_vgdf_exposure
    TRUE, # highest_academic_degree
    TRUE, # SEI
    TRUE, # times_exercise_per_week
    TRUE, # attack_10y --- respiratory symptom variables
    TRUE, # attack_12m
    TRUE, # attack_exercise
    TRUE, # attack_cold
    FALSE, # attack_dust
    FALSE, # attack_tobacco
    FALSE, # attack_fume
    TRUE, # attack_dust_tobacco_fume
    FALSE, # attack_cold_dust_tobacco_fume
    TRUE, # attack_pollen
    TRUE, # attack_fur
    FALSE, # attack_pollen_fur
    TRUE, # dyspnea_painkiller
    TRUE, # wheezing_12m
    TRUE, # wheezing_sob_12m
    TRUE, # wheezing_wo_cold_12m
    TRUE, # wheezing_recurrent
    TRUE, # cough_longstanding_12m
    TRUE, # cough_productive_recurrent
    TRUE, # cough_productive_3m
    TRUE, # cough_productive_3m_2y
    TRUE, # dyspnea_ground_level_walking
    TRUE, # woken_by_sob
    TRUE, # woken_by_cough
    TRUE, # woken_by_chest_tightness
    FALSE, # woken_by_sob_cough_chest_tightness
    TRUE, # rhinitis
    FALSE, # rhinitis_currently
    TRUE, # rhinitis_12m
    TRUE, # rhinitis_5d
    TRUE, # rhinitis_5d_5w
    TRUE, # rhinitis_conjunctivitis
    TRUE, # nasal_obstruction_recurrent
    TRUE, # rhinorrhea_recurrent
    TRUE, # nasal_obstruction_13w
    TRUE, # aching_sinus_13w
    TRUE, # nasal_secretion_13w
    TRUE # reduced_smell_13w
)
# print(length(characteristics_plot))

# define which variables should be in the missingness plot
missingness_plot <- c(
    FALSE, # unique_id --- identifier variables
    FALSE, # person_id
    FALSE, # cohort
    'non-clustering', # age --- background variables
    'non-clustering', # gender
    'non-clustering', # height
    'non-clustering', # weight
    FALSE, # bmi
    'non-clustering', # asthma_family_history
    'non-clustering', # asthma_self_reported
    'non-clustering', # asthma_physician_diagnosed
    'non-clustering', # asthma_physician_diagnosed_age
    'non-clustering', # asthma_medication_use
    'non-clustering', # asthma_hospitalization
    'non-clustering', # copd_family_history
    'non-clustering', # copd_self_reported
    'non-clustering', # copd_physician_diagnosed
    'non-clustering', # copd_medication_use
    'non-clustering', # rhinitis_conjunctivitis_family_history
    'non-clustering', # chronic_sinusitis_physician_diagnosed
    'non-clustering', # rash_6m
    'non-clustering', # rash_6m_12m
    'non-clustering', # rash_only_hands
    'non-clustering', # eczema_skin_allergy
    'non-clustering', # snoring_loudly
    'non-clustering', # difficulty_falling_asleep
    'non-clustering', # waking_up_night
    'non-clustering', # sleepy_during_day
    'non-clustering', # waking_up_unable_fall_asleep
    'non-clustering', # sleep_medication_use
    'non-clustering', # other_lung_disease_self_reported
    'non-clustering', # antihypertensive_medication_use
    'non-clustering', # diabetes_medication_use
    'non-clustering', # smoking_currently
    'non-clustering', # smoking_previously
    FALSE, # smoking_status
    'non-clustering', # cigarettes_per_day
    'non-clustering', # age_start_smoking
    'non-clustering', # age_quit_smoking
    FALSE, # daily_snuff_6m
    FALSE, # daily_snuff_6m_currently
    FALSE, # snuff_status
    'non-clustering', # rural_childhood
    'non-clustering', # farm_childhood
    'non-clustering', # occupational_vgdf_exposure
    'non-clustering', # highest_academic_degree
    'non-clustering', # SEI
    'non-clustering', # times_exercise_per_week
    'clustering', # attack_10y --- respiratory symptom variables
    'clustering', # attack_12m
    'clustering', # attack_exercise
    'clustering', # attack_cold
    FALSE, # attack_dust
    FALSE, # attack_tobacco
    FALSE, # attack_fume
    'clustering', # attack_dust_tobacco_fume
    FALSE, # attack_cold_dust_tobacco_fume
    'clustering', # attack_pollen
    'clustering', # attack_fur
    FALSE, # attack_pollen_fur
    'clustering', # dyspnea_painkiller
    'clustering', # wheezing_12m
    'clustering', # wheezing_sob_12m
    'clustering', # wheezing_wo_cold_12m
    'clustering', # wheezing_recurrent
    'clustering', # cough_longstanding_12m
    'clustering', # cough_productive_recurrent
    'clustering', # cough_productive_3m
    'clustering', # cough_productive_3m_2y
    'clustering', # dyspnea_ground_level_walking
    'clustering', # woken_by_sob
    'clustering', # woken_by_cough
    'clustering', # woken_by_chest_tightness
    FALSE, # woken_by_sob_cough_chest_tightness
    'clustering', # rhinitis
    FALSE, # rhinitis_currently
    'clustering', # rhinitis_12m
    'clustering', # rhinitis_5d
    'clustering', # rhinitis_5d_5w
    'clustering', # rhinitis_conjunctivitis
    'clustering', # nasal_obstruction_recurrent
    'clustering', # rhinorrhea_recurrent
    'clustering', # nasal_obstruction_13w
    'clustering', # aching_sinus_13w
    'clustering', # nasal_secretion_13w
    'clustering' # reduced_smell_13w
)
# print(length(missingness_plot))

# sample (not used by its own, but rather if another column should be added)
SAMPLE <- c(
    '', # unique_id --- identifier variables
    '', # person_id
    '', # cohort
    '', # age --- background variables
    '', # gender
    '', # height
    '', # weight
    '', # bmi
    '', # asthma_family_history
    '', # asthma_self_reported
    '', # asthma_physician_diagnosed
    '', # asthma_physician_diagnosed_age
    '', # asthma_medication_use
    '', # asthma_hospitalization
    '', # copd_family_history
    '', # copd_self_reported
    '', # copd_physician_diagnosed
    '', # copd_medication_use
    '', # rhinitis_conjunctivitis_family_history
    '', # chronic_sinusitis_physician_diagnosed
    '', # rash_6m
    '', # rash_6m_12m
    '', # rash_only_hands
    '', # eczema_skin_allergy
    '', # snoring_loudly
    '', # difficulty_falling_asleep
    '', # waking_up_night
    '', # sleepy_during_day
    '', # waking_up_unable_fall_asleep
    '', # sleep_medication_use
    '', # other_lung_disease_self_reported
    '', # antihypertensive_medication_use
    '', # diabetes_medication_use
    '', # smoking_currently
    '', # smoking_previously
    '', # smoking_status
    '', # cigarettes_per_day
    '', # age_start_smoking
    '', # age_quit_smoking
    '', # daily_snuff_6m
    '', # daily_snuff_6m_currently
    '', # snuff_status
    '', # rural_childhood
    '', # farm_childhood
    '', # occupational_vgdf_exposure
    '', # highest_academic_degree
    '', # SEI
    '', # times_exercise_per_week
    '', # attack_10y --- respiratory symptom variables
    '', # attack_12m
    '', # attack_exercise
    '', # attack_cold
    '', # attack_dust
    '', # attack_tobacco
    '', # attack_fume
    '', # attack_dust_tobacco_fume
    '', # attack_cold_dust_tobacco_fume
    '', # attack_pollen
    '', # attack_fur
    '', # attack_pollen_fur
    '', # dyspnea_painkiller
    '', # wheezing_12m
    '', # wheezing_sob_12m
    '', # wheezing_wo_cold_12m
    '', # wheezing_recurrent
    '', # cough_longstanding_12m
    '', # cough_productive_recurrent
    '', # cough_productive_3m
    '', # cough_productive_3m_2y
    '', # dyspnea_ground_level_walking
    '', # woken_by_sob
    '', # woken_by_cough
    '', # woken_by_chest_tightness
    '', # woken_by_sob_cough_chest_tightness
    '', # rhinitis
    '', # rhinitis_currently
    '', # rhinitis_12m
    '', # rhinitis_5d
    '', # rhinitis_5d_5w
    '', # rhinitis_conjunctivitis
    '', # nasal_obstruction_recurrent
    '', # rhinorrhea_recurrent
    '', # nasal_obstruction_13w
    '', # aching_sinus_13w
    '', # nasal_secretion_13w
    '' # reduced_smell_13w
)
# print(length(SAMPLE))



# compile everything to a data.table
VARIABLES <- data.table(
    name,
    code_OLIN_IV_1996,
    code_OLIN_VI_2006,
    code_OLIN_VII_2016,
    code_WSAS_I_2008,
    code_WSAS_II_2016,
    cluster_variable,
    variable_type,
    missingness_plot,
    characteristics_plot,
    imputation_order
)
names(VARIABLES) <- c(
    'name',
    'OLIN-IV-1996',
    'OLIN-VI-2006',
    'OLIN-VII-2016',
    'WSAS-I-2008',
    'WSAS-II-2016',
    'cluster_variable',
    'variable_type',
    'missingness_plot',
    'characteristics_plot',
    'imputation_order'
)