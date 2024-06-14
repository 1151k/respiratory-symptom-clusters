# observe the distribution of variables prior to imputation by cohort



# load external packages
packages_all = c("data.table", "dplyr", "haven", "gtsummary")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# load data
load(paste0(folder_path, 'output/rda/', 'raw-data--comorbidity', '.Rda')) # raw data with comorbidity
dt <- copy(dt_working_comorbidity)
print(colnames(dt))
# define the composite smoking status variable
dt[, smoking_status := ifelse(smoking_currently == 1, 1, ifelse(smoking_previously == 1, 2, ifelse(smoking_currently == 0 & smoking_previously == 0, 0, NA)))]
# define bmi
dt[, bmi := weight / (height/100)^2]
# categorize CCI to 0, 1-2, and ≥3 (curently integer but should be strings)
dt[, CCI := cut(CCI, breaks = c(-Inf, 1, 2, Inf), labels = c('0', '1-2', '≥3'))]

# load all variables of interest
all_variables <- as.vector(VARIABLES[characteristics_plot == TRUE | VARIABLES$name == 'cohort', 'name', with = FALSE])
# add CCI
all_variables <- rbind(all_variables, data.table(name = 'CCI'))
# add smoking_currently, smoking_previously (uncomment these for specific smoking variables [lacking in ≥1 cohort])
# all_variables <- rbind(all_variables, data.table(name = c('smoking_currently', 'smoking_previously')))
# limit variables to selected ones
dt_all <- dt[, .SD, .SDcols = all_variables$name]
# set order of cohort column
dt_all$cohort <- factor(dt_all$cohort, levels = c('WSAS-I-2008', 'WSAS-II-2016', 'OLIN-IV-1996', 'OLIN-VI-2006', 'OLIN-VII-2016'))


# uncomment any of the below to retrieve p-value for variables which are lacking in ≥1 cohort
# bmi (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('bmi', 'cohort')]
# print('special run for bmi')
# asthma_physician_diagnosed_age (select only subjects with asthma_physician_diagnosed = 1, and then remove OLIN-IV-1996)
# dt_all <- dt_all[asthma_physician_diagnosed == 1 & cohort != 'OLIN-IV-1996', c('asthma_physician_diagnosed_age', 'cohort')]
# print('special run for asthma_physician_diagnosed_age')
# asthma_hospitalization (select only subjects with asthma_self_reported = 1, and then remove OLIN-IV-1996)
# dt_all <- dt_all[asthma_self_reported == 1 & cohort != 'OLIN-IV-1996', c('asthma_hospitalization', 'cohort')]
# print('special run for asthma_hospitalization')
# copd_medication_use (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('copd_medication_use', 'cohort')]
# print('special run for copd_medication_use')
# chronic_sinusitis_physician_diagnosed (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('chronic_sinusitis_physician_diagnosed', 'cohort')]
# print('special run for chronic_sinusitis_physician_diagnosed')
# rash_6m (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rash_6m', 'cohort')]
# print('special run for rash_6m')
# rash_6m_12m (select only subjects with rash_6m = 1 and remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rash_6m == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rash_6m_12m', 'cohort')]
# print('special run for rash_6m_12m')
# rash_only_hands (select only subjects with rash_6m = 1 and remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rash_6m == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rash_only_hands', 'cohort')]
# print('special run for rash_only_hands')
# eczema_skin_allergy (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('eczema_skin_allergy', 'cohort')]
# print('special run for eczema_skin_allergy')
# snoring_loudly (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('snoring_loudly', 'cohort')]
# print('special run for snoring_loudly')
# difficulty_falling_asleep (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('difficulty_falling_asleep', 'cohort')]
# print('special run for difficulty_falling_asleep')
# waking_up_night (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('waking_up_night', 'cohort')]
# print('special run for waking_up_night')
# sleepy_during_day (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('sleepy_during_day', 'cohort')]
# print('special run for sleepy_during_day')
# waking_up_unable_fall_asleep (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('waking_up_unable_fall_asleep', 'cohort')]
# print('special run for waking_up_unable_fall_asleep')
# sleep_medication_use (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('sleep_medication_use', 'cohort')]
# print('special run for sleep_medication_use')
# antihypertensive_medication_use (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('antihypertensive_medication_use', 'cohort')]
# print('special run for antihypertensive_medication_use')
# diabetes_medication_use (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('diabetes_medication_use', 'cohort')]
# print('special run for diabetes_medication_use')
# age_start_smoking (select only subjects with smoking_currently = 1 or smoking_previously = 1, and then remove OLIN-IV-1996)
# dt_all <- dt_all[(smoking_currently == 1 | smoking_previously == 1) & cohort != 'OLIN-IV-1996', c('age_start_smoking', 'cohort')]
# print('special run for age_start_smoking')
# age_quit_smoking (select only subjects with smoking_previously = 1, and then remove OLIN-IV-1996)
# dt_all <- dt_all[smoking_previously == 1 & cohort != 'OLIN-IV-1996', c('age_quit_smoking', 'cohort')]
# print('special run for age_quit_smoking')
# rural_childhood (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('rural_childhood', 'cohort')]
# print('special run for rural_childhood')
# farm_childhood (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('farm_childhood', 'cohort')]
# print('special run for farm_childhood')
# occupational_vgdf_exposure (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('occupational_vgdf_exposure', 'cohort')]
# print('special run for occupational_vgdf_exposure')
# highest_academic_degree (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('highest_academic_degree', 'cohort')]
# print('special run for highest_academic_degree')
# times_exercise_per_week (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('times_exercise_per_week', 'cohort')]
# print('special run for times_exercise_per_week')
# dyspnea_painkiller (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('dyspnea_painkiller', 'cohort')]
# print('special run for dyspnea_painkiller')
# woken_by_sob (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('woken_by_sob', 'cohort')]
# print('special run for woken_by_sob')
# woken_by_cough (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('woken_by_cough', 'cohort')]
# print('special run for woken_by_cough')
# rhinitis_12m (select only subjects with rhinitis = 1 and then remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rhinitis == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rhinitis_12m', 'cohort')]
# print('special run for rhinitis_12m')
# rhinitis_5d (select only subjects with rhinitis_12m = 1 and then remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rhinitis_12m == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rhinitis_5d', 'cohort')]
# print('special run for rhinitis_5d')
# rhinitis_5d_5w (select only subjects with rhinitis_5d = 1 and then remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rhinitis_5d == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rhinitis_5d_5w', 'cohort')]
# print('special run for rhinitis_5d_5w')
# rhinitis_conjunctivitis (select only subjects with rhinitis_12m and then remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[rhinitis_12m == 1 & cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('rhinitis_conjunctivitis', 'cohort')]
# print('special run for rhinitis_conjunctivitis')
# nasal_obstruction_recurrent (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('nasal_obstruction_recurrent', 'cohort')]
# print('special run for nasal_obstruction_recurrent')
# rhinorrhea_recurrent (remove OLIN-IV-1996)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996', c('rhinorrhea_recurrent', 'cohort')]
# print('special run for rhinorrhea_recurrent')
# nasal_obstruction_13w (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('nasal_obstruction_13w', 'cohort')]
# print('special run for nasal_obstruction_13w')
# aching_sinus_13w (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('aching_sinus_13w', 'cohort')]
# print('special run for aching_sinus_13w')
# nasal_secretion_13w (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('nasal_secretion_13w', 'cohort')]
# print('special run for nasal_secretion_13w')
# reduced_smell_13w (remove OLIN-IV-1996 and OLIN-VI-2006)
# dt_all <- dt_all[cohort != 'OLIN-IV-1996' & cohort != 'OLIN-VI-2006', c('reduced_smell_13w', 'cohort')]
# print('special run for reduced_smell_13w')

# summarize all variables and save to .docx
theme_gtsummary_journal(journal = "lancet") # uncomment when retrieving p-values
table <- dt_all %>%
    gtsummary::tbl_summary(
        by = cohort,
        statistic = list(all_continuous() ~ "{mean} ± {sd}")
        ) %>%
    # add_p() %>%
    add_p(test.args = all_tests("fisher.test") ~ list(workspace=2e9)) %>% # needed for some variables (e.g., chronic_sinusititis_physician_diagnosed)
    # add_p(test.args = all_categorical("fisher.test") ~ list(simulate.p.value = TRUE)) %>% # needed for some variables (e.g., snoring_loudly)
    add_n() %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(folder_path, 'output/docx/', 'characteristics--pre-imputed-by-cohort', '.docx'))