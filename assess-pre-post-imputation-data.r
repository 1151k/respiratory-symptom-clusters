# observe the distribution of variables prior to imputation (full study population) vs after imputation (pooled imputed data)



# load external packages
packages_all = c("data.table", "flextable", "gtsummary")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# raw data
load(paste0(folder_path, 'output/rda/', 'raw-data', '.Rda')) # raw data
dt$imputed <- 'Non-imputed data'
# define the composite smoking status variable
dt[, smoking_status := ifelse(smoking_currently == 1, 1, ifelse(smoking_previously == 1, 2, ifelse(smoking_currently == 0 & smoking_previously == 0, 0, NA)))]
# define bmi
# define attack_cold_dust_tobacco_fume, attack_pollen_fur, and woken_by_sob_cough_chest_tightness
dt[, attack_cold_dust_tobacco_fume := ifelse(attack_cold == 1 | attack_dust_tobacco_fume == 1, 1, 0)]
dt[, attack_pollen_fur := ifelse(attack_pollen == 1 | attack_fur == 1, 1, 0)]
dt[, woken_by_sob_cough_chest_tightness := ifelse(woken_by_sob == 1 | woken_by_cough == 1 | woken_by_chest_tightness == 1, 1, 0)]
dt[, bmi := weight / (height/100)^2]

# pooled imputed data
load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled', '.Rda')) # pooled imputed data with comorbidity variables
dt_pooled_imputed$imputed <- 'Pooled imputed data'



# check the dimensions of the datasets
print('dimensions of the individual datasets')
print(dim(dt))
print(dim(dt_pooled_imputed))
# merge the datasets
dt_all <- rbind(dt, dt_pooled_imputed)
print('dimensions of the merged dataset')
print(dim(dt_all))



# load all variables of interest
all_variables <- as.vector(VARIABLES[characteristics_plot == TRUE, 'name', with = FALSE])
# add imputed
all_variables <- rbind(all_variables, data.table(name = 'imputed'))
# add attack_cold_dust_tobacco_fume, attack_pollen_fur, and woken_by_sob_cough_chest_tightness
all_variables <- rbind(all_variables, data.table(name = c('attack_cold_dust_tobacco_fume', 'attack_pollen_fur', 'woken_by_sob_cough_chest_tightness')))
# limit variables to selected ones
dt_all <- dt_all[, .SD, .SDcols = all_variables$name]
print('loaded all variables of interest')
print(dim(dt_all))



# summarize all variables and save to .docx
table <- dt_all %>%
    gtsummary::tbl_summary(
        by = imputed,
        statistic = list(all_continuous() ~ "{mean} ± {sd}")
        ) %>%
    # add_p(test.args = all_categorical("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
    add_p() %>%
    add_n() %>% # to highlight the missingness in the variables
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(folder_path, 'output/docx/', 'characteristics--pre-post-imputated', '.docx'))