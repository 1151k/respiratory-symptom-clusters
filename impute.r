# impute data, evaluate imputation, and form composite variables



# load external packages
packages_all = c("data.table", "ggplot2", "miceRanger")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# local variables
load(paste0(folder_path, 'output/rda/', 'raw-data', '.Rda')) # raw data
i_start <- 94 # at which index (of 100 imputed datasets) to start
i_stop <- 100 # at which index (of 100 imputed datasets) to stop
max_iter <- 10
verbose <- TRUE
seed <- 1 + i_start
print(paste0('i_start:', i_start))
print(paste0('i_stop:', i_stop))
print(paste0('max_iter:', max_iter))
print(paste0('verbose:', verbose))
print(paste0('seed:', seed))
print('-------')
# variable sets for imputation
dependent_variables_level_1 <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1'), 'name', with = FALSE])
dependent_variables_level_2 <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2'), 'name', with = FALSE])
independent_variables <- as.vector(VARIABLES[imputation_order == 'independent', 'name', with = FALSE])
asthma_diagnosis_age_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-asthma'), 'name', with = FALSE])
smoking_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-smoker'), 'name', with = FALSE])
ever_smoking_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-ever-smoker'), 'name', with = FALSE])
ex_smoker_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-ex-smoker'), 'name', with = FALSE])



for (i in i_start:i_stop) {
    print(paste('STARTING FROM BEGINNING WITH DATASET', i, 'AND SEED', seed+i))
    # select independent variables
    dt_independent <- dt[, independent_variables$name, with = FALSE]
    # double-check to make sure that SEI is treated as a factor
    dt_independent$SEI <- as.factor(dt_independent$SEI)
    ###
    # dt_independent <- dt_independent[40000:45000,]
    ###
    # get a sense of the data
    print(paste0('dimensions of independent data:', dim(dt_independent)[1], ', ', dim(dt_independent)[2]))
    print('variable classes:')
    print(sapply(dt_independent, class))
    # impute independent variables
    set.seed(seed+i)
    mice_independent <- miceRanger::miceRanger(
        dt_independent,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    print('completed imputation of independent variables')
    # save model
    saveRDS(mice_independent, file = paste0(folder_path, 'output/rds/', 'mice_independent.rds'))
    # get imputed datasets
    imputed_independent_datasets <- miceRanger::completeData(mice_independent)
    # evaluate imputation
    model_oob_error_independent <- miceRanger::plotModelError(mice_independent)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--independent', '--', i, '.svg'), model_oob_error_independent, dpi = 300, width = 24, height = 15)
    model_variable_convergence <- miceRanger::plotVarConvergence(mice_independent)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--independent', '--', i, '.svg'), model_variable_convergence, dpi = 300, width = 24, height = 15)
    model_distribution <- miceRanger::plotDistributions(mice_independent)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--independent', '--', i, '.svg'), model_distribution, dpi = 300, width = 24, height = 15)
    # done with independent variables, now move on to the dependent variables
    dt_working <- copy(dt)
    ###
    # dt_working <- dt_working[40000:45000,]
    ###
    # select the nth imputed dataset (of the independent imputation results)
    imputed_independent_dataset <- imputed_independent_datasets[[1]]
    # update independent variables with imputed values
    for (independent_variable in independent_variables) {
        dt_working[, (independent_variable) := imputed_independent_dataset[, (independent_variable), with = FALSE]]
    }
    # update values of dependent variables
    dt_working$asthma_physician_diagnosed[dt_working$asthma_self_reported == 0] <- 0
    dt_working$asthma_physician_diagnosed_age[dt_working$asthma_physician_diagnosed == 0] <- NA
    dt_working$asthma_hospitalization[dt_working$asthma_self_reported == 0] <- 0
    dt_working$copd_physician_diagnosed[dt_working$copd_self_reported == 0] <- 0
    dt_working$rash_6m_12m[dt_working$rash_6m == 0] <- 0
    dt_working$rash_only_hands[dt_working$rash_6m == 0] <- 0
    dt_working$cigarettes_per_day[dt_working$smoking_currently == 0] <- NA
    dt_working$age_start_smoking[dt_working$smoking_currently == 0 & dt_working$smoking_previously == 0] <- NA
    dt_working$age_quit_smoking[dt_working$smoking_previously == 0] <- NA
    dt_working$attack_12m[dt_working$attack_10y == 0] <- 0
    dt_working$wheezing_sob_12m[dt_working$wheezing_12m == 0] <- 0
    dt_working$wheezing_wo_cold_12m[dt_working$wheezing_12m == 0] <- 0
    dt_working$cough_productive_3m[dt_working$cough_productive_recurrent == 0] <- 0
    dt_working$cough_productive_3m_2y[dt_working$cough_productive_3m == 0] <- 0
    dt_working$rhinitis_12m[dt_working$rhinitis == 0] <- 0
    dt_working$rhinitis_5d[dt_working$rhinitis_currently == 0 | dt_working$rhinitis_12m == 0] <- 0
    dt_working$rhinitis_5d_5w[dt_working$rhinitis_currently == 0 | dt_working$rhinitis_12m == 0] <- 0
    dt_working$rhinitis_conjunctivitis[dt_working$rhinitis_currently == 0 | dt_working$rhinitis_12m == 0] <- 0
    dt_working$smoking_previously[dt_working$smoking_currently == 1] <- 0
    # update values of composite variables
    dt_working$bmi <- dt_working$weight / (dt_working$height / 100) ^ 2
    dt_working$attack_cold_dust_tobacco_fume <- ifelse(
        dt_working$attack_cold == 1 | dt_working$attack_dust_tobacco_fume == 1,
        1,
        0
    )
    dt_working$attack_pollen_fur <- ifelse(
        dt_working$attack_pollen == 1 | dt_working$attack_fur == 1,
        1,
        0
    )
    dt_working$woken_by_sob_cough_chest_tightness <- ifelse(
        dt_working$woken_by_sob == 1 | dt_working$woken_by_cough == 1 | dt_working$woken_by_chest_tightness == 1,
        1,
        0
    )
    # select level-1 dependent data
    dt_dependent_level_1 <- dt_working[, dependent_variables_level_1$name, with = FALSE]
    print(paste('dimensions of dependent level 1 data:', dim(dt_dependent_level_1)[1], ', ', dim(dt_dependent_level_1)[2]))
    print('variable classes:')
    print(sapply(dt_dependent_level_1, class))
    # impute level-1 dependent variables
    set.seed(seed+i)
    mice_dependent_level_1 <- miceRanger::miceRanger(
        dt_dependent_level_1,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_dependent_level_1_dataset <- miceRanger::completeData(mice_dependent_level_1)[[1]] # select random dataset
    # evaluate imputation
    model_oob_error_dependent_level_1 <- miceRanger::plotModelError(mice_dependent_level_1)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--dependent-1', '--', i, '.svg'), model_oob_error_dependent_level_1, dpi = 300, width = 12, height = 7.5)
    model_variable_convergence_dependent_level_1 <- miceRanger::plotVarConvergence(mice_dependent_level_1)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--dependent-1', '--', i, '.svg'), model_variable_convergence_dependent_level_1, dpi = 300, width = 12, height = 7.5)
    model_distribution_dependent_level_1 <- miceRanger::plotDistributions(mice_dependent_level_1)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--dependent-1', '--', i, '.svg'), model_distribution_dependent_level_1, dpi = 300, width = 12, height = 7.5)
    # select level-2 dependent variables
    dependent_variables_level_2 <- as.vector(VARIABLES[cluster_variable == TRUE & (imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2'), 'name', with = FALSE])
    # update level-1 dependent variables with imputed values
    dt_working[, (dependent_variables_level_1$name) := imputed_dependent_level_1_dataset[, (dependent_variables_level_1$name), with = FALSE]]
    # update values of dependent variables
    dt_working$cough_productive_3m_2y[dt_working$cough_productive_3m == 0] <- 0
    dt_working$rhinitis_5d_5w[dt_working$rhinitis_5d == 0] <- 0
    dt_working$smoking_status <- ifelse(
        dt_working$smoking_currently == 1,
        1,
        ifelse(
            dt_working$smoking_previously == 1,
            2,
            0
        )
    )
    # select level-2 dependent data
    dt_dependent_level_2 <- dt_working[, dependent_variables_level_2$name, with = FALSE]
    print(paste('dimensions of dependent level 2 data:', dim(dt_dependent_level_2)[1], ', ', dim(dt_dependent_level_2)[2]))
    print('variable classes:')
    print(sapply(dt_dependent_level_2, class))
    # impute level-2 dependent variables
    set.seed(seed+i)
    mice_dependent_level_2 <- miceRanger::miceRanger(
        dt_dependent_level_2,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_dependent_level_2_dataset <- miceRanger::completeData(mice_dependent_level_2)[[1]]
    # evaluate imputation
    model_oob_error_dependent_level_2 <- miceRanger::plotModelError(mice_dependent_level_2)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--dependent-2', '--', i, '.svg'), model_oob_error_dependent_level_2, dpi = 300, width = 8, height = 2.5)
    model_variable_convergence_dependent_level_2 <- miceRanger::plotVarConvergence(mice_dependent_level_2)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--dependent-2', '--', i, '.svg'), model_variable_convergence_dependent_level_2, dpi = 300, width = 8, height = 2.5)
    model_distribution_dependent_level_2 <- miceRanger::plotDistributions(mice_dependent_level_2)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--dependent-2', '--', i, '.svg'), model_distribution_dependent_level_2, dpi = 300, width = 8, height = 2.5)
    # update level-2 dependent variables with imputed values
    dt_working[, (dependent_variables_level_2$name) := imputed_dependent_level_2_dataset[, (dependent_variables_level_2$name), with = FALSE]]
    # impute diagnosis age in subjects with asthma_physician_diagnosed == 1
    dt_asthma <- dt_working[asthma_physician_diagnosed == 1, asthma_diagnosis_age_variables$name, with = FALSE]
    print(paste('dimensions of asthma data:', dim(dt_asthma)[1], ', ', dim(dt_asthma)[2]))
    print('variable classes:')
    print(sapply(dt_asthma, class))
    set.seed(seed+i)
    mice_asthma <- miceRanger::miceRanger(
        dt_asthma,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_asthma_dataset <- miceRanger::completeData(mice_asthma)[[1]]
    # evaluate imputation
    model_oob_error_asthma <- miceRanger::plotModelError(mice_asthma)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--asthma', '--', i, '.svg'), model_oob_error_asthma, dpi = 300, width = 4, height = 2.5)
    model_variable_convergence_asthma <- miceRanger::plotVarConvergence(mice_asthma)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--asthma', '--', i, '.svg'), model_variable_convergence_asthma, dpi = 300, width = 4, height = 2.5)
    model_distribution_asthma <- miceRanger::plotDistributions(mice_asthma)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--asthma', '--', i, '.svg'), model_distribution_asthma, dpi = 300, width = 4, height = 2.5)
    # update asthma diagnosis age variables with imputed values
    dt_working[asthma_physician_diagnosed == 1, asthma_diagnosis_age_variables$name] <- imputed_asthma_dataset[, asthma_diagnosis_age_variables$name, with = FALSE]
    # impute cigarettes per day in current smokers
    dt_smoking <- dt_working[smoking_currently == 1, smoking_variables$name, with = FALSE]
    print(paste('dimensions of smoking data:', dim(dt_smoking)[1], ', ', dim(dt_smoking)[2]))
    print('variable classes:')
    print(sapply(dt_smoking, class))
    set.seed(seed+i)
    mice_smoking <- miceRanger::miceRanger(
        dt_smoking,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_smoking_dataset <- miceRanger::completeData(mice_smoking)[[1]]
    # evaluate imputation
    model_oob_error_smoking <- miceRanger::plotModelError(mice_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--smoking', '--', i, '.svg'), model_oob_error_smoking, dpi = 300, width = 4, height = 2.5)
    model_variable_convergence_smoking <- miceRanger::plotVarConvergence(mice_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--smoking', '--', i, '.svg'), model_variable_convergence_smoking, dpi = 300, width = 4, height = 2.5)
    model_distribution_smoking <- miceRanger::plotDistributions(mice_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--smoking', '--', i, '.svg'), model_distribution_smoking, dpi = 300, width = 4, height = 2.5)
    # update smoking variables with imputed values
    dt_working[smoking_currently == 1, smoking_variables$name] <- imputed_smoking_dataset[, smoking_variables$name, with = FALSE]
    # impute starting age of smoking in ever smokers
    dt_ever_smoking <- dt_working[smoking_currently == 1 | smoking_previously == 1, ever_smoking_variables$name, with = FALSE]
    print(paste('dimensions of ever smoking data:', dim(dt_ever_smoking)[1], ', ', dim(dt_ever_smoking)[2]))
    print('variable classes:')
    print(sapply(dt_ever_smoking, class))
    set.seed(seed+i)
    mice_ever_smoking <- miceRanger(
        dt_ever_smoking,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_ever_smoking_dataset <- miceRanger::completeData(mice_ever_smoking)[[1]]
    # evaluate imputation
    model_oob_error_ever_smoking <- miceRanger::plotModelError(mice_ever_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--ever-smoking', '--', i, '.svg'), model_oob_error_ever_smoking, dpi = 300, width = 4, height = 2.5)
    model_variable_convergence_ever_smoking <- miceRanger::plotVarConvergence(mice_ever_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--ever-smoking', '--', i, '.svg'), model_variable_convergence_ever_smoking, dpi = 300, width = 4, height = 2.5)
    model_distribution_ever_smoking <- miceRanger::plotDistributions(mice_ever_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--ever-smoking', '--', i, '.svg'), model_distribution_ever_smoking, dpi = 300, width = 4, height = 2.5)
    # update ever smoking variables with imputed values
    dt_working[smoking_currently == 1 | smoking_previously == 1, ever_smoking_variables$name] <- imputed_ever_smoking_dataset[, ever_smoking_variables$name, with = FALSE]
    # impute qutting age of smoking in past smokers
    dt_past_smoking <- dt_working[smoking_previously == 1, ex_smoker_variables$name, with = FALSE]
    print(paste('dimensions of past smoking data:', dim(dt_past_smoking)[1], ', ', dim(dt_past_smoking)[2]))
    print('variable classes:')
    print(sapply(dt_past_smoking, class))
    set.seed(seed+i)
    mice_past_smoking <- miceRanger(
        dt_past_smoking,
        m = 1, # as we will only use one imputation anyway
        maxiter = max_iter,
        verbose = verbose,
        returnModels = FALSE
    )
    imputed_past_smoking_dataset <- miceRanger::completeData(mice_past_smoking)[[1]]
    # evaluate imputation
    model_oob_error_past_smoking <- miceRanger::plotModelError(mice_past_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--oob-error--past-smoking', '--', i, '.svg'), model_oob_error_past_smoking, dpi = 300, width = 4, height = 2.5)
    model_variable_convergence_past_smoking <- miceRanger::plotVarConvergence(mice_past_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-convergence--past-smoking', '--', i, '.svg'), model_variable_convergence_past_smoking, dpi = 300, width = 4, height = 2.5)
    model_distribution_past_smoking <- miceRanger::plotDistributions(mice_past_smoking)
    ggsave(paste0(folder_path, 'output/svg/', 'imputation--variable-distribution--past-smoking', '--', i, '.svg'), model_distribution_past_smoking, dpi = 300, width = 4, height = 2.5)
    # update past smoking variables with imputed values
    dt_working[smoking_previously == 1, ex_smoker_variables$name] <- imputed_past_smoking_dataset[, ex_smoker_variables$name, with = FALSE]

    # save imputed data
    save(dt_working, file = paste0(folder_path, 'output/rda/', 'imputed-data--', i, '.Rda'))
    rm(dt_working)
    print(paste('Saved imputed data for dataset', i))
}