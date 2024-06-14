# prepare data for cluster analysis



# load external packages
packages_all = c("data.table", "haven", "dplyr", "clustMixType")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')
options(warn = 1)



# local variables
i_start <- 1
i_end <- 100
combine <- TRUE
random_removal <- TRUE
removal_level <- 0.5 # proportion of rows (subject) to randomly remove; only read if random_removal = TRUE



for (i in i_start:i_end) {
    # local variables
    load(paste0(folder_path, 'output/rda/', 'imputed-data--', i, '.Rda'))

    # load all variables of interest
    cluster_variables <- as.vector(VARIABLES[cluster_variable == TRUE, 'name', with = FALSE])
    # cluster_variables$name <- c(cluster_variables$name, 'attack_pollen_fur', 'attack_cold_dust_tobacco_fume', 'woken_by_sob_cough_chest_tightness')
    # limit variables to selected ones
    dt_cluster_variables <- dt_working[, .SD, .SDcols = cluster_variables$name]
    # make all variables numeric
    dt_cluster_variables <- dt_cluster_variables[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cluster_variables$name]

    if (combine) {
        # remove rhinitis and attack_10y
        dt_cluster_variables <- dt_cluster_variables[, -c('rhinitis', 'attack_10y'), with = FALSE]
        # remove nasal_obstruction_13w, nasal_secretion_13w, aching_sinus_13w, and reduced_smell_13w
        dt_cluster_variables <- dt_cluster_variables[, -c('nasal_obstruction_13w', 'nasal_secretion_13w', 'aching_sinus_13w', 'reduced_smell_13w'), with = FALSE]
        # remove wheezing_wo_cold_12m
        dt_cluster_variables <- dt_cluster_variables[, -c('wheezing_wo_cold_12m'), with = FALSE]
        # remove rhinitis_conjunctivitis
        dt_cluster_variables <- dt_cluster_variables[, -c('rhinitis_conjunctivitis'), with = FALSE]

        # combine waking up from variables
        dt_cluster_variables[, WOKEN_BY_SOB_COUGH_CHEST_TIGHTNESS := ifelse(woken_by_sob == 1 | woken_by_cough == 1 | woken_by_chest_tightness == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness'), with = FALSE]

        # combine attack_fur and attack_pollen
        dt_cluster_variables[, ATTACK_POLLEN_FUR := ifelse(attack_fur == 1 | attack_pollen == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('attack_fur', 'attack_pollen'), with = FALSE]

        # combine attack_cold and attack_dust_tobacco_fume
        dt_cluster_variables[, ATTACK_COLD_DUST_TOBACCO_FUME := ifelse(attack_cold == 1 | attack_dust_tobacco_fume == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('attack_cold', 'attack_dust_tobacco_fume'), with = FALSE]

        # combine rhinitis_5d and rhinitis_5d_5w (ordinal variable: rhinitis_12m = 0, rhinitis_12m = 1, rhinitis_12m = 1 and rhinitis_5d = 1, rhinitis_5d = 1 and rhinitis_5d_5w = 1)
        dt_cluster_variables[, RHINITIS := ifelse(rhinitis_12m == 1 & rhinitis_5d == 0, 1, ifelse(rhinitis_12m == 1 & rhinitis_5d == 1 & rhinitis_5d_5w == 0, 2, ifelse(rhinitis_12m == 1 & rhinitis_5d == 1 & rhinitis_5d_5w == 1, 3, 0)))]
        dt_cluster_variables <- dt_cluster_variables[, -c('rhinitis_12m', 'rhinitis_5d', 'rhinitis_5d_5w'), with = FALSE]

        # make ordinal variable from chronic cough (cough_productive_recurrent = 0, cough_productive_recurrent = 1 and the rest 0, cough_productive_recurrent = 1 and cough_productive_3m = 1, and cough_productive_recurrent = 1 and cough_productive_3m = 0 and cough_productive_3m_2y = 1)
        dt_cluster_variables[, PRODUCTIVE_COUGH := ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 0 & cough_productive_3m_2y == 0, 1, ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 1 & cough_productive_3m_2y == 0, 2, ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 1 & cough_productive_3m_2y == 1, 3, 0)))]
        dt_cluster_variables <- dt_cluster_variables[, -c('cough_productive_recurrent', 'cough_productive_3m', 'cough_productive_3m_2y'), with = FALSE]

        # combine wheezing_12m and wheezing_sob_12m and wheezing_recurrent (ordinal)
        dt_cluster_variables[, WHEEZING := ifelse(wheezing_12m == 1 & wheezing_sob_12m == 0 & wheezing_recurrent == 0, 1, ifelse(wheezing_12m == 1 & ((wheezing_sob_12m == 1 & wheezing_recurrent == 0) | (wheezing_sob_12m == 0 & wheezing_recurrent == 1)), 2, ifelse(wheezing_12m == 1 & wheezing_sob_12m == 1 & wheezing_recurrent == 1, 3, 0)))]
        dt_cluster_variables <- dt_cluster_variables[, -c('wheezing_12m', 'wheezing_sob_12m', 'wheezing_recurrent'), with = FALSE]

        # order columns
        dt_cluster_variables <- dt_cluster_variables[, c('WHEEZING', 'attack_12m', 'attack_exercise', 'ATTACK_COLD_DUST_TOBACCO_FUME', 'ATTACK_POLLEN_FUR', 'dyspnea_painkiller', 'dyspnea_ground_level_walking', 'RHINITIS', 'rhinorrhea_recurrent', 'nasal_obstruction_recurrent', 'cough_longstanding_12m', 'PRODUCTIVE_COUGH', 'WOKEN_BY_SOB_COUGH_CHEST_TIGHTNESS'), with = FALSE]
    } else {
        # combine attack_fur and attack_pollen
        dt_cluster_variables[, attack_pollen_fur := ifelse(attack_fur == 1 | attack_pollen == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('attack_fur', 'attack_pollen'), with = FALSE]

        # combine attack_cold and attack_dust_tobacco_fume
        dt_cluster_variables[, attack_cold_dust_tobacco_fume := ifelse(attack_cold == 1 | attack_dust_tobacco_fume == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('attack_cold', 'attack_dust_tobacco_fume'), with = FALSE]

        # combine waking up from variables
        dt_cluster_variables[, woken_by_sob_cough_chest_tightness := ifelse(woken_by_sob == 1 | woken_by_cough == 1 | woken_by_chest_tightness == 1, 1, 0)]
        dt_cluster_variables <- dt_cluster_variables[, -c('woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness'), with = FALSE]

        # remove
        dt_cluster_variables <- dt_cluster_variables[, -c('attack_cold', 'attack_dust_tobacco_fume', 'attack_pollen', 'attack_fur', 'woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness', 'attack_10y', 'rhinitis'), with=F]# 'rhinitis_conjunctivitis', 'rhinitis_5d', 'wheezing_wo_cold_12m', 'reduced_smell_13w', 'aching_sinus_13w', 'nasal_obstruction_13w', 'nasal_secretion_13w'), with = FALSE]
    }


    # select subjects with all only 0 in all columns and put the indices in a vector
    indices <- which(rowSums(dt_cluster_variables == 0) != ncol(dt_cluster_variables))
    if (random_removal) {
        # among indices, randomly select a subset of indices corresponding to a proportion (defined by removal_level)
        indices <- sample(indices, floor(length(indices) * removal_level))
    }
    # remove subjects with all 0 in all columns
    dt_cluster_variables <- dt_cluster_variables[indices, ]
    # save indices
    save(indices, file = paste0(folder_path, 'output/rda/', 'cluster-data--indices--', i, '.Rda'))

    print('dim after removing all zeros')
    print(dim(dt_cluster_variables))
    print('colnames at this point')
    print(colnames(dt_cluster_variables))
    print('number of columns with NAs')
    print(sum(colSums(is.na(dt_cluster_variables)) > 0))


    # save data
    save(dt_cluster_variables, file = paste0(folder_path, 'output/rda/', 'cluster-data--', i, '.Rda'))


}
