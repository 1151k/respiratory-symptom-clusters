# pool imputations and merge the resulting point estimates to on .Rda file



# load external packages
packages_all = c("data.table")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# local variables
# raw data
load(paste0(folder_path, 'output/rda/', 'raw-data', '.Rda'))
dt_raw <- copy(dt)
# indices of imputed datasets
imputed_datasets <- 1:100
variables <- VARIABLES[(imputation_order != 'N/A'), 'name', with = FALSE]



# remove from variables the individual variables that the composite variables are composed of
variables <- variables[!name %in% c('weight', 'height', 'smoking_currently', 'smoking_previously')] # previously also excluded 'attack_cold', 'attack_dust_tobacco_fume', 'attack_pollen', 'attack_fur', 'woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness'
# variables composed of multiple variables to add to the list of variables
composite_variables <- c('bmi', 'smoking_status', 'attack_cold_dust_tobacco_fume', 'attack_pollen_fur', 'woken_by_sob_cough_chest_tightness')
variables <- rbind(variables, data.table(name = composite_variables))
# variables to only pool in specific subgroups
subgroup_variables <- c('asthma_physician_diagnosed_age', 'cigarettes_per_day', 'age_start_smoking', 'age_quit_smoking')
variables <- variables[!name %in% subgroup_variables]
variables <- rbind(variables, data.table(name = subgroup_variables))



# remove everything except a few selected variables for testing purposes
# variables <- variables[name %in% c('cigarettes_per_day')]
print(variables)



# function to calculate mode
# loop each variable
for (variable in variables$name) {
    # check if variable is numeric or factor
    if (class(dt_raw[[variable]]) == 'numeric') {
        variable_type <- 'numeric'
    } else {
        variable_type <- 'factor'
    }
    print(paste('pooling for variable:', variable, 'of type:', variable_type))
    print('pre-imputation distribution')
    if (variable_type == 'factor') {
        print(table(dt_raw[[variable]], useNA = 'always'))
    } else {
        print(summary(dt_raw[[variable]]))
        print(paste('missing:', sum(is.na(dt_raw[[variable]]))))
    }
    # in case it's a subgroup variable, get a list of the IDs of the subjects in this subgroup
    if (variable %in% subgroup_variables) {
        if (variable == 'asthma_physician_diagnosed_age') {
            subgroup_ids <- dt_raw[asthma_physician_diagnosed == 1, unique_id]
        } else if (variable == 'cigarettes_per_day') {
            subgroup_ids <- dt_raw[smoking_currently == 1, unique_id]
        } else if (variable == 'age_start_smoking') {
            subgroup_ids <- dt_raw[smoking_currently == 1 | smoking_previously == 1, unique_id]
        } else if (variable == 'age_quit_smoking') {
            subgroup_ids <- dt_raw[smoking_previously == 1, unique_id]
        }
        # order by unique_id
        subgroup_ids <- subgroup_ids[order(subgroup_ids)]
        n_subjects <- length(subgroup_ids)
    } else {
        n_subjects <- nrow(dt_raw)
    }
    # make a data.table to hold all imputed values and the majority vote
    pooled_values <- data.table(pooled_value = rep(NA, n_subjects))
    for (i in imputed_datasets) {
        dataset_column_name <- paste0('dataset', i)
        # load imputed dataset
        load(paste0(folder_path, 'output/rda/', 'imputed-data--', i, '.Rda'))
        # define composite variables if needed
        if (variable == 'attack_cold_dust_tobacco_fume') {
            dt_working$attack_cold_dust_tobacco_fume <- ifelse(dt_working$attack_cold == 1 | dt_working$attack_dust_tobacco_fume == 1, 1, 0)
        } else if (variable == 'attack_pollen_fur') {
            dt_working$attack_pollen_fur <- ifelse(dt_working$attack_pollen == 1 | dt_working$attack_fur == 1, 1, 0)
        } else if (variable == 'woken_by_sob_cough_chest_tightness') {
            dt_working$woken_by_sob_cough_chest_tightness <- ifelse(dt_working$woken_by_sob == 1 | dt_working$woken_by_cough == 1 | dt_working$woken_by_chest_tightness == 1, 1, 0)
        }
        # in case it's a subgroup variable, select only the subjects in this subgroup
        if (variable %in% subgroup_variables) {
            setorder(dt_working, unique_id)
            dt_working <- dt_working[unique_id %in% subgroup_ids, ]
        }
        # select the variable of interest
        dt_working <- dt_working[, variable, with = FALSE]
        # show distribution in the imputed dataset
        if (variable_type == 'factor') {
            print(table(dt_working[[variable]], useNA = 'always'))
        } else {
            print(summary(dt_working[[variable]]))
        }
        # rename the variable to dataset_column_name
        setnames(dt_working, variable, dataset_column_name)
        # cbind the imputed data to pooled_values
        pooled_values <- cbind(pooled_values, dt_working)
        # remove imputed data
        rm(dt_working)
    }
    # calculate the majority vote, as mode if factor and mean if numeric
    if (variable_type == 'factor') {
        pooled_values[, pooled_value := apply(pooled_values, 1, function(x) {
            names(table(x))[which.max(table(x))]
        })]
    } else {
        pooled_values[, pooled_value := rowMeans(.SD, na.rm = TRUE)]
    }
    print(head(pooled_values))
    # check if there is at least one column with a value not identical to the pooled value
    if (any(apply(pooled_values, 1, function(x) {
        !all(x == x[1])
    }))) {
        print('WARNING: there are subjects with different imputed values')
        # count the number of subjects with different imputed values
        pooled_values[, different_values := apply(pooled_values, 1, function(x) {
            length(unique(x))
        })]
        print(pooled_values[different_values > 1, ])
        print(nrow(pooled_values[different_values > 1, ]))
    }
    if (variable %in% subgroup_variables) {
        # for each subject in the subgroup, replace the value with the pooled value
        # update values in column indicated by variable in dt_raw (only for subjects in subgroup_ids)
        dt_raw[unique_id %in% subgroup_ids, (variable) := pooled_values$pooled_value]
        print('post-merging distribution')
        if (variable_type == 'factor') {
            print(table(dt_raw[unique_id %in% subgroup_ids, ..variable], useNA = 'always'))
        } else {
            print(summary(dt_raw[unique_id %in% subgroup_ids, ..variable]))
            print(paste('missing:', sum(is.na(dt_raw[, ..variable]))))
        }  
    } else { # not a subgroup variable
        print('post-imputation distribution')
        if (variable_type == 'factor') {
            # print the rows in pooled_values correpsonding to the rows with misssing value for said variable in dt_raw
            print(pooled_values[is.na(dt_raw[[variable]]), ])
            print(table(pooled_values$pooled_value, useNA = 'always'))
        } else {
            # print the rows in pooled_values correpsonding to the rows with misssing value for said variable in dt_raw
            print(pooled_values[is.na(dt_raw[[variable]]), ])
            print(summary(pooled_values$pooled_value))
            print(paste('missing:', sum(is.na(pooled_values$pooled_value))))
        }
        # for each subject for said variable, replace the value with the pooled value
        dt_raw[, (variable) := pooled_values$pooled_value]
    }
    print('------')
    # save the pooled imputed data so far
    dt_pooled_imputed <- copy(dt_raw)
    save(dt_pooled_imputed, file = paste0(folder_path, 'output/rda/', 'imputed-data--pooled', '.Rda'))
    # remove pooled data.tables and explicitly run garbage collection
    rm(pooled_values)
    rm(dt_pooled_imputed)
    gc()
}