# run an export results of mortality analyses



# load external packages
packages_all = c("cmprsk", "haven", "dplyr", "survival", "survminer", "ggplot2", "gridExtra", "cowplot")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# variables to set
cause_specific_analysis <- FALSE # if true, one of: mortality_date_cardiovascular, mortality_date_respiratory, or mortality_date_lung_cancer
subgroup <- '' # gender, age, CCI, time, asthma, COPD, time--5--age, time--10--age, time--5--gender, or time--10--gender (to do subgroup analysis)
gender_selected <- '' # when 0: men, 1: women
age_selected <- '' # '≤60', or '>60'
CCI_selected <- '' # '0', '1-2', or '≥3'
time_selected <- '' # '5' or '10'
asthma_present <- '' # '0' or '1'
COPD_present <- '' # '0' or '1'
i_range <- 1:100
pooled <- FALSE
clustering_method <- 'lsh-k-prototypes'
k <- 5
cli <- T # if running multiple iterations of this script through commandline, set to TRUE
# constants
end_date_OLIN <- "2016-12-31"
end_date_WSAS <- "2021-07-27"
investigated_deceased_codes <- list(
    'mortality_date_cardiovascular' = 1,
    'mortality_date_respiratory' = 2,
    'mortality_date_lung_cancer' = 3,
    'mortality_date_other' = 4
)
gender_data <- list(
    'men' = 0,
    'women' = 1
)
# get comorbidity and mortality data
load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled--comorbidity--mortality', '.Rda'))
comorbidity_mortality_data <- copy(dt)
comorbidity_mortality_data <- comorbidity_mortality_data %>% select(unique_id, person_id, cohort, mortality_date, mortality_date_cardiovascular, mortality_date_respiratory, mortality_date_lung_cancer, mortality_date_cause, CCI, CCI_no_asthma_COPD, CCI_no_respiratory, Register_Asthma, Register_COPD, Chronic_other_pulmonary_disease, comorbidity_copd, comorbidity_asthma)
# print('dim of comorbidity_mortality_data')
# print(dim(comorbidity_mortality_data))



# if cli (cli == TRUE) override arguments with command-line arguments
if (cli) {
    args <- commandArgs(trailingOnly = TRUE)
    print('OVERRIDING ARGUMENTS WITH CLI ARGUMENTS')
    if (args == '1') {
        cause_specific_analysis <- FALSE
        subgroup <- ''
    } else if (args == '2') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- ''
    } else if (args == '3') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- ''
    } else if (args == '4') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- ''
    } else if (args == '5') {
        cause_specific_analysis <- FALSE
        subgroup <- 'gender'
        gender_selected <- '0'
    } else if (args == '6') {
        cause_specific_analysis <- FALSE
        subgroup <- 'gender'
        gender_selected <- '1'
    } else if (args == '7') {
        cause_specific_analysis <- FALSE
        subgroup <- 'age'
        age_selected <- '≤60'
    } else if (args == '8') {
        cause_specific_analysis <- FALSE
        subgroup <- 'age'
        age_selected <- '>60'
    } else if (args == '9') {
        cause_specific_analysis <- FALSE
        subgroup <- 'CCI'
        CCI_selected <- '0'
    } else if (args == '10') {
        cause_specific_analysis <- FALSE
        subgroup <- 'CCI'
        CCI_selected <- '1-2'
    } else if (args == '11') {
        cause_specific_analysis <- FALSE
        subgroup <- 'CCI'
        CCI_selected <- '≥3'
    } else if (args == '12') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time'
        time_selected <- '5'
    } else if (args == '13') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time'
        time_selected <- '10'
    } else if (args == '14') {
        cause_specific_analysis <- FALSE
        subgroup <- 'asthma'
        asthma_present <- '0'
    } else if (args == '15') {
        cause_specific_analysis <- FALSE
        subgroup <- 'asthma'
        asthma_present <- '1'
    } else if (args == '16') {
        cause_specific_analysis <- FALSE
        subgroup <- 'COPD'
        COPD_present <- '0'
    } else if (args == '17') {
        cause_specific_analysis <- FALSE
        subgroup <- 'COPD'
        COPD_present <- '1'
    } else if (args == '18') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '≤60'
    } else if (args == '19') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '>60'
    } else if (args == '20') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '≤60'
    } else if (args == '21') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '>60'
    } else if (args == '22') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '0'
    } else if (args == '23') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '1'
    } else if (args == '24') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '0'
    } else if (args == '25') {
        cause_specific_analysis <- FALSE
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '1'
    } else if (args == '26') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'gender'
        gender_selected <- '0'
    } else if (args == '27') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'gender'
        gender_selected <- '1'
    } else if (args == '28') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'age'
        age_selected <- '≤60'
    } else if (args == '29') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'age'
        age_selected <- '>60'
    } else if (args == '30') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'CCI'
        CCI_selected <- '0'
    } else if (args == '31') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'CCI'
        CCI_selected <- '1-2'
    } else if (args == '32') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'CCI'
        CCI_selected <- '≥3'
    } else if (args == '33') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time'
        time_selected <- '5'
    } else if (args == '34') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time'
        time_selected <- '10'
    } else if (args == '35') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'asthma'
        asthma_present <- '0'
    } else if (args == '36') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'asthma'
        asthma_present <- '1'
    } else if (args == '37') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'COPD'
        COPD_present <- '0'
    } else if (args == '38') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'COPD'
        COPD_present <- '1'
    } else if (args == '39') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '≤60'
    } else if (args == '40') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '>60'
    } else if (args == '41') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '≤60'
    } else if (args == '42') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '>60'
    } else if (args == '43') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '0'
    } else if (args == '44') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '1'
    } else if (args == '45') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '0'
    } else if (args == '46') {
        cause_specific_analysis <- 'mortality_date_cardiovascular'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '1'
    } else if (args == '47') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'gender'
        gender_selected <- '0'
    } else if (args == '48') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'gender'
        gender_selected <- '1'
    } else if (args == '49') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'age'
        age_selected <- '≤60'
    } else if (args == '50') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'age'
        age_selected <- '>60'
    } else if (args == '51') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'CCI'
        CCI_selected <- '0'
    } else if (args == '52') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'CCI'
        CCI_selected <- '1-2'
    } else if (args == '53') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'CCI'
        CCI_selected <- '≥3'
    } else if (args == '54') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time'
        time_selected <- '5'
    } else if (args == '55') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time'
        time_selected <- '10'
    } else if (args == '56') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'asthma'
        asthma_present <- '0'
    } else if (args == '57') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'asthma'
        asthma_present <- '1'
    } else if (args == '58') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'COPD'
        COPD_present <- '0'
    } else if (args == '59') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'COPD'
        COPD_present <- '1'
    } else if (args == '60') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '≤60'
    } else if (args == '61') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '>60'
    } else if (args == '62') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '≤60'
    } else if (args == '63') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '>60'
    } else if (args == '64') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '0'
    } else if (args == '65') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '1'
    } else if (args == '66') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '0'
    } else if (args == '67') {
        cause_specific_analysis <- 'mortality_date_respiratory'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '1'
    } else if (args == '68') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'gender'
        gender_selected <- '0'
    } else if (args == '69') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'gender'
        gender_selected <- '1'
    } else if (args == '70') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'age'
        age_selected <- '≤60'
    } else if (args == '71') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'age'
        age_selected <- '>60'
    } else if (args == '72') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'CCI'
        CCI_selected <- '0'
    } else if (args == '73') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'CCI'
        CCI_selected <- '1-2'
    } else if (args == '74') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'CCI'
        CCI_selected <- '≥3'
    } else if (args == '75') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time'
        time_selected <- '5'
    } else if (args == '76') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time'
        time_selected <- '10'
    } else if (args == '77') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'asthma'
        asthma_present <- '0'
    } else if (args == '78') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'asthma'
        asthma_present <- '1'
    } else if (args == '79') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'COPD'
        COPD_present <- '0'
    } else if (args == '80') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'COPD'
        COPD_present <- '1'
    } else if (args == '81') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '≤60'
    } else if (args == '82') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--5--age'
        time_selected <- '5'
        age_selected <- '>60'
    } else if (args == '83') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '≤60'
    } else if (args == '84') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--10--age'
        time_selected <- '10'
        age_selected <- '>60'
    } else if (args == '85') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '0'
    } else if (args == '86') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--5--gender'
        time_selected <- '5'
        gender_selected <- '1'
    } else if (args == '87') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '0'
    } else if (args == '88') {
        cause_specific_analysis <- 'mortality_date_lung_cancer'
        subgroup <- 'time--10--gender'
        time_selected <- '10'
        gender_selected <- '1'
    } else {
        print('invalid argument')
        quit()
    }
    print(args)
}



for (dataset_i in i_range) {
    # LOAD AND COMBINE DATA
    if (pooled) {
        # load pooled imputed data
        load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled.Rda'))
        dt_imputed <- copy(dt_pooled_imputed)
        print('loaded pooled imputed dataset')
    } else {
        # load imputed data
        load(paste0(folder_path, 'output/rda/', 'imputed-data--', dataset_i, '.Rda'))
        dt_imputed <- copy(dt_working)
        print(paste('loaded imputed dataset', dataset_i))
    }
    # print('dim of dt_imputed')
    # print(dim(dt_imputed))

    if (pooled) {
        # load cluster labels
        load(paste0(folder_path, 'output/rda/', 'cluster-data--labels--pooled--', clustering_method, '-', k, '.Rda'))
        # print('length of cluster_labels (pooled)')
        # print(length(cluster_labels))
        # add cluster labels to dt_imputed
        dt_imputed$cluster <- cluster_labels
    } else {
        # load cluster labels
        load(paste0(folder_path, 'output/rda/', 'cluster-data--labels--modified--', clustering_method, '--', dataset_i, '--', k, '.Rda'))
        # print(paste0('dim of cluster_labels (', dataset_i, ')'))
        # print(dim(cluster_labels))

        # load indices
        load(paste0(folder_path, 'output/rda/', 'cluster-data--indices--', dataset_i, '.Rda'))
        # print('length of indices')
        # print(length(indices))

        # set cluster column (value 999) by default
        dt_imputed$cluster <- 999
        # in subjects with indices in the indices list, set the cluster label according to the cluster_labels vector
        dt_imputed$cluster[indices] <- cluster_labels
    }
    # set cluster to numerical column
    dt_imputed$cluster <- as.numeric(dt_imputed$cluster)
    # multiple cluster column values with 100 (all)
    dt_imputed$cluster <- dt_imputed$cluster * 100
    # if 0 set to 0, if 400 set to 2, if 300 set to 3, if 100 set to 4, if 200 set to 5, if 99900 set to 999
    dt_imputed$cluster[dt_imputed$cluster == 0] <- 1
    dt_imputed$cluster[dt_imputed$cluster == 400] <- 2
    dt_imputed$cluster[dt_imputed$cluster == 300] <- 3
    dt_imputed$cluster[dt_imputed$cluster == 100] <- 4
    dt_imputed$cluster[dt_imputed$cluster == 200] <- 5
    dt_imputed$cluster[dt_imputed$cluster == 99900] <- 999
    # set to factor (ordered as 999, 1, 2, 3, 4, 5)
    dt_imputed$cluster <- factor(dt_imputed$cluster, levels = c(999, 1, 2, 3, 4, 5), labels = c('999', '1', '2', '3', '4', '5'))
    # print('table of cluster')
    # print(table(dt_imputed$cluster))



    # sort dt_imputed and comorbidty_mortality_data by unique_id so that these can be merged easily
    setorder(dt_imputed, unique_id)
    setorder(comorbidity_mortality_data, unique_id)
    # merge dt_imputed with comorbidity_mortality_data
    dt_everything <- cbind(dt_imputed, comorbidity_mortality_data)
    # print('dim of dt_everything')
    # print(dim(dt_everything))
    # print('number of deaths in total (all cohorts)')
    # print(nrow(dt_everything[!is.na(dt_everything$mortality_date), ]))

    # add start date for each cohort
    dt_everything$start_date <- ifelse(
        dt_everything$cohort == 'OLIN-IV-1996',
        '1996-01-01',
        ifelse(
            dt_everything$cohort == 'WSAS-I-2008',
            '2008-01-01',
            ifelse(
                dt_everything$cohort == 'OLIN-VI-2006',
                '2006-01-01',
                ifelse(
                    dt_everything$cohort == 'WSAS-II-2016',
                    '2016-01-01',
                    NA
                )
            )
        )
    )


    # EXCLUDE SUBJECTS AND SELECT SUBCOHORTS
    # exclude subjects with cohort = 'OLIN-VII-2016' given their too short follow-up time
    dt_everything <- dt_everything[dt_everything$cohort != 'OLIN-VII-2016', ]


    # MORTALITY DEFINITIONS
    if (cause_specific_analysis != FALSE) {
        print(paste0('cause-specific analysis (', cause_specific_analysis, ')'))
        dt_everything$deceased <- ifelse(!is.na(dt_everything[[cause_specific_analysis]]), 1, 0)
    } else {
        print('all-cause analysis')
        dt_everything$deceased <- ifelse(!is.na(dt_everything$mortality_date), 1, 0)
    }
    # deceased codes
    if (cause_specific_analysis != FALSE) {
        dt_everything$decease_code <- ifelse(
            !is.na(dt_everything[[cause_specific_analysis]]), # cause of death is the investigated cause of death
            investigated_deceased_codes[[cause_specific_analysis]],
            ifelse(
                !is.na(dt_everything$mortality_date), # death but not the investigated cause of death
                4, # other cause of death
                0 # no death
            )
        )
    } else {
        dt_everything$decease_code <- ifelse(
            !is.na(dt_everything$mortality_date),
            4, # other cause of death
            0 # no death
        )
    }
    # days until event or end of follow-up
    dt_everything$survival_time <- ifelse(
        !is.na(dt_everything$mortality_date), # person has died
        difftime(as.Date(dt_everything$mortality_date), as.Date(dt_everything$start_date), units = 'days'),
        ifelse( # person has not died
            dt_everything$cohort == 'OLIN-IV-1996' | dt_everything$cohort == 'OLIN-VI-2006', 
            difftime(as.Date(end_date_OLIN), as.Date(dt_everything$start_date), units = 'days'), # if from OLIN (different from OLIN due to differing follow-up range)
            ifelse( # if from WSAS (different from OLIN due to differing follow-up range)
                dt_everything$cohort == 'WSAS-I-2008' | dt_everything$cohort == 'WSAS-II-2016',
                difftime(as.Date(end_date_WSAS), as.Date(dt_everything$start_date), units = 'days'),
                NA
            )
        )
    )
    dt_everything$survival_time <- as.numeric(dt_everything$survival_time)
    # print('years of follow-up')
    # print(sum(dt_everything$survival_time)/365)
    # print('number of deaths in total (all cohorts except OLIN-VII-2016)')
    # print(sum(dt_everything$deceased))


    # PREPARE CONFOUNDERS
    dt_everything$cluster = relevel(dt_everything$cluster, ref = "999")
    dt_everything$CCI_no_respiratory <- cut(dt_everything$CCI_no_respiratory, breaks = c(-1, 0, 2, 100), labels = c('0', '1-2', '≥3'))
    dt_everything$cohort <- as.factor(dt_everything$cohort)
    dt_everything$gender <- as.factor(dt_everything$gender)
    dt_everything$occupational_vgdf_exposure <- as.factor(dt_everything$occupational_vgdf_exposure)
    dt_everything$highest_academic_degree <- as.factor(dt_everything$highest_academic_degree)
    dt_everything$SEI <- as.numeric(dt_everything$SEI)
    dt_everything$SEI[dt_everything$SEI > 6] <- 7
    dt_everything$SEI <- as.factor(dt_everything$SEI)
    dt_everything$smoking_status <- as.factor(dt_everything$smoking_status)
    dt_everything$comorbidity_copd <- as.factor(dt_everything$comorbidity_copd)
    dt_everything$comorbidity_asthma <- as.factor(dt_everything$comorbidity_asthma)


    # NARROW DOWN TO SUBGROUPS WHERE NECESSARY
    subgroup_suffix <- ''
    forest_title <- 'Full cohort'
    if (subgroup != '') {
        # gender
        if (subgroup == 'gender' | subgroup == 'time--5--gender' | subgroup == 'time--10--gender') {
            dt_everything <- dt_everything[dt_everything$gender == gender_selected, ]
            subgroup_suffix <- paste0('--gender--', gender_selected)
            forest_title <- ifelse(gender_selected == '0', 'Men', 'Women')
        }
        # age
        if (subgroup == 'age' | subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            if (age_selected == '≤60') {
                dt_everything <- dt_everything[dt_everything$age <= 60, ]
            } else if (age_selected == '>60') {
                dt_everything <- dt_everything[dt_everything$age > 60, ]
            }
            subgroup_suffix <- paste0('--age--', age_selected)
            forest_title <- ifelse(age_selected == '≤60', '≤60 years', '>60 years')
        }
        # CCI
        if (subgroup == 'CCI') {
            if (CCI_selected == '0') {
                dt_everything <- dt_everything[dt_everything$CCI_no_respiratory == '0', ]
            } else if (CCI_selected == '1-2') {
                dt_everything <- dt_everything[dt_everything$CCI_no_respiratory == '1-2', ]
            } else if (CCI_selected == '≥3') {
                dt_everything <- dt_everything[dt_everything$CCI_no_respiratory == '≥3', ]
            }
            subgroup_suffix <- paste0('--CCI--', CCI_selected)
            forest_title <- ifelse(CCI_selected == '0', 'CCI 0', ifelse(CCI_selected == '1-2', 'CCI 1-2', 'CCI ≥3'))
        }
        # asthma
        if (subgroup == 'asthma') {
            dt_everything <- dt_everything[dt_everything$comorbidity_asthma == asthma_present, ]
            subgroup_suffix <- paste0('--asthma--', asthma_present)
            forest_title <- ifelse(asthma_present == '0', 'No asthma', 'Asthma')
        }
        # COPD
        if (subgroup == 'COPD') {
            dt_everything <- dt_everything[dt_everything$comorbidity_copd == COPD_present, ]
            subgroup_suffix <- paste0('--COPD--', COPD_present)
            forest_title <- ifelse(COPD_present == '0', 'No COPD', 'COPD')
        }
        # time
        if (subgroup == 'time' | subgroup == 'time--5' | subgroup == 'time--10' | subgroup == 'time--5--gender' | subgroup == 'time--10--gender' | subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            if (time_selected == '10' | subgroup == 'time--10--gender' | subgroup == 'time--10--age') {
                print('setting follow-up time to 10 years')
                # remove WSAS-II-2016 subjects, given that their follow-up time is ≤5 years
                dt_everything <- dt_everything[dt_everything$cohort != 'WSAS-II-2016', ]
                dt_everything$cohort <- factor(dt_everything$cohort, levels = c('OLIN-IV-1996', 'OLIN-VI-2006', 'WSAS-I-2008'))
                # set those with survival_time >= 10*365 to 10*365, and in these, make sure to set decease_code to 0 and deceased to 0
                dt_everything$decease_code[dt_everything$survival_time >= 10*365] <- 0
                dt_everything$deceased[dt_everything$survival_time >= 10*365] <- 0
                dt_everything$survival_time[dt_everything$survival_time >= 10*365] <- 10*365
            } else if (time_selected == '5' | subgroup == 'time--5--gender' | subgroup == 'time--5--age') {
                print('setting follow-up time to 5 years')
                # set those with survival_time >= 5*365 to 5*365, and in these, make sure to set decease_code to 0 and deceased to 0
                dt_everything$decease_code[dt_everything$survival_time >= 5*365] <- 0
                dt_everything$deceased[dt_everything$survival_time >= 5*365] <- 0
                dt_everything$survival_time[dt_everything$survival_time >= 5*365] <- 5*365
            }
            subgroup_suffix <- paste0('--time--', time_selected)
            forest_title <- ifelse(time_selected == '10', '10 years follow-up', '5 years follow-up')
        }
        # double subgroup handling (i.e., if subgrouping by both time AND age or gender)
        if (subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            if (subgroup == 'time--5--age') {
                subgroup_suffix <- paste0('--time--5--age--', age_selected)
            } else if (subgroup == 'time--10--age') {
                subgroup_suffix <- paste0('--time--10--age--', age_selected)
            }
            forest_title <- paste0(forest_title, ' (', ifelse(age_selected == '≤60', '≤60 years', '>60 years'), ')')
        } else if (subgroup == 'time--5--gender' | subgroup == 'time--10--gender') {
            if (subgroup == 'time--5--gender') {
                subgroup_suffix <- paste0('--time--5--gender--', gender_selected)
            } else if (subgroup == 'time--10--gender') {
                subgroup_suffix <- paste0('--time--10--gender--', gender_selected)
            }
            forest_title <- paste0(forest_title, ' (', ifelse(gender_selected == '0', 'Men', 'Women'), ')')
        }
    }
    print('number of subjects in current analysis')
    n_subjects_analysis <- nrow(dt_everything)
    print(n_subjects_analysis)
    print('number of deaths in current analysis')
    n_events_analysis <- sum(dt_everything$deceased)
    print(n_events_analysis)

    # set forest title (for the pooled forest plots)
    subgroup_value <- ''
    if (subgroup != '') {
        if (subgroup == 'gender') {
            subgroup_value <- paste0(subgroup, '--', gender_selected)
        } else if (subgroup == 'age') {
            subgroup_value <- paste0(subgroup, '--', age_selected)
        } else if (subgroup == 'CCI') {
            subgroup_value <- paste0(subgroup, '--', CCI_selected)
        } else if (subgroup == 'asthma') {
            subgroup_value <- paste0(subgroup, asthma_present)
        } else if (subgroup == 'COPD') {
            subgroup_value <- paste0(subgroup, COPD_present)
        } else if (subgroup == 'time') {
            subgroup_value <- paste0(subgroup, '--', time_selected)
        } else if (subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            subgroup_value <- paste0(subgroup, '--', age_selected)
        } else if (subgroup == 'time--5--gender' | subgroup == 'time--10--gender') {
            subgroup_value <- paste0(subgroup, '--', gender_selected)
        }
    }
    # if (!pooled) {
    print('not pooled, thus saving forest plot title')
    forest_title <- paste0(forest_title, ' (n=', format(n_subjects_analysis, big.mark=","), ', deaths=', format(n_events_analysis, big.mark=","), ')')
    write(forest_title, paste0(folder_path, 'output/txt/forest-plot-title--', ifelse(cause_specific_analysis != FALSE, cause_specific_analysis, 'all-cause-mortality'), ifelse(subgroup != '', paste0('--', subgroup_value), ''), '.txt'))
    print(paste('saved forest plot title:', forest_title, 'to url:', paste0(folder_path, 'output/txt/forest-plot-title--', ifelse(cause_specific_analysis != FALSE, cause_specific_analysis, 'all-cause-mortality'), ifelse(subgroup != '', paste0('--', subgroup_value), ''), '.txt')))
    # }
    
    
    # MORTALITY ANALYSES
    if (cause_specific_analysis == FALSE) {
        if (subgroup == '' & pooled == TRUE) {
            # Kaplan-Meier curves
            sfit <- survival::survfit(Surv(survival_time, deceased)~cluster, data=dt_everything)
            gg <- survminer::ggsurvplot(
                sfit,
                # (1: #608e38, 2: #b38b13, 3: #b85232, 4: #2f5495, 5: #a65b89)
                palette = c("#1b1b1b", "#608e38", "#b38b13", "#b85232", "#2f5495", "#a65b89"),
                surv.scale = "percent",
                risk.table=TRUE,
                legend.labs = c("Asymptomatic", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
                xlab = "Time (years)",
                ylab = "Probability of survival (%)",
                conf.int = TRUE,
                fontsize = 4,
                xscale = 'd_y',
                risk.table.title = 'Number at risk',
                break.x.by = 365.25*5 # break axis at even intervals
            ) 
            gg$plot <- gg$plot + theme(
                strip.text.x = element_text(hjust = 0),
                plot.margin = margin(5, 20, 5, 5),
                panel.grid.major.x = element_line(color = "#c3c3c3", linewidth = 0),
                panel.grid.major.y = element_line(color = "#c3c3c3", linewidth = 0.5),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "#ffffff"),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                # axis.title.x = element_text(margin = margin(t = 17)),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.title.y = element_text(margin = margin(r = 17)),
                legend.title = element_blank(),
                legend.position = "bottom"
            ) + scale_y_continuous(breaks = seq(0,1,by=0.25), labels = seq(0,100,by=25)) + guides(colour = guide_legend(nrow = 1)) # scale_x_continuous(expand = c(0, 0), breaks = seq(0, 365.25*20, by = 365.25*5), labels = seq(0, 20, by = 5))
            gg$table <- gg$table + labs(title = 'At risk') + theme(
                plot.title = element_text(size=12),
                axis.title.x = element_text(margin = margin(t = 17)),
                axis.title.y = element_blank()
            ) #+ scale_x_continuous(expand = c(0, 0), breaks = seq(0, 365.25*20, by = 365.25*5), labels = seq(0, 20, by = 5))
            ggsave(paste0(folder_path, 'output/svg/km-curve', subgroup_suffix, '.svg'), plot = gg$plot, dpi = 300, width = 8, height = 4.1)
            ggsave(paste0(folder_path, 'output/svg/km-table', subgroup_suffix, '.svg'), plot = gg$table, dpi = 300, width = 8, height = 1.9)


            # Save the plots
            plot1 <- gg$plot
            plot2 <- gg$table

            # Use plot_grid to combine the plots with the same width
            combined_plot <- plot_grid(plot1, plot2, ncol = 1, align = "v", rel_heights = c(1, 1))

            # Save the combined plot
            ggsave(paste0(folder_path, 'output/svg/combined_plot', subgroup_suffix, '.svg'), plot = combined_plot, dpi = 300, width = 8, height = 6)
        }

        # unadjusted Cox proportional hazards model
        print('COX PROPORTIONAL HAZARDS MODEL –– UNADJUSTED')
        cox_unadjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster, data = dt_everything)
        print(summary(cox_unadjusted))
        if (pooled != FALSE & subgroup == '') { quit() }

        # adjusted Cox proportional hazards model
        print('COX PROPORTIONAL HAZARDS MODEL –– ADJUSTED')
        if (subgroup == 'COPD') {
            print('not including comorbidity_copd in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        } else if (subgroup == 'asthma') {
            print('not including comorbidity_asthma in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_copd + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        } else if (subgroup == 'gender' | subgroup == 'time--5--gender' | subgroup == 'time--10--gender') {
            print('not including gender in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + age + bmi + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        } else if (subgroup == 'age' | subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            print('not including age in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        } else if (subgroup == 'CCI') {
            print('not including CCI_no_asthma_COPD (or comorbidity_asthma or comorbidity_copd) in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        } else {
            print('including all covariates in model')
            cox_adjusted <- survival::coxph(Surv(survival_time, deceased) ~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything)
        }
        print(summary(cox_adjusted))

        # save results from adjusted model
        csv_rows <- data.table(
            hr_log = numeric(),
            hr = numeric(),
            lower_ci = numeric(),
            upper_ci = numeric(),
            se = numeric(),
            p = numeric()
        )
        for (j in 1:k) {
            se_p <- c(summary(cox_adjusted)$coefficients[j, c(1,3,5)])
            hr_95ci <- c(summary(cox_adjusted)$conf.int[j, c(1,3,4)])
            csv_rows <- rbind(csv_rows, list(se_p[1], hr_95ci[1], hr_95ci[2], hr_95ci[3], se_p[2], se_p[3]))
        }
        print(csv_rows)
        # if (!pooled) {
        print('not pooled, thus saving csv file')
        # save csv
        fwrite(csv_rows, paste0(folder_path, 'output/csv/cox-proportional-hazards-model--', dataset_i, subgroup_suffix, '.csv'))
        # }

        if (subgroup == '' & pooled == TRUE) {
            # proportionality test and Schoenfeld residuals
            print('PROPORTIONALITY TEST')
            proprtionality_test <- survival::cox.zph(cox_adjusted)
            print(proprtionality_test)
            schoenfeld_residual_plots <- survminer::ggcoxzph(proprtionality_test, point.col = "#1b1b1b") + xlab("Time (days)")
            ggsave(paste0(folder_path, 'output/svg/cox-proportional-hazards-model--schoenfeld-residuals', subgroup_suffix, '.svg'), dpi = 300, plot = gridExtra::grid.arrange(grobs = schoenfeld_residual_plots, ncol = 3), width = 7*3, height = 4.5*4)
        }
    } else {
        # Fine-Gray subdistribution hazards model
        print('FINE-GRAY SUBDISTRIBUTION HAZARDS MODEL')
        if (subgroup == 'COPD') {
            print('not including comorbidity_copd in model')
            cov1 <- model.matrix(~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        } else if (subgroup == 'asthma') {
            print('not including comorbidity_asthma in model')
            cov1 <- model.matrix(~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_copd + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        } else if (subgroup == 'gender' | subgroup == 'time--5--gender' | subgroup == 'time--10--gender') {
            print('not including gender in model')
            cov1 <- model.matrix(~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + age + bmi + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        } else if (subgroup == 'age' | subgroup == 'time--5--age' | subgroup == 'time--10--age') {
            print('not including age in model')
            cov1 <- model.matrix(~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        } else if (subgroup == 'CCI') {
            print('not including CCI_no_asthma_COPD (or comorbidity_asthma or comorbidity_copd) in model')
            cov1 <- model.matrix(~ cluster + cohort + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        } else {
            cov1 <- model.matrix(~ cluster + cohort + CCI_no_asthma_COPD + comorbidity_asthma + comorbidity_copd + age + bmi + gender + occupational_vgdf_exposure + highest_academic_degree + SEI + smoking_status, data = dt_everything) [, -1]
        }
        CRR1 <- cmprsk::crr(ftime = dt_everything$survival_time, fstatus = dt_everything$decease_code, failcode = investigated_deceased_codes[[cause_specific_analysis]], cencode = 0, cov1 = cov1, maxiter = 100)
        print(summary(CRR1))

        if (subgroup == '' & pooled == TRUE) {
            # draw Schoenfeld plots for each cluster level
            svg(filename = paste0(folder_path, "output/svg/fine-gray--schoenfeld-residuals--", cause_specific_analysis, subgroup_suffix, ".svg"), width = 7, height = 7, pointsize = 12, bg = "white")
            par(mfrow = c(3, 2))  # Adjust as needed
            for (j in 1:5) {
                scatter.smooth(
                    CRR1$uft, CRR1$res[,j],
                    # main = names(CRR1$coef)[j],
                    main = paste("Cluster", j),
                    xlab = "Time (days)",
                    ylab = "Schoenfeld residuals"
                )
            }
            dev.off()
        }

        # save results
        csv_rows <- data.table(
            hr_log = numeric(),
            hr = numeric(),
            lower_ci = numeric(),
            upper_ci = numeric(),
            se = numeric(),
            p = numeric()
        )
        for (j in 1:k) {
            se_p <- c(summary(CRR1)$coef[j,c(1,3,5)])
            hr_95ci <- c(summary(CRR1)$conf.int[j,c(1,3,4)])
            csv_rows <- rbind(csv_rows, list(se_p[1], hr_95ci[1], hr_95ci[2], hr_95ci[3], se_p[2], se_p[3]))
        }
        print(csv_rows)
        # if (!pooled) {
        print('not pooled, thus saving csv')
        # save csv
        fwrite(csv_rows, paste0(folder_path, 'output/csv/fine-gray-subdistribution-hazards-model--', cause_specific_analysis, '--', dataset_i, subgroup_suffix, '.csv'))
        # }
    }
    cat('==============\n')
}