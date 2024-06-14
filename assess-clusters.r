# observe the distribution of variables in derived clusters



# Load packages
packages_all = c("data.table", "gtsummary", "flextable", "dplyr", "reticulate", "officer")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
use_python("/usr/local/bin/python3")
# local constants
source('VARIABLES.r')
source('CONSTANTS.r')



# local variables
clustering_method <- 'lsh-k-prototypes'
k <- 5
pooled_results <- FALSE
dataset_i <- 68 # only relevant if pooled_results == FALSE
comparison_cluster <- "2" # only relevant if pooled_results == TRUE, if so, set to FALSE or a any of 999, 0, 4, 3, 1, 2 (in "") for p-value calculation



# print dataset and k
print(paste('dataset =', dataset_i))
print(paste('clustering_method =', clustering_method))
print(paste('k =', k))
print(paste('pooled_results =', pooled_results))
cat('======\n')



if (pooled_results) {
    # load comorbidity
    load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled--comorbidity', '.Rda'))
    comorbidity_data <- copy(dt_working_comorbidity)
    # comorbidity_data <- comorbidity_data %>% select(unique_id, CCI_no_asthma_COPD, comorbidity_copd, comorbidity_asthma)
    comorbidity_data <- comorbidity_data %>% select(unique_id, CCI, CCI_no_asthma_COPD, CCI_no_respiratory, CCI_CHF, CCI_PVD, CCI_CVD, CCI_RD, CCI_Dementia, CCI_Hemiplegia, CCI_Diabetes_without_chronic_complication, CCI_Diabetes_with_chronic_complication, CCI_Renal_disease, CCI_Mild_liver_disease, CCI_Severe_liver_disease, CCI_Peptic_ulcer_disease, CCI_Malignancy, CCI_Metastatic_solid_tumor, CCI_Aids, CCI_Asthma, CCI_COPD, CCI_Chronic_other_pulmonary_disease)
    # order by unique_id
    comorbidity_data <- comorbidity_data[order(unique_id)]
    # load subject data
    load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled.Rda'))
    df <- copy(dt_working_comorbidity)
    # order by unique_id
    df <- df[order(unique_id)]
    print(dim(df))
    # load labels from cluster analysis
    load(paste0(folder_path, 'output/rda/', 'cluster-data--labels--pooled--', clustering_method, '-', k, '.Rda'))
} else {
    # load subject data
    load(paste0('output/rda/', 'imputed-data--', dataset_i, '.Rda'))
    df <- copy(dt_working)
    print(dim(df))
    # load labels from cluster analysis
    if (clustering_method == 'lsh-k-representatives' | clustering_method == 'lsh-k-prototypes') {
        np <- import("numpy")
        if (clustering_method == 'lsh-k-representatives') {
            cluster_labels <- np$load(paste0('output/npy/', 'cluster-data--labels--lsh-k-representatives--', dataset_i, '-k-', k, '.npy'))
        } else {
            cluster_labels <- np$load(paste0('output/npy/', 'cluster-data--labels--lsh-k-prototypes--', dataset_i, '-k-', k, '.npy'))
        }
    }
}
print('length')
print(length(cluster_labels))



####################
if (pooled_results) {
# load variables
cluster_variables <- VARIABLES[cluster_variable == TRUE, 'name', with = FALSE]
cluster_and_id_variables <- copy(cluster_variables)
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'person_id'))
non_cluster_variables <- as.vector(VARIABLES[cluster_variable == FALSE, 'name', with = FALSE])
characteristics_variables <- VARIABLES[characteristics_plot == TRUE, 'name', with = FALSE]
# set CCI_no_asthma_COPD to be a factor (levels: 0, 1-2, and ≥3, so slice accordingly)
print(table(df$CCI_no_asthma_COPD))
df$CCI_no_asthma_COPD <- cut(df$CCI_no_asthma_COPD, breaks = c(-1, 0, 2, 100), labels = c('0', '1-2', '≥3'))
# add some more variables as defined in the cluster analyses
df[, RHINITIS := ifelse(rhinitis_12m == 1 & rhinitis_5d == 0, 1, ifelse(rhinitis_12m == 1 & rhinitis_5d == 1 & rhinitis_5d_5w == 0, 2, ifelse(rhinitis_12m == 1 & rhinitis_5d == 1 & rhinitis_5d_5w == 1, 3, 0)))]
df[, PRODUCTIVE_COUGH := ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 0 & cough_productive_3m_2y == 0, 1, ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 1 & cough_productive_3m_2y == 0, 2, ifelse(cough_productive_recurrent == 1 & cough_productive_3m == 1 & cough_productive_3m_2y == 1, 3, 0)))]
df[, WHEEZING := ifelse(wheezing_12m == 1 & wheezing_sob_12m == 0 & wheezing_recurrent == 0, 1, ifelse(wheezing_12m == 1 & ((wheezing_sob_12m == 1 & wheezing_recurrent == 0) | (wheezing_sob_12m == 0 & wheezing_recurrent == 1)), 2, ifelse(wheezing_12m == 1 & wheezing_sob_12m == 1 & wheezing_recurrent == 1, 3, 0)))]
# set the above three variables as factors (ordered)
df$RHINITIS <- factor(df$RHINITIS, levels = c(0, 1, 2, 3), labels = c('0', 'rhinitis_12m', 'rhinitis_5d', 'rhinitis_5d_5w'))
df$PRODUCTIVE_COUGH <- factor(df$PRODUCTIVE_COUGH, levels = c(0, 1, 2, 3), labels = c('0', 'cough_productive_recurrent', 'cough_productive_3m', 'cough_productive_3m_2y'))
df$WHEEZING <- factor(df$WHEEZING, levels = c(0, 1, 2, 3), labels = c('0', 'wheezing_12m', 'wheezing recurrent or sob', 'wheezing recurrent and sob'))
# add the additional variables of interest
characteristics_variables <- rbind(characteristics_variables, list(name = 'cluster'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_no_asthma_COPD'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'attack_cold_dust_tobacco_fume'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'attack_pollen_fur'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'woken_by_sob_cough_chest_tightness'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'RHINITIS'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'PRODUCTIVE_COUGH'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'WHEEZING'))
# add the other CCI variables as well
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_CHF'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_PVD'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_CVD'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_RD'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Dementia'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Hemiplegia'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Diabetes_without_chronic_complication'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Diabetes_with_chronic_complication'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Renal_disease'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Mild_liver_disease'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Severe_liver_disease'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Peptic_ulcer_disease'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Malignancy'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Metastatic_solid_tumor'))
characteristics_variables <- rbind(characteristics_variables, list(name = 'CCI_Aids'))
} else {
# load variables
cluster_variables <- VARIABLES[cluster_variable == TRUE, 'name', with = FALSE]
cluster_and_id_variables <- copy(cluster_variables)
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'person_id'))
non_cluster_variables <- as.vector(VARIABLES[cluster_variable == FALSE, 'name', with = FALSE])
characteristics_variables <- VARIABLES[characteristics_plot == TRUE, 'name', with = FALSE]
characteristics_variables <- rbind(characteristics_variables, list(name = 'cluster'))
print(characteristics_variables)
}
####################





if (pooled_results) {
    # assign clusters correctly
    df$cluster <- cluster_labels
    # change the order of cluster (make it factor and sorted as follows: 999, 0, 4, 3, 1, 2)
    df$cluster <- factor(df$cluster, levels = c(999, 0, 4, 3, 1, 2))
} else {
    # load cluster-data--indices--', dataset_i from an .Rda file (R Data file)
    load(paste0('output/rda/', 'cluster-data--indices--', dataset_i, '.Rda'))
    # select the rows in df from cluster_data_indices
    df <- df[indices,]
    print(dim(df))
    # combine data with cluster labels
    df <- cbind(df, cluster = cluster_labels)
    print(dim(df))
}

# reduce the columns to the ones that will be shown in the characteristics tables
print('HIT')
df_clustered <- df[, characteristics_variables$name, with = FALSE]
print(colnames(df_clustered))
# make a CCI_diabetes a factor variable (1 if either of the two diabetes variables is 1, otherwise 0)
df_clustered$CCI_Diabetes <- as.factor(ifelse(df_clustered$CCI_Diabetes_without_chronic_complication == 1 | df_clustered$CCI_Diabetes_with_chronic_complication == 1, 1, 0))
# make a CCI_liver_disease a factor variable (1 if either of the two liver disease variables is 1, otherwise 0)
df_clustered$CCI_Liver_disease <- as.factor(ifelse(df_clustered$CCI_Mild_liver_disease == 1 | df_clustered$CCI_Severe_liver_disease == 1, 1, 0))
# make a CCI_cardiovascular a factor variable (1 if either of the two cardiovascular variables is 1, otherwise 0)
df_clustered$CCI_Cardiovascular <- as.factor(ifelse(df_clustered$CCI_CHF == 1 | df_clustered$CCI_PVD == 1 | df_clustered$CCI_CVD == 1, 1, 0))
# make a cancer a factor variable (1 if either of the two cancer variables is 1, otherwise 0)
df_clustered$CCI_Cancer <- as.factor(ifelse(df_clustered$CCI_Malignancy == 1 | df_clustered$CCI_Metastatic_solid_tumor == 1, 1, 0))

# set CCI_* variables as factor
df_clustered$CCI_CHF <- as.factor(df_clustered$CCI_CHF)
df_clustered$CCI_PVD <- as.factor(df_clustered$CCI_PVD)
df_clustered$CCI_CVD <- as.factor(df_clustered$CCI_CVD)
df_clustered$CCI_RD <- as.factor(df_clustered$CCI_RD)
df_clustered$CCI_Dementia <- as.factor(df_clustered$CCI_Dementia)
df_clustered$CCI_Hemiplegia <- as.factor(df_clustered$CCI_Hemiplegia)
df_clustered$CCI_Diabetes_without_chronic_complication <- as.factor(df_clustered$CCI_Diabetes_without_chronic_complication)
df_clustered$CCI_Diabetes_with_chronic_complication <- as.factor(df_clustered$CCI_Diabetes_with_chronic_complication)
df_clustered$CCI_Renal_disease <- as.factor(df_clustered$CCI_Renal_disease)
df_clustered$CCI_Mild_liver_disease <- as.factor(df_clustered$CCI_Mild_liver_disease)
df_clustered$CCI_Severe_liver_disease <- as.factor(df_clustered$CCI_Severe_liver_disease)
df_clustered$CCI_Peptic_ulcer_disease <- as.factor(df_clustered$CCI_Peptic_ulcer_disease)
df_clustered$CCI_Malignancy <- as.factor(df_clustered$CCI_Malignancy)
df_clustered$CCI_Metastatic_solid_tumor <- as.factor(df_clustered$CCI_Metastatic_solid_tumor)
df_clustered$CCI_Aids <- as.factor(df_clustered$CCI_Aids)


# get frequencies of positive answers for each cluster and each cluster (respiratory symptom) variable
results <- data.table()
# loop over each cluster
for (c in unique(df_clustered$cluster)) {
    if (!is.na(c)) {
        # subset the data for the current cluster
        df_subset <- df_clustered[cluster == c]
        df_subset <- df_subset[, cluster_variables$name, with = FALSE]
        # get the number of rows in which all values are 0
        n_no_symptoms <- nrow(df_subset[rowSums(df_subset == 1) == 0])
        print(paste('NO SYMPTOMS:', n_no_symptoms))
        for (col in colnames(df_subset)) {
            if (col != 'cluster') {
                positive = nrow(df_subset[eval(as.name(col)) == "1"])
                percentage = round(positive / nrow(df_subset) * 100, 1)
                results <- rbind(results, data.table(cluster = c, variable = col, perc = percentage))
            }
        }
    }
}
results <- results[order(cluster)]
# present the results
for (c in unique(results$cluster)) {
    if (!is.na(c)) {
        results_subset <- results[cluster == c]
        results_subset <- results_subset[order(-perc)]
        print(paste0('cluster ', c, ' dataset ', dataset_i, ' (', nrow(df_clustered[cluster == c]), ' subjects)'))
        print(results_subset[1:5,])
    }
}
# quit()


# if pooled_results, print the min and max of all continuous variables
if (pooled_results) {
    # get the min and max of all continuous variables
    results <- data.table()
    for (col in colnames(df_clustered)) {
        if (class(df_clustered[[col]]) == 'numeric') {
            min_val <- min(df_clustered[[col]], na.rm = TRUE)
            max_val <- max(df_clustered[[col]], na.rm = TRUE)
            results <- rbind(results, data.table(variable = col, min = min_val, max = max_val))
        }
    }
    print(results)
}


# # below can be uncommented if wanting to copy-paste values to a table without bothering with excluding the asymptomatic group
# df_clustered <- df_clustered[cluster != "999"]
# df_clustered$cluster <- factor(df_clustered$cluster, levels = c(0, 4, 3, 1, 2))

if (pooled_results & comparison_cluster != FALSE) {
    # drop 999 cluster (asymptomatic)
    df_clustered <- df_clustered[cluster != "999"]
    # change all cluster labels to 9999 except the comparison cluster
    df_clustered$cluster <- ifelse(df_clustered$cluster == comparison_cluster, comparison_cluster, "9999")
    print(paste('prepared to comparse cluster', comparison_cluster, 'to the rest'))
    # set theme for gtsummary to present p-values in the right fashion
    theme_gtsummary_journal("lancet")
}


# summarize data
if (pooled_results) {
    docx_name <- paste0('output/docx/', 'characteristics--clusters', '--', clustering_method, '--pooled-', k, '.docx')
} else {
    docx_name <- paste0('output/docx/', 'characteristics--clusters', '--', clustering_method, '--', dataset_i, '-', k, '.docx')
}
sect_properties <- officer::prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3,
    height = 11.7
  )
)
table <- df_clustered %>%
    tbl_summary(
        by = cluster,
        statistic = list(
            all_continuous() ~ "{mean} ± {sd}",
            all_categorical() ~ "{p}"
        )
    ) %>%
    add_p() %>%
    bold_labels() %>%
    as_flex_table() %>%
    save_as_docx(path = docx_name, pr_section = sect_properties)