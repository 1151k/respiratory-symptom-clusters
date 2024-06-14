# quantify and visualize frequency of respiratory symptoms



# load external packages
packages_all = c("data.table", "haven", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# local variables
load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled', '.Rda')) # pooled imputed data
# dt_pooled_imputed <- copy(dt_working)

print('loaded pooled imputed data')


# ####################

# get respiratory symptoms
cluster_variables <- as.vector(VARIABLES[cluster_variable == TRUE, 'name', with = FALSE])$name

print(table(dt_pooled_imputed$woken_by_cough, useNA = "always"))


# # remove the following from cluster_variables: c('attack_pollen', 'attack_fur', 'woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness', 'attack_cold', 'attack_dust_tobacco_fume')
# remove_vars <- c('attack_pollen', 'attack_fur', 'woken_by_sob', 'woken_by_cough', 'woken_by_chest_tightness', 'attack_cold', 'attack_dust_tobacco_fume')
# cluster_variables <- cluster_variables[!cluster_variables %in% remove_vars]
# # add the following to cluster_variables: attack_pollen_fur, attack_cold_dust_tobacco_fume, woken_by_sob_cough_chest_tightness
# add_vars <- c('attack_pollen_fur', 'attack_cold_dust_tobacco_fume', 'woken_by_sob_cough_chest_tightness')
# cluster_variables <- c(cluster_variables, add_vars)
print(cluster_variables)

dt_respiratory <- dt_pooled_imputed[, cluster_variables, with = FALSE]
# make all columns numeric
dt_respiratory[, (cluster_variables) := lapply(.SD, as.numeric), .SDcols = cluster_variables]

# # substract 1 from all values (because when recoding to numeric, the values are coded 1, 2 instead of 0, 1)
# dt_respiratory[, (cluster_variables) := lapply(.SD, function(x) x - 1), .SDcols = cluster_variables]
print(colnames(dt_respiratory))
# make a new column, called n_respiratory_symptoms, that counts the number of respiratory symptoms (values of 1)) for each subject
dt_respiratory[, n_respiratory_symptoms := rowSums(.SD), .SDcols = cluster_variables]
# in instances where n_respiratory_symptoms is >19, set it to 20
dt_respiratory[n_respiratory_symptoms > 19, n_respiratory_symptoms := 20]

# make a vector (0: number of rows in dt_respiratory with n_respiratory_symptoms == 0, 1: number of rows in dt_respiratory with n_respiratory_symptoms == 1 etc)
n_respiratory_symptoms_vector <- dt_respiratory[, .N, by = n_respiratory_symptoms]
# order the vector by n_respiratory_symptoms
n_respiratory_symptoms_vector <- n_respiratory_symptoms_vector[order(n_respiratory_symptoms)]
# change N to the percentage (N / nrow(dt_respiratory) * 100)
n_respiratory_symptoms_vector[, N := round(N / nrow(dt_respiratory) * 100,1)]
print(n_respiratory_symptoms_vector)

# ################



# Add a new variable to distinguish the first bar
n_respiratory_symptoms_vector$bar_type <- ifelse(n_respiratory_symptoms_vector$n_respiratory_symptoms == min(n_respiratory_symptoms_vector$n_respiratory_symptoms), 'first', 'rest')

symptom_freuqency_plot <- ggplot(n_respiratory_symptoms_vector, aes(x = n_respiratory_symptoms, y = N, fill = bar_type)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(N, "")), vjust = -0.9, size = 5) +
    labs(x = 'Number of respiratory symptoms', y = 'Percentage (%) of subjects') +
    scale_fill_manual(values = c('first' = '#1b1b1b', 'rest' = '#d3d3d3')) +
    scale_x_continuous(breaks = seq(0, 20, 1), labels = function(x) ifelse(x == 20, "≥20", as.character(x)), expand = c(0, 0)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "#ffffff", color = NA),
        axis.title.x = element_text(margin = margin(t = 12), size = 22),
        axis.title.y = element_text(margin = margin(r = 12), size = 22),
        axis.text.x = element_text(size = 19, color = "black", margin = margin(t = -20)),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        legend.box = "vertical",
        legend.key=element_blank(),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 21),
        plot.title = element_text(size = 26.5, hjust = 0.5, margin = margin(b = 12)),
        plot.margin = margin(t = 10, l = 10, b = 0, r = 10, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = NA),
        axis.line = element_blank()
    )
ggsave(paste0(folder_path, 'output/svg/', 'respiratory-symptoms-frequency.svg'), symptom_freuqency_plot, width = 16.5, height = 9.5, dpi = 300)

print(n_respiratory_symptoms_vector)
print('number of subjects with 5 or more respiratory symptoms:')
print(sum(n_respiratory_symptoms_vector[n_respiratory_symptoms >= 5, N]))
print('number of subjects with 10 or more respiratory symptoms:')
print(sum(n_respiratory_symptoms_vector[n_respiratory_symptoms >= 10, N]))
print('number of subjects with 20 or more respiratory symptoms:')
print(sum(n_respiratory_symptoms_vector[n_respiratory_symptoms >= 20, N]))



