# train a gradient boost model and assess importance of features



# load external packages
packages_all = c("data.table", "catboost", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')
source('VARIABLES.r')



# settings
k <- 5
clustering_method <- 'lsh-k-prototypes'



# local pooled imputed data
load(paste0(folder_path, 'output/rda/', 'imputed-data--pooled', '.Rda'))
dt <- copy(dt_pooled_imputed)
# select only respiratory symptom variables
cluster_variables <- as.vector(VARIABLES[cluster_variable == TRUE, 'name', with = FALSE])
dt <- dt[, .SD, .SDcols = cluster_variables$name]
print(dim(dt))
# load pooled cluster labels
load(paste0(folder_path, 'output/rda/', 'cluster-data--labels--pooled--', clustering_method, '-', k, '.Rda'))
cluster_labels <- as.numeric(cluster_labels)
print(length(cluster_labels))
print(table(cluster_labels))
# make all columns factors
for (col in colnames(dt)) {
    dt[[col]] <- as.numeric(dt[[col]])
}
matrix <- as.matrix(dt)
cat_features_indices <- seq_along(dt) - 1


# train model
pool = catboost.load_pool(data = matrix, label = cluster_labels, cat_features = cat_features_indices)
fit_params <- list(
    iterations = 100,
    loss_function = 'MultiClass',
    random_seed = 1
)
pool = catboost.load_pool(matrix, label = cluster_labels)
model <- catboost.train(pool, params = fit_params)
catboost.get_model_params(model)
print('---------')

# extract feature importance
feature_importance <- catboost.get_feature_importance(
    model,
    pool = NULL,
    type = 'PredictionValuesChange',
    thread_count = -1
)
print('feature importance')
print(feature_importance)
# bind feature importance to variable names
feature_importance <- data.table(
    variable = colnames(dt),
    importance = feature_importance
)
# order by decreasing importance
setnames(feature_importance, 'importance.V1', 'importance')
feature_importance <- feature_importance[order(-importance)]
print('feature importance')
print(feature_importance)
print(sum(feature_importance$importance))


# plot
p <- ggplot(data = feature_importance, aes(x = reorder(variable, -importance), y = importance)) +
    geom_bar(stat = 'identity', fill = '#1b1b1b') +
    labs(y = 'Importance') +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 17, color = "black"),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 12), size = 22),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        legend.position = "none",
        legend.box = "vertical",
        legend.key=element_blank(),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 21),
        plot.title = element_text(size = 26.5, hjust = 0.5, margin = margin(b = 12)),
        plot.margin = margin(t = 10, l = 10, b = 0, r = 10, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_line(color = "#c3c3c3", linewidth = 0.5),
        panel.border = element_rect(fill = NA, color = NA),
        axis.line = element_blank()
    )
# save as svg 300 dpi output/svg
ggsave(paste0(folder_path, 'output/svg/', 'feature-importance--', clustering_method, '--', k, '.svg'), p, width = 16.5, height = 8.2, dpi = 300)