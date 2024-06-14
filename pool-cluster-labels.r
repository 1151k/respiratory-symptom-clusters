# pool cluster labels



# Load packages
packages_all = c("data.table", "gtsummary", "flextable", "dplyr", "reticulate")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
use_python("/usr/local/bin/python3")



# local variables
pooled_results <- FALSE
clustering_method <- 'lsh-k-prototypes'
k <- 5
dataset_start <- 1
dataset_end <- 100



# print settings
print(paste('clustering_method =', clustering_method))
print(paste('k =', k))
print(paste('dataset_start =', dataset_start))
print(paste('dataset_end =', dataset_end))
cat('========\n')



# make an empty data.table with rows: 63060 and one column (unique_id)
dt_pool <- data.table(unique_id = 1:63060)
dt_pool[, majority_cluster := NA]
dt_pool[, majority_count := NA]



switch_labels <- function(labels, order) {
    # if the labels are 0-5, and the order is c(2, 0, 1, 3, 5, 4), then 0 will be changed to 2, 1 will be changed to 0, 2 will be changed to 1, etc, but first, multiply all labels by 100
    labels <- labels*100
    for (i in 1:length(order)) {
        labels <- ifelse(labels == (i - 1)*100, order[i], labels)
    }
    return(labels)
}




for (dataset_i in dataset_start:dataset_end) {
    if (clustering_method == 'lsh-k-representatives' | clustering_method == 'lsh-k-prototypes') {
        np <- import("numpy")
        if (clustering_method == 'lsh-k-representatives') {
            cluster_labels <- np$load(paste0('output/npy/', 'cluster-data--labels--lsh-k-representatives--', dataset_i, '-k-', k, '.npy'))
        } else {
            cluster_labels <- np$load(paste0('output/npy/', 'cluster-data--labels--lsh-k-prototypes--', dataset_i, '-k-', k, '.npy'))
        }
    } else {
        load(paste0('output/rda/', 'cluster-data--labels--', clustering_method, '--', dataset_i, '--', k, '.Rda'))
    }

    # load indices
    load(paste0('output/rda/', 'cluster-data--indices--', dataset_i, '.Rda'))
    
    # switch labels
    order <- c(0, 1, 2, 3, 4)
    # if (dataset_i == X) {
    #     order <- c(0, 1, 2, 3, 4)
    # }
    print(table(cluster_labels))
    cluster_labels <- switch_labels(cluster_labels, order)
    print(table(cluster_labels))

    # save modified labels
    save(cluster_labels, file = paste0('output/rda/', 'cluster-data--labels--modified--', clustering_method, '--', dataset_i, '--', k, '.Rda'))

    # make a new column in dt_pool named dataset_i
    dt_pool[, paste0('dataset_', dataset_i) := 999]

    # set the cluster labels in dt_pool by the indices in indices in the column dataset_i
    dt_pool[indices, paste0('dataset_', dataset_i) := cluster_labels]

    print(paste('done with dataset', dataset_i))
    print('--------')

}

# set the most common cluster for each subject
dt_pool[, majority_cluster := apply(dt_pool, 1, function(x) {
    names(table(x))[which.max(table(x))]
})]
# count the number of times the most common cluster appears
dt_pool[, majority_count := apply(dt_pool, 1, function(x) {
    max(table(x))
})]

print(head(dt_pool))
print(table(dt_pool$majority_cluster))


# save labels
cluster_labels <- dt_pool$majority_cluster
print(table(dt_pool$majority_count))
# get rows with majority_count <= 50
print(table(dt_pool$majority_count <= 50))
# get rows with majority count >= 90
print(table(dt_pool$majority_count >= 90))
save(cluster_labels, file = paste0('output/rda/', 'cluster-data--labels--pooled--', clustering_method, '-', k, '.Rda'))