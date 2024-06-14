# pool point estimates of HRs (and corresponding 95%CIs)



# Load packages
packages_all = c("data.table", "dplyr", "forestploter", "ggplot2", "grid")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
source('CONSTANTS.r')
source('VARIABLES.r')



# local variables
analysis <- 'mortality_date_lung_cancer' # 'all-cause-mortality', 'mortality_date_cardiovascular', 'mortality_date_respiratory', or 'mortality_date_lung_cancer'
subgroup_suffix <- 'asthma--1' # e.g., asthma, or gender--0
clustering_method <- 'lsh-k-prototypes'
k <- 5
dataset_start <- 1
dataset_end <- 100
xlim <-  c(0.5, 4.5)



# print settings
print(paste('clustering_method =', clustering_method))
print(paste('k =', k))
print(paste('dataset_start =', dataset_start))
print(paste('dataset_end =', dataset_end))
cat('========\n')




# data.table to hold all data of interest
dt_pooled <- data.table(
    dataset = integer(),
    cluster = integer(),
    point_estimate = numeric(),
    se = numeric()
)
# loop datasets
for (dataset_i in dataset_start:dataset_end) {
    # load data
    print(dataset_i)
    # add suffix if it's a subgroup analysis
    if (subgroup_suffix != '') {
        subgroup_suffix_linker <- '--'
    } else {
        subgroup_suffix_linker <- ''
    }
    # select the right file based on the analysis (all-cause or cause-specific mortality analysis)
    if (analysis == 'all-cause-mortality') {
        dt <- fread(paste0(folder_path, 'output/csv/cox-proportional-hazards-model--', dataset_i, subgroup_suffix_linker, subgroup_suffix, '.csv'))
    } else {
        dt <- fread(paste0(folder_path, 'output/csv/fine-gray-subdistribution-hazards-model--', analysis, '--', dataset_i, subgroup_suffix_linker, subgroup_suffix, '.csv'))
    }
    print(dt)
    # insert column 1, and 2 as point_estimate and se, respectively, the row as cluster, and dataset_i as dataset
    dt$cluster <- 1:nrow(dt)
    dt_pooled <- rbind(dt_pooled, data.table(dataset = dataset_i, cluster = dt$cluster, point_estimate = dt$hr_log, se = dt$se))
}



# function to pool hazard ratio and return pooled point estimate and 95%CI
pool_hr <- function(hr_estimates, SEs) {
    # get average of log hazard ratios
    hr_average <- mean(hr_estimates)
    # get confidence interval
    variances <- SEs^2
    within_variance <- mean(variances)
    between_variance <- (sum((hr_estimates-hr_average)^2) / (dataset_end-1))
    total_variance <- within_variance + between_variance + (between_variance / dataset_end)
    rm <- (between_variance+(between_variance/dataset_end))/within_variance
    degrees_of_freedom <- (dataset_end-1)*((1+1/(rm))^2)
    quantile <- qt(0.975, df = degrees_of_freedom, lower.tail = TRUE)
    lower <- hr_average - quantile * sqrt(total_variance)
    upper <- hr_average + quantile * sqrt(total_variance)
    # transform back hr_average, lower, and upper from log format
    hr_average <- exp(hr_average)
    lower <- exp(lower)
    upper <- exp(upper)
    # return list of the requested values
    return(list(hr_average, lower, upper, total_variance))
}



print('------------')
print(dt_pooled)
# make a data.table to hold the pooled point estimates and 95%CIs
dt_forest <- data.table(
    cluster = integer(),
    point_estimate = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    total_variance = numeric()
)
# for each unique value of "cluster" (in order; 1:k), get the pooled point estimate and 95%CI based on column hr_log and se
for (cluster_curr in 1:k) {
    # subset data.table
    print(paste('pooling for cluster=', cluster_curr))
    dt_subset <- dt_pooled[cluster == cluster_curr,]
    print(dt_subset)
    # ### USE FOR ASTHMA == 1 IN LUNG CANCER MORTALITY
    # # remove rows where either point_estimate or se is NA
    # dt_subset <- dt_subset[!is.na(point_estimate) & !is.na(se)]
    # ###
    # get pooled point estimate and 95%CI
    pooled_values <- pool_hr(dt_subset$point_estimate, dt_subset$se)
    # print results
    print(paste('cluster:', cluster_curr))
    print(paste('pooled point estimate:', pooled_values[[1]]))
    print(paste('95%CI:', pooled_values[[2]], pooled_values[[3]]))
    print(paste('total variance:', pooled_values[[4]]))
    print('------------')
    # add to dt_forest
    dt_forest <- rbind(dt_forest, data.table(cluster = cluster_curr, point_estimate = pooled_values[[1]], lower_ci = pooled_values[[2]], upper_ci = pooled_values[[3]], total_variance = pooled_values[[4]]))
}
# get the maximum and minimum total_variance
max_total_variance <- max(dt_forest$total_variance)
min_total_variance <- min(dt_forest$total_variance)
# make a column "size", which is inverted total_variance (max: 0.8 [for the lowest variance], min: 0.1 [for the highest variance])
dt_forest$point_estimation_box_size <- 0.3 + 0.6 * (1 - (dt_forest$total_variance - min_total_variance) / (max_total_variance - min_total_variance))
# make a formatted column (formatted as point_estimate (lower_ci, upper_ci)
dt_forest$effect_formatted <- paste0(round(dt_forest$point_estimate, 2), ' (', round(dt_forest$lower_ci, 2), ', ', round(dt_forest$upper_ci, 2), ')')
# append "Cluster " to the cluster column
dt_forest$cluster <- paste0('Cluster ', dt_forest$cluster)
# remove total_variance column
dt_forest <- dt_forest[, -c('total_variance')]
# add empty column with content paste(rep(" ", 26), collapse = " "
dt_forest$ci_column <- paste(rep(" ", 21), collapse = " ")
# rename the columns in dt_forest to publication-friendly names
setnames(dt_forest, c('cluster', 'effect_formatted', 'ci_column'), c('Cluster', 'HR (95%CI)', ' '))
print('=========')
print(dt_forest)
print(colnames(dt_forest))


# make forest plot
# set "pretty" x-axis ticks
min_point_estimate <- min(dt_forest$lower_ci)
max_point_estimate <- max(dt_forest$upper_ci)
geom_mean <- sqrt(min_point_estimate * max_point_estimate)
percentile_5 <- quantile(dt_forest$point_estimate, 0.01)
percentile_95 <- quantile(dt_forest$point_estimate, 0.99)

print(paste('min_point_estimate:', min_point_estimate))
print(paste('max_point_estimate:', max_point_estimate))
print(paste('geom_mean:', geom_mean))
x_ticks_locations <- round(c(percentile_5, geom_mean, percentile_95),1)
print(x_ticks_locations)
if (!any(abs(x_ticks_locations - 1) < 0.1)) {
    x_ticks_locations <- c(x_ticks_locations, 1)
}
x_ticks_locations <- sort(x_ticks_locations)

# # if it's not the overall analysis, remove the first column in the forest plot
# if (subgroup_suffix != '') {
#     forest_input <- dt_forest[, c(6, 7)]
#     ci_col <- 2
# } else { # main analysis
#     forest_input <- dt_forest[, c(1, 6, 7)]
#     ci_col <- 3
# }
forest_input <- dt_forest[, c(1, 6, 7)]
ci_col <- 3

# set forest plot title
forest_title <- readLines(paste0(folder_path, 'output/txt/forest-plot-title--', analysis, ifelse(subgroup_suffix != '', paste0('--', subgroup_suffix), ''), '.txt'))

forest_theme <- forestploter::forest_theme(
    core = list(bg_params=list(fill = c("transparent"))),
    xaxis_lwd = 1.3,
    refline_lwd = 1.3,
    refline_lty = 1,
    base_size = 9,
    footnote_cex = 0.75,
    summary_fill = "#575e92",
    summary_col = "#575e92",
    title_size = 0.4
)
forest_plot <- forestploter::forest(
    forest_input,
    est = dt_forest$point_estimate,
    lower = dt_forest$lower_ci,
    upper = dt_forest$upper_ci,
    sizes = dt_forest$point_estimation_box_size,
    ci_column = ci_col,
    ref_line = 1,
    x_trans = "log",
    # xlim = xlim,
    ticks_at = x_ticks_locations,
    theme = forest_theme
)

# change color by cluster (1: #608e38, 2: #b38b13, 3: #b85232, 4: #2f5495, 5: #a65b89)
# text color
forest_plot <- forestploter::edit_plot(forest_plot, col = 1:(ci_col-1) , row = 1, gp = grid::gpar(col = "#608e38"))
forest_plot <- forestploter::edit_plot(forest_plot, col = 1:(ci_col-1) , row = 2, gp = grid::gpar(col = "#b38b13"))
forest_plot <- forestploter::edit_plot(forest_plot, col = 1:(ci_col-1), row = 3, gp = grid::gpar(col = "#b85232"))
forest_plot <- forestploter::edit_plot(forest_plot, col = 1:(ci_col-1), row = 4, gp = grid::gpar(col = "#2f5495"))
forest_plot <- forestploter::edit_plot(forest_plot, col = 1:(ci_col-1), row = 5, gp = grid::gpar(col = "#a65b89"))
# ci color
forest_plot <- forestploter::edit_plot(forest_plot, col = ci_col , row = 1, which = "ci", gp = grid::gpar(fill = "#608e38", col = "#608e38"))
forest_plot <- forestploter::edit_plot(forest_plot, col = ci_col, row = 2, which = "ci", gp = grid::gpar(fill = "#b38b13", col = "#b38b13"))
forest_plot <- forestploter::edit_plot(forest_plot, col = ci_col, row = 3, which = "ci", gp = grid::gpar(fill = "#b85232", col = "#b85232"))
forest_plot <- forestploter::edit_plot(forest_plot, col = ci_col, row = 4, which = "ci", gp = grid::gpar(fill = "#2f5495", col = "#2f5495"))
forest_plot <- forestploter::edit_plot(forest_plot, col = ci_col, row = 5, which = "ci", gp = grid::gpar(fill = "#a65b89", col = "#a65b89"))
# add title
forest_plot <- add_text(
    forest_plot,
    text = forest_title,
    part = "header",
    row = 0,
    col = 1,
    just = "left",
    gp = gpar(fontface = "bold", fontsize = 10 )
)

# save as svg
ggsave(
    paste0(folder_path, 'output/svg/', 'forest-plot--', clustering_method, '--', k, '--', analysis, subgroup_suffix_linker, subgroup_suffix, '.svg'),
    forest_plot,
    width = 4,
    height = 5,
    dpi = 300
)