# evaluate missingness in data



# load external packages
packages_all = c("data.table", "visdat", "naniar", "ggplot2")
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



# get data and preprocess
# select all variables of interest
all_variables <- as.vector(VARIABLES[missingness_plot != FALSE, 'name', with = FALSE])
dt <- dt[, .SD, .SDcols = all_variables$name]

# print dimensions of data
print(paste0('dimensions of data: ', dim(dt)[1], ', ', dim(dt)[2]))
# count (and print) the number (and percentage) of rows (subjects) with no missing data
n_total <- nrow(dt)
n_no_missing <- sum(complete.cases(dt))
print(paste('number of subjects with no missing data:', n_no_missing))
# count (and print) the number (and percentage) of rows (subjects) with 1 missing data
n_one_missing <- sum(rowSums(is.na(dt)) == 1)
print(paste('number of subjects with 1 missing data:', n_one_missing))
# count (and print) the number (and percentage) of rows (subjects) with 2 missing data
n_two_missing <- sum(rowSums(is.na(dt)) == 2)
print(paste('number of subjects with 2 missing data:', n_two_missing))
# count (and print) the number (and percentage) of rows (subjects) with 3 missing data
n_three_missing <- sum(rowSums(is.na(dt)) == 3)
print(paste('number of subjects with 3 missing data:', n_three_missing))
print('----------')

# preprocess (variables that are dependent should be dealt with separately)
# asthma diagnosis age (set to 0 [temporary placeholder value to not overestimate missingness] if not diagnosed with asthma)
dt$asthma_physician_diagnosed_age <- ifelse(
    dt$asthma_physician_diagnosed == 0,
    0,
    dt$asthma_physician_diagnosed_age
)
# number of cigarettes smoked per day (set to 0 [temporary placeholder value to not overestimate missingness] if not currently smoking)
dt$cigarettes_per_day <- ifelse(
    dt$smoking_currently == 0,
    0,
    dt$cigarettes_per_day
)
# age of starting smoking (set to 0 [temporary placeholder value to not overestimate missingness] if never smoking)
dt$age_start_smoking <- ifelse(
    dt$smoking_currently == 0 & dt$smoking_previously == 0,
    0,
    dt$age_start_smoking
)
# age of quitting smoking (set to 0 [temporary placeholder value to not overestimate missingness] if not previously smoking)
dt$age_quit_smoking <- ifelse(
    dt$smoking_previously == 0,
    0,
    dt$age_quit_smoking
)
# set cigarettes_per_day to factor (for some reason not doing it in the previous step)
dt$cigarettes_per_day <- as.factor(dt$cigarettes_per_day)
# set SEI to factor (for some reason not doing it in the previous step)
dt$SEI <- as.factor(dt$SEI)
print('done with preprocessing data')
print('----------')



# plot missingness across all variables
# get the percentage of NAs in dt
na_percentage <- round(sum(is.na(dt)) / (nrow(dt) * ncol(dt)) * 100, 1)
# make the plot
variable_missingness <- visdat::vis_miss(
    dt,
    warn_large_data = FALSE,
    sort_miss = TRUE
) + labs(
    y = 'Subjects'
) + scale_fill_discrete(
    type = c('#d3d3d3', '#1b1b1b'),
    labels = c(paste0('Present (', (100-na_percentage), '%)'), paste0('Missing (', na_percentage, '%)'))
) + theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
) + scale_y_reverse(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
ggsave(
    paste0(folder_path, 'output/svg/', 'missingness--variables.jpg'),
    variable_missingness,
    dpi = 300,
    width = 14,
    height = 8.5
)
print('done with assessing missingness on variable basis')



# # plot missingness across all subjects (this unfortunately does not plot the subjects missing a few variables...)
# subject_missingness <- naniar::gg_miss_case(
#     dt,
#     order_cases = TRUE
# ) + labs(
#     x = 'Subjects',
#     y = 'Number of variables with missing data'
# ) + theme(
#     # panel.grid.major = element_blank(),
#     # panel.grid.minor = element_blank()
# ) + scale_x_reverse(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
# ggsave(
#     paste0(folder_path, 'output/svg/', 'missingness--subjects.svg'),
#     subject_missingness,
#     dpi = 300,
#     width = 28,
#     height = 17
# )
# print('done with assessing missingness on subject basis')
