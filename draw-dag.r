# add comorbidity data



# load external packages
packages_all = c("dagitty", "ggdag", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# load internal constants
source('CONSTANTS.r')



dag <- ggdag::dagify(
    # syntax: affected ~ cause
    # investigated causative path
    mortality ~ respiratory_symptoms,
    # affecting mortality
    mortality ~ age,
    mortality ~ smoking,
    mortality ~ occupational_vgdf_exposure,
    mortality ~ SES,
    mortality ~ sex,
    mortality ~ education,
    mortality ~ CCI,
    mortality ~ cohort,
    mortality ~ follow_up_time,
    mortality ~ BMI,
    # affecting respiratory symptoms
    respiratory_symptoms ~ age,
    respiratory_symptoms ~ smoking,
    respiratory_symptoms ~ occupational_vgdf_exposure,
    respiratory_symptoms ~ SES,
    respiratory_symptoms ~ education,
    respiratory_symptoms ~ CCI,
    respiratory_symptoms ~ cohort,
    respiratory_symptoms ~ sex,
    respiratory_symptoms ~ BMI,
    # affecting CCI
    CCI ~ age,
    CCI ~ smoking,
    CCI ~ sex,
    CCI ~ BMI,
    # affecting BMI
    BMI ~ age,
    BMI ~ SES,
    BMI ~ education,
    # affective SES
    SES ~ age,
    SES ~ education,
    # affecting education
    education ~ age,
    # affectign smoking
    smoking ~ age,
    smoking ~ SES,
    smoking ~ education,
    smoking ~ cohort,
    # affecting occupational_vgdf_exposure
    occupational_vgdf_exposure ~ education,
    # affecting age
    # affecting sex
    # definitions
    exposure = "respiratory_symptoms",
    outcome = "mortality",
    labels = c(
        age = "Age",
        respiratory_symptoms = "Respiratory symptoms (clusters)",
        mortality = "Mortality",
        smoking = "Smoking",
        occupational_vgdf_exposure = "Occupational VGDF exposure",
        SES = "Socioeconomic status",
        education = "Highest level of education",
        CCI = "Charlson Comorbidity Index (CCI)/respiratory comorbidity",
        cohort = "Cohort",
        follow_up_time = "Mortality register follow-up time",
        sex = "Sex",
        BMI = "Body mass index (BMI)"
    )
)

# Add the adjusted column
dag_plot <- ggdag::ggdag_adjustment_set(
    dag,
    use_labels = "label",
    shadow = TRUE,
    text = FALSE,
) + 
scale_color_manual(
    values = c("adjusted" = "#1b1b1b", "unadjusted" = "#d3d3d3"),
    labels = c("adjusted" = "Adjusted", "unadjusted" = "Unadjusted")
) +
theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.title = element_blank(),

)
ggsave(paste0(folder_path, 'output/svg/', 'dag.svg'), dag_plot, width = 16, height = 13, dpi = 300)
