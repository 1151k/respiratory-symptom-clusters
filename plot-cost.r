# observe the distribution of variables in derived clusters



# Load packages
packages_all = c("data.table", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# local variables
clustering_method <- 'lsh-k-prototypes'
dataset_start <- 1
dataset_end <- 100


lines <- data.table(
    dataset = integer(),
    x = integer(),
    y = numeric()

)
for (i in dataset_start:dataset_end) {
    print(paste('dataset =', i))
    
    # load .csv file
    costs <- fread(paste0('output/csv/', 'cost--', clustering_method, '--dataset-', i, '.csv'), header = FALSE)
    # add costs to lines
    lines <- rbind(lines, data.table(dataset = i, x = 2:10, y = costs$V1))
}

print(lines)


# make a line plot based on lines: x: x, y:y, and each line is a different dataset, then save it as svg 300 dpi to output/svg
p <- ggplot(lines, aes(x = x, y = y, group = dataset)) +
    geom_line(color = "#1b1b1b") +
    scale_x_continuous(breaks = seq(from = 2, to = 10, by = 1), limits = c(0.9, 10.4), expand = c(0, 0)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    theme(
        panel.grid.major.x = element_line(color = "#c3c3c3", linewidth = 0),
        panel.grid.major.y = element_line(color = "#c3c3c3", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#ffffff"),
        legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 22), size = 24),
        axis.title.y = element_text(margin = margin(r = 22), size = 24),
        axis.text.x = element_text(size = 23, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 23, color = "black", margin = margin(r = 6)),
        plot.margin = margin(t = 50, l = 50),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
        x = 'Number of clusters',
        y = 'Cost (sum of within-cluster distance)'
    )
ggsave(paste0('output/svg/', 'cost--', clustering_method, '--datasets-', dataset_start, '-', dataset_end, '.svg'), plot = p, device = 'svg', dpi = 300, width = 16, height = 11)

print(warnings())