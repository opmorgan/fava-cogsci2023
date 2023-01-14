## Functions to style plots
gg_style <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 10) +
    theme(aspect.ratio = 1 / 1,
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA)
    )
    return(g_styled)
}

gg_style_demo <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 8) +
    theme(aspect.ratio = 1 / 1,
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, color = "gray50")
    )
    return(g_styled)
}

gg_style_means <- function(g) {
  g_styled <- g +
  theme_minimal() +
  theme(
    aspect.ratio = 2 / 1,
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_line(color = "gray92", linewidth = .2),
    panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
    panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
    panel.border = element_rect(fill = NA, color = "gray50"),
    panel.background = element_rect(fill = 'white', color = 'white'),
    plot.background = element_rect(fill = "white", color = "white") 
  )
    return(g_styled)
}


gg_color <- function(g, plot_colors) {
  if (missing(plot_colors)) {
    # By default, use colorblind-safe categorical palette
    plot_colors <- c("#4477AA", "#EE6677", "#CCBB44", "#66CCEE",
                     "#AA3377", "#228833", "#BBBBBB")
  }
  g_colored <- g +
    scale_color_manual(values = plot_colors) +
    scale_fill_manual(values = plot_colors)
  return(g_colored)
}