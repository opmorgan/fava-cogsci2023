## Functions to style plots
gg_style <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 10) +
    theme(aspect.ratio = 1 / 1,
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA),
          ## Set a white background (so pngs aren't transparent)
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = "white", color = "white")
    )
  return(g_styled)
}

gg_style_demo <- function(g) {
  g_styled <- g +
    theme(
      aspect.ratio = 1 / 1,
    plot.title = element_text(hjust = 0.5),
      axis.ticks.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_rect(fill = NA,  color = "gray20")
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
    panel.border = element_rect(fill = NA, color = "gray50")
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


## 4a_figures_cogsci2023.Rmd
# gg_rt_1_common <- function(g, plot_colors) {
#   g_out <- g +
#         geom_hline(yintercept = 0, color = "gray50", linewidth = .5) +
#     geom_quasirandom(
#       alpha = .25, show.legend = F,
#       ) +
#     geom_boxplot(
#       alpha = 0.5,
#       size = 0.3,
#       varwidth = F,
#       outlier.shape = NA,
#       color = "gray20",
#       show.legend = F
#     ) +
#     stat_summary(
#       fun.data = mean_se,
#       geom = "errorbar",
#       position = "dodge",
#       linetype = 1,
#       color = "gray5",
#       width = .3,
#       show.legend = F
#     ) +
#     geom_point(
#       data = rt_descriptive_plot,
#       aes(y = mean),
#       color = "black",
#       shape = 21,
#       show.legend = F
#     ) +
#     annotate("text", x = 1.55, y = 350, hjust = "left", label = "⟵ \n LVF Global Bias" , size = 3) +
#     annotate("text", x = 1.55, y = -380, hjust = "right", label = "⟶ \n RVF Global Bias", size = 3) +
#     scale_y_reverse(breaks = seq(-1000, 1000, 50),
#                     minor_breaks = seq(-1000 , 1000, 25)) +
#     coord_flip(ylim = c(340,-390)) +
#     scale_fill_manual(values = plot_colors[c(1, 2)]) +
#     scale_color_manual(values = plot_colors[c(1, 2)]) +
#     labs(x = "Handedness", 
#          y = "RVF - LVF, Local - Global reaction time (ms)")
#   
#   g_out <- g_out |> 
#     gg_style() +
#     theme(
#       aspect.ratio = 1 / 6,
#       plot.title = element_text(hjust = 0.5),
#       strip.background = element_blank(),
#       panel.grid.minor = element_line(color = "gray92", linewidth = .2),
#       panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
#       panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
#       panel.border = element_rect(fill = NA, color = "gray50"),
#       ggh4x.facet.nestline = element_line(color = "gray50")
#     )
#   
#   return(g_out)
# }

gg_rt_1_horizontal <- function(title,
                               rt_subject_plot,
                               plot_colors) {
  
  rt_subject_plot_proc <- rt_subject_plot |>
    ## Relevel handedness so right is on top
    mutate(handedness = factor(handedness, levels = c("Left", "Right"))) |>
    ## Rename Y variable (LVF Global Bias)
    pivot_wider(names_from = c(field, level),
                values_from = rt) |>
    mutate(LVF_Global_Bias = (RVF_Global - RVF_Local) - (LVF_Global - LVF_Local)) |>
    mutate(all_one_group = "all_one_group") |>
    ungroup()
  
  rt_descriptive_plot <- rt_subject_plot_proc |>
  group_by(all_one_group, handedness) |>
  summarize(
    median = median(LVF_Global_Bias),
    mean = mean(LVF_Global_Bias),
    SE = sd(LVF_Global_Bias) / sqrt(length((LVF_Global_Bias)))
  )
  
  g <- ggplot(
    rt_subject_plot_proc,
    aes(
      x = handedness,
      y = LVF_Global_Bias,
      fill = handedness,
      color = handedness
  )) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = .5) +
    geom_quasirandom(
      alpha = .25, show.legend = F,
      ) +
    geom_boxplot(
      alpha = 0.5,
      size = 0.3,
      varwidth = F,
      outlier.shape = NA,
      color = "gray20",
      show.legend = F
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      position = "dodge",
      linetype = 1,
      color = "gray5",
      width = .3,
      show.legend = F
    ) +
    geom_point(
      data = rt_descriptive_plot,
      aes(y = mean),
      color = "black",
      shape = 21,
      show.legend = F
    ) +
    annotate("text", x = 1.55, y = 340, hjust = "left", label = "⟵ \n LVF Global Bias" , size = 3) +
    annotate("text", x = 1.55, y = -390, hjust = "right", label = "⟶ \n RVF Global Bias", size = 3) +
    scale_y_reverse(breaks = seq(-1000, 1000, 50),
                    minor_breaks = seq(-1000 , 1000, 25)) +
    coord_flip(ylim = c(340,-390)) +
    scale_fill_manual(values = plot_colors[c(1, 2)]) +
    scale_color_manual(values = plot_colors[c(1, 2)]) +
    labs(title = title,
         x = "Handedness", 
         y = "RVF - LVF, Local - Global reaction time (ms)")
  
  g <- g |> 
    gg_style() +
    theme(
      aspect.ratio = 1 / 6,
      plot.title = element_text(hjust = 0.5),
      strip.background = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50"),
      ggh4x.facet.nestline = element_line(color = "gray50")
    )
  
    return(g)
}

gg_rt_2_horizontal <- function(title,
                               rt_subject_plot,
                               plot_colors,
                               handedness_labeller = NULL)
{
  
  
  ## LVF_Bias by level for each subject
  rt_subject_plot_proc <- rt_subject_plot |> 
     ## Relevel so Right & Global are on top
      mutate(handedness = factor(handedness, levels = c("Right", "Left"))) |>
      mutate(level = factor(level, levels = c("Local", "Global"))) |>
    pivot_wider(names_from = c(field),
                values_from = rt) |> 
    mutate(LVF_Bias = RVF - LVF)
  
  
  ## Mean LVF_Bias by level, handedness
  rt_descriptive_plot <- rt_subject_plot_proc |>
    group_by(level, handedness) |>
    summarize(
      median = median(LVF_Bias),
      mean = mean(LVF_Bias),
      SE = sd(LVF_Bias) / sqrt(length((LVF_Bias)))
    )
  
  ## make default handedness labeller 
  if (is.null(handedness_labeller)) {
    n_subjects <- rt_subject_plot_proc |> group_by(handedness) |> summarize(n = n_distinct(subject))
    handedness_labeller <- c(
      Right = str_c(
        "Right handed \n (n = ",
        n_subjects |> filter(handedness == "Right") |> pull(n),
        ")"
      ),
      Mixed = str_c(
        "Mixed handed \n (n = ",
        n_subjects |> filter(handedness == "Mixed") |> pull(n),
        ")"
      ),
      Left = str_c(
        "Left handed \n (n = ",
        n_subjects |> filter(handedness == "Left") |> pull(n),
        ")"
      )
    )
  }
   
  
  g <- ggplot(
    rt_subject_plot_proc,
    aes(
      x = level,
      y = LVF_Bias,
      fill = handedness,
      color = handedness
  )) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = .5) +
    geom_quasirandom(
      alpha = .25, show.legend = F,
      ) +
    geom_boxplot(
      alpha = 0.5,
      size = 0.3,
      varwidth = F,
      outlier.shape = NA,
      color = "gray20",
      show.legend = F
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      position = "dodge",
      linetype = 1,
      color = "gray5",
      width = .3,
      show.legend = F
    ) +
    geom_point(
      data = rt_descriptive_plot,
      aes(y = mean),
      color = "black",
      shape = 21,
      show.legend = F
    ) +
    # facet_grid(rows = var(handedness)) +
    facet_wrap(~ handedness, nrow = 2, strip.position = "left",
               labeller = labeller(handedness = handedness_labeller)) +
    annotate("text", x = 1.55, y = 340, hjust = "left", label = "⟵ \n LVF Bias" , size = 3) +
    annotate("text", x = 1.55, y = -390, hjust = "right", label = "⟶ \n RVF Bias", size = 3) +
    scale_y_reverse(breaks = seq(-1000, 1000, 50),
                    minor_breaks = seq(-1000 , 1000, 25)) +
    coord_flip(ylim = c(340,-390)) +
    scale_fill_manual(values = plot_colors[c(2, 1)]) +
    scale_color_manual(values = plot_colors[c(2, 1)]) +
    labs(title = title,
         x = "", 
         # y = "Hemifield bias: RVF - LVF reaction time (ms)")
         y = "Difference in reaction time between RVF and LVF (ms)")
  
  g <- g |> 
    gg_style() +
    theme(
      aspect.ratio = 1 / 6,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      # axis.text.y = element_text(angle = 90, vjust = 0.0, hjust = 0.5),
      # strip.background = element_blank(),
      # strip.text.y.left = element_text(angle = 0),
      # strip.text.y.right = element_text(angle = 0),
      strip.background = element_rect(fill = "gray99", color = "gray50"),
      # strip.background = element_part_rect(side = "r", fill = "gray98", color = "gray50"),
      strip.placement = "outside",
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50"),
      ggh4x.facet.nestline = element_line(color = "gray50")
    )
  
    return(g)
}
