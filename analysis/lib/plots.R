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


#### 4a_figures_cogsci2023.Rmd ####
gg_ehi_dotarea <- function(ehi_plot_data,
                           plot_color) {

  # ehi_n_leftest <- ehi_plot_data |> filter(ehi == -100) |> pull(n)
  # ehi_n_rightest <- ehi_plot_data |> filter(ehi == 100) |> pull(n)

  line_color = plot_color

  g <- ggplot(ehi_plot_data, aes(x = ehi, y = n)) +
    geom_vline(xintercept = -40, color = "gray50") +
    geom_vline(xintercept = 40, color = "gray50") +
    geom_area(
      fill = plot_color,
      alpha = .5,
      color = NA,
      linewidth = 0
    ) +
    geom_line(color = line_color, linewidth = .5) +
    geom_point(fill = plot_color,
               shape = 21,
               size = 3) +
    annotate("text", x = -69, y = 170, label = "Left \n (n = 331)", size = 3.5) +
    annotate("text", x = 0, y = 170, label = "Mixed \n (n = 135)", size = 3.5) +
    annotate("text", x = 69, y = 170, label = "Right \n (n = 378)", size = 3.5) +
    annotate("text", x = -41, y = 90, label = "-40", size = 3, hjust = "right") +
    annotate("text", x = 39, y = 90, label = "+40", size = 3, hjust = "right") +
    scale_x_continuous(
      breaks = seq(-100, 100, 25),
      minor_breaks = seq(-100, 100, 25),
      expand = expansion(mult = c(.025, .025))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(x = "Laterality quotient from 4-item Veale Edinburgh Handedness Inventory (EHI)",
         y = "Number of participants",
         title = "Hand preference distribution")

  g <- g + theme_minimal(base_size = 10) +
    theme(
      aspect.ratio = 1 / 2,
      plot.title = element_text(margin = margin(b = 10, unit = "pt"),
                                hjust = 0.5),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .4),
      panel.grid.minor.x = element_blank(),
    )

  return(g)
}


## Categorical RT plots
gg_rt_1_horizontal <- function(title,
                               rt_subject_plot,
                               plot_colors,
                               handedness_labeller = NULL) {

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

  ## Make default handedness labeller
  if (is.null(handedness_labeller)) {
    n_subjects <- rt_subject_plot_proc |> group_by(handedness) |> summarize(n = n_distinct(subject))
    handedness_labeller <- c(
      Right = str_c(
        "Right \n(n = ",
        n_subjects |> filter(handedness == "Right") |> pull(n),
        ")"
      ),
      Mixed = str_c(
        "Mixed \n(n = ",
        n_subjects |> filter(handedness == "Mixed") |> pull(n),
        ")"
      ),
      Left = str_c(
        "Left \n(n = ",
        n_subjects |> filter(handedness == "Left") |> pull(n),
        ")"
      )
    )
  }

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
    annotate("text", x = 1.55, y = 340, hjust = "left", color = "gray30",
             label = "⟵ \n LVF Global Bias" , size = 3) +
    annotate("text", x = 1.55, y = -390, hjust = "right", color = "gray30",
             label = "⟶ \n RVF Global Bias", size = 3) +
    # scale_x_discrete(labels = labeller(handedness = handedness_labeller)) +
    scale_x_discrete(labels = c(handedness_labeller[["Left"]],
                                handedness_labeller[["Right"]])) +
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
                               handedness_labeller = NULL) {

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

  ## Prepare data to annotate first facet
  data_facet1 <- rt_descriptive_plot |>
    filter(handedness == "Right")


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
    geom_text(
      data = data_facet1, color = "gray30",
      x = 1.55, y = -340, hjust = "left",
      label = "⟵ \n LVF Bias" , size = 3
    ) +
    geom_text(
      data = data_facet1, color = "gray30",
      x = 1.55, y = 390, hjust = "right",
      label = "⟶ \n RVF Bias", size = 3
    ) +
    # annotate("text", x = 1.55, y = 340, hjust = "left", label = "⟵ \n LVF Bias" , size = 3) +
    # annotate("text", x = 1.55, y = -390, hjust = "right", label = "⟶ \n RVF Bias", size = 3) +
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


gg_rt_4_horizontal <- function(title,
                               rt_subject_plot,
                               plot_colors,
                               handedness_labeller = NULL) {


  ## RT by level for each subject
  rt_subject_plot_proc <- rt_subject_plot |>
     ## Relevel so Right & Global are on top
      mutate(handedness = factor(handedness, levels = c("Right", "Left"))) |>
      mutate(level = factor(level, levels = c("Local", "Global"))) |>
      group_by(subject, field, level, handedness) |>
      summarize(rt = median(rt))

  rt_subject_plot_proc


  ## Mean RT by level, handedness
  rt_descriptive_plot <- rt_subject_plot_proc |>
    group_by(field, level, handedness) |>
    summarize(
      median = median(rt),
      mean = mean(rt),
      SE = sd(rt) / sqrt(length((rt)))
    )

  rt_descriptive_plot

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

  ## Prepare data to annotate first facet
  data_facet1 <- rt_descriptive_plot |>
    filter(handedness == "Right" & field == "LVF")

  ## Make Field labeller
  field_labeller <- c(
    LVF = "Left Visual Field (LVF)",
    RVF = "Right Visual Field (RVF)"
  )


  g <- ggplot(
    rt_subject_plot_proc,
    aes(
      x = level,
      y = rt,
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
      show.legend = F,
      size = 1
    ) +
    # facet_grid(rows = var(handedness)) +
    facet_grid2(rows = vars(handedness), cols = vars(field),
               switch = "y",
               labeller = labeller(handedness = handedness_labeller,
                                   field = field_labeller)) +
    geom_text(
      data = data_facet1, color = "gray30",
      x = 1.55, y = 50, hjust = "left",
      label = "⟵ \n Faster" , size = 2.5
    ) +
    geom_text(
      data = data_facet1, color = "gray30",
      x = 1.55, y = 1250, hjust = "right",
      label = "⟶ \n Slower", size = 2.5
    ) +
    scale_y_continuous(
      minor_breaks = seq(0 , 1500, 100),
                       breaks = seq(0, 1500, 200),
      expand = expansion(mult = c(0, .08)), limits = c(0, NA)) +
    coord_flip(ylim = c(0, 1200)) +
    scale_fill_manual(values = plot_colors[c(2, 1)]) +
    scale_color_manual(values = plot_colors[c(2, 1)]) +
    labs(title = title,
         x = "",
         y = "Reaction time (ms)")

  g <- g |>
    gg_style() +
    theme(
      aspect.ratio = 1 / 4,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      strip.background = element_rect(fill = "gray99", color = "gray50"),
      strip.placement = "outside",
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50"),
      ggh4x.facet.nestline = element_line(color = "gray50")
    )

    return(g)
}

## Continuous RT plots
## Plot every data point ("cor")
gg_rt_1_cor <- function(title,
                        rt_subject_plot,
                        plot_colors) {
  rt_subject_plot_proc <- rt_subject_plot
  
  
  ## Prepare data to annotate first facet
  data_facet1 <- rt_subject_plot_proc
  
  g <-
    rt_subject_plot_proc |> ggplot(aes(x = ehi, y = LVF_Global_Bias)) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    geom_quasirandom(
      alpha = .2,
      aes(fill = handedness),
      shape = 21,
      show.legend = F
    ) +
    geom_smooth(method = "lm", color = "gray30") +
    scale_fill_manual(values = h_plot_colors) +
    scale_color_manual(values = plot_colors[c(1, 2)]) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 0,
      y = 310,
      hjust = "center",
      label = "↑ \n LVF Bias" ,
      size = 3
    ) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 1.55,
      y = -350,
      hjust = "center",
      label = "RVF Bias \n ↓",
      size = 3
    ) +
    scale_y_continuous(breaks = seq(-1000, 1000, 50),
                       minor_breaks = seq(-1000 , 1000, 25)) +
    coord_cartesian(ylim = c(-400, 350)) +
    labs(x = "Laterality quotient from EHI", y = "RVF - LVF, Local - Global reaction time (ms)",
         title = title)
  
  g <- g |> gg_style() +
    theme(
      aspect.ratio = 1 / 1,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50")
    )
  
  g + theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}

gg_rt_2_cor <- function(title,
                        rt_subject_plot,
                        plot_colors) {
  rt_subject_plot_proc <- rt_subject_plot
  
  
  ## Prepare data to annotate first facet
  data_facet1 <- rt_subject_plot_proc |> filter(level == "Global")
  
  g <- rt_subject_plot_proc |> ggplot(aes(x = ehi, y = LVF_Bias)) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    geom_quasirandom(
      alpha = .2,
      aes(fill = handedness),
      shape = 21,
      show.legend = F
    ) +
    geom_smooth(method = "lm", color = "gray30") +
    scale_fill_manual(values = h_plot_colors) +
    scale_color_manual(values = plot_colors[c(1, 2)]) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 0,
      y = 310,
      hjust = "center",
      label = "↑ \n LVF Bias" ,
      size = 3
    ) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 1.55,
      y = -350,
      hjust = "center",
      label = "RVF Bias \n ↓",
      size = 3
    ) +
    scale_y_continuous(breaks = seq(-1000, 1000, 50),
                       minor_breaks = seq(-1000 , 1000, 25)) +
    coord_cartesian(ylim = c(-400, 350)) +
    facet_wrap(~ level) +
    labs(x = "Laterality quotient from EHI", y = "Difference in reaction time between LVF and RVF (ms)",
         title = title)
  
  g <- g |> gg_style() +
    theme(
      aspect.ratio = 1 / 1,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50")
    )
  
  g + theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}


gg_rt_4_cor <- function(title,
                        rt_subject_plot,
                        plot_colors) {
  rt_subject_plot_proc <- rt_subject_plot
  
  
  ## Prepare data to annotate first facet
  data_facet1 <-
    rt_subject_plot_proc |> filter(level == "Global" & field == "LVF")
  
  ## Make Field labeller
  field_labeller <- c(LVF = "Left Visual Field (LVF)",
                      RVF = "Right Visual Field (RVF)")
  
  g <- rt_subject_plot_proc |> ggplot(aes(x = ehi, y = rt)) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    geom_quasirandom(
      alpha = .2,
      aes(fill = handedness),
      shape = 21,
      show.legend = F
    ) +
    geom_smooth(method = "lm", color = "gray30") +
    scale_fill_manual(values = h_plot_colors) +
    scale_color_manual(values = plot_colors[c(1, 2)]) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 0,
      y = 1150,
      hjust = "center",
      label = "↑ \n Slower" ,
      size = 2.5
    ) +
    geom_text(
      data = data_facet1,
      color = "gray50",
      x = 1.55,
      y = 150,
      hjust = "center",
      label = "Faster \n ↓",
      size = 2.5
    ) +
    scale_y_continuous(
      minor_breaks = seq(0 , 1500, 100),
      breaks = seq(0, 1500, 200),
      expand = expansion(mult = c(0, .08)),
      limits = c(0, NA)
    ) +
    coord_cartesian(ylim = c(0, 1200)) +
    facet_grid2(level ~ field,
                labeller = labeller(field = field_labeller)) +
    labs(x = "Laterality quotient from EHI", y = "Reaction time (ms)",
         title = title)
  
  g <- g |> gg_style() +
    theme(
      aspect.ratio = 1 / 1,
      plot.title = element_text(hjust = 0.5),
      strip.background = element_rect(fill = "gray99", color = "gray50"),
      strip.placement = "outside",
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50")
    )
  
  g + theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}


## Plot summary stats for binned data ("cor bin")
## Common style function for all cor bin plots
gg_style_cor_bin <- function(g) {
  g_out <- g |>
    gg_style() +
    theme(
      aspect.ratio = 1 / 1,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50")
    )
  return(g_out)
}

gg_rt_1_ntiles <- function(title = "",
                           y_title = "DV",
                           rt_subject_plot,
                           n_bins = 17,
                           plot_colors,
                           plot_color = "black",
                           group_by_level = FALSE,
                           direction_labels = NULL,
                           direction_labels_pos = NULL,
                           ylims = NULL,
                           ybreaks = NULL) {
  #### Common prep for all bin plots
  ##### Could be put into function: find_quantile_values()
  ## Add variable for bin
  bin_data <-
    rt_subject_plot |> mutate(bin = cut_interval(ehi, n_bins))
  ## Calculate mean, sem by quantile.
  bin_data_summary <- bin_data |>
    group_by(bin) |>
    summarize(
      mean_ehi = mean(ehi),
      # for sanity check
      mean_dv = mean(dv),
      median_dv = median(dv),
      n = n(),
      sem_dv = sd(dv) / sqrt(n)
    )
  
  ## Hack to quickly make two-box plot
  if (group_by_level == T) {
      bin_data_summary <- bin_data |>
    group_by(bin, level) |>
    summarize(
      mean_ehi = mean(ehi),
      # for sanity check
      mean_dv = mean(dv),
      median_dv = median(dv),
      n = n(),
      sem_dv = sd(dv) / sqrt(n)
    )
  }
  
  #### Function to plot 1-box graph
  ## Prepare data to annotate first facet
  data_facet1 <- bin_data
  
  ## TODO. If there are more than 5 bins, show just the lowest, middlest, and highest X-axis labels
  ## Prepare X-axis labels
  bin_levels <- levels(bin_data$bin)
  if (n_bins <= 5) {
    x_labels <- bin_levels
  } else if (n_bins > 5) {
    label_lower <- bin_levels[[1]]
    idx_middle <- median(1:n_bins) |> ceiling()
    label_middle <- bin_levels[[idx_middle]]
    label_upper <- bin_levels[[n_bins]]
    ## If odd, then show lower, upper, and middle.
    if ((n_bins %% 2)) {
      filler_labels <- rep("", (n_bins - 3) / 2)
      x_labels <-
        c(label_lower,
          filler_labels,
          label_middle,
          filler_labels,
          label_upper)
      ## If even, show lower, upper, and "middle"
    } else if (!(n_bins %% 2)) {
      filler_labels_left <- rep("", ((n_bins - 3) / 2 + 1))
      filler_labels_right <- rep("", (n_bins - 3) / 2)
      x_labels <- c(
        label_lower,
        filler_labels_left,
        label_middle,
        filler_labels_right,
        label_upper
      )
    }
  }
  
  ## Make plot
  g <- ggplot(data = bin_data, aes(x = bin, y = dv)) +
      stat_summary(
        fun.data = mean_se,
        geom = "linerange",
        linetype = 1,
        color = "gray20",
        show.legend = F,
      ) +
      geom_point(
        data = bin_data_summary,
        aes(y = mean_dv, size = n),
        fill = plot_color,
        color = "gray20",
        shape = 21,
        show.legend = F
      ) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    # scale_fill_manual(values = h_plot_colors) + ## to color handedness groups
    scale_x_discrete(labels = x_labels) +
    {
      if (!is.null(ybreaks))
        scale_y_continuous(
          breaks = seq(-1000, 1000, ybreaks$major),
          minor_breaks = seq(-1000 , 1000, ybreaks$minor)
        )
    } +
    {
      if (!is.null(ylims))
        coord_cartesian(ylim = c(ylims$lower, ylims$upper))
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = (n_bins / 2) + 0.5,
          y = direction_labels_pos$up,
          hjust = "center",
          label = str_c("↑ \n",  direction_labels$up) ,
          size = 3
        )
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = (n_bins / 2) + 0.5,
          y = direction_labels_pos$down,
          hjust = "center",
          label = str_c(direction_labels$down, "\n ↓"),
          size = 3
        )
    } +
    labs(title = title,
         y = y_title,
         x = "Laterality quotient from EHI")
  return(g)
}

gg_rt_1_line <- function(title = "",
                         y_title = "DV",
                         rt_subject_plot,
                         plot_colors,
                         plot_color = "black",
                         group_by_level = FALSE,
                         direction_labels = NULL,
                         direction_labels_pos = NULL,
                         ylims = NULL,
                         ybreaks = NULL) {
  
  ## Calculate mean, sem by quantile.
  summary_data <- rt_subject_plot |>
    group_by(ehi) |>
    summarize(
      # for sanity check
      mean_dv = mean(dv),
      median_dv = median(dv),
      n = n(),
      sem_dv = sd(dv) / sqrt(n)
    )
  
  
  #### Function to plot 1-box graph
  ## Prepare data to annotate first facet
  data_facet1 <- summary_data
  
  ## Make plot
  g <- ggplot(data = rt_subject_plot, aes(x = ehi, y = dv)) +
    stat_summary(
      fun.data = mean_se,
      geom = "linerange",
      linetype = 1,
      color = "gray70",
      show.legend = F,
      linewidth = .5
    ) +
    geom_point(
      data = summary_data,
      aes(y = mean_dv, size = n),
      fill = plot_color,
      color = "gray20",
      shape = 21,
      show.legend = F
    ) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    # scale_fill_manual(values = h_plot_colors) + ## to color handedness groups
    # scale_x_discrete(labels = x_labels) +
    {
      if (!is.null(ybreaks))
        scale_y_continuous(
          breaks = seq(-1000, 1000, ybreaks$major),
          minor_breaks = seq(-1000 , 1000, ybreaks$minor)
        )
    } +
    {
      if (!is.null(ylims))
        coord_cartesian(ylim = c(ylims$lower, ylims$upper))
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = 10,
          y = direction_labels_pos$up,
          hjust = "center",
          label = str_c("↑ \n",  direction_labels$up) ,
          size = 3
        )
    } +
    # {
    #   if (!is.null(direction_labels) & !is.null(direction_labels_pos))
    #     geom_text(
    #       data = data_facet1,
    #       color = "gray50",
    #       x = (n_bins / 2) + 0.5,
    #       y = direction_labels_pos$down,
    #       hjust = "center",
    #       label = str_c(direction_labels$down, "\n ↓"),
    #       size = 3
    #     )
  # } +
  labs(title = title,
       y = y_title,
       x = "EHI laterality quotient")
  return(g)
}

gg_rt_2_lmer <- function(title,
                         plot_data,
                         plot_colors,
                         handedness_labeller = NULL,
                         direction_labels = NULL,
                         direction_labels_pos = NULL,
                         ylims = NULL,
                         ybreaks = NULL,
                         n_subjects = list(
                           right = NA,
                           left = NA
                         )) {
  ## LVF blias by level for each group, witih 95%CI
  plot_proc <- plot_data |>
    mutate(handedness = factor(handedness, levels = c("Right", "Left"))) |>
    mutate(level = factor(level, levels = c("Local", "Global")))
  
  ## make default handedness labeller
  if (is.null(handedness_labeller)) {
    # n_subjects <- rt_subject_plot_proc |> group_by(handedness) |> summarize(n = n_distinct(subject))
    handedness_labeller <- c(
      Right = str_c("Right handed \n (n = ", n_subjects$right, ")"),
      Left = str_c("Left handed \n (n = ", n_subjects$left, ")")
    )
  }

  ## Prepare data to annotate first facet
  data_facet1 <- plot_data |>
    filter(handedness == "Right")


  g <- ggplot(
    plot_proc,
    aes(
      x = level,
      y = estimate,
      fill = handedness,
      color = handedness
  )) +
    # geom_line(aes(group = handedness), show.legend = F) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = .5) +
    geom_linerange(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      color = "gray30",
      position = position_dodge(1),
      linewidth = .5
    ) +
      geom_point(
      color = "black",
      shape = 23,
      show.legend = F,
      size = 4,
      position = position_dodge(1)
    )  +
    facet_wrap(~ handedness, nrow = 2, strip.position = "left",
               labeller = labeller(handedness = handedness_labeller)) +
    {
      if (!is.null(ybreaks))
        scale_y_reverse(
          breaks = seq(-1000, 1000, ybreaks$major),
          minor_breaks = seq(-1000 , 1000, ybreaks$minor)
        )
    } +
    {
      if (!is.null(ylims))
        coord_flip(ylim = c(ylims$lower, ylims$upper))
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = 1.5,
          y = direction_labels_pos$up,
          hjust = "left",
          label = str_c("⟵  \n",  direction_labels$up) ,
          size = 3
        )
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = 1.5,
          y = direction_labels_pos$down,
          hjust = "right",
          label = str_c("⟶ \n",  direction_labels$down),
          size = 3
        )
    } +
    scale_fill_manual(values = plot_colors[c(2, 1)]) +
    scale_color_manual(values = plot_colors[c(2, 1)]) +
    labs(title = title,
         x = "",
         # y = "Hemifield bias: RVF - LVF reaction time (ms)")
         y = "Difference in RT between LVF and RVF (ms)")
  
  g <- g |>
    gg_style() +
    theme(
      aspect.ratio = 1 / 2,
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
