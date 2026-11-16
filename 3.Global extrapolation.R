library(ggplot2)
library(dplyr)
library(readxl)

script_dir <- function() {
  files <- c(
    sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)),
    unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  )
  files <- files[basename(files) == "3.Global extrapolation.R"]
  path <- if (length(files)) tail(files, 1L) else "3.Global extrapolation.R"
  dirname(normalizePath(path, mustWork = TRUE))
}

prediction_panel <- function(data, model, label, crop, show_x, show_y) {
  points <- data.frame(predicted = data[[model]], observed = data$Observed)
  fit <- summary(lm(observed ~ predicted, data = points))
  coefficients <- fit$coefficients
  slope <- coefficients["predicted", "Estimate"]
  intercept <- coefficients["(Intercept)", "Estimate"]
  p <- coefficients["predicted", "Pr(>|t|)"]
  stars <- as.character(symnum(p, cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                               symbols = c("***", "**", "*", "")))
  rmse <- sqrt(mean((points$observed - points$predicted)^2))
  annotation <- sprintf("y = %.2fx %s %.2f\nR² = %.2f%s\nRMSE = %.2f",
                        slope, if (intercept < 0) "-" else "+", abs(intercept),
                        fit$r.squared, stars, rmse)
  limit <- if (crop) 15 else 25
  ticks <- seq(0, limit, 5)

  ggplot(points, aes(predicted, observed)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.35) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "#D73027", linewidth = 0.35) +
    geom_point(shape = if (crop) 1 else 2, colour = if (crop) "#66C2A5" else "#BEBADA",
               size = 1.25, stroke = 0.5) +
    annotate("text", x = 0.04 * limit, y = 0.96 * limit, label = label,
             hjust = 0, vjust = 1, size = 2.8) +
    annotate("text", x = 0.17 * limit, y = 0.96 * limit, label = annotation,
             hjust = 0, vjust = 1, size = 2.8, lineheight = 1.05) +
    scale_x_continuous(breaks = ticks, labels = if (show_x) waiver() else NULL) +
    scale_y_continuous(breaks = ticks, labels = if (show_y) waiver() else NULL) +
    coord_fixed(xlim = c(0, limit), ylim = c(0, limit), expand = FALSE) +
    labs(x = if (show_x) "Predicted fraction (%)", y = if (show_y) "Observed fraction (%)") +
    theme_test(base_size = 9) +
    theme(axis.ticks.length = grid::unit(-1.1, "mm"),
          axis.text.x = element_text(margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 5)))
}

importance_panel <- function(data, label, crop) {
  colours <- c(Experiment = "#4EB3D3", soil = "#B3DE69", climate = "#C994C7")
  variable_labels <- function(labels) {
    as.expression(lapply(labels, function(label) {
      if (label %in% c("Atmospheric CO2", "Atmospheric CO₂")) {
        quote("Atmospheric" ~ CO[2])
      } else {
        label
      }
    }))
  }
  data$variable <- factor(data$variable, levels = data$variable[order(data$Percent)])
  data$Category <- factor(data$Category, levels = names(colours))
  axis_title <- if (crop) "Relative importance for cropland ecosystem (%)" else
    "Relative importance for grassland ecosystem (%)"
  limit <- if (crop) 30 else 35

  bars <- ggplot(data, aes(Percent, variable, fill = Category)) +
    geom_col(width = 0.55, colour = "black", linewidth = 0.3) +
    scale_y_discrete(labels = variable_labels) +
    scale_x_continuous(breaks = seq(0, limit, length.out = 5), expand = expansion(mult = 0)) +
    scale_fill_manual(values = colours) +
    coord_cartesian(xlim = c(0, limit)) +
    labs(x = axis_title, y = NULL) +
    theme_test(base_size = 9) +
    theme(legend.position = "none", axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.margin = margin(5.5, 12, 5.5, 5.5))

  shares <- data %>%
    group_by(Category) %>%
    summarise(percent = sum(Percent), .groups = "drop")
  # Round category labels to whole percentages while preserving a 100% total.
  whole <- floor(shares$percent)
  largest <- order(shares$percent - whole, decreasing = TRUE)
  add <- largest[seq_len(100L - sum(whole))]
  whole[add] <- whole[add] + 1L
  shares$label <- paste0(whole, "%")

  donut <- ggplot(shares, aes(x = 2, y = percent, fill = Category)) +
    geom_col(width = 0.8, colour = NA) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.8) +
    scale_fill_manual(values = colours) +
    scale_x_continuous(limits = c(0.65, 2.45), expand = expansion(mult = 0)) +
    coord_polar(theta = "y", direction = -1) +
    theme_void() +
    theme(legend.position = "none")

  cowplot::ggdraw(bars) +
    cowplot::draw_plot(donut, x = 0.60, y = 0.14, width = 0.33, height = 0.30) +
    cowplot::draw_label(label, x = 0.02, y = 0.97, hjust = 0, size = 8)
}

plot_fig5 <- function(workbook) {
  predictions <- read_xlsx(workbook, sheet = "Fig.5_performance")
  importance <- read_xlsx(workbook, sheet = "Fig.5_importance")
  models <- c("RF", "SVM", "GBM", "BPNN")

  ecosystem_row <- function(ecosystem, labels) {
    crop <- ecosystem == "Cropland"
    points <- predictions[predictions$Ecosystem == ecosystem, ]
    drivers <- importance[importance$Ecosystem == ecosystem, ]
    panels <- lapply(seq_along(models), function(i) {
      prediction_panel(points, models[i], paste0("(", labels[i], ")"), crop,
                       show_x = i > 2L, show_y = i %% 2L == 1L)
    })
    scatter <- cowplot::plot_grid(plotlist = panels, ncol = 2, align = "hv", axis = "tblr")
    bars <- importance_panel(drivers, paste0("(", labels[5], ")"), crop)
    cowplot::plot_grid(scatter, bars, nrow = 1, rel_widths = c(1, 0.90))
  }

  cowplot::plot_grid(ecosystem_row("Cropland", letters[1:5]),
                     ecosystem_row("Grassland", letters[6:10]), ncol = 1)
}

plot_fig6 <- function(workbook, boundaries_file) {
  grid_types <- c(rep("numeric", 4), "text")
  crop <- read_xlsx(workbook, sheet = "Fig.6_Cropland", col_types = grid_types)
  grass <- read_xlsx(workbook, sheet = "Fig.6_Grassland", col_types = grid_types)
  world_map <- sf::st_read(boundaries_file, quiet = TRUE)

  colours <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B",
               "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")

  raster_grid <- function(data) {
    # Complete one row and column with invisible cells so gaps retain the
    # original 5-arcmin spacing when ggplot2 builds the raster.
    longitudes <- -180 + (seq_len(4320L) - 0.5) / 12
    latitudes <- 90 - (seq_len(2160L) - 0.5) / 12
    data$x <- longitudes[round((data$x + 180) * 12 - 0.5) + 1L]
    data$y <- latitudes[round((90 - data$y) * 12 - 0.5) + 1L]
    data$opacity <- 1
    latitude <- min(data$y)
    longitude <- min(longitudes)
    missing_x <- setdiff(longitudes, data$x[data$y == latitude])
    missing_y <- setdiff(latitudes[latitudes >= latitude & latitudes <= max(data$y)],
                         c(latitude, data$y[data$x == longitude]))
    bind_rows(
      data,
      data.frame(x = missing_x, y = rep(latitude, length(missing_x)),
                 opacity = rep(0, length(missing_x))),
      data.frame(x = rep(longitude, length(missing_y)), y = missing_y,
                 opacity = rep(0, length(missing_y)))
    )
  }

  map_base <- function(label) {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = world_map, fill = NA, color = "gray52", linewidth = 0.055) +
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-70, 90), expand = FALSE) +
      ggspatial::annotation_north_arrow(height = grid::unit(1.0, "cm"), width = grid::unit(1.0, "cm"),
                             which_north = "grid", pad_x = grid::unit(0.055, "npc"),
                             pad_y = grid::unit(0.57, "npc"), style = ggspatial::north_arrow_nautical) +
      ggplot2::scale_y_continuous(breaks = seq(-60, 60, 30)) +
      ggplot2::scale_x_continuous(breaks = seq(-180, 180, 90)) +
      ggplot2::annotate("text", x = -165, y = 80, label = label,
                        hjust = 0, size = 2.8, fontface = "bold") +
      ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(family = "sans", size = 10, color = "black", face = "bold"),
            panel.border = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.65),
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            panel.grid.major = ggplot2::element_line(color = "gray82", linewidth = 0.22),
            axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(0, 0, 0, 0),
            legend.title = ggplot2::element_text(size = 9, face = "bold"),
            legend.text = ggplot2::element_text(size = 8.5, face = "bold"),
            legend.direction = "horizontal", legend.position = "inside",
            legend.position.inside = c(0.61, 0.025),
            legend.justification.inside = c(0.5, 0),
            legend.margin = ggplot2::margin(0, 0, 0, 0),
            legend.background = ggplot2::element_rect(fill = "white", colour = NA))
  }

  map_latitude <- function(data, column, limits, breaks, label, legend_title) {
    # Draw the original 5-arcmin cells without interpolation.
    p <- map_base(label) +
      ggplot2::geom_raster(data = raster_grid(data),
                           ggplot2::aes(x = x, y = y, fill = .data[[column]], alpha = opacity),
                           interpolate = FALSE) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_fill_gradientn(
        name = legend_title, colours = colours, limits = limits, breaks = breaks,
        na.value = "white", oob = scales::squish,
        guide = ggplot2::guide_colourbar(
          theme = ggplot2::theme(
            legend.title.position = "top",
            legend.title = ggplot2::element_text(hjust = 0.5),
            legend.key.width = grid::unit(3.4, "in"),
            legend.key.height = grid::unit(0.16, "in")
          )
        )
      )

    # Insets show each latitude’s cell mean and mean ± 1.96 standard errors.
    latitude_summary <- data %>%
      group_by(y) %>%
      summarise(mean_value = mean(.data[[column]], na.rm = TRUE),
                se = sd(.data[[column]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[column]]))),
                .groups = "drop") %>%
      mutate(lower = mean_value - 1.96 * se, upper = mean_value + 1.96 * se)

    a <- ggplot2::ggplot(latitude_summary, ggplot2::aes(x = mean_value, y = y)) +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = lower, xmax = upper), orientation = "y", fill = "lightblue", na.rm = TRUE) +
      ggplot2::geom_line(color = "#0868AC", orientation = "y", linewidth = 0.2) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0, add = 0), breaks = c(-60, -30, 0, 30, 60)) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0, add = 0), breaks = breaks) +
      ggplot2::coord_cartesian(xlim = limits, ylim = c(-70, 90), expand = FALSE) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_test() +
      ggplot2::theme(text = ggplot2::element_text(family = "sans", size = 8),
            axis.ticks.length = grid::unit(-0.15, "cm"),
            axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.3),
            axis.text = ggplot2::element_text(color = "black", size = 7),
            axis.text.x = ggplot2::element_text(size = 6.2, margin = ggplot2::margin(t = 3)),
            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
            plot.margin = ggplot2::margin(1, 6, 1, 1),
            panel.border = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.2))

    cowplot::ggdraw(p) +
      cowplot::draw_plot(a, x = 0.034, y = 0.037, width = 0.21, height = 0.51)
  }

  fraction <- "Gpp_allocated_to_net_rhizodeposition"
  annual_amount <- "Annual_net_rhizodeposition"
  fraction_title <- "Fraction of GPP allocated to net rhizodeposition (%)"
  annual_title <- expression(bold("Net rhizodeposition per hectare (kg C ha"^{-1} * ")"))
  annual_breaks <- seq(0, 1600, 400)

  g1 <- map_latitude(crop, fraction, c(3, 9), c(3, 4.5, 6, 7.5, 9),
                     "(a)", fraction_title)
  g3 <- map_latitude(grass, fraction, c(4, 16), c(4, 7, 10, 13, 16),
                     "(b)", fraction_title)
  g2 <- map_latitude(crop, annual_amount, c(0, 1600), annual_breaks,
                     "(e)", annual_title)
  g4 <- map_latitude(grass, annual_amount, c(0, 1600), annual_breaks,
                     "(f)", annual_title)

  variable_colours <- c(Clay = "#D53E4F", CO2 = "#E6AB02", MAT = "#BC80BD",
                        "N rate" = "#1E90FF", pH = "#B3DE69", SOC = "#1B9E77")
  variable_names <- expression(Clay = "Clay", SOC = "SOC", MAT = "MAT",
                               "N rate" = "N rate", pH = "pH", CO2 = CO[2])
  map_importance <- function(data, order, label) {
    map_base(label) +
      ggplot2::geom_raster(data = raster_grid(data),
                           ggplot2::aes(x = x, y = y, fill = Dominant_modelled_association, alpha = opacity),
                           interpolate = FALSE, na.rm = TRUE, key_glyph = ggplot2::draw_key_rect) +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_fill_manual(name = "", values = variable_colours,
                        limits = order, labels = unname(variable_names[order]),
                        na.value = "white", na.translate = FALSE) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
      ggplot2::theme(legend.position.inside = c(0.5, 0.025),
                     legend.key.width = grid::unit(0.20, "in"),
                     legend.key.height = grid::unit(0.10, "in"),
                     legend.key.spacing.x = grid::unit(0.22, "in"))
  }
  p5 <- map_importance(crop, c("Clay", "SOC", "MAT", "N rate", "pH", "CO2"), "(c)")
  p6 <- map_importance(grass, c("SOC", "Clay", "N rate", "MAT", "pH", "CO2"), "(d)")

  # Place equal-sized map frames first, then draw the shared outer axes.
  left <- c(0.041, 0.526)
  bottom <- c(0.691, 0.370, 0.049)
  panel_width <- 0.472
  panel_height <- 0.307
  lat_ticks <- seq(-60, 60, 30)
  lon_ticks <- seq(-180, 180, 90)
  maps <- list(list(g1, g3), list(p5, p6), list(g2, g4))
  figure <- cowplot::ggdraw()
  for (row in seq_along(bottom)) {
    for (col in seq_along(left)) {
      figure <- figure + cowplot::draw_plot(maps[[row]][[col]], x = left[col], y = bottom[row],
                                            width = panel_width, height = panel_height)
    }
    figure <- figure +
      ggplot2::annotate("text", label = lat_ticks, x = left[1] - 0.009,
                        y = bottom[row] + panel_height * (lat_ticks + 70) / 160,
                        hjust = 1, size = 3, fontface = "bold") +
      cowplot::draw_label("Latitude", x = 0.007, y = bottom[row] + panel_height / 2,
                          angle = 90, size = 8.5, fontface = "bold")
  }
  for (x in left) {
    tick_x <- x + panel_width * (lon_ticks + 180) / 360
    figure <- figure +
      ggplot2::annotate("text", label = lon_ticks, x = tick_x,
                        y = 0.032, hjust = c(0, 0.5, 0.5, 0.5, 1),
                        size = 3, fontface = "bold") +
      cowplot::draw_label("Longitude", x = x + panel_width / 2, y = 0.011,
                          size = 8.5, fontface = "bold")
  }
  figure
}

save_figure <- function(path, plot, ...) {
  extension <- paste0(".", tools::file_ext(path))
  staged <- tempfile(pattern = ".figure-", tmpdir = dirname(path), fileext = extension)
  on.exit(unlink(staged))
  ggsave(staged, plot, ...)
  if (!suppressWarnings(file.rename(staged, path))) {
    stop("Could not write ", path, ". If it is open in a viewer, close it and run again.",
         call. = FALSE)
  }
}

write_figures <- function(root = script_dir()) {
  data_dir <- file.path(root, "Data")
  workbook <- file.path(data_dir, "03_global_extrapolation.xlsx")
  fig5 <- plot_fig5(workbook)
  fig6 <- plot_fig6(workbook, file.path(data_dir, "03_boundaries.gpkg"))
  output_dir <- file.path(root, "Results")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  save_figure(file.path(output_dir, "Fig. 5.pdf"), fig5,
              width = 7.4, height = 7.7, device = grDevices::pdf, useDingbats = FALSE, bg = "white")
  save_figure(file.path(output_dir, "Fig. 6.pdf"), fig6,
              width = 13, height = 8.9, device = grDevices::cairo_pdf, bg = "white")
}

write_figures()
