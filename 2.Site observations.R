# Figs. 2b and S4: ratio of total respiration to aboveground C after single pulse labelling.
# Read observations, fitted coefficients, and pointwise 95% confidence limits
# from the site observation workbook.

site_script_dir <- function() {
  if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
    # Use UTF-8 for the Chinese directory and sheet names on Windows.
    invisible(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.utf8"))
  }
  files <- c(
    sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)),
    unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  )
  files <- files[basename(files) == "2.Site observations.R"]
  path <- if (length(files)) tail(files, 1L) else "2.Site observations.R"
  dirname(normalizePath(path, mustWork = TRUE))
}

site_require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install the required R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

plot_respiration_ratio <- function(code_dir = site_script_dir()) {
  site_require_packages(c("ggplot2", "readxl"))
  workbook <- file.path(code_dir, "Data", "02_site_observation.xlsx")

  systems <- c("Cropland", "Grassland", "Forest")
  colours <- c(Cropland = "chartreuse4", Grassland = "red", Forest = "blue")
  raw_shapes <- c(Cropland = 16, Grassland = 15, Forest = 17)
  mean_shapes <- c(Cropland = 21, Grassland = 22, Forest = 24)
  # Preserve the archived size relationship, scaled to the screenshot's layout.
  mean_size <- function(n) {
    0.4 + 0.94 * (1.7 + 2.5 * sqrt(n / 57)) * pmin(1, pmax(0.5, sqrt(n / 40)))
  }
  observations <- readxl::read_excel(workbook, sheet = "Fig.2a", skip = 1,
    col_types = c("skip", "text", "text", "skip", "skip", "text", "numeric",
                  "skip", "numeric", "skip"))
  names(observations) <- c("ecosystem", "labelling", "day", "aboveground", "respiration")
  pulse <- observations[which(observations$labelling == "Single pulse labelling"), ]
  ecosystem_names <- c("Crop ecosystem", "Grassland ecosystem", "Forest ecosystem")
  raw <- data.frame(
    system = factor(pulse$ecosystem, levels = ecosystem_names, labels = systems),
    day = as.numeric(pulse$day),
    resp_to_shoot_ratio = pulse$respiration / pulse$aboveground
  )
  stopifnot(!anyNA(raw$system), all(is.finite(raw$day)), all(is.finite(raw$resp_to_shoot_ratio)))

  # Group fractional days after suppressing Excel's floating point noise.
  daily <- stats::aggregate(raw$resp_to_shoot_ratio,
    list(system = raw$system, day = round(raw$day, 12)),
    function(value) c(mean = mean(value), n = length(value)))
  means <- data.frame(system = daily$system, day = daily$day,
                      mean = daily$x[, "mean"], n = daily$x[, "n"])

  coefficients <- readxl::read_excel(workbook, sheet = "Fig.2b", skip = 1)
  intervals <- readxl::read_excel(workbook, sheet = "Fig2b_S4_confidence_interval",
                                  col_types = c("text", "numeric", "numeric", "numeric"))
  curves <- data.frame(
    system = intervals$Ecosystem, day = intervals[["Day after labelling"]],
    ci_low = intervals$ci_low, ci_high = intervals$ci_high
  )
  curves$system <- factor(curves$system, levels = systems)
  par <- coefficients[match(as.character(curves$system), coefficients$Ecosystem), ]
  stopifnot(all(is.finite(par$a)), all(is.finite(par$b)))
  curves$fitted <- par$a * -expm1(-par$b * curves$day)

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = curves, ggplot2::aes(day, ymin = ci_low, ymax = ci_high, fill = system),
      alpha = 0.13, colour = NA
    ) +
    ggplot2::geom_point(
      data = raw, ggplot2::aes(day, resp_to_shoot_ratio, colour = system, shape = system),
      size = 1.05, alpha = 0.34, stroke = 0
    ) +
    ggplot2::geom_line(
      data = curves, ggplot2::aes(day, fitted, colour = system), linewidth = 0.5
    ) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_shape_manual(values = raw_shapes) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(breaks = seq(0, 80, 20)) +
    ggplot2::scale_y_continuous(breaks = 0:4) +
    # Clip the visible area; retain all archived observations in the layers.
    ggplot2::coord_cartesian(xlim = c(-3, 81), ylim = c(-0.18, 4), expand = FALSE) +
    ggplot2::labs(x = "Day after labelling (days)",
                  y = "Ratio of total respiration to aboveground C") +
    ggplot2::theme_classic(base_family = "Arial", base_size = 13) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.35),
      axis.ticks.length = grid::unit(-1.55, "mm"),
      axis.text = ggplot2::element_text(colour = "black", size = 13, face = "bold"),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title = ggplot2::element_text(size = 13, face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
      legend.position = "none",
      plot.margin = ggplot2::margin(12, 14, 14, 5.5)
    )

  for (system in systems) {
    p <- p + ggplot2::geom_point(
      data = means[means$system == system, ],
      ggplot2::aes(day, mean, size = mean_size(n)),
      shape = mean_shapes[[system]], fill = "white", colour = colours[[system]],
      stroke = 0.45
    )
  }

  # n counts the observations contributing to a daily mean within each ecosystem.
  key <- data.frame(day = 65, y = c(3.48, 3.12, 2.76), n = c(10, 30, 50))
  for (system in systems) {
    p <- p + ggplot2::geom_point(
      data = key, ggplot2::aes(day, y, size = mean_size(n)),
      shape = mean_shapes[[system]], fill = NA, colour = colours[[system]], stroke = 0.45
    )
  }
  p <- p +
    ggplot2::annotate("text", x = 71, y = 3.82, label = "n",
                      family = "Arial", fontface = "bold", size = 4.6) +
    ggplot2::annotate("text", x = 71, y = key$y, label = as.character(key$n),
                      family = "Arial", fontface = "bold", size = 4.6)

  results <- file.path(code_dir, "Results")
  dir.create(results, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file.path(results, "Fig. 2b&Fig. S4.pdf"), p, width = 4.32, height = 4.30,
                  device = grDevices::cairo_pdf, bg = "transparent")
  invisible(p)
}

# Fig. 2d: from plant categories to ecosystem-level estimates.
plot_ecosystem_scaling <- function(code_dir = site_script_dir()) {
  site_require_packages(c("ggplot2", "readxl", "gtable"))
  data <- readxl::read_excel(file.path(code_dir, "Data", "02_site_observation.xlsx"),
                             sheet = "Fig.2d", skip = 1)
  systems <- c("Cropland", "Grassland", "Forest")
  data$value <- data[["GPP to net rhizodeposition"]]
  stopifnot(setequal(data$Ecosystem, systems), all(is.finite(data$value)))
  data$Ecosystem <- factor(data$Ecosystem, levels = systems)

  # Original plotted weighted means (percent) from the archived R script.
  marks <- data.frame(Ecosystem = factor(systems, levels = systems),
                      weighted_mean = c(5.55, 9.99, 5.28))
  # As in the manuscript panel, count records in categories matched to a
  # species weight, including zero weights. All categories enter the violin.
  counts <- tapply(data$n_observations[!is.na(data$species_weight)],
                   data$Ecosystem[!is.na(data$species_weight)], sum)
  marks$label <- sprintf("(%d)", counts[systems])
  marks$label_y <- 25.5

  p <- ggplot2::ggplot(data, ggplot2::aes(Ecosystem, value)) +
    ggplot2::geom_violin(
      trim = FALSE, scale = "width", width = 0.6, fill = "#FF8181",
      alpha = 0.5, colour = "#333333", linewidth = 0.57
    ) +
    # The archived figure used the 1.5 x IQR rule, not the 5th and 95th
    # percentile whiskers described in the manuscript caption.
    ggplot2::geom_boxplot(
      width = 0.18, coef = 1.5, outlier.shape = NA,
      fill = "white", colour = "#333333", linewidth = 0.5, median.linewidth = 0.55
    ) +
    ggplot2::geom_errorbar(
      data = marks, ggplot2::aes(Ecosystem, ymin = weighted_mean, ymax = weighted_mean),
      inherit.aes = FALSE, width = 0.2, colour = "red", linetype = "dashed",
      linewidth = 0.54
    ) +
    ggplot2::geom_text(
      data = marks, ggplot2::aes(Ecosystem, label_y, label = label),
      inherit.aes = FALSE, family = "Arial", fontface = "bold", size = 4.45
    ) +
    ggplot2::scale_x_discrete(labels = c("Crop-\nland", "Grass-\nland", "Forest")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 28, 7), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::coord_cartesian(ylim = c(-3, 28)) +
    ggplot2::labs(x = NULL, y = "Fraction of GPP allocated to\nnet rhizodeposition (%)") +
    ggplot2::theme_classic(base_family = "Arial", base_size = 12.5) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.35),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "black", face = "bold", size = 12.5),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 9)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 11)),
      axis.ticks = ggplot2::element_line(linewidth = 0.5),
      axis.ticks.length = grid::unit(-2.35, "mm"),
      plot.margin = ggplot2::margin(6, 7, 5, 7)
    )

  results <- file.path(code_dir, "Results")
  dir.create(results, showWarnings = FALSE, recursive = TRUE)
  grDevices::cairo_pdf(file.path(results, "Fig. 2d.pdf"), width = 3.45, height = 4.18,
                       bg = "transparent")
  tryCatch(grid::grid.draw(ecosystem_panel_grob(p)), finally = grDevices::dev.off())
  invisible(p)
}

ecosystem_panel_grob <- function(p) {
  # The panel heading has the same two colours as the manuscript.
  x_title <- grid::unit(7, "pt")
  step <- grid::textGrob("(d) Step 4", x = x_title, just = "left",
                         gp = grid::gpar(fontfamily = "Arial", fontface = "bold",
                                         fontsize = 11, col = "#E60012"))
  heading <- grid::textGrob("Scaling up to ecosystem scale",
                            x = x_title + grid::grobWidth(step) + grid::unit(4, "pt"),
                            just = "left",
                            gp = grid::gpar(fontfamily = "Arial", fontface = "bold",
                                            fontsize = 11))
  figure <- ggplot2::ggplotGrob(p)
  figure <- gtable::gtable_add_rows(figure, grid::unit(20, "pt"), pos = 0)
  gtable::gtable_add_grob(figure, grid::grobTree(step, heading),
                          t = 1, l = 1, r = ncol(figure), clip = "off")
}

# Fig. 4: draw the archived summaries from the site observation workbook.
plot_rhizodeposition_distribution <- function(code_dir = site_script_dir()) {
  site_require_packages(c("ggplot2", "readxl", "cowplot", "scales"))
  d <- readxl::read_excel(file.path(code_dir, "Data", "02_site_observation.xlsx"),
                          sheet = "Fig.3&4", range = "A2:H27")
  names(d) <- c("panel", "indicator", "ecosystem", "mean", "se",
                "observations", "studies", "letter")
  colours <- c(Cropland = "#8DD3C7", Grassland = "#FFFFB3", Forest = "#BEBADA")
  stopifnot(identical(as.integer(table(factor(d$panel, levels = letters[1:4]))),
                      c(4L, 6L, 12L, 3L)),
            all(d$ecosystem %in% names(colours)), all(is.finite(d$mean)))

  # Layout values follow the final figure. Fractions and SEs stay in percent.
  draw_panel <- function(bars, breaks, labels, xlim, ylim, ybreaks, ytitle,
                         tag, letter_gap, count_size = 4.7, stack = NULL, extra = NULL,
                         ylabels = ggplot2::waiver(), secondary = ggplot2::waiver(),
                         legend = NULL) {
    errors <- bars[!is.na(bars$se), ]
    letters <- bars[!is.na(bars$letter), ]
    columns <- if (is.null(stack)) bars else bars[!bars$x %in% stack$x, ]
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(data = columns, ggplot2::aes(x, mean, fill = ecosystem),
                        width = bars$width[1], colour = "black", linewidth = 0.62)
    if (!is.null(stack)) {
      p <- p + ggplot2::geom_rect(
        data = stack, ggplot2::aes(xmin = x - width / 2, xmax = x + width / 2,
                                   ymin = low, ymax = high, fill = ecosystem),
        colour = "black", linewidth = 0.62
      )
    }
    p <- p +
      ggplot2::geom_errorbar(data = errors,
        ggplot2::aes(x, ymin = mean - se, ymax = mean + se),
        width = bars$width[1] * 0.42, linewidth = 0.62) +
      ggplot2::geom_text(data = letters,
        ggplot2::aes(letter_x, mean + ifelse(is.na(se), 0, se) + letter_gap, label = letter),
        family = "Arial", size = 4.65, vjust = 0) +
      ggplot2::geom_text(data = bars,
        ggplot2::aes(x, count_y, label = sprintf("(%d)", observations)),
        family = "Arial", size = count_size, angle = 90, hjust = 0) +
      ggplot2::annotate("text", x = xlim[1] + 0.03 * diff(xlim), y = ylim[2] - 0.04 * diff(ylim),
                        label = paste0("(", tag, ")"), hjust = 0, vjust = 1,
                        family = "Arial", fontface = "bold", size = 4.85) +
      ggplot2::scale_fill_manual(values = colours) +
      ggplot2::scale_x_continuous(breaks = breaks, labels = labels) +
      ggplot2::scale_y_continuous(breaks = ybreaks, labels = ylabels, sec.axis = secondary) +
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(x = NULL, y = ytitle) +
      ggplot2::theme_classic(base_size = 13.2, base_family = "Arial") +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.border = ggplot2::element_rect(fill = NA, colour = "#333333", linewidth = 0.48),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_line(linewidth = 0.45),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.length = grid::unit(-2, "mm"),
        axis.text = ggplot2::element_text(colour = "black", size = 12.7),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
        axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
        axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 6)),
        axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
        axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 7)),
        legend.position = "none", plot.margin = ggplot2::margin(7, 4, 5, 4)
      )
    if (!is.null(legend)) {
      p <- p +
        ggplot2::geom_rect(data = legend,
          ggplot2::aes(xmin = x, xmax = x + key_width, ymin = y - key_height / 2,
                       ymax = y + key_height / 2, fill = ecosystem),
          colour = "black", linewidth = 0.56) +
        ggplot2::geom_text(data = legend, ggplot2::aes(x + 1.25 * key_width, y, label = ecosystem),
                           family = "Arial", hjust = 0, size = 4.75)
    }
    if (!is.null(extra)) p <- p + extra
    p
  }

  a <- d[d$panel == "a", ]
  a$x <- match(a$indicator, c("MAOC", "POC")) + ifelse(a$ecosystem == "Cropland", -0.1, 0.1)
  a$width <- 0.2
  a$count_y <- 4.5
  a$letter_x <- a$x
  pa <- draw_panel(a, 1:2, c("MAOC", "POC"), c(0.4, 2.6), c(0, 80), seq(0, 80, 20),
    "Proportion of net rhizodeposition\nremaining in POC and MAOC (%)", "a", 0.8,
    legend = data.frame(ecosystem = names(colours)[1:2], x = 0.75, y = c(74.2, 68),
                        key_width = 0.14, key_height = 2.6))

  b <- d[d$panel == "b" & d$indicator %in% c("MBC", "MNC"), ]
  b$x <- c(0.136, 0.337, 0.539, 0.852)[match(b$ecosystem, names(colours))]
  b$x[b$indicator == "MNC"] <- 0.852
  b$width <- 0.09
  b$count_y <- 2.2
  b$letter_x <- b$x
  fungal <- d$mean[d$indicator == "MNC_Fungal"]
  bacterial <- d$mean[d$indicator == "MNC_Bacterial"]
  total <- b$mean[b$indicator == "MNC"]
  stopifnot(length(fungal) == 1L, length(bacterial) == 1L, length(total) == 1L,
            abs(fungal + bacterial - total) < 1e-8)
  mnc <- data.frame(x = 0.852, width = 0.09, ecosystem = "Cropland",
                    low = c(0, fungal), high = c(fungal, total))
  pb <- draw_panel(b, c(0.337, 0.852), c("MBC", "MNC"), c(0, 1), c(0, 40), seq(0, 40, 10),
    "Proportion of net rhizodeposition\nremaining in MBC and MNC (%)", "b", 0.5, stack = mnc,
    legend = data.frame(ecosystem = names(colours), x = 0.148, y = c(37.1, 34, 31),
                        key_width = 0.066, key_height = 1.3),
    extra = list(
      ggplot2::geom_vline(xintercept = 0.705, linetype = "dashed", colour = "grey65", linewidth = 0.6),
      ggplot2::annotate("text", x = 0.95, y = c(7.7, 25.7),
                        label = c("Fungal derived", "Bacterial derived"),
                        angle = 90, family = "Arial", size = 4.05)))

  c <- d[d$panel == "c", ]
  types <- c("PLFA", "GP", "GN", "Act", "AM", "Fungi")
  c$x <- match(c$indicator, types) + ifelse(c$ecosystem == "Cropland", -0.135, 0.135)
  # The left axis (PLFA) is 1/30 of the right axis used for microbial groups.
  plfa <- c$indicator == "PLFA"
  c$mean[plfa] <- 30 * c$mean[plfa]
  c$se[plfa] <- 30 * c$se[plfa]
  c$width <- 0.27
  c$count_y <- ifelse(c$indicator %in% c("Act", "AM"), 14, 0.55)
  c$letter_x <- c$x + ifelse(plfa & c$ecosystem == "Cropland", 0.26, 0)
  pc <- draw_panel(c, 1:6, c("PLFA", "GP", "GN", "Act", "AMF", "Fungi"),
    c(0.4, 6.6), c(0, 80), seq(0, 75, 15),
    "Proportion of net rhizodeposition\nremaining in PLFA (%)", "c", 0.65, count_size = 3.25,
    ylabels = function(y) sprintf("%.1f", y / 30),
    secondary = ggplot2::dup_axis(name = "Proportion of PLFA in\ndifferent microbial groups (%)",
                                  labels = scales::label_number()),
    legend = data.frame(ecosystem = names(colours)[1:2], x = c(1.31, 3.55), y = 74.8,
                        key_width = 0.42, key_height = 2.6))

  doc <- d[d$panel == "d", ]
  doc$x <- match(doc$ecosystem, names(colours))
  doc$width <- 0.45
  doc$count_y <- 0.09
  doc$letter_x <- doc$x
  pd <- draw_panel(doc, 1:3, c("Crop-\nland", "Grass-\nland", "Forest"), c(0.24, 3.76),
    c(0, 4), 0:4, "Proportion of net rhizodeposition\nremaining in DOC (%)", "d", 0.045)

  results <- file.path(code_dir, "Results")
  dir.create(results, showWarnings = FALSE, recursive = TRUE)
  grDevices::cairo_pdf(file.path(results, "Fig. 4.pdf"), width = 8.46, height = 7.92, bg = "transparent")
  tryCatch({
    grid::grid.newpage()
    panels <- lapply(list(pa, pb, pc, pd), ggplot2::ggplotGrob)
    row1 <- cowplot::plot_grid(panels[[1]], panels[[2]], ncol = 2, align = "h", axis = "tb",
                               rel_widths = c(0.52, 0.48))
    row2 <- cowplot::plot_grid(panels[[3]], panels[[4]], ncol = 2, align = "h", axis = "tb",
                               rel_widths = c(0.635, 0.365))
    figure <- cowplot::plot_grid(row1, row2, ncol = 1)
    print(figure, newpage = FALSE)
  }, finally = grDevices::dev.off())
  invisible(list(figure = figure, panels = list(pa, pb, pc, pd), data = d))
}

plot_respiration_ratio()
plot_ecosystem_scaling()
plot_rhizodeposition_distribution()
