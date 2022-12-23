###
# THEME

plot_theme <- function() {
    library(ggplot2)

    thm <- theme_bw(base_size = 11) %+replace%
        theme(
            # text = element_text(debug = TRUE),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 7),
            legend.title = element_text(size = 8),
            legend.box.margin = margin(0, 0, 0, 0),
            legend.box.spacing = unit(0.2, "lines"),
            legend.margin = margin(0, 0, 0, 0),
            legend.text = element_text(size = 8, margin = margin(0, 0, 0, -3)),
            legend.key.size = unit(0.8, "lines"),
            legend.spacing = unit(0.05, "cm"),
            strip.text = element_text(size = 7, margin = margin(1, 0, 1, 0)),
            strip.background = element_blank(),
            panel.grid.major = element_line(color = "grey90", linewidth = 0.1),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            plot.margin = margin(5, 8, 5, 5)
        )

    return(thm)
}

reds <- function() {
    c("#D94701", "#FD8D3C", "#ffac64", "#ffc096")
}
blues <- function() {
    c("#2B8CBE", "#74A9CF", "#A6BDDB", "#D0D1E6")
}


saveplot <- function(p, fn, folder = file.path("out"), width = 11, height = width, ...) {
    library(ggplot2)
    library(ragg)

    if (tools::file_ext(fn) == "png") {
        ggsave(
            filename = fn,
            plot = p,
            device = agg_png,
            width = width,
            height = height,
            units = "cm",
            dpi = 300,
            ...
        )
    } else if (tools::file_ext(fn) == "svg") {
        ggsave(
            filename = fn,
            plot = p,
            #device = { function(filename, ...) Cairo::CairoSVG(file = filename, ...) },
            device = svg,
            width = width,
            height = height,
            units = "cm",
            dpi = 300,
            pointsize = 11,
            ...
        )
    } else if (tools::file_ext(fn) == "pdf") {
        ggsave(
            filename = fn,
            plot = p,
            device = cairo_pdf,
            width = width,
            height = height,
            units = "cm",
            dpi = 300,
            pointsize = 11,
            ...
        )
    } else { stop("File format not supported.") }

    return(fn)
}

plot_radar <- function(plotdata, fn, groups = NULL, title = NULL, lines = nrow(plotdata) - 3,
                       draw_legend = lines > 1, crop = FALSE) {
    library(data.table)
    library(ragg)
    library(fmsb)
    library(knitr)

    # Ensure directory exists
    dir.create(dirname(fn), recursive = TRUE, showWarnings = FALSE)

    # Create plot device
    asp_ratio <- 1 #1.618
    height_px <- 2000
    if (tools::file_ext(fn) == "png") {
        agg_png(fn, width = asp_ratio * height_px, height = height_px, res = 300, pointsize = 12)
    } else if (tools::file_ext(fn) == "svg") {
        svg(fn, width = asp_ratio * (height_px / 300),
            height = height_px / 300, pointsize = 11) # 11 cm = 4.3in
    } else if (tools::file_ext(fn) == "pdf") {
        cairo_pdf(fn, width = asp_ratio * (height_px / 300),
                  height = height_px / 300, pointsize = 11) # 11 cm = 4.3in
    } else { stop("File type not supported.") }

    # Plot options
    op <- par(
        mar = c(3, 2, 2, 2),
        xpd = NA
        #mfrow=c(2, 2)
    )

    # Line colors depend on groups
    gr_index <- as.integer(gsub("^([0-9])(bn)?\\..*", "\\1", groups))
    # Land is ordered in the wrong order (3, not 1, is worst-off-group - Outside of Europe)
    if (any(grepl("land", groups)) & length(groups) <= 1) gr_index <- 4 - gr_index

    main_colors <- reds()[gr_index]
    colors <- c("#BBBBBB", main_colors)
    if (lines > 1 & length(groups) <= 1) colors <- c(colors[1], blues()[1:lines-1], colors[2])

    # Render chart
    radarchart(plotdata,
        title = title,
        centerzero = FALSE,
        na.itp = FALSE, # Don't interpolate NA's

        axistype = 1,
        seg = 5, # The number of segments for each axis (default 4).
        caxislabels = seq(plotdata[2, 1], plotdata[1, 1], length.out = 6), # labels for axis segments
        calcex = 0.7, # font size labels
        vlcex = 1.1, # font size labels around chart

        # Data
        pty = 16, # Point symbol
        pcol = paste0(colors, "FF"), # Data colors
        plty = 1, # Vector of line types
        plwd = 4, # vector of line widths

        # Filling polygons
        # pdensity = ,
        # pfcol = paste0(palette_qualitative, "33"), # Colors

        # Custom grid
        cglty = 1, # Line type for radar grids: Default 3, which means dotted line.
        cglwd = 1, # Line width for radar grids: Default 1, which means thinnest line.
        cglcol = "grey", # Line color for radar grids: Default "navy"
        axislabcol = "grey40", # Color of axis label and numbers: Default "blue"

        # Arguments for plot.default()
        xpd = NA
    )

    if (draw_legend) {
        legend(x = 0, y = -1.3, legend = rownames(plotdata[-c(1, 2), ]),
               bty = "n", # type of box
               pch = 16, # plotting symbols
               col = colors, # colors
               text.col = "black", # text color
               cex = 1.1, # character expansion
               pt.cex = 2.5, # character expansion factor for points
               xpd = NA,
               horiz = TRUE,
               xjust = 0.5)
    }

    par(op)
    dev.off()

    if (tools::file_ext(fn) == "pdf" & crop == TRUE) {
        # Crops too much...
        knitr::plot_crop(fn, quiet = TRUE)
    }

    return(fn)
}


regression_table <- function(f_regression_results, f_varlabels, outcome, sample = "working_age", digits = 3) {
    library(targets)
    library(data.table)
    library(modelsummary)

    varlabs <- fread(f_varlabels)

    dt <- fread(f_regression_results, check.names = TRUE)[y == outcome & est %in% paste0(c("", "onemodel_"), sample)]
    tab <- dt[, .(var, est = fmt_est(coef * 100, pval, digits = digits), se = fmt_se(stderr * 100, digits = digits), X_estimates_name)]
    tab <- dcast(melt(tab, measure.vars = c("est", "se")), var+variable~X_estimates_name)
    tab[, label := factor(var, varlabs$level, varlabs$label)]
    tab <- tab[order(match(var, varlabs$level))]
    tab <- tab[!(var %in% varlabs[drop == 1, level])]
    tab[variable == "se", label := ""]
    tab <- tab[, -c("var", "variable")]

    stats <- rbind(
        dcast(dt[var == "_cons", .(label = "Adjusted R2", X_estimates_name, format(round(r2_a, digits), nsmall = digits, trim = TRUE))], label~X_estimates_name, value.var = "V3"),
        dcast(dt[var == "_cons", .(label = "Observations", X_estimates_name, si_num(N))], label~X_estimates_name, value.var = "V3")
    )
    tab <- rbind(tab, stats, use.names = TRUE, fill = TRUE)

    setcolorder(tab, c("label", paste0(outcome, c("_inkomst", "_utbildning", "_kon", "_land"))))
    setnames(tab, c("{}", braces(paste0("(", 1:5, ")"))))

    datasummary_df(tab,
            output = "latex_tabular",
            fmt = NULL,
            booktabs = TRUE,
            linesep = "",
            hrule = c(max_na_on_empty(grep("^~~", tab$`{}`)) + 2, which(tab$`{}` == "Constant") + 2),
            escape = FALSE) |>
            #{\(x) gsub("\\\\vphantom\\{[0-9]+\\}", paste0("(0.", paste0(rep(0, digits), collapse = ""), ")"), x) }() |>
            add_S_align(S_fmt = c(paste0("2.", digits)), ncols = 5, text_before = "-", text_after = "\\\\tnote{***}") |>
            add_tabularnewline()
}
