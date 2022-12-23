source(file.path("src", "helper_functions.R"))
source(file.path("src", "data_processing.R"))
source(file.path("src", "figures.R"))
library(targets)

# Set target-specific options such as packages.
tar_option_set(
    packages = c("data.table", "lubridate", "bit64", "Cairo", "ISOweek", "stringr",
                 "ggplot2", "ragg", "cowplot", "modelsummary"),
    format = "qs"
)
options(datatable.na.strings = "")

# End this file with a list of target objects.
list(
    # Base levels
    tar_target(f_base_levels, file.path("data", "2016-2020.csv"), format = "file"),
    tar_target(dt_base_levels, fread(f_base_levels, col.names = c("sample", "y", "year", "coef")) |>
               DT(CJ(sample = unique(sample), y = unique(y), year = 2016:2020), on = c("sample", "y", "year")) |>
               DT(, sample := as.character(factor(sample, levels = c("All", "Ages 25-64"), labels = c("full", "working")))) |>
               DT(sample == "full" & !(y %in% c("Unemployment", "Income loss")), mixed_sample := TRUE) |> DT(sample == "working" & y %in% c("Unemployment", "Income loss"), mixed_sample := TRUE) |>
               DT(, y := factor(y, levels = c("C19 positive", "C19 hospital", "C19 death", "Death", "Antidepressants", "Sedatives", "Cancer diagnosis", "Surgery", "Unemployment", "Income loss"),
                                labels = c("Positive case", "Hospitalization, covid-19", "Death, covid-19", "Death, all causes", "Antidepressants", "Sedatives", "Cancer diagnosis", "Surgical procedure", "Unemployment", "Income loss"))) |>
               DT(, year := forcats::fct_rev(factor(year)))),
    tarchetypes::tar_map(
        values = list(est_sample = c("working", "full", "mixed")),
        names = est_sample,
        tar_target(
            out_base_levels_table,
            dt_base_levels |>
                {\(x) if (est_sample == "mixed") { x[mixed_sample == TRUE] } else { x[sample == est_sample] } }() |>
                DT(!(y %in% c("Sedatives"))) |>
                DT(, coef := paste0(format(round(coef * 100, 2), nsmall = 2, trim = TRUE), "\\%")) |>
                DT(coef == "NA\\%", coef := "") |>
                DT(, sample := NULL) |>
                {\(x) dcast(x, y~year, value.var = "coef") }() |>
                {\(x) setcolorder(x, c("y", as.character(2016:2020))) }() |>
                {\(x) setnames(x, braces(c("", 2016:2020))) }() |>
                datasummary_df(output = "latex_tabular", fmt = NULL, booktabs = TRUE, escape = FALSE) |>
                add_S_align(S_fmt = c("2.2"), ncols = 5, text_before = "", text_after = "\\\\%") |>
                add_tabularnewline() |>
                save_table(paste0("base_levels_", est_sample, ".tex")),
                format = "file"
        )
    ),
    # Base levels plots
    # tarchetypes::tar_map(
    #     values = list(outcome = list(c("Positive case", "Hospitalization, covid-19", "Death, covid-19"), c("Death, all causes", "Antidepressants", "Cancer diagnosis", "Surgical procedure"), c("Unemployment", "Income loss")),
    #                   outcome_labels = c("Covid-19", "Indirect health outcomes", "Indirect economic outcomes"),
    #                   outcome_names = c("c19", "health", "economic")),
    #     names = outcome_names,
    #     tar_target(p_dt_base_levels_full, dt_base_levels[y %in% outcome & sample == "full"]),
    #     tar_target(p_base_levels_full,
    #         ggplot(data = p_dt_base_levels_full,
    #                aes(y = year, x = coef, group = year)) +
    #             geom_bar(stat = "identity", aes(fill = year), width = 0.3) +
    #             geom_point(aes(color = year), size = 1.8) +
    #             scale_x_continuous(expand = expansion(mult = c(0, 0.3)), labels = scales::percent) +
    #             #guides(color = "none", fill = guide_legend(title = NULL, reverse = TRUE)) +
    #             guides(color = "none", fill = "none") +
    #             labs(x = NULL, y = NULL) +
    #             scale_fill_discrete(type = c(reds()[1], blues())) +
    #             scale_color_discrete(type = c(reds()[1], blues())) +
    #             facet_wrap(vars(y), ncol = 1, scales = "free_x") +
    #             plot_theme() +
    #             theme(
    #                 strip.background = element_blank(),
    #                 strip.text = element_text(size = 11, margin = margin(5, 0, 3, 0)),
    #                 legend.position = "right"
    #             )
    #     ),
    #     tar_target(out_p_base_levels_full, saveplot(p_base_levels_full,
    #                                                  paste0("out/base_levels_", outcome_names, ".pdf"),
    #                                                  height = 11/4 * length(outcome), width = 11 * 1.615), format = "file")
    # ),

    # Radar plots
    tar_target(f_margins_data, file.path("data", "margins_results.csv"), format = "file"),
    tar_target(margins_data, fread(f_margins_data)),
    tarchetypes::tar_map(
        values = list(est_type = c("onemodel_full_population", "full_population", "onemodel_working_age", "working_age", "onemodel_mixed", "mixed")),
        names = est_type,
        tar_target(plotdata, create_plotdata(margins_data, est_type)),
        tar_target(groups, unique(plotdata[!(group %in% c("0bn.utbildning", "0bn.inkomst", "0bn.land", "0bn.kon")), .(dimension, dimension_label, group, group_label)])),

        # tar_target(dt_radar_2020_vs_2016_2019, prepare_radar_data(plotdata[group == groups$group & !(outcome %in% c("positivt_fall", "covid_sjukhus", "covid_dod"))]),
        #            pattern = map(groups), iteration = "list"),
        # tar_target(p_radar_2020_vs_2016_2019,
        #            plot_radar(dt_radar_2020_vs_2016_2019,
        #                       fn = file.path("out", "2020_vs_2016_2019", paste0("", groups$dimension, "_", groups$group, "_2020_vs_2016_2019_", est_type, ".pdf")),
        #                       groups = groups$group,
        #                       #title = paste0(groups$dimension_label, ": ", groups$group_label),
        #                       draw_legend = FALSE),
        #            pattern = map(groups, dt_radar_2020_vs_2016_2019), format = "file"),

        tar_target(dimensions, unique(plotdata[,.(dimension, dimension_label)])),
        tar_target(dt_radar_allgroups, prepare_radar_data(plotdata[dimension == dimensions$dimension & year == 2020 & !(group %in% c("0bn.utbildning", "0bn.inkomst", "0bn.land"))]),
                   pattern = map(dimensions), iteration = "list"),
        tar_target(p_radar_allgroups,
                   plot_radar(dt_radar_allgroups,
                              fn = file.path("out", "all_groups", paste0("", dimensions$dimension, "_allgroups_", est_type, ".pdf")),
                              groups = groups[dimension == dimensions$dimension, group],
                              #title = dimensions$dimension_label,
                              draw_legend = TRUE),
                   pattern = map(dimensions, dt_radar_allgroups), format = "file")
    ),

    # Plots of absolute levels
    tarchetypes::tar_map(
        values = list(est_type = c("onemodel_full_population", "full_population", "onemodel_working_age", "working_age", "onemodel_mixed", "mixed")),
        names = est_type,
        tar_target(dt_abslvls, create_absolute_plotdata(margins_data, est_type)),

        tarchetypes::tar_map(
            values = list(dimension_type = c("kon", "inkomst", "utbildning", "land")),
            names = dimension_type,

            tar_target(p_abslvls,
                    ggplot(dt_abslvls[dimension == dimension_type & group != "pop_avg"], aes(x = group_label, y = coef)) +
                            geom_point(aes(color = group_label), size = 2) +
                            geom_line(data = dt_abslvls[dimension == dimension_type & group != "pop_avg" & !(outcome %in% c("positivt_fall", "covid_sjukhus", "covid_dod"))], aes(color = group_label), size = 0.9, lineend = "butt", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"), type = "open")) +
                            geom_hline(data = dt_abslvls[dimension == dimension_type & group == "pop_avg"], aes(yintercept = coef), color = "grey50", linetype = "dashed") +
                            geom_segment(data = dcast(dt_abslvls[dimension == dimension_type & group == "pop_avg", -c("var", "ci_lower", "ci_upper")], as.formula("...~year"), value.var = "coef"),
                                        aes(x = 0, y = `2016-2019`, xend = 0, yend = `2020`), size = 0.9, color = "grey50", lineend = "butt", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"), type = "open")) +
                            facet_wrap(vars(outcome_label), scales = "free_y") +
                            scale_y_continuous(labels = scales::percent) +
                            scale_color_discrete(type = reds()[1:uniqueN(dt_abslvls[dimension == dimension_type]$group_label)-1]) +
                            coord_cartesian(clip = "off") +
                            guides(color = "none", fill = "none") +
                            labs(x = NULL, y = NULL, color = NULL) +
                            plot_theme() +
                            theme(axis.text.x = element_text(angle = 30, hjust = 1))),
            tar_target(out_abslvls,
                       saveplot(p_abslvls,
                                 paste0("out/absolute_levels/", dimension_type, "_", est_type, ".pdf"),
                                 height = 18, width = 18), format = "file")
        )
    ),

    # Scatter plot of changes between 2016-2019 and 2020
    tar_target(dt_scatter, create_plotdata(margins_data, "mixed") |>
        {\(x) dcast(x[, .(outcome_label, group, group_label, dimension, dimension_label, year, coef)], outcome_label + group + group_label + dimension + dimension_label ~ year, value.var = "coef") }() |>
        {\(x) setnames(x, c("2016-2019", "2020"), c("year_2016_2019", "year_2020")) }() |>
        DT(!is.na(year_2016_2019)) |>
        #DT(dimension_label == "Utbildning") |>
        DT(!(group %in% c("0bn.utbildning", "0bn.inkomst", "0bn.land", "0bn.kon"))) |>
        DT(, group_nr := gsub("[^0-9]+", "", group))),
    # tar_target(p_scatter, ggplot(data = dt_scatter, aes(x = year_2016_2019, y = year_2020, color = outcome_label, shape = dimension_label)) +
    #     geom_abline(intercept = 0, slope = 1, color = "gray30") +
    #     geom_point(size = 0.9) +
    #     scale_x_continuous(limits = c(0,2.1), expand = expansion(0)) +
    #     scale_y_continuous(limits = c(0,2.1), expand = expansion(0)) +
    #     coord_fixed() +
    #     labs(y = "2020", x = "2016-2019 average", color = "Outcome", shape = "Dimension") +
    #     #labs(y = "2020", x = "Genomsnitt 2016-2019", color = "Utfall", shape = "Dimension") + # SWEDISH
    #     plot_theme() + theme(legend.title.align = 0, legend.title = element_text(face = "bold"), legend.spacing = unit(0.5, "lines"))),
    # tar_target(p_scatter,
    #     ggplot(data = dt_scatter, aes(x = year_2016_2019, y = year_2020, color = group_label, shape = outcome_label)) +
    #         geom_abline(intercept = 0, slope = 1, color = "gray30", linewidth = 0.4) +
    #         geom_point(size = 1.6, alpha = 0.85) +
    #         scale_x_continuous(limits = c(0,2.1), expand = expansion(0)) +
    #         scale_y_continuous(limits = c(0,2.1), expand = expansion(0)) +
    #         coord_fixed() +
    #         facet_wrap(vars(dimension_label)) +
    #         scale_color_manual(values = setNames(c("black", reds(), "black", reds()[1:3], "black", reds()[1:3], reds()[1:2]), levels(dt_scatter$group_label))) +
    #         labs(y = "2020", x = "2016-2019 average", color = "Outcome", shape = "Dimension") +
    #         #labs(y = "2020", x = "Genomsnitt 2016-2019", color = "Utfall", shape = "Dimension") + # SWEDISH
    #         plot_theme() +
    #         theme(legend.title.align = 0,
    #               legend.title = element_text(face = "bold"),
    #               legend.spacing = unit(0.5, "lines"),
    #               panel.spacing.x = unit(0.5, "lines"))),
    # tar_target(out_scatter, saveplot(p_scatter, paste0("out/2020_vs_2016_2019/scatter.pdf"), width = 13, height = 10), format = "file"),

    # Scatter by group
    tar_target(scatter_groups, unique(dt_scatter[, .(dimension, dimension_label)])[c(4,1,2,3)] |> DT(, dimension_label_title := paste(c("(a)", "(b)", "(c)", "(d)"), dimension_label))),
    tar_target(p_scatter_by_group,
        ggplot(data = dt_scatter[dimension == scatter_groups$dimension],
               aes(x = year_2016_2019, y = year_2020, color = group_label, shape = outcome_label)) +
            geom_abline(intercept = 0, slope = 1, color = "gray30", linewidth = 0.4) +
            geom_point(size = 1.4) +
            scale_x_continuous(limits = c(0, 2.1), expand = expansion(0)) +
            scale_y_continuous(limits = c(0, 2.1), expand = expansion(0)) +
            coord_fixed() +
            labs(y = "2020", x = "2016-2019 average") +
            #labs(y = "2020", x = "Genomsnitt 2016-2019", color = "Utfall", shape = "Dimension") + # SWEDISH
            scale_color_discrete(type = reds()[1:uniqueN(dt_scatter[dimension == scatter_groups$dimension]$group_label)]) +
            scale_shape_manual(values = setNames(0:uniqueN(dt_scatter$outcome_label), unique(dt_scatter$outcome_label))) +
            plot_theme() +
            guides(shape = "none", color = guide_legend(title = scatter_groups$dimension_label_title, title.position = "top", title.hjust = 0.5, ncol = 2, byrow = TRUE)) +
            theme(legend.title.align = 0.5,
                  #legend.title = element_text(face = "bold"),
                  legend.text = element_text(margin = margin(0,-2,0,-5)),
                  plot.margin = margin(5,0,3,0),
                  legend.margin = margin(2, 0, 0, 0),
                  legend.position = "bottom"),
        pattern = map(scatter_groups), iteration = "list"
    ),
    tar_target(out_scatter_by_group, saveplot(p_scatter_by_group, tolower(paste0("out/2020_vs_2016_2019/scatter_", scatter_groups$dimension, ".pdf")), width = 10, height = 11), pattern = map(p_scatter_by_group, scatter_groups), format = "file"),

    # Create a grid of plots using cowplot and add a shared shape legend
    tar_target(p_scatter_shape_legend, get_legend(ggplot(data = dt_scatter, aes(x = year_2016_2019, y = year_2020, color = group_label, shape = outcome_label)) + geom_point() +
        scale_shape_manual(values = setNames(0:uniqueN(dt_scatter$outcome_label), unique(dt_scatter$outcome_label))) +
        guides(color = "none", shape = guide_legend(NULL)) + plot_theme() + theme(legend.position = "bottom"))),
    tar_target(out_scatter_by_group_joined,
        save_plot("out/2020_vs_2016_2019/scatter_all.pdf",
                  plot_grid(plot_grid(plotlist = p_scatter_by_group, align = "hv", axis = "tblr"), p_scatter_shape_legend, ncol = 1, rel_heights = c(0.92, 0.08)),
                  base_height = 3.5, base_asp = 0.8, ncol = 2, nrow = 2), format = "file"),

    # Interaction effects
    tar_target(f_extra, file.path("data", "extra_results_margins.csv"), format = "file"),
    tar_target(dt_interactions, create_plotdata(fread(f_extra), est_type = "working_age") |>
        #DT(outcome == "cdead_hosp", `:=`(outcome_label = "Interaction", int1 = "covid_dod", int2 = "covid_sjukhus")) |>
        #DT(outcome == "pos_unemp", `:=`(outcome_label = "Interaction", int1 = "positivt_fall", int2 = "arblos")) |>
        #DT(outcome == "pos_inc", `:=`(outcome_label = "Interaction", int1 = "positivt_fall", int2 = "tapp_disp")) |>
        #DT(outcome == "unemp_inc", `:=`(outcome_label = "Interaction", int1 = "arblos", int2 = "tapp_disp")) |>
        DT(outcome == "hosp_unemp", `:=`(outcome_label = "Interaction", int1 = "covid_sjukhus", int2 = "arblos")) |>
        DT(outcome == "hosp_inc", `:=`(outcome_label = "Interaction", int1 = "covid_sjukhus", int2 = "tapp_disp")) |>
        DT(!is.na(outcome_label))
    ),
    tar_target(interactions, unique(dt_interactions$outcome)),
    tar_target(out_interaction_plot,
        dt_interactions[outcome == interactions] |>
        {\(x) if (!(interactions %in% c("unemp_inc"))) { x[year == "2020"] } else { x } }() |>
        {\(x) rbind(x,
            create_plotdata(margins_data, est_type = "working_age") |>
                DT(outcome %in% unique(c(dt_interactions[outcome == interactions, int1], dt_interactions[outcome == interactions, int2]))),
            fill = TRUE)[group != "pop_avg"][!(group %in% c("0bn.utbildning", "0bn.inkomst", "0bn.land"))]
        }() |>
        {\(x)
        ggplot(x[year == "2020"], aes(x = outcome_label, y = coef - 1, fill = group_label, group = group_label)) +
            geom_hline(yintercept = 0, size = 0.3, color = "gray20") +
            geom_bar(stat = "identity", position = position_dodge(width = 1)) +
            geom_point(data = x[year == "2016-2019"], shape = 21, color = "black", size = 2, position = position_dodge(width = 1)) +
            facet_wrap(vars(dimension_label), scales = "free_y") +
            scale_fill_discrete(type = c(reds(), reds()[1:3], reds()[1:3], reds()[1:2])) +
            scale_y_continuous(labels = function(x) x + 1) +
            labs(x = NULL, y = NULL, color = NULL) +
            guides(color = "none", fill = "none") +
            plot_theme() + theme(axis.text.y = element_text(size = 6), axis.text.x = element_text(size = 6, angle = 7, hjust = 0.5))
        }() |>
        saveplot(file.path(paste0("out/interactions/", interactions, ".pdf"))),
        pattern = map(interactions), iteration = "list"),

    # Regression tables
    tar_target(f_regression_results, file.path("data", "regression_results.csv"), format = "file"),
    tar_target(f_varlabels, file.path("data", "varlabels.csv"), format = "file"),
    tarchetypes::tar_map(
        values = list(outcome = c("tapp_disp", "arblos", "operation", "cancer", "antidepressiva", "dod", "covid_dod", "covid_sjukhus", "positivt_fall")),
        names = outcome,
        tarchetypes::tar_map(
            values = list(sample = c("full_population", "working_age")),
            names = sample,
            tar_target(
                reg_tab,
                regression_table(f_regression_results, f_varlabels, outcome, sample, digits = 2) |>
                save_table(paste0(outcome, "_", sample, ".tex")),
                format = "file")
        )
    ),

    # Appendix table of postponed surgeries
    tar_target(f_surgeries, file.path("data", "surgery_stats.csv"), format = "file"),
    tar_target(p_surgeries,
        fread(f_surgeries) |>
            DT(, week := ISOweek::ISOweek2date(paste("2020", paste0("W", stringr::str_pad(week, 2, pad = "0")), "1", sep = "-"))) |>
            DT(is.na(hospitalizations), hospitalizations := 0) |>
            melt(id.vars = "week") |>
            DT(, variable_label := factor(variable, levels = c("elective", "emergency", "hospitalizations"), labels = c("Elective", "Emergency", "Hospitalizations"))) |>
            {\(x)
                ggplot() +
                geom_area(data = x[variable == "hospitalizations"], aes(x = week, y = value / 60), fill = "gray70") +
                geom_line(data = x[variable != "hospitalizations"], aes(x = week, y = value, color = variable_label), linewidth = 0.9) +
                geom_hline(yintercept = 100) +
                scale_color_discrete(type = reds()) +
                scale_x_date(expand = expansion(0)) +
                scale_y_continuous(expand = expansion(add = c(0, 5)), sec.axis = sec_axis(~ . * 60, name = "Number of hospitalizations")) +
                guides(color = guide_legend(title = "Number of surgeries")) +
                labs(x = "Month, 2020", y = "Number of surgeries") +
                plot_theme() + theme(legend.position = "bottom")
            }()
        ),
    tar_target(out_surgeries, saveplot(p_surgeries, "out/surgeries.pdf"), format = "file"),

    tar_target(end, 1)
)