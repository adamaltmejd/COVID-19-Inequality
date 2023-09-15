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
    # 9 main outcomes
    tar_target(main_outcomes, c("covid_pos", "covid_hosp", "covid_dead", "dead_any_cause", "psych_outp", "dispinc_drop", "unemployed", "cancer_dead_365d", "surg_dead_30d")),
    #tar_target(main_outcomes, c("antidepressants", "sedatives", "psych_1177", "psych_death", "psych_hosp", "psych_outp")),
    # "surgery", "cancer", "not_in_emp", "covid_novacc", "covid_vaccinated"
    # "hosp_unemp", "hosp_inc"

    # Base levels
    tar_target(f_base_levels, file.path("data", "population_averages.csv"), format = "file"),
    tar_target(dt_base_levels,
        fread(f_base_levels) |>
            DT(CJ(year = 2016:2021, outcome = unique(outcome)), on = c("year", "outcome")) |>
            set_labels(out = _) |>
            DT(, year_int := as.integer(as.character(year)))
    ),
    # tar_target(
    #     out_base_levels_table,
    #     dt_base_levels |>
    #         DT(outcome %in% main_outcomes) |>
    #         DT(, coef := paste0(format(round(value * 100, 2), nsmall = 2, trim = TRUE), "\\%")) |>
    #         DT(coef == "NA\\%", coef := "") |>
    #         (\(x) dcast(x, outcome_label ~ year, value.var = "coef") )() |>
    #         (\(x) setcolorder(x, c("outcome_label", as.character(2016:2020))) )() |>
    #         (\(x) setnames(x, braces(c("", 2016:2020))) )() |>
    #         datasummary_df(output = "latex_tabular", fmt = NULL, booktabs = TRUE, escape = FALSE) |>
    #         add_S_align(S_fmt = c("2.2"), ncols = 6, text_before = "", text_after = "\\\\%") |>
    #         add_tabularnewline() |>
    #         save_table(paste0("base_levels.tex")),
    #     format = "file"
    # ),

    tar_target(p_base_levels,
        ggplot(data = dt_base_levels[outcome %in% main_outcomes & !is.na(value)] |>
            DT(, year_2020 := ifelse(year_int >= 2020, TRUE, FALSE)),
               aes(x = year_int, y = value, color = year_2020)) +
            geom_point(na.rm = TRUE) +
            stat_smooth(
                data = dt_base_levels[outcome %in% main_outcomes & year_int < 2020],
                method = "lm", formula = y ~ x, fullrange = TRUE, se = TRUE, alpha = 0, na.rm = TRUE,
                linetype = "dashed", color = "grey30", linewidth = 0.5) +
            facet_wrap(vars(outcome_label), scales = "free_y") +
            scale_color_manual(values = c("black", reds()[1])) +
            guides(color = "none") +
            # Fake point to get unemployment away from limit
            geom_point(data = set_labels(data.table(outcome = "unemployed", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.07)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "psych_outp", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.033)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "covid_hosp", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.00360)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "covid_hosp", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.00455)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "covid_dead", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.00060)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "covid_dead", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.00150)], alpha = 0) +
            scale_y_continuous(
                # Y axis limits should never be below zero
                #limits = function(x) c(max(0, x[1], na.rm = TRUE), x[2]),
                expand = expansion(0,0),
                labels = scales::percent_format(accuracy = 0.01)) +
            labs(x = NULL, y = NULL) +
            plot_theme() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    ),
    tar_target(out_p_base_levels,
               saveplot(p_base_levels, file.path("out", "base_levels.eps"), width = 14, height = 14), format = "file"),

    tar_target(p_base_levels_psych,
        ggplot(data = dt_base_levels[outcome %in% c("antidepressants", "psych_death", "psych_outp") & !is.na(value) & value != 0] |> # "psych_1177", "psych_outp", "sedatives"
            DT(, year_2020 := ifelse(year_int >= 2020, TRUE, FALSE)),
               aes(x = year_int, y = value, color = year_2020)) +
            geom_point(na.rm = TRUE) +
            stat_smooth(
                data = dt_base_levels[outcome %in% c("antidepressants", "psych_death", "psych_outp") & year_int < 2020],
                method = "lm", formula = y ~ x, fullrange = TRUE, se = TRUE, alpha = 0, na.rm = TRUE,
                linetype = "dashed", color = "grey30", linewidth = 0.5) +
            facet_wrap(vars(outcome_label), scales = "free_y") +
            scale_color_manual(values = c("black", reds()[1])) +
            guides(color = "none") +
            # Fake point to get unemployment away from limit
            geom_point(data = set_labels(data.table(outcome = "psych_outp", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.033)], alpha = 0) +
            geom_point(data = set_labels(data.table(outcome = "psych_death", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.00015)], alpha = 0) +
            #geom_point(data = set_labels(data.table(outcome = "psych_1177", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.002)], alpha = 0) +
            #geom_point(data = set_labels(data.table(outcome = "psych_1177", year = 2020), return = TRUE)[, `:=`(year_int = 2020, year_2020 = TRUE, value = 0.0027)], alpha = 0) +
            scale_y_continuous(
                # Y axis limits should never be below zero
                #limits = function(x) c(max(0, x[1], na.rm = TRUE), x[2]),
                expand = expansion(0,0),
                labels = scales::percent_format(accuracy = 0.001)) +
            labs(x = NULL, y = NULL) +
            plot_theme() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    ),
    tar_target(out_p_base_levels_psych,
               saveplot(p_base_levels_psych, file.path("out", "base_levels_psych.pdf"), width = 14, height = 7), format = "file"),

    # Radar plots
    tar_target(f_margins_data, file.path("data", "margins_results.csv"), format = "file"),
    tar_target(margins_data, fread(f_margins_data, check.names = TRUE) |> DT(, var := gsub("(o|b|bn)\\.", "\\.", var))),
    tar_target(plotdata, create_plotdata(margins_data, "manymodels", radar_order = FALSE)),
    tar_target(plotdata_abs, create_absolute_plotdata(margins_data, "manymodels")),
    tar_target(groups, unique(plotdata[!(group %in% c("0.education", "0.country")), .(dimension, dimension_label, group, group_label)])),
    tar_target(dimensions, unique(plotdata[,.(dimension, dimension_label)])),
    tarchetypes::tar_map(
        values = list(est_type = c("manymodels")), # "onemodel"
        names = est_type,
        tar_target(plotdata_radar, create_plotdata(margins_data, est_type, radar_order = TRUE)),

        tarchetypes::tar_map(
            values = list(year_gr = c("2016-2019", "2020", "2021"), year_name = c("2016_2019", "2020", "2021")),
            names = year_name,
            tar_target(dt_radar, prepare_radar_data(plotdata_radar[dimension == dimensions$dimension & outcome %in% main_outcomes &
                                                                   year == year_gr & !(group %in% c("0.education", "0.country"))], include_outcomes = main_outcomes),
                   pattern = map(dimensions), iteration = "list"),
            tar_target(p_radar,
                    plot_radar(dt_radar,
                                fn = file.path("out", "radar_plots", paste0("", dimensions$dimension, "_", year_name, "_", est_type, ".eps")),
                                groups = groups[dimension == dimensions$dimension, group],
                                draw_legend = TRUE),
                    pattern = map(dimensions, dt_radar), format = "file")
        )
    ),

    ###
    # Scatter plot of changes between 2016-2019 and 2020 (and 2020 vs 2021)
    tar_target(dt_scatter, plotdata |>
        DT(outcome %in% main_outcomes) |>
        {\(x) dcast(x[, .(outcome_label, group, group_label, dimension, dimension_label, year, coef)], outcome_label + group + group_label + dimension + dimension_label ~ year, value.var = "coef") }() |>
        {\(x) setnames(x, c("2016-2019", "2020", "2021"), c("year_2016_2019", "year_2020", "year_2021")) }() |>
        DT(!is.na(year_2016_2019)) |>
        DT(!(group %in% c("0.education", "0.country"))) |>
        DT(, group_nr := gsub("[^0-9]+", "", group))),

    # Scatter
    tar_target(scatter_groups, unique(dt_scatter[, .(dimension, dimension_label)]) |> DT(, dimension_label_title := paste(c("(a)", "(b)", "(c)", "(d)"), dimension_label))),
    tar_target(p_scatter_by_group,
        ggplot(data = dt_scatter[dimension == scatter_groups$dimension],
               aes(x = year_2016_2019, y = year_2020)) +
            geom_ribbon(data = data.table(x = seq(0, 2.4, 0.1)),
                        aes(x = x, y = x, ymin = x * 0.95, ymax = x * 1.05), fill = "grey60", alpha = 0.5) +
            geom_ribbon(data = data.table(x = seq(0, 2.4, 0.1)),
                        aes(x = x, y = x, ymin = x * 0.90, ymax = x * 1.1), fill = "grey80", alpha = 0.5) +
            geom_abline(intercept = 0, slope = 1, color = "gray60", linewidth = 0.3) +
            geom_point(aes(color = group_label), size = 1.2, alpha = 0.7) +
            scale_x_continuous(expand = expansion(0, 0), limits = c(0, 2.3), breaks = seq(0, 2, 0.5), oob = scales::squish) +
            scale_y_continuous(expand = expansion(0, 0), limits = c(0, 2.3), breaks = seq(0, 2, 0.5), oob = scales::squish) +
            coord_fixed() +
            labs(y = "2020", x = "2016-2019 average") +
            scale_color_discrete(type = reds()[1:uniqueN(dt_scatter[dimension == scatter_groups$dimension]$group_label)]) +
            plot_theme() +
            guides(shape = "none",
                   color = guide_legend(title = scatter_groups$dimension_label_title, title.position = "top",
                                        title.hjust = 0.5, ncol = 2, byrow = TRUE)) +
            theme(legend.title.align = 0.5,
                  legend.text = element_text(margin = margin(0,-2,0,-5)),
                  plot.margin = margin(5,0,3,0),
                  legend.margin = margin(2, 0, 0, 0),
                  legend.position = "bottom"),
        pattern = map(scatter_groups), iteration = "list"),
    tar_target(out_scatter_by_group_joined,
        save_plot("out/scatter/scatter_2016_2019_vs_2020.eps", device = cairo_ps,
                  plot_grid(plotlist = p_scatter_by_group, align = "hv", axis = "tblr", nrow = 2, ncol = 2),
                  base_height = 3.5, base_asp = 0.8, ncol = 2, nrow = 2), format = "file"),

    # 2020 vs 2021
    tar_target(p_scatter_by_group_2021,
        ggplot(data = dt_scatter[dimension == scatter_groups$dimension & !is.na(year_2021)] |> melt(measure.vars = c("year_2020", "year_2021")),
               aes(x = year_2016_2019, y = value)) +
            geom_ribbon(data = data.table(x = seq(0, 2.4, 0.1)),
                        aes(x = x, y = x, ymin = x * 0.95, ymax = x * 1.05), fill = "grey60", alpha = 0.5) +
            geom_ribbon(data = data.table(x = seq(0, 2.4, 0.1)),
                        aes(x = x, y = x, ymin = x * 0.90, ymax = x * 1.1), fill = "grey80", alpha = 0.5) +
            geom_abline(intercept = 0, slope = 1, color = "gray60", linewidth = 0.3) +
            geom_point(aes(color = group_label, shape = variable), size = 1.2, alpha = 0.7) +
            scale_x_continuous(expand = expansion(0, 0), limits = c(0, 2.3), breaks = seq(0, 2, 0.5), oob = scales::squish) +
            scale_y_continuous(expand = expansion(0, 0), limits = c(0, 2.3), breaks = seq(0, 2, 0.5), oob = scales::squish) +
            coord_fixed() +
            labs(y = "2020, 2021", x = "2016-2019 average") +
            scale_color_discrete(type = reds()[1:uniqueN(dt_scatter[dimension == scatter_groups$dimension]$group_label)]) +
            scale_shape_manual(values = c(16, 3)) +
            plot_theme() +
            guides(shape = "none",
                   color = guide_legend(title = scatter_groups$dimension_label_title, title.position = "top",
                                        title.hjust = 0.5, ncol = 2, byrow = TRUE)) +
            theme(legend.title.align = 0.5,
                  legend.text = element_text(margin = margin(0,-2,0,-5)),
                  plot.margin = margin(5,0,3,0),
                  legend.margin = margin(2, 0, 0, 0),
                  legend.position = "bottom"),
        pattern = map(scatter_groups), iteration = "list"),
    tar_target(p_scatter_shape_legend,
        get_legend(
            ggplot(data = dt_scatter[dimension == scatter_groups$dimension & !is.na(year_2021)] |> melt(measure.vars = c("year_2020", "year_2021")),
                          aes(x = year_2016_2019, y = value, color = group_label, shape = variable)) + geom_point() +
            scale_shape_manual(values = c(16, 3), breaks = c("year_2020", "year_2021"), labels = c("2020", "2021")) +
            guides(color = "none", shape = guide_legend(title = NULL, nrow = 1)) + plot_theme() + theme(legend.position = "bottom")
        )),
    tar_target(out_scatter_by_group_joined_2021,
        save_plot("out/scatter/scatter_2016_2019_vs_2020_2021.eps", device = cairo_ps,
                  plot_grid(plot_grid(plotlist = p_scatter_by_group_2021, align = "hv", axis = "tblr", nrow = 2, ncol = 2), p_scatter_shape_legend, nrow = 2, ncol = 1, rel_heights = c(0.96, 0.04)),
                  base_height = 3.5, base_asp = 0.8, ncol = 2, nrow = 2), format = "file"),

    # Interaction effects
    tar_target(dt_interactions, plotdata[outcome %in% c("hosp_unemp", "hosp_inc")] |>
        DT(outcome == "hosp_unemp", `:=`(outcome_label = "Interaction", int1 = "covid_hosp", int2 = "unemployed")) |>
        DT(outcome == "hosp_inc", `:=`(outcome_label = "Interaction", int1 = "covid_hosp", int2 = "dispinc_drop")) |>
        DT(!is.na(outcome_label))
    ),
    tar_target(interactions, unique(dt_interactions$outcome)),
    tar_target(out_interaction_plot,
        dt_interactions[outcome == interactions] |>
            (\(x) rbind(x,
                plotdata |>
                    DT(outcome %in% unique(c(dt_interactions[outcome == interactions, int1], dt_interactions[outcome == interactions, int2]))),
                fill = TRUE)[group != "pop_avg"][!(group %in% c("0.education", "0.income_qt", "0.country"))]
            )() |>
            (\(x)
            ggplot(x[year == "2020"], aes(x = outcome_label, y = coef - 1, fill = group_label, group = group_label)) +
                geom_hline(yintercept = 0, linewidth = 0.3, color = "gray20") +
                geom_bar(stat = "identity", position = position_dodge(width = 1)) +
                geom_point(data = x[year == "2016-2019"], shape = 21, color = "black", size = 2, position = position_dodge(width = 1)) +
                facet_wrap(vars(dimension_label), scales = "free_y") +
                scale_fill_discrete(type = c(reds(), reds()[1:3], reds()[1:3], reds()[1:2])) +
                scale_y_continuous(labels = function(x) x + 1) +
                labs(x = NULL, y = NULL, color = NULL) +
                guides(color = "none", fill = "none") +
                plot_theme() + theme(axis.text.y = element_text(size = 6), axis.text.x = element_text(size = 6, angle = 7, hjust = 0.5))
            )() |>
            saveplot(file.path(paste0("out/interactions/", interactions, ".pdf"))),
            pattern = map(interactions), iteration = "list"),

    # Regression tables
    tar_target(f_regression_results, file.path("data", "regression_results.csv"), format = "file"),
    tar_target(regression_results, fread(f_regression_results, check.names = TRUE) |> DT(, var := gsub("(o|b|bn)\\.", "\\.", var))),
    tar_target(f_varlabels, file.path("data", "varlabels.csv"), format = "file"),
    tar_target(varlabels, fread(f_varlabels)),

    tar_target(tab_regressions,
               regression_table(regression_results, varlabels, main_outcomes, digits = 2) |>
                   save_table(paste0(main_outcomes, ".tex")),
               format = "file", pattern = map(main_outcomes)),

    # Tables of all relative effects
    tar_target(tab_relative_effects,
               relative_effects_table(
                    .dt_abs =  plotdata_abs[outcome %in% main_outcomes & dimension == dimensions$dimension],
                    .dt_rel = plotdata[outcome %in% main_outcomes & dimension == dimensions$dimension]
                ) |> save_table(paste0("relative_effects_", dimensions$dimension, ".tex")),
               format = "file", pattern = map(dimensions)),

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