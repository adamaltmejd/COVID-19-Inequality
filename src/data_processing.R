# RR_change <- function(.dt, group) {
#     if (.dt[, any(grepl("2019", var))]) {
#         group_change <- .dt[grep(paste0("2020.*", group), var), coef] / .dt[grep(paste0("2019.*", group), var), coef]
#         base_change <- .dt[grep("2020\\.year$", var), coef] / .dt[grep("2019bn\\.year$", var), coef]
#         return(group_change / base_change)
#     } else {
#         return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
#     }
# }

RR_level <- function(.dt, group, year, year_2020 = FALSE) {
    if (year_2020 == TRUE) {
        if (.dt[, any(grepl("year_2020", var))]) {
            if (year == 2019) {
                return(.dt[grep(paste0(group, "#0\\.year_2020"), var), coef] / .dt[var == "0.year_2020", coef])
            } else if (year == 2020) {
                return(.dt[grep(paste0(group, "#1\\.year_2020"), var), coef] / .dt[var == "1.year_2020", coef])
            } else { stop("Year needs to be 2019 or 2020.")}
        } else {
            if (year %in% 2016:2019) return(NA_real_)
            else return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
        }
    } else {
        if (.dt[, any(grepl("2019", var))]) {
            return(.dt[grep(paste0(year, ".*", group), var), coef] / .dt[grep(paste0(year, ".*\\.year$"), var), coef])
        } else {
            if (year %in% 2016:2019) return(NA_real_)
            else return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
        }
    }
}

RR_base <- function(.dt, year) {
    if (.dt[, any(grepl("2019", var))]) {
        return(.dt[grep(paste0(year, ".*\\.year$"), var), unique(coef)])
    } else {
        if (year == 2019) return(NA_real_)
        else return(.dt[var == "_cons", unique(coef)])
    }
}

set_labels <- function(out, radar_order = FALSE, return = FALSE) {
    if ("outcome" %in% names(out)) {
        if (radar_order) {
            out[, outcome_label := factor(
                outcome,
                levels = c("covid_pos", "covid_dead", "covid_novacc",
                           "dead_any_cause", "psych_outp",
                           "antidepressants", "sedatives", "psych_1177", "psych_death", "psych_hosp",
                           "surgery", "surg_dead_30d", "cancer", "cancer_dead_365d",
                           "dispinc_drop", "unemployed", "not_in_emp",
                           "hosp_inc", "hosp_unemp",
                           "covid_hosp"),
                labels = c("Positive, COVID-19", "Death, COVID-19", "Not vaccinated, COVID-19",
                           "Death, all causes", "Psychiatric care visit",
                           "New antidepressant use", "New sedative use", "Mental health hotline call", "Suicide", "Psychiatric inpatient care",
                           "Surgical procedure", "30d perioperative non-survival", "Cancer diagnosis", "1yr cancer non-survival",
                           "Income loss", "Unemployment", "Not in employment",
                           "C19 Hospitalization + Disposable income drop", "C19 Hospitalization + Unemployment",
                           "Hospitalization, COVID-19")
                )]
        } else {
            out[, outcome_label := factor(
                outcome,
                levels = c("covid_pos", "covid_hosp", "covid_dead", "covid_novacc",
                           "dead_any_cause", "psych_outp",
                           "antidepressants", "sedatives", "psych_1177", "psych_death", "psych_hosp",
                           "surgery", "surg_dead_30d", "cancer", "cancer_dead_365d",
                           "dispinc_drop", "unemployed", "not_in_emp",
                           "hosp_inc", "hosp_unemp"),
                labels = c("Positive, COVID-19", "Hospitalization, COVID-19", "Death, COVID-19", "Not vaccinated, COVID-19",
                           "Death, all causes", "Psychiatric care visit",
                           "New antidepressant use", "New sedative use", "Mental health hotline call", "Suicide", "Psychiatric inpatient care",
                           "Surgical procedure", "30d perioperative non-survival", "Cancer diagnosis", "1yr cancer non-survival",
                           "Income loss", "Unemployment", "Not in employment",
                           "C19 Hospitalization + Disposable income drop", "C19 Hospitalization + Unemployment")
                )]
        }

        if (return) return(out)
    }
    if ("dimension" %in% names(out)) {
        out[, dimension_label := factor(dimension,
            levels = c("male", "country", "education", "income_qt"),
            labels = c("Gender", "Region of birth", "Level of education", "Income quartile"))]
    }
    if ("group" %in% names(out)) {
        out[, group_label := factor(group,
            levels = c("pop_avg",
                    "1.income_qt", "2.income_qt", "3.income_qt", "4.income_qt", # "0bn.income_qt",
                    "0.education", "1.education", "2.education", "3.education",
                    "0.country", "3.country", "2.country", "1.country",
                    "1.male", "0.male"),
            labels = c("Population average",
                    "Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4", # "Missing income",
                    "Missing education", "Compulsory", "Upper secondary", "Post-secondary",
                    "Missing country of birth", "Outside of Europe", "Europe", "Sweden",
                    "Men", "Women"))]
    }
    if ("year" %in% names(out)) {
        out[, year := factor(year, levels = c("2016-2019", "2016", "2017", "2018", "2019", "2020", "2021"))]
    }

    # Swedish
    # out[, outcome_label := factor(outcome,
    #     levels = c("covid_pos", "covid_hosp", "covid_dead", "dead_any_cause", "antidepressants", "lugnande", "surgery", "cancer", "dispinc_drop", "tapp_brutto", "unemployed"),
    #     labels = c("Positivt fall", "Sjukhusinläggning, covid-19", "Dödsfall, covid-19", "Dödsfall, alla orsaker", "antidepressants", "Lugnande", "surgery", "Cancerdiagnos", "Inkomstförlust", "Inkomstförlust, brutto", "Arbetslöshet")
    #     )]
    # out[, dimension_label := factor(dimension,
    #     levels = c("inkomst", "utbildning", "land", "kon"),
    #     labels = c("Inkomst", "Utbildning", "Födelseland", "Kön"))]
    # out[, group_label := factor(group,
    #     levels = c("pop_avg",
    #                "1bn.income_qt", "2.income_qt", "3.income_qt", "4.income_qt", # "0bn.income_qt",
    #                "0bn.education", "1.education", "2.education", "3.education",
    #                "0bn.country", "3.country", "2.country", "1.country",
    #                "1bn.male", "2.male"),
    #     labels = c("Populationsgenomsnitt",
    #                "Kvartil 1", "Kvartil 2", "Kvartil 3", "Kvartil 4", # "Missing income",
    #                "Utbildningsnivå saknas", "Förgymnasial", "Gymnasial", "Eftergymnasial",
    #                "Födelseland saknas", "Utanför Europa", "Europa", "Sverige",
    #                "Män", "Kvinnor"))]
    # out[, year := factor(year, levels = c("2016-2019", "2016", "2017", "2018", "2019", "2020"))]
}

create_base_levels <- function(margins_data) {

    outcomes <- margins_data[, unique(y)]

    out <- data.table()
    for (outcome in outcomes) {
        out <- rbind(
            out,
            data.table(outcome = outcome, dimension = NA_character_, group = NA_character_, year = 2019,
                       coef = RR_base(margins_data[y == outcome & x == "inkomst"], 2019)),
            data.table(outcome = outcome, dimension = NA_character_, group = NA_character_, year = 2020,
                       coef = RR_base(margins_data[y == outcome & x == "inkomst"], 2020))
        )
    }

    set_labels(out)

    return(out)
}

create_plotdata <- function(margins_data, est_type = "manymodels", radar_order = FALSE) {
    library(data.table)

    .dt <- margins_data[est == est_type]

    outcomes <- .dt[, unique(y)]
    dimensions <- .dt[, unique(x)]
    #years <- .dt[, sort(unique(na.omit(as.integer(gsub("^([0-9]{4})?.+$", "\\1", var)))))]

    out <- data.table()
    for (dimension in dimensions) {
        for (outcome in outcomes) {
            # if (length(years) > 0) {
            #     groups <- .dt[y == outcome & x == dimension &
            #                          grepl("\\.", var) & !grepl("\\.year$", var),
            #                          unique(gsub("(20[012][0-9](bn)?\\.year#)", "", var))]
            # } else {
            #     groups <- .dt[y == outcome & x == dimension & grepl("\\.", var) & !grepl("\\.year_2020$", var), unique(var)]
            # }

            groups <- .dt[y == outcome & x == dimension & grepl("\\.", var) & !grepl("\\.year_2020$", var), unique(var)]

            for (group in groups) {
                out <- rbind(
                            out,
                            data.table(outcome = outcome, dimension = dimension, group = group, year = "2016-2019",
                                       coef = RR_level(.dt[y == outcome & x == dimension], group, "2019", year_2020 = TRUE)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = "2020",
                                       coef = RR_level(.dt[y == outcome & x == dimension], group, "2020", year_2020 = TRUE))
                        )
            }
        }
    }

    set_labels(out, radar_order = radar_order)

    return(out)
}

create_absolute_plotdata <- function(margins_data, est_type = "manymodels") {
    .dt <- margins_data[est == est_type]

    .dt <- .dt[, .(est, var, coef, y, x, ci_lower, ci_upper)]

    .dt[grep("1.year_2020", var), year := "2020"]
    .dt[grep("0.year_2020", var), year := "2016-2019"]

    #.dt[y %in% c("covid_dead", "covid_hosp", "covid_pos"), year := "2020"]
    .dt <- .dt[!is.na(year)]

    .dt[, group := gsub("#?(0|1).year_2020", "", var)]
    .dt[group == "" | group == "_cons", group := "pop_avg"]
    .dt <- .dt[!(group %in% c("0.education", "0.country"))]
    setnames(.dt, c("y", "x"), c("outcome", "dimension"))
    set_labels(.dt)

    return(.dt)
}

prepare_radar_data <- function(margins_plotdata, dt_min = 0, dt_max = 2.5, ref = TRUE, include_outcomes = NULL) {
    library(data.table)

    if (!is.null(include_outcomes)) {
        extra_rows <- set_labels(data.table(tidyr::crossing(
            data.table(outcome = include_outcomes)[!(outcome %in% margins_plotdata$outcome)],
            unique(margins_plotdata[, .(dimension, group, year)])
        )))
        extra_rows[, coef := NA_real_]
        margins_plotdata <- rbind(margins_plotdata, extra_rows)
    }

    if (margins_plotdata[, uniqueN(group), outcome][, unique(V1)] > 1) {
        # If multiple groups in same plot
        if (margins_plotdata[, uniqueN(year)] > 1) stop("Can't have both multiple groups and multiple years.")
        data <- margins_plotdata[, .(outcome_label, row_label = as.character(group_label), coef)]
    } else {
        data <- margins_plotdata[, .(outcome_label, row_label = as.character(year), coef)]
    }

    data <- rbind(
        data,
        unique(data[, .(outcome_label, row_label = "1", coef = dt_max)]),
        unique(data[, .(outcome_label, row_label = "2", coef = dt_min)])
    )
    if (ref) data <- rbind(data, unique(data[, .(outcome_label, row_label = "Ref.", coef = 1)]))
    data <- dcast(data, row_label ~ outcome_label, value.var = "coef")

    # Keep order of group levels by factor
    data <- data[order(match(row_label, levels(margins_plotdata$group_label)))]
    # Set order so reference is always first
    data[, row_order := 10 + 1:nrow(data)]
    data[row_label == "1", row_order := 1]
    data[row_label == "2", row_order := 2]
    data[row_label == "Ref.", row_order := 3]
    data[row_label == "2016", row_order := 4]
    data[row_label == "2017", row_order := 5]
    data[row_label == "2018", row_order := 6]
    data[row_label == "2019", row_order := 7]
    setorder(data, row_order)
    data[, row_order := NULL]

    data <- as.data.frame(data)
    rownames(data) <- data$row_label
    data <- data[names(data) != "row_label"]

    return(data)
}
