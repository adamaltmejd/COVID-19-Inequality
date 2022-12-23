RR_change <- function(.dt, group) {
    if (.dt[, any(grepl("2019", var))]) {
        group_change <- .dt[grep(paste0("2020.*", group), var), coef] / .dt[grep(paste0("2019.*", group), var), coef]
        base_change <- .dt[grep("2020\\.ar$", var), coef] / .dt[grep("2019bn\\.ar$", var), coef]
        return(group_change / base_change)
    } else {
        return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
    }
}

RR_level <- function(.dt, group, year, ar_2020 = FALSE) {
    if (ar_2020 == TRUE) {
        if (.dt[, any(grepl("ar_2020", var))]) {
            if (year == 2019) {
                return(.dt[grep(paste0(group, "#0bn\\.ar_2020"), var), coef] / .dt[var == "0bn.ar_2020", coef])
            } else if (year == 2020) {
                return(.dt[grep(paste0(group, "#1\\.ar_2020"), var), coef] / .dt[var == "1.ar_2020", coef])
            } else { stop("Year needs to be 2019 or 2020.")}
        } else {
            if (year %in% 2016:2019) return(NA_real_)
            else return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
        }
    } else {
        if (.dt[, any(grepl("2019", var))]) {
            return(.dt[grep(paste0(year, ".*", group), var), coef] / .dt[grep(paste0(year, ".*\\.ar$"), var), coef])
        } else {
            if (year %in% 2016:2019) return(NA_real_)
            else return(.dt[grep(group, var), coef] / .dt[grep("_cons", var), coef])
        }
    }
}

RR_base <- function(.dt, year) {
    if (.dt[, any(grepl("2019", var))]) {
        return(.dt[grep(paste0(year, ".*\\.ar$"), var), unique(coef)])
    } else {
        if (year == 2019) return(NA_real_)
        else return(.dt[var == "_cons", unique(coef)])
    }
}

set_labels <- function(out) {
    out[, outcome_label := factor(outcome,
        levels = c("positivt_fall", "covid_sjukhus", "covid_dod", "dod", "antidepressiva", "lugnande", "operation", "cancer", "tapp_disp", "tapp_brutto", "arblos"),
        labels = c("Positive case", "Hospitalization, covid-19", "Death, covid-19", "Death, all causes", "Antidepressants", "Sedatives", "Surgical procedure", "Cancer diagnosis", "Income loss", "Gross income loss", "Unemployment")
        )]
    out[, dimension_label := factor(dimension,
        levels = c("kon", "land", "utbildning", "inkomst"),
        labels = c("Gender", "Region of birth", "Level of education", "Income quartile"))]
    out[, group_label := factor(group,
        levels = c("pop_avg",
                   "1bn.inkomst", "2.inkomst", "3.inkomst", "4.inkomst", # "0bn.inkomst",
                   "0bn.utbildning", "1.utbildning", "2.utbildning", "3.utbildning",
                   "0bn.land", "3.land", "2.land", "1.land",
                   "1bn.kon", "2.kon"),
        labels = c("Population average",
                   "Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4", # "Missing income",
                   "Missing education", "Compulsory", "Upper secondary", "Post-secondary",
                   "Missing country of birth", "Outside of Europe", "Europe", "Sweden",
                   "Men", "Women"))]
    out[, year := factor(year, levels = c("2016-2019", "2016", "2017", "2018", "2019", "2020"))]

    # Swedish
    # out[, outcome_label := factor(outcome,
    #     levels = c("positivt_fall", "covid_sjukhus", "covid_dod", "dod", "antidepressiva", "lugnande", "operation", "cancer", "tapp_disp", "tapp_brutto", "arblos"),
    #     labels = c("Positivt fall", "Sjukhusinläggning, covid-19", "Dödsfall, covid-19", "Dödsfall, alla orsaker", "Antidepressiva", "Lugnande", "Operation", "Cancerdiagnos", "Inkomstförlust", "Inkomstförlust, brutto", "Arbetslöshet")
    #     )]
    # out[, dimension_label := factor(dimension,
    #     levels = c("inkomst", "utbildning", "land", "kon"),
    #     labels = c("Inkomst", "Utbildning", "Födelseland", "Kön"))]
    # out[, group_label := factor(group,
    #     levels = c("pop_avg",
    #                "1bn.inkomst", "2.inkomst", "3.inkomst", "4.inkomst", # "0bn.inkomst",
    #                "0bn.utbildning", "1.utbildning", "2.utbildning", "3.utbildning",
    #                "0bn.land", "3.land", "2.land", "1.land",
    #                "1bn.kon", "2.kon"),
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

create_plotdata <- function(margins_data, est_type, change = FALSE) {
    library(data.table)
    if (est_type == "mixed") {
        .dt <- margins_data[(est == "full_population" & !(y %in% c("arblos", "tapp_disp"))) | (est == "working_age" & y %in% c("arblos", "tapp_disp"))]
    } else if (est_type == "onemodel_mixed") {
        .dt <- margins_data[(est == "onemodel_full_population" & !(y %in% c("arblos", "tapp_disp"))) | (est == "onemodel_working_age" & y %in% c("arblos", "tapp_disp"))]
    } else {
        .dt <- margins_data[est == est_type]
    }
    outcomes <- .dt[, unique(y)]
    dimensions <- .dt[, unique(x)]
    years <- .dt[, sort(unique(na.omit(as.integer(gsub("^([0-9]{4})?.+$", "\\1", var)))))]

    out <- data.table()
    for (dimension in dimensions) {
        for (outcome in outcomes) {
            if (length(years) > 0) {
                groups <- .dt[y == outcome & x == dimension &
                                     grepl("\\.", var) & !grepl("\\.ar$", var),
                                     unique(gsub("(20[012][0-9](bn)?\\.ar#)", "", var))]
            } else {
                groups <- .dt[y == outcome & x == dimension & grepl("\\.", var) & !grepl("\\.ar_2020$", var), unique(var)]
            }

            for (group in groups) {

                # if (change) {
                #     out <- rbind(
                #         out,
                #         data.table(outcome = outcome, dimension = dimension, group = group, year = 2020,
                #                    coef = RR_change(.dt[y == outcome & x == dimension], group))
                #     )
                # }

                if (!change) {
                    if (length(years) == 0) {
                        out <- rbind(
                            out,
                            data.table(outcome = outcome, dimension = dimension, group = group, year = "2016-2019",
                                       coef = RR_level(.dt[y == outcome & x == dimension], group, 2019, ar_2020 = TRUE)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2020,
                                       coef = RR_level(.dt[y == outcome & x == dimension], group, "2020", ar_2020 = TRUE))
                        )
                    } else {
                        out <- rbind(
                            out,
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2016,
                                    coef = RR_level(.dt[y == outcome & x == dimension], group, 2016)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2017,
                                    coef = RR_level(.dt[y == outcome & x == dimension], group, 2017)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2018,
                                    coef = RR_level(.dt[y == outcome & x == dimension], group, 2018)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2019,
                                    coef = RR_level(.dt[y == outcome & x == dimension], group, 2019)),
                            data.table(outcome = outcome, dimension = dimension, group = group, year = 2020,
                                    coef = RR_level(.dt[y == outcome & x == dimension], group, 2020))
                        )
                    }
                }
            }
        }
    }

    set_labels(out)

    return(out)
}

create_absolute_plotdata <- function(margins_data, est_type) {
    if (est_type == "mixed") {
        .dt <- margins_data[(est == "full_population" & !(y %in% c("arblos", "tapp_disp"))) | (est == "working_age" & y %in% c("arblos", "tapp_disp"))]
    } else if (est_type == "onemodel_mixed") {
        .dt <- margins_data[(est == "onemodel_full_population" & !(y %in% c("arblos", "tapp_disp"))) | (est == "onemodel_working_age" & y %in% c("arblos", "tapp_disp"))]
    } else {
        .dt <- margins_data[est == est_type]
    }
    .dt <- .dt[, .(est, var, coef, y, x, ci_lower, ci_upper)]

    .dt[grep("1.ar_2020", var), year := "2020"]
    .dt[grep("0bn.ar_2020", var), year := "2016-2019"]
    .dt[y %in% c("covid_dod", "covid_sjukhus", "positivt_fall"), year := "2020"]
    .dt <- .dt[!is.na(year)]

    .dt[, group := gsub("#?(0bn|1).ar_2020", "", var)]
    .dt[group == "" | group == "_cons", group := "pop_avg"]
    .dt <- .dt[!(group %in% c("0bn.utbildning", "0bn.inkomst", "0bn.land", "0bn.kon"))]
    setnames(.dt, c("y", "x"), c("outcome", "dimension"))
    set_labels(.dt)

    return(.dt)
}

prepare_radar_data <- function(margins_plotdata, dt_min = 0, dt_max = 2.5, ref = TRUE) {
    library(data.table)

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
