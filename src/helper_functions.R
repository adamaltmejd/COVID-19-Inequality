convert_character_cols_to_native <- function(.dt) {
    if (any(sapply(.dt, is.character))) {
        cols <- .dt[, names(.SD), .SDcols = is.character]
        .dt[, (cols) := lapply(.SD, enc2native), .SDcols = cols] # xfun::native_encode()
    }
}

save_table <- function(.table, fn, folder = file.path("out", "tables")) {
    library(kableExtra)
    fn <- file.path(folder, fn)

    if ("character" %in% class(.table)) {
        .table |> cat(file = fn)
    } else {
        .table |> save_kable(fn)
    }

    return(fn)
}

braces <- function(x) {
    paste0("{", trimws(x), "}")
}

si_num <- function(x) {
    braces(paste0("\\num{", x, "}"))
}

escape_tex <- function(x) {
    x <- gsub("\\", "$\\backslash$", x, fixed = TRUE)
    x <- gsub("#", "\\\\#", x)
    x <- gsub("\\$", "\\\\$", x)
    x <- gsub("%", "\\\\%", x)
    x <- gsub("&", "\\\\&", x)
    x <- gsub("~", "\\\\~", x)
    x <- gsub("_", "\\\\_", x)
    x <- gsub("\\^", "\\\\^", x)
    x <- gsub("\\{", "\\\\{", x)
    x <- gsub("\\}", "\\\\}", x)
    x <- gsub(">", "$>$", x)
    x <- gsub("<", "$<$", x)
    return(x)
}

stars <- function() {
    c("\\tnote{***}" = 0.001, "\\tnote{**}" = 0.01, "\\tnote{*}" = 0.05, "\\tnote{\\dagger}" = 0.1)
}

get_stars <- function(p) {
    as.character(cut(p, breaks = c(-Inf, stars(), Inf), label = c(names(stars()), "")))
}

add_S_align <- function(.table, S_fmt = "1.3", ncols = NULL,
    text_before = rep("(", length(S_fmt)),
    text_after = rep("\\\\textsuperscript{***}", length(S_fmt))
    ) {

    if (length(text_before) != length(S_fmt)) { stop("text_before and S_fmt need to be same length.")}
    if (length(text_after) != length(S_fmt)) { stop("text_after and S_fmt need to be same length.")}

    # Replaces tabular alignment with siunitx columns
    si_setup <- paste0("table-format={", text_before, "}", S_fmt, "{", text_after, "}")

    if (length(S_fmt) > 1) {
        if (!is.null(ncols)) stop("Either provide a vector for S_fmt or ncols.")
        ncols <- 1
        si_setup <- paste(paste0("S\\[", si_setup, "\\]"), collapse = "")
    } else {
        si_setup <- paste0("S\\[", si_setup, "\\]")
    }

    # If ncols is null try to guess it from existing specification
    # works if table format is set by *{5}{l}, not if its {lllll}
    if (is.null(ncols)) ncols <- nchar(gsub("^.*\\\\begin\\{tabular\\}(\\[\\w*\\])?\\{l+(c*)}.*$", "\\2", .table))
    if (ncols == 0) stop("Error guessing ncols.")

    .table <- gsub(
        "\\\\begin\\{tabular\\}(\\[\\w+\\])?\\{\\w+\\}\n",
        paste0("\\\\begin\\{tabular\\}\\{@\\{\\}l*\\{", ncols,
               "\\}\\{", si_setup, "\\}@\\{\\}\\}\n"),
        .table)

    return(.table)
}

add_tabularnewline <- function(.table) {
    # Change newlines at end of line to \tabularnewline
    gsub("\\\\\\\\\\n", "\\\\tabularnewline%\n", .table)
}

fmt_est <- function(est, p = NULL, unit_txt = NULL, digits = 2) {
    out <- format(round(est, digits = digits), trim = TRUE, nsmall = digits)
    if (!is.null(p)) {
        p <- ifelse(is.na(p), "", get_stars(p))
        out <- paste0(out, unit_txt, p)
    }
    return(out)
}

fmt_se <- function(se, par = TRUE, digits = 2) {
    out <- format(round(se, digits = digits), trim = TRUE, nsmall = digits)
    if (par) out <- paste0("(", out, ")")
    return(out)
}

max_na_on_empty <- function(x) {
    x <- na.omit(x)
    if (length(x) == 0) return(NA)
    max(x)
}