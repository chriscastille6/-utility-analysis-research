# Shared helpers for Simulation Decision Lens Excel uploads

sim_lens_safe_num <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  s <- trimws(as.character(x[1]))
  s <- gsub(",", "", s, fixed = TRUE)
  s <- gsub("%", "", s, fixed = TRUE)
  out <- suppressWarnings(as.numeric(s))
  if (is.na(out)) NA_real_ else out
}

sim_lens_get_file <- function(upload_df, exact_name) {
  if (is.null(upload_df) || nrow(upload_df) == 0) {
    return(NULL)
  }
  idx <- which(tolower(upload_df$name) == tolower(exact_name))
  if (length(idx) == 0) {
    return(NULL)
  }
  upload_df$datapath[idx[1]]
}

sim_lens_read_matrix <- function(path) {
  if (is.null(path) || !file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    return(NULL)
  }
  suppressWarnings(readxl::read_excel(path, col_names = FALSE))
}

sim_lens_find_row <- function(df, label) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(NULL)
  }
  row_labels <- trimws(tolower(as.character(df[[1]])))
  idx <- which(row_labels == trimws(tolower(label)))
  if (length(idx) == 0) {
    return(NULL)
  }
  as.list(df[idx[1], , drop = TRUE])
}

sim_lens_sum_numeric <- function(x) {
  vals <- suppressWarnings(as.numeric(gsub(",", "", as.character(unlist(x)), fixed = TRUE)))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) NA_real_ else sum(vals)
}

sim_lens_parse_upload <- function(upload_df) {
  if (is.null(upload_df) || nrow(upload_df) == 0) {
    return(list(ok = FALSE, message = "No files uploaded.", values = list(), warnings = "Upload one or more .xlsx files."))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(list(ok = FALSE, message = "readxl is not available.", values = list(), warnings = "Install readxl or use manual assumptions."))
  }

  out <- list(
    headcount_total = NA_real_,
    weighted_avg_annual_salary = NA_real_,
    sdy_40pct = NA_real_,
    projected_turnover_rate = NA_real_,
    absenteeism_days = NA_real_,
    productivity = NA_real_,
    quality_index = NA_real_,
    training_budget_total = NA_real_,
    dei_program_cost = NA_real_,
    total_hires = NA_real_
  )
  warnings <- character(0)

  compensation <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "department-compensation.xlsx"))
  if (!is.null(compensation)) {
    pay_row <- sim_lens_find_row(compensation, "Estimated Annual Pay")
    emp_row <- sim_lens_find_row(compensation, "Employees")
    if (!is.null(pay_row) && !is.null(emp_row)) {
      pays <- suppressWarnings(as.numeric(unlist(pay_row[2:6])))
      emps <- suppressWarnings(as.numeric(unlist(emp_row[2:6])))
      if (all(!is.na(pays)) && all(!is.na(emps)) && sum(emps) > 0) {
        out$headcount_total <- sum(emps)
        out$weighted_avg_annual_salary <- sum(emps * pays) / sum(emps)
      } else {
        warnings <- c(warnings, "Could not parse level pay/count values in department-compensation.xlsx.")
      }
    } else {
      warnings <- c(warnings, "department-compensation.xlsx missing 'Estimated Annual Pay' or 'Employees' rows.")
    }
  } else {
    warnings <- c(warnings, "department-compensation.xlsx not found.")
  }

  staffing_report <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "department-staffing_report.xlsx"))
  if (!is.null(staffing_report)) {
    turnover_row <- sim_lens_find_row(staffing_report, "Projected Turnover")
    hires_row <- sim_lens_find_row(staffing_report, "New Hires / Layoffs")
    if (!is.null(turnover_row)) {
      total_turnover <- sim_lens_safe_num(turnover_row[[7]])
      if (is.na(total_turnover)) {
        level_turnover <- suppressWarnings(as.numeric(unlist(turnover_row[2:6])))
        level_counts <- if (!is.null(sim_lens_find_row(staffing_report, "Employees Available"))) {
          suppressWarnings(as.numeric(unlist(sim_lens_find_row(staffing_report, "Employees Available")[2:6])))
        } else {
          rep(NA_real_, 5)
        }
        if (all(!is.na(level_turnover)) && all(!is.na(level_counts)) && sum(level_counts) > 0) {
          total_turnover <- sum(level_turnover * level_counts) / sum(level_counts)
        }
      }
      out$projected_turnover_rate <- total_turnover
    }
    if (!is.null(hires_row)) {
      out$total_hires <- sim_lens_safe_num(hires_row[[7]])
    }
  }

  relations <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "department-relations.xlsx"))
  if (!is.null(relations)) {
    quarter_col <- suppressWarnings(as.numeric(relations[[1]]))
    latest_idx <- which(!is.na(quarter_col))
    if (length(latest_idx) > 0) {
      out$absenteeism_days <- sim_lens_safe_num(relations[[4]][max(latest_idx)])
    }
  }

  production <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "department-production.xlsx"))
  if (!is.null(production)) {
    prod_row <- sim_lens_find_row(production, "PRODUCTION_ACTUAL")
    if (!is.null(prod_row)) {
      out$productivity <- sim_lens_safe_num(prod_row[[2]])
    }
  }

  development <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "department-development.xlsx"))
  if (!is.null(development)) {
    quarter_col <- suppressWarnings(as.numeric(development[[1]]))
    latest_idx <- which(!is.na(quarter_col))
    if (length(latest_idx) > 0) {
      out$quality_index <- sim_lens_safe_num(development[[3]][max(latest_idx)])
    }
  }

  training <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "decisions-training.xlsx"))
  if (!is.null(training)) {
    labels <- trimws(tolower(as.character(training[[1]])))
    idx <- which(grepl("training budget", labels, fixed = TRUE))
    if (length(idx) > 0) {
      out$training_budget_total <- sim_lens_sum_numeric(training[idx, 2, drop = TRUE])
    }
  }

  programs <- sim_lens_read_matrix(sim_lens_get_file(upload_df, "decisions-programs.xlsx"))
  if (!is.null(programs)) {
    labels <- trimws(tolower(as.character(programs[[1]])))
    dei_idx <- which(labels == "dei program")
    if (length(dei_idx) > 0) {
      selected <- trimws(tolower(as.character(programs[[3]][dei_idx[1]])))
      if (identical(selected, "yes")) {
        out$dei_program_cost <- sim_lens_safe_num(programs[[2]][dei_idx[1]])
      } else {
        out$dei_program_cost <- 0
      }
    }
  }

  if (!is.na(out$weighted_avg_annual_salary)) {
    out$sdy_40pct <- out$weighted_avg_annual_salary * 0.4
  }

  list(
    ok = TRUE,
    message = paste0("Imported ", nrow(upload_df), " file(s)."),
    values = out,
    warnings = warnings
  )
}
