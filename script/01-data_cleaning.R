# script/01-data_cleaning.R
# Purpose: read raw Bundesliga CSVs, standardise columns, fix FTR from goals,
#          normalise team names, order chronologically, and write to data/cleaned_data

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(fs)
  library(here)
})

# --- project-aware paths -----------------------------------------------------
PROJECT   <- tryCatch(here::here(), error = function(e) getwd())
RAW_DIR   <- fs::path(PROJECT, "data", "raw_data")
CLEAN_DIR <- fs::path(PROJECT, "data", "cleaned_data")
fs::dir_create(CLEAN_DIR, recurse = TRUE)

FILE_REGEX <- "^(D1).*\\.[Cc][Ss][Vv]$"

# --- helpers -----------------------------------------------------------------

# 1) Drop "Unnamed:*" and columns that are entirely NA
drop_empty_cols <- function(df) {
  df <- df[, !grepl("^Unnamed", names(df)), drop = FALSE]
  df[, colSums(!is.na(df)) > 0, drop = FALSE]
}

# 2) Simple normalisation of team strings
normalize_team <- function(x) stringr::str_trim(stringr::str_replace_all(x, "\\s+", " "))

# 3) Parse Date/Time (football-data often uses d/m/yy or d/m/yyyy)
parse_and_order <- function(df) {
  # Date (d/m/yy or d/m/yyyy)
  if ("Date" %in% names(df)) {
    d <- suppressWarnings(lubridate::dmy(df$Date))
    if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(df$Date, locale = readr::locale(date_names = "en_GB")))
    df$Date <- d
  }
  
  # Time — coerce to character; normalise 3–4 digit HHMM like 1530 -> 15:30
  if ("Time" %in% names(df)) {
    if (!is.character(df$Time)) df$Time <- as.character(df$Time)
    
    fix_hhmm <- function(x) {
      x <- trimws(x)
      x[x %in% c("", "NA")] <- NA_character_
      hhmm <- grepl("^\\d{3,4}$", x)              # 900, 0930, 1530, etc.
      x[hhmm] <- sprintf("%s:%s",
                         stringr::str_pad(substr(x[hhmm], 1, nchar(x[hhmm]) - 2), 2, "left", "0"),
                         substr(x[hhmm], nchar(x[hhmm]) - 1, nchar(x[hhmm])))
      x
    }
    
    df$Time <- fix_hhmm(df$Time)
    df$Time <- suppressWarnings(readr::parse_time(df$Time, na = c("", "NA")))
    df <- dplyr::arrange(df, Date, Time, HomeTeam, AwayTeam)
  } else {
    df <- dplyr::arrange(df, Date, HomeTeam, AwayTeam)
  }
  df
}


# 4) Make FTR consistent with goals (H/A/D)
recompute_FTR <- function(df) {
  if (all(c("FTHG","FTAG") %in% names(df))) {
    ftr_calc <- dplyr::case_when(
      df$FTHG > df$FTAG ~ "H",
      df$FTHG < df$FTAG ~ "A",
      TRUE              ~ "D"
    )
    if (!"FTR" %in% names(df)) {
      df$FTR <- ftr_calc
    } else {
      bad <- which(df$FTR != ftr_calc & !is.na(df$FTR))
      if (length(bad) > 0) message("  Fixed ", length(bad), " FTR mismatches (from goals).")
      df$FTR <- ftr_calc
    }
  }
  df
}

# 5) Clean one file and write it next to the others in cleaned_data
clean_one_file <- function(infile) {
  message("Cleaning: ", fs::path_file(infile))
  df <- suppressMessages(
    readr::read_csv(infile, locale = readr::locale(encoding = "Latin1"), show_col_types = FALSE)
  )
  
  req <- c("HomeTeam","AwayTeam","FTHG","FTAG")
  if (!all(req %in% names(df))) {
    stop("Missing required columns in ", fs::path_file(infile), ": ",
         paste(setdiff(req, names(df)), collapse = ", "))
  }
  
  df <- df %>%
    drop_empty_cols() %>%
    mutate(
      HomeTeam = normalize_team(HomeTeam),
      AwayTeam = normalize_team(AwayTeam),
      FTHG = suppressWarnings(as.integer(FTHG)),
      FTAG = suppressWarnings(as.integer(FTAG))
    ) %>%
    parse_and_order() %>%
    recompute_FTR()
  
  if (!"Div" %in% names(df)) df$Div <- NA_character_
  
  core <- c("Div","Date","Time","HomeTeam","AwayTeam","FTHG","FTAG","FTR")
  df <- df[, c(intersect(core, names(df)), setdiff(names(df), core)), drop = FALSE]
  
  out_fp <- fs::path(CLEAN_DIR, fs::path_file(infile))
  readr::write_csv(df, out_fp)
  message("  -> wrote: ", out_fp)
  invisible(out_fp)
}

# --- discover files and run cleaning ----------------------------------------
all_files <- fs::dir_ls(RAW_DIR, type = "file", recurse = FALSE)
files <- all_files[grepl(FILE_REGEX, fs::path_file(all_files), perl = TRUE)]

if (length(files) == 0) {
  message("Working directory: ", PROJECT)
  message("Looked for files in: ", RAW_DIR)
  if (fs::dir_exists(fs::path(PROJECT, "data"))) {
    message("data/ tree:\n"); print(fs::dir_tree(fs::path(PROJECT, "data"), recurse = 2))
  }
  stop("No raw CSVs found under data/raw_data. Open the .Rproj or setwd() to project root.")
}

message("Found ", length(files), " raw CSV(s): ",
        paste(fs::path_file(files), collapse = ", "))

invisible(lapply(files, clean_one_file))
message("leaning complete. Files written to: ", CLEAN_DIR)
