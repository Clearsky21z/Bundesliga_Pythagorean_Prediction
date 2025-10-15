# script/02-data_aggregating.R
# Purpose: build end-of-season (EoS) tables per season from cleaned_data,
#          and save stacked pool to data/analysis_data

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(here)
})

PROJECT   <- tryCatch(here::here(), error = function(e) getwd())
CLEAN_DIR <- fs::path(PROJECT, "data", "cleaned_data")
ANAL_DIR  <- fs::path(PROJECT, "data", "analysis_data")
fs::dir_create(ANAL_DIR, recurse = TRUE)

FILE_REGEX <- "^(D1).*\\.[Cc][Ss][Vv]$"

# --- league table utilities --------------------------------------------------
outcome <- function(hGoals, aGoals){
  res <- character(length(hGoals))
  res[hGoals >  aGoals] <- "H"
  res[hGoals <  aGoals] <- "A"
  res[hGoals == aGoals] <- "D"
  res
}

create.table <- function(hTeam, aTeam, hGoals, aGoals){
  results <- outcome(hGoals, aGoals)
  teams <- sort(unique(c(hTeam, aTeam)))
  n <- length(teams)
  
  x <- numeric(n)
  out <- data.frame(PLD=x, HW=x, HD=x, HL=x, AW=x, AD=x, AL=x, GF=x, GA=x, GD=x, PTS=x,
                    row.names = teams, check.names = FALSE)
  
  for(i in seq_along(hTeam)){
    ht <- hTeam[i]; at <- aTeam[i]
    hg <- hGoals[i]; ag <- aGoals[i]; r <- results[i]
    
    out[ht, "PLD"] <- out[ht, "PLD"] + 1
    out[at, "PLD"] <- out[at, "PLD"] + 1
    
    out[ht, "GF"]  <- out[ht, "GF"] + hg; out[ht, "GA"] <- out[ht, "GA"] + ag
    out[at, "GF"]  <- out[at, "GF"] + ag; out[at, "GA"] <- out[at, "GA"] + hg
    
    if(r=="H"){ out[ht,"HW"] <- out[ht,"HW"] + 1; out[at,"AL"] <- out[at,"AL"] + 1; out[ht,"PTS"] <- out[ht,"PTS"] + 3 }
    if(r=="A"){ out[at,"AW"] <- out[at,"AW"] + 1; out[ht,"HL"] <- out[ht,"HL"] + 1; out[at,"PTS"] <- out[at,"PTS"] + 3 }
    if(r=="D"){ out[ht,"HD"] <- out[ht,"HD"] + 1; out[at,"AD"] <- out[at,"AD"] + 1; out[ht,"PTS"] <- out[ht,"PTS"] + 1; out[at,"PTS"] <- out[at,"PTS"] + 1 }
  }
  
  out$GD <- out$GF - out$GA
  out[order(-out$PTS, -out$GD, -out$GF), , drop = FALSE]
}

# --- build EoS per file and pool --------------------------------------------
cleaned <- fs::dir_ls(CLEAN_DIR, type = "file", recurse = FALSE)
cleaned <- cleaned[grepl(FILE_REGEX, fs::path_file(cleaned), perl = TRUE)]

if (length(cleaned) == 0) stop("No cleaned CSVs found in ", CLEAN_DIR,
                               ". Run script/01-data_cleaning.R first.")

message("Found ", length(cleaned), " cleaned CSV(s): ",
        paste(fs::path_file(cleaned), collapse = ", "))

pool_rows <- list()

for (fp in cleaned) {
  season_name <- fs::path_file(fp)  # keep file name as season tag
  df <- readr::read_csv(fp, show_col_types = FALSE)
  
  req <- c("HomeTeam","AwayTeam","FTHG","FTAG")
  if (!all(req %in% names(df))) {
    warning("Skipping (missing cols): ", season_name)
    next
  }
  
  tab <- create.table(df$HomeTeam, df$AwayTeam, df$FTHG, df$FTAG)
  tab$Season <- season_name
  tab$Team   <- rownames(tab)
  rownames(tab) <- NULL
  tab <- tab[, c("Season","Team","PLD","GF","GA","PTS")]
  
  # save per-season table
  out_eos <- fs::path(ANAL_DIR, paste0("eos_", season_name))
  readr::write_csv(tab, out_eos)
  message("  -> wrote: ", out_eos)
  
  pool_rows[[length(pool_rows)+1]] <- tab
}

pool <- dplyr::bind_rows(pool_rows)

# Save pooled EoS stack
pool_fp <- fs::path(ANAL_DIR, "bundesliga_eos_pool.csv")
readr::write_csv(pool, pool_fp)
message("Wrote pooled EoS stack: ", pool_fp)
