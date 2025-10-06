## --- 0) Setup (project-aware paths) ---------------------------------------
library(dplyr); library(ggplot2)

# If you're opening the .Rproj, your working directory is the project root:
data_dir <- "."   # = "/Users/clearsky21/Bundesliga_Pythagorean_Prediction"

# Pattern matches BOTH old D1_* and new DL_* filenames
bl_pattern <- "^(D1|DL)_.*\\.csv$"

# Quick sanity check: list the files we’ll load
csvs <- list.files(data_dir, pattern = bl_pattern, full.names = TRUE)
if (length(csvs) == 0) stop("No Bundesliga CSVs found. Check names or pattern.")
print(basename(csvs))


## --- 1) Utilities to build PiT tables (book-style) ------------------------

# 1A. Match outcome vector (H/A/D) from goals
outcome <- function(hGoals, aGoals){
  res <- character(length(hGoals))
  res[hGoals >  aGoals] <- "H"
  res[hGoals <  aGoals] <- "A"
  res[hGoals == aGoals] <- "D"
  res
}

# 1B. Create league table from any prefix of matches (PiT)
create.table <- function(hTeam, aTeam, hGoals, aGoals){
  results <- outcome(hGoals, aGoals)
  teams <- sort(unique(c(hTeam, aTeam)))
  n <- length(teams)
  
  # init
  x <- numeric(n)
  out <- data.frame(PLD=x, HW=x, HD=x, HL=x, AW=x, AD=x, AL=x, GF=x, GA=x, GD=x, PTS=x,
                    row.names = teams, check.names = FALSE)
  
  # accumulate
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

## Convenience: full-season table
tab_EoS <- with(raw, create.table(HomeTeam, AwayTeam, FTHG, FTAG))

## --- 2) Pythagorean functions --------------------------------------------

pythag_frac <- function(GF, GA, b, c, d) (GF^b) / ((GF^c) + (GA^d))
pythag_pts  <- function(PLD, GF, GA, a, b, c, d) a * pythag_frac(GF,GA,b,c,d) * PLD

## --- 3) Estimate (a,b,c,d) on MULTIPLE seasons (Bundesliga) --------------

# 3A) Helpers ---------------------------------------------------------------

# Read one season (football-data format) and return end-of-season table
read_season_eos <- function(csv_path) {
  dat <- read.csv(csv_path, stringsAsFactors = FALSE)
  req <- c("HomeTeam","AwayTeam","FTHG","FTAG","FTR")
  stopifnot(all(req %in% names(dat)))
  # EoS table using your book-style function
  tab <- with(dat, create.table(HomeTeam, AwayTeam, FTHG, FTAG))
  # Keep a Season label derived from file name
  tab$Season <- basename(csv_path)
  tab$Team   <- rownames(tab)
  rownames(tab) <- NULL
  tab[, c("Season","Team","PLD","GF","GA","PTS")]
}

# Pool all seasons that match a filename pattern (e.g., DL_2010_2011.csv … DL_2023_24.csv)
load_bundesliga_eos <- function(data_dir, pattern = "^DL_.*\\.csv$") {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matched. Check pattern/path.")
  tabs <- lapply(files, read_season_eos)
  dplyr::bind_rows(tabs)
}

# Objective across pooled seasons (MAE by default)
obj_mae_pool <- function(par, pool_df) {
  a <- par[1]; b <- par[2]; c <- par[3]; d <- par[4]
  pred <- pythag_pts(pool_df$PLD, pool_df$GF, pool_df$GA, a,b,c,d)
  mean(abs(pool_df$PTS - pred))
}

# Convenience: evaluate fit with MAE and r per-season and overall
evaluate_pool <- function(pool_df, par) {
  a <- par[1]; b <- par[2]; c <- par[3]; d <- par[4]
  pool_df$pred <- pythag_pts(pool_df$PLD, pool_df$GF, pool_df$GA, a,b,c,d)
  overall <- list(
    MAE = mean(abs(pool_df$PTS - pool_df$pred)),
    r   = cor(pool_df$PTS, pool_df$pred)
  )
  by_season <- pool_df |>
    dplyr::group_by(Season) |>
    dplyr::summarise(
      MAE = mean(abs(PTS - pred)),
      r   = cor(PTS, pred),
      .groups = "drop"
    )
  list(overall = overall, by_season = by_season, preds = pool_df)
}

# Leave-one-season-out CV
loso_cv <- function(pool_df, start_par = c(2.78,1.24,1.24,1.25)) {
  seasons <- unique(pool_df$Season)
  rows <- list()
  for (s in seasons) {
    train <- subset(pool_df, Season != s)
    test  <- subset(pool_df, Season == s)
    fit_s <- optim(
      par = start_par, fn = obj_mae_pool, pool_df = train,
      method = "Nelder-Mead",
      control = list(maxit = 4000, reltol = 1e-12)
    )
    par_s <- fit_s$par
    pred  <- pythag_pts(test$PLD, test$GF, test$GA, par_s[1],par_s[2],par_s[3],par_s[4])
    rows[[s]] <- data.frame(
      Season = s,
      a = par_s[1], b = par_s[2], c = par_s[3], d = par_s[4],
      MAE = mean(abs(test$PTS - pred)),
      r   = cor(test$PTS, pred)
    )
  }
  do.call(rbind, rows)
}

# 3B) Load all seasons ------------------------------------------------------

# Set your folder; pattern matches any file named like "DL_2010_2011.csv", "DL_2023_24.csv", etc.
pool <- load_bundesliga_eos(data_dir, pattern = "^DL_.*\\.csv$")

# 3C) Fit pooled coefficients (minimise MAE) --------------------------------

start <- c(a=2.78, b=1.24, c=1.24, d=1.25)  # good starting point
fit_pool <- optim(
  par = start,
  fn  = obj_mae_pool,
  pool_df = pool,
  method  = "Nelder-Mead",
  control = list(maxit = 8000, reltol = 1e-12)
)

coef_pool <- setNames(fit_pool$par, c("a","b","c","d"))
cat(sprintf("Bundesliga pooled fit (all seasons) -> a=%.3f b=%.3f c=%.3f d=%.3f\n",
            coef_pool["a"],coef_pool["b"],coef_pool["c"],coef_pool["d"]))

# 3D) In-sample summary and by-season metrics -------------------------------

eval_pool <- evaluate_pool(pool, coef_pool)
cat(sprintf("Overall: MAE=%.2f | r=%.3f\n", eval_pool$overall$MAE, eval_pool$overall$r))
print(head(eval_pool$by_season))

# 3E) Leave-one-season-out CV (out-of-sample check) -------------------------

cv_loso <- loso_cv(pool, start_par = start)
cat(sprintf("LOSO (median): MAE=%.2f | r=%.3f\n",
            median(cv_loso$MAE), median(cv_loso$r)))
# Inspect worst/best seasons if desired:
# cv_loso[order(-cv_loso$MAE), ][1:3, ]
