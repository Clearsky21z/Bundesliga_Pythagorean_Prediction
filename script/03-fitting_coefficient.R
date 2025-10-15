# script/03-fitting_coefficients.R
# Purpose: fit pooled Pythagorean coefficients on stacked EoS table
#          using ONLY seasons 2010/11 ... 2023/24, report metrics, write outputs.

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(here)
  library(stringr)
})

PROJECT   <- tryCatch(here::here(), error = function(e) getwd())
ANAL_DIR  <- fs::path(PROJECT, "data", "analysis_data")  # input source (from script 02)
OUT_DIR   <- fs::path(PROJECT, "output")                 # <- write outputs here
fs::dir_create(OUT_DIR, recurse = TRUE)

pool_fp <- fs::path(ANAL_DIR, "bundesliga_eos_pool.csv")
if (!fs::file_exists(pool_fp)) {
  stop("Missing ", pool_fp, ". Run script/02-data_aggregating.R first.")
}

pool_raw <- readr::read_csv(pool_fp, show_col_types = FALSE)

# --- keep ONLY 2010/11 ... 2023/24 ------------------------------------------
# Season is typically like "D1_2010_2011.csv" (or "DL_..."); we grab the first year.
get_first_year <- function(s) {
  m <- str_match(s, "^(?:D1|DL)_(\\d{4})_\\d{4}(?:\\.csv)?$")
  suppressWarnings(as.integer(m[,2]))
}

pool <- pool_raw %>%
  mutate(SeasonFile = Season,
         FirstYear  = get_first_year(Season)) %>%
  filter(!is.na(FirstYear), FirstYear >= 2010, FirstYear <= 2023) %>%
  select(-FirstYear)

incl <- unique(pool$SeasonFile)
excl <- setdiff(unique(pool_raw$Season), incl)

cat("Including seasons (fit set):\n - ", paste(incl, collapse = "\n - "), "\n\n", sep = "")
if (length(excl)) {
  cat("Excluded seasons:\n - ", paste(excl, collapse = "\n - "), "\n\n", sep = "")
}

# --- Pythagorean functions and helpers --------------------------------------
pythag_frac <- function(GF, GA, b, c, d) (GF^b) / ((GF^c) + (GA^d))
pythag_pts  <- function(PLD, GF, GA, a, b, c, d) a * pythag_frac(GF,GA,b,c,d) * PLD

obj_mae_pool <- function(par, pool_df) {
  a <- par[1]; b <- par[2]; c <- par[3]; d <- par[4]
  pred <- pythag_pts(pool_df$PLD, pool_df$GF, pool_df$GA, a,b,c,d)
  mean(abs(pool_df$PTS - pred))
}

# --- multi-start optimisation (robust) --------------------------------------
starts <- rbind(
  c(2.78, 1.24, 1.24, 1.25),                        # literature-ish anchor
  c(2.60, 1.45, 1.45, 1.40),                        # nearby
  matrix(c(2.4,2.9, 1.1,1.5, 1.1,1.5, 1.1,1.5), ncol=4, byrow=TRUE)  # small warm grid
)

best_val <- Inf
best_par <- NULL
for (k in 1:nrow(starts)) {
  fitk <- optim(par = starts[k,], fn = obj_mae_pool, pool_df = pool,
                method = "Nelder-Mead",
                control = list(maxit = 12000, reltol = 1e-12))
  if (fitk$value < best_val) { best_val <- fitk$value; best_par <- fitk$par }
}

coef_pool <- setNames(best_par, c("a","b","c","d"))
cat(sprintf("\nBundesliga pooled coefficients (2010/11–2023/24): a=%.4f  b=%.4f  c=%.4f  d=%.4f\n",
            coef_pool["a"], coef_pool["b"], coef_pool["c"], coef_pool["d"]))

# --- in-sample metrics (on the filtered pool only) --------------------------
pool$pred <- pythag_pts(pool$PLD, pool$GF, pool$GA,
                        coef_pool["a"], coef_pool["b"], coef_pool["c"], coef_pool["d"])
overall_mae <- mean(abs(pool$PTS - pool$pred))
overall_r   <- cor(pool$PTS, pool$pred)
cat(sprintf("In-sample (fit set): MAE=%.2f, r=%.3f\n", overall_mae, overall_r))

by_season <- pool %>%
  group_by(Season) %>%
  summarise(MAE = mean(abs(PTS - pred)),
            r   = cor(PTS, pred), .groups = "drop")

# --- LOSO cross-validation (within the fit set only) ------------------------
loso_cv <- function(pool_df, start_par = c(2.78,1.24,1.24,1.25)) {
  seasons <- unique(pool_df$Season)
  rows <- vector("list", length(seasons))
  for (i in seq_along(seasons)) {
    s <- seasons[i]
    train <- pool_df[pool_df$Season != s, , drop = FALSE]
    test  <- pool_df[pool_df$Season == s, , drop = FALSE]
    fit_s <- optim(par = start_par, fn = obj_mae_pool, pool_df = train,
                   method = "Nelder-Mead",
                   control = list(maxit = 6000, reltol = 1e-12))
    par_s <- fit_s$par
    pred  <- pythag_pts(test$PLD, test$GF, test$GA, par_s[1],par_s[2],par_s[3],par_s[4])
    rows[[i]] <- data.frame(
      Season = s, a = par_s[1], b = par_s[2], c = par_s[3], d = par_s[4],
      MAE = mean(abs(test$PTS - pred)),
      r   = cor(test$PTS, pred)
    )
  }
  dplyr::bind_rows(rows)
}

cv <- loso_cv(pool, start_par = as.numeric(coef_pool))
cat(sprintf("LOSO (fit set) median: MAE=%.2f | r=%.3f\n", median(cv$MAE), median(cv$r)))

# --- write outputs (to output/) ---------------------------------------------
coef_fp      <- fs::path(OUT_DIR, "bundesliga_coefs_pooled.csv")
by_season_fp <- fs::path(OUT_DIR, "bundesliga_in_sample_by_season.csv")
cv_fp        <- fs::path(OUT_DIR, "bundesliga_loso_cv.csv")
pool_pred_fp <- fs::path(OUT_DIR, "bundesliga_pool_with_pred.csv")

readr::write_csv(as.data.frame(t(coef_pool)), coef_fp)
readr::write_csv(by_season, by_season_fp)
readr::write_csv(cv, cv_fp)
readr::write_csv(pool, pool_pred_fp)

message("\nWrote to ", OUT_DIR, ":")
message("  - ", coef_fp)
message("  - ", by_season_fp)
message("  - ", cv_fp)
message("  - ", pool_pred_fp)
