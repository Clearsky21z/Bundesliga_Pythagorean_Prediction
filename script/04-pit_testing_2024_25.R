# script/04-pit_testing_2024_25.R
# Purpose: Use pooled Pythagorean coefs (fit on 2010/11–2023/24)
#          to do Points-in-Table (PiT) predictions for 2024/25,
#          evaluate after 9/18/27 rounds, and write plots + tables.

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(fs)
  library(here)
  library(stringr)
})

# --- project-aware paths -----------------------------------------------------
PROJECT   <- tryCatch(here::here(), error = function(e) getwd())
DATA_DIR  <- fs::path(PROJECT, "data")
RAW_DIR   <- fs::path(DATA_DIR, "raw_data")
OUT_DIR   <- fs::path(PROJECT, "output")       # <- write all outputs here
fs::dir_create(OUT_DIR, recurse = TRUE)

# --- load pooled coefficients (written by script 03 into output/) -----------
coef_fp <- fs::path(OUT_DIR, "bundesliga_coefs_pooled.csv")
if (!fs::file_exists(coef_fp)) {
  stop("Missing ", coef_fp, ". Run script/03-fitting_coefficients.R first (it writes to output/).")
}
coef_tbl  <- readr::read_csv(coef_fp, show_col_types = FALSE)
coef_pool <- as.numeric(coef_tbl[1, c("a","b","c","d")])
names(coef_pool) <- c("a","b","c","d")
cat(sprintf("Loaded pooled coefs (2010/11–2023/24): a=%.4f b=%.4f c=%.4f d=%.4f\n",
            coef_pool["a"], coef_pool["b"], coef_pool["c"], coef_pool["d"]))

# --- bring in the 2024/25 match file ---------------------------------------
file_2425 <- fs::path(RAW_DIR, "D1_2024_2025.csv")
if (!fs::file_exists(file_2425)) {
  stop("Could not find test file: ", file_2425)
}
df_2425 <- read.csv(file_2425, stringsAsFactors = FALSE, check.names = FALSE)

req <- c("HomeTeam","AwayTeam","FTHG","FTAG","FTR")
stopifnot(all(req %in% names(df_2425)))

# --- minimal helpers (replicate from 02/03 as needed) -----------------------
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

pythag_frac <- function(GF, GA, b, c, d) (GF^b) / ((GF^c) + (GA^d))

# --- End-of-season actual table for 2024/25 ---------------------------------
eos_2425 <- create.table(df_2425$HomeTeam, df_2425$AwayTeam, df_2425$FTHG, df_2425$FTAG)
eos_2425$Team <- rownames(eos_2425); rownames(eos_2425) <- NULL

nteams <- nrow(eos_2425)
matches_per_round <- nteams / 2            # 18 teams -> 9 matches per round
nGames <- (nteams - 1) * 2                 # total matches in BL season: 34

# --- PiT function ------------------------------------------------------------
pit_predict_and_plot <- function(df_matches, rounds, coefs, eos_tab, save_png = TRUE, out_dir = ".") {
  take_n <- as.integer(rounds * (nrow(eos_tab)/2))
  if (nrow(df_matches) < take_n) stop("Not enough matches for rounds=", rounds, " (need ", take_n, ")")
  df_pit <- utils::head(df_matches, take_n)
  tab_pit <- create.table(df_pit$HomeTeam, df_pit$AwayTeam, df_pit$FTHG, df_pit$FTAG)
  
  frac <- pythag_frac(tab_pit$GF, tab_pit$GA, coefs["b"], coefs["c"], coefs["d"])
  pred_tot <- tab_pit$PTS + coefs["a"] * frac * (nGames - tab_pit$PLD)
  
  pit_df <- transform(tab_pit,
                      Team = rownames(tab_pit),
                      PredTot = as.numeric(round(pred_tot, 1)),
                      PythagFrac = round(frac, 3))
  rownames(pit_df) <- NULL
  
  aligned <- merge(eos_tab[, c("Team","PTS")], pit_df[, c("Team","PredTot")],
                   by = "Team", sort = FALSE)
  
  r_val  <- suppressWarnings(cor(aligned$PTS, aligned$PredTot))
  mae    <- mean(abs(aligned$PTS - aligned$PredTot))
  
  plt <- ggplot(aligned, aes(x = PTS, y = PredTot)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, linetype = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    labs(x = "Actual EoS points (2024–25)",
         y = sprintf("Predicted EoS points (after %d rounds)", rounds),
         title = sprintf("Bundesliga 2024–25 — after %d rounds (r = %.3f, MAE = %.2f)", rounds, r_val, mae))
  
  print(plt)
  if (save_png) {
    fname <- fs::path(out_dir, sprintf("BL_2024_25_PiT_round_%02d.png", rounds))
    ggsave(filename = fname, plot = plt, width = 7, height = 5, dpi = 150)
  }
  
  list(rounds = rounds, r = r_val, MAE = mae, aligned = aligned, plot = plt)
}

# --- Run after 9, 18, 27 rounds (write to output/) --------------------------
coefs_bl <- coef_pool
res9  <- pit_predict_and_plot(df_2425,  9, coefs_bl, eos_2425, save_png = TRUE, out_dir = OUT_DIR)
res18 <- pit_predict_and_plot(df_2425, 18, coefs_bl, eos_2425, save_png = TRUE, out_dir = OUT_DIR)
res27 <- pit_predict_and_plot(df_2425, 27, coefs_bl, eos_2425, save_png = TRUE, out_dir = OUT_DIR)

sum_tab <- data.frame(
  Rounds = c(9, 18, 27),
  r      = c(res9$r,  res18$r,  res27$r),
  MAE    = c(res9$MAE, res18$MAE, res27$MAE)
)
print(sum_tab)
readr::write_csv(sum_tab, fs::path(OUT_DIR, "BL_2024_25_PiT_summary.csv"))

# --- Compare final EoS vs prediction made after 27 rounds -------------------
rounds <- 27L
take_n <- as.integer(rounds * (nrow(eos_2425)/2))
df27   <- utils::head(df_2425, take_n)
tab27  <- create.table(df27$HomeTeam, df27$AwayTeam, df27$FTHG, df27$FTAG)

frac27    <- pythag_frac(tab27$GF, tab27$GA, coef_pool["b"], coef_pool["c"], coef_pool["d"])
predTot27 <- tab27$PTS + coef_pool["a"] * frac27 * (nGames - tab27$PLD)

pred27 <- data.frame(Team = rownames(tab27),
                     PredTot = round(as.numeric(predTot27), 1),
                     PythagFrac = round(frac27, 3),
                     row.names = NULL)

aligned27 <- merge(eos_2425[, c("Team","PTS")],
                   pred27[, c("Team","PredTot")],
                   by = "Team", sort = FALSE)

r27   <- suppressWarnings(cor(aligned27$PTS, aligned27$PredTot))
mae27 <- mean(abs(aligned27$PTS - aligned27$PredTot))
cat(sprintf("Round 27 comparison -> r = %.3f, MAE = %.2f\n", r27, mae27))

p27 <- ggplot(aligned27, aes(x = PTS, y = PredTot)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Actual end-of-season points (2024–25)",
       y = "Predicted EoS points (after 27 rounds)",
       title = sprintf("Bundesliga 2024–25: prediction after 27 rounds (r = %.3f, MAE = %.2f)", r27, mae27))
print(p27)
ggsave(fs::path(OUT_DIR, "BL_2024_25_PiT_round_27.png"), p27, width = 7, height = 5, dpi = 150)

# --- Biggest over/under vs final at round 27 --------------------------------
diff27 <- aligned27 %>%
  mutate(err = PredTot - PTS,
         abs_err = abs(err),
         flag = ifelse(err > 0, "Over (pred>actual)", "Under (pred<actual)")) %>%
  arrange(desc(abs_err))

readr::write_csv(diff27, fs::path(OUT_DIR, "BL_2024_25_team_diffs_round27.csv"))
print(head(diff27, 5))

message("\nOutputs written to: ", OUT_DIR)
