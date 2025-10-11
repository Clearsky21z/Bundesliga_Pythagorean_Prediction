file_2425 <- file.path(data_dir, "test_file_2024_2025.csv")
df_2425 <- read.csv(file_2425, stringsAsFactors = FALSE)
req <- c("HomeTeam","AwayTeam","FTHG","FTAG","FTR")
stopifnot(all(req %in% names(df_2425)))

# End-of-season actual table
eos_2425 <- create.table(df_2425$HomeTeam, df_2425$AwayTeam, df_2425$FTHG, df_2425$FTAG)
eos_2425$Team <- rownames(eos_2425); rownames(eos_2425) <- NULL
nteams <- nrow(eos_2425)
matches_per_round <- nteams / 2      # Bundesliga: 18 teams -> 9 matches/round
nGames <- (nteams - 1) * 2           # total matches in season: 34

pit_predict_and_plot <- function(df_matches, rounds, coefs, eos_tab, save_png = TRUE) {
  take_n <- as.integer(rounds * matches_per_round)
  if (nrow(df_matches) < take_n) stop("Not enough matches in file for rounds=", rounds)
  
  df_pit <- utils::head(df_matches, take_n)
  tab_pit <- create.table(df_pit$HomeTeam, df_pit$AwayTeam, df_pit$FTHG, df_pit$FTAG)
  
  frac <- pythag_frac(tab_pit$GF, tab_pit$GA, coefs["b"], coefs["c"], coefs["d"])
  pred_tot <- tab_pit$PTS + coefs["a"] * frac * (nGames - tab_pit$PLD)
  
  pit_df <- transform(tab_pit,
                      Team = rownames(tab_pit),
                      PredTot = as.numeric(round(pred_tot, 1)),
                      PythagFrac = round(frac, 3))
  rownames(pit_df) <- NULL
  

  aligned <- merge(eos_tab[, c("Team","PTS")], pit_df[, c("Team","PredTot")], by = "Team", sort = FALSE)
  
  r_val  <- cor(aligned$PTS, aligned$PredTot)
  mae    <- mean(abs(aligned$PTS - aligned$PredTot))
  
  plt <- ggplot(aligned, aes(x = PTS, y = PredTot)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, linetype = 1) +
    labs(x = "Actual EoS points (2024-25)",
         y = sprintf("Predicted EoS points (after %d rounds)", rounds),
         title = sprintf("Bundesliga 2024-25 — after %d rounds (r = %.3f, MAE = %.2f)",
                         rounds, r_val, mae))
  
  print(plt)
  if (save_png) {
    fname <- sprintf("BL_2024_25_PiT_round_%02d.png", rounds)
    ggsave(filename = fname, plot = plt, width = 7, height = 5, dpi = 150)
  }
  
  list(rounds = rounds, r = r_val, MAE = mae, aligned = aligned, plot = plt)
}

coefs_bl <- coef_pool  # from your pooled fit (DL_2010_2011 ... DL_2023_24)

res9  <- pit_predict_and_plot(df_2425,  9, coefs_bl, eos_2425)
res18 <- pit_predict_and_plot(df_2425, 18, coefs_bl, eos_2425)
res27 <- pit_predict_and_plot(df_2425, 27, coefs_bl, eos_2425)

sum_tab <- data.frame(
  Rounds = c(9, 18, 27),
  r      = c(res9$r,  res18$r,  res27$r),
  MAE    = c(res9$MAE, res18$MAE, res27$MAE)
)
print(sum_tab)
write.csv(sum_tab, "BL_2024_25_PiT_summary.csv", row.names = FALSE)

# Compare final EoS vs prediction made after 27 rounds
rounds <- 27L
take_n <- as.integer(rounds * matches_per_round)
df27   <- utils::head(df_2425, take_n)

tab27 <- create.table(df27$HomeTeam, df27$AwayTeam, df27$FTHG, df27$FTAG)

frac27    <- pythag_frac(tab27$GF, tab27$GA, coef_pool["b"], coef_pool["c"], coef_pool["d"])
predTot27 <- tab27$PTS + coef_pool["a"] * frac27 * (nGames - tab27$PLD)  # nGames = 34 for BL

pred27 <- data.frame(Team = rownames(tab27),
                     PredTot = round(as.numeric(predTot27), 1),
                     PythagFrac = round(frac27, 3),
                     row.names = NULL)

aligned27 <- merge(eos_2425[, c("Team","PTS")], pred27[, c("Team","PredTot")],
                   by = "Team", sort = FALSE)

r27   <- cor(aligned27$PTS, aligned27$PredTot)
mae27 <- mean(abs(aligned27$PTS - aligned27$PredTot))
cat(sprintf("Round 27 comparison -> r = %.3f, MAE = %.2f\n", r27, mae27))

# Plot: Actual final vs Predicted from round 27
library(ggplot2)
p27 <- ggplot(aligned27, aes(x = PTS, y = PredTot)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Actual end-of-season points (2024–25)",
       y = "Predicted EoS points (after 27 rounds)",
       title = sprintf("Bundesliga 2024–25: prediction after 27 rounds (r = %.3f, MAE = %.2f)", r27, mae27))
print(p27)
ggsave("BL_2024_25_PiT_round_27.png", p27, width = 7, height = 5, dpi = 150)

# Biggest over/under vs final at round 27
library(dplyr)
diff27 <- aligned27 %>%
  mutate(err = PredTot - PTS, abs_err = abs(err),
         flag = ifelse(err > 0, "Over (pred>actual)", "Under (pred<actual)")) %>%
  arrange(desc(abs_err))
head(diff27, 5)  # top 5 absolute gaps

write.csv(diff27, "BL_2024_25_team_diffs_round27.csv", row.names = FALSE)

