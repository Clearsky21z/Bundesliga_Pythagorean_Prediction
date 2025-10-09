rm(list = ls())
library(dplyr)
library(ggplot2)

data_dir   <- "."
file_regex <- "^(D1|DL)_.*\\.csv$"

csvs <- list.files(data_dir, pattern = file_regex, full.names = TRUE)
if (length(csvs) == 0) stop("No Bundesliga CSVs found. Check names/pattern.")
message("Files found: ", paste(basename(csvs), collapse = ", "))

## Build PiT/EoS tables
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

## Pythagorean functions
pythag_frac <- function(GF, GA, b, c, d) (GF^b) / ((GF^c) + (GA^d))
pythag_pts  <- function(PLD, GF, GA, a, b, c, d) a * pythag_frac(GF,GA,b,c,d) * PLD

# Pooled coefficient estimation over ALL seasons

read_season_eos <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  req <- c("HomeTeam","AwayTeam","FTHG","FTAG","FTR")
  stopifnot(all(req %in% names(df)))
  
  tab <- create.table(df$HomeTeam, df$AwayTeam, df$FTHG, df$FTAG)
  tab$Season <- basename(csv_path)
  tab$Team   <- rownames(tab)
  rownames(tab) <- NULL
  tab[, c("Season","Team","PLD","GF","GA","PTS")]
}

load_bundesliga_eos <- function(data_dir, pattern = file_regex) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matched. Check pattern/path.")
  dplyr::bind_rows(lapply(files, read_season_eos))
}

# Objective across pooled seasons (MAE)
obj_mae_pool <- function(par, pool_df) {
  a <- par[1]; b <- par[2]; c <- par[3]; d <- par[4]
  pred <- pythag_pts(pool_df$PLD, pool_df$GF, pool_df$GA, a,b,c,d)
  mean(abs(pool_df$PTS - pred))
}

pool <- load_bundesliga_eos(data_dir, pattern = file_regex)

start <- c(a=2.78, b=1.24, c=1.24, d=1.25)
fit_pool <- optim(par = start, fn = obj_mae_pool, pool_df = pool,
                  method = "Nelder-Mead",
                  control = list(maxit = 8000, reltol = 1e-12))
coef_pool <- setNames(fit_pool$par, c("a","b","c","d"))
cat(sprintf("Bundesliga pooled fit -> a=%.3f b=%.3f c=%.3f d=%.3f\n",
            coef_pool["a"],coef_pool["b"],coef_pool["c"],coef_pool["d"]))

pool$pred <- pythag_pts(pool$PLD, pool$GF, pool$GA,
                        coef_pool["a"],coef_pool["b"],coef_pool["c"],coef_pool["d"])
cat(sprintf("Overall (in-sample): MAE=%.2f | r=%.3f\n",
            mean(abs(pool$PTS - pool$pred)), cor(pool$PTS, pool$pred)))

by_season <- pool |>
  group_by(Season) |>
  summarise(MAE = mean(abs(PTS - pred)),
            r = cor(PTS, pred), .groups = "drop")
print(head(by_season))

# Leave-one-season-out CV (out-of-sample)
loso_cv <- function(pool_df, start_par = c(2.78,1.24,1.24,1.25)) {
  seasons <- unique(pool_df$Season)
  rows <- vector("list", length(seasons))
  for (i in seq_along(seasons)) {
    s <- seasons[i]
    train <- pool_df[pool_df$Season != s, , drop = FALSE]
    test  <- pool_df[pool_df$Season == s, , drop = FALSE]
    
    fit_s <- optim(par = start_par, fn = obj_mae_pool, pool_df = train,
                   method = "Nelder-Mead",
                   control = list(maxit = 4000, reltol = 1e-12))
    par_s <- fit_s$par
    pred  <- pythag_pts(test$PLD, test$GF, test$GA, par_s[1],par_s[2],par_s[3],par_s[4])
    rows[[i]] <- data.frame(Season = s, a = par_s[1], b = par_s[2], c = par_s[3], d = par_s[4],
                            MAE = mean(abs(test$PTS - pred)),
                            r   = cor(test$PTS, pred))
  }
  do.call(rbind, rows)
}

cv <- loso_cv(pool, start_par = start)
cat(sprintf("LOSO median: MAE=%.2f | r=%.3f\n", median(cv$MAE), median(cv$r)))
# View worst/best seasons:
# cv[order(-cv$MAE), ][1:3, ]
# cv[order(cv$MAE),  ][1:3, ]

cat("Seasons loaded:", length(unique(pool$Season)), "\n")


# --- Print and save
cat(sprintf("Bundesliga pooled coefficients (all seasons): a=%.4f  b=%.4f  c=%.4f  d=%.4f\n",
            coef_pool["a"], coef_pool["b"], coef_pool["c"], coef_pool["d"]))

pool$pred <- pythag_pts(pool$PLD, pool$GF, pool$GA,
                        coef_pool["a"], coef_pool["b"], coef_pool["c"], coef_pool["d"])
cat(sprintf("Overall in-sample: MAE=%.2f, r=%.3f\n",
            mean(abs(pool$PTS - pool$pred)), cor(pool$PTS, pool$pred)))

write.csv(data.frame(t(coef_pool)), "bundesliga_coefs_pooled.csv", row.names = FALSE)

