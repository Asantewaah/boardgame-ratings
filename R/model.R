# Feature engineering, model fitting and app export for the board game ratings project.

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(ranger)
})

# Numeric features used by every model. Year is centred on 2010 and in decades;
# playing time is on a log2 scale, so +1 means "twice as long".
make_features <- function(d) {
  d |>
    transmute(
      year_c = (year - 2010) / 10,
      year_c2 = year_c^2,
      log2_time = log2(playing_time),
      min_age, min_players,
      max_players = pmin(max_players, 10),
      n_mechanics,
      across(starts_with("mech_")),
      across(starts_with("cat_"))
    )
}

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
rsq  <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

split_data <- function(games, prop = 0.8, seed = 42) {
  set.seed(seed)
  idx <- sample(nrow(games), floor(prop * nrow(games)))
  list(train = games[idx, ], test = games[-idx, ])
}

fit_lasso <- function(train, seed = 42) {
  set.seed(seed)
  cv.glmnet(as.matrix(make_features(train)), train$average, alpha = 1, nfolds = 10)
}

fit_forest <- function(train, seed = 42) {
  ranger(x = make_features(train), y = train$average, num.trees = 500,
         importance = "permutation", seed = seed)
}

evaluate <- function(test, train, lasso, forest) {
  X <- as.matrix(make_features(test))
  preds <- list(
    "Baseline (average rating)" = rep(mean(train$average), nrow(test)),
    "Lasso regression" = predict(lasso, X, s = "lambda.min")[, 1],
    "Random forest" = predict(forest, make_features(test))$predictions
  )
  imap_dfr(preds, \(p, m) tibble(model = m, rmse = rmse(test$average, p), r_squared = rsq(test$average, p)))
}

# Partial dependence: average forest prediction when one feature is set to each grid value.
partial_dependence <- function(forest, data, feature, grid, n = 2000, seed = 1) {
  set.seed(seed)
  base <- make_features(slice_sample(data, n = min(n, nrow(data))))
  map_dfr(grid, \(v) {
    x <- base
    x[[feature]] <- v
    tibble(value = v, prediction = mean(predict(forest, x)$predictions))
  })
}

# Everything the Shiny app needs, as small plain CSV files (no model objects),
# so the app runs in the browser with only base R, shiny and ggplot2.
export_app_files <- function(lasso, games, labels, dir = "app") {
  co <- coef(lasso, s = "lambda.min")
  coefs <- tibble(feature = rownames(co), coefficient = as.numeric(co[, 1]))
  write_csv(coefs, file.path(dir, "coefficients.csv"))
  write_csv(labels, file.path(dir, "feature_labels.csv"))

  X <- make_features(games)
  typical <- tibble(feature = names(X), value = map_dbl(X, mean))
  write_csv(typical, file.path(dir, "typical_game.csv"))

  games |>
    mutate(predicted = predict(lasso, as.matrix(X), s = "lambda.min")[, 1]) |>
    filter(users_rated >= 200) |>   # well-known games for the "similar games" list
    select(name, year, min_players, max_players, playing_time, min_age, average,
           users_rated, predicted, starts_with("mech_"), starts_with("cat_")) |>
    write_csv(file.path(dir, "games.csv"))
}
