# Clean the BoardGameGeek data (TidyTuesday, 2022-01-25) into one modelling table.
# Run from the repository root: Rscript R/prepare_data.R

suppressPackageStartupMessages(library(tidyverse))

raw_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/"
read_raw <- function(name) {
  local <- file.path("data-raw", paste0(name, ".csv"))
  if (!file.exists(local)) {
    dir.create("data-raw", showWarnings = FALSE)
    download.file(paste0(raw_url, name, ".csv"), local, quiet = TRUE)
  }
  read_csv(local, show_col_types = FALSE)
}

ratings <- read_raw("ratings")
details <- read_raw("details")

# BGG stores lists as Python-style strings: "['Dice Rolling', 'Hand Management']"
parse_list <- function(x) {
  x |>
    str_remove_all("^\\[|\\]$") |>
    str_split("', |\", ") |>
    map(\(v) str_remove_all(v, "^['\"]|['\"]$")) |>
    map(\(v) v[v != "" & !is.na(v)])
}
clean_name <- function(x) x |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "_") |> str_remove_all("^_|_$")

games <- details |>
  select(id, name = primary, year = yearpublished, min_players = minplayers,
         max_players = maxplayers, playing_time = playingtime, min_age = minage,
         mechanics = boardgamemechanic, categories = boardgamecategory) |>
  inner_join(select(ratings, id, average, users_rated), by = "id") |>
  filter(
    users_rated >= 50,                     # enough ratings for a stable average
    between(year, 1970, 2021),
    between(playing_time, 5, 600),
    between(min_players, 1, 8), max_players >= min_players, max_players <= 20,
    between(min_age, 3, 18)
  ) |>
  mutate(mechanics = parse_list(mechanics), categories = parse_list(categories))

# One-hot encode the 25 most common mechanics and 20 most common categories
top_terms <- function(col, n) {
  games |> select(id, term = {{ col }}) |> unnest(term) |> count(term, sort = TRUE) |>
    slice_head(n = n) |> pull(term)
}
top_mech <- top_terms(mechanics, 25)
top_cat  <- top_terms(categories, 20)

to_dummies <- function(df, col, terms, prefix) {
  wide <- df |> select(id, term = {{ col }}) |> unnest(term) |> filter(term %in% terms) |>
    distinct() |> mutate(value = 1L, term = paste0(prefix, clean_name(term))) |>
    pivot_wider(names_from = term, values_from = value, values_fill = 0L)
  df |> left_join(wide, by = "id") |> mutate(across(starts_with(prefix), \(x) replace_na(x, 0L)))
}

games_wide <- games |>
  to_dummies(mechanics, top_mech, "mech_") |>
  to_dummies(categories, top_cat, "cat_") |>
  mutate(n_mechanics = lengths(mechanics)) |>
  select(-mechanics, -categories)

write_csv(games_wide, "data/games.csv")
write_csv(tibble(term = c(top_mech, top_cat),
                 column = c(paste0("mech_", clean_name(top_mech)), paste0("cat_", clean_name(top_cat))),
                 type = rep(c("Mechanic", "Category"), c(length(top_mech), length(top_cat)))),
          "data/feature_labels.csv")
message(nrow(games_wide), " games written to data/games.csv")
