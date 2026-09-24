# What makes a board game highly rated?

**An R analysis of 15,249 BoardGameGeek games, with an interactive app that predicts how a new game would be rated.**

[![Live app](https://img.shields.io/badge/Live_app-Try_it_in_your_browser-F2A900?style=for-the-badge&logo=r&logoColor=1D1B4C)](https://asantewaah.github.io/boardgame-ratings/)

<a href="https://asantewaah.github.io/boardgame-ratings/"><img src="assets/app-demo.gif" alt="The app: moving the year slider back lowers the predicted rating; adding longer play time and miniatures raises it; adding roll-and-move lowers it" width="760"></a>

Describe a game, and the app predicts its BoardGameGeek rating, shows what's pushing it up or down, and lists real games like it. It runs entirely in your browser: R compiled to WebAssembly via [shinylive](https://posit-dev.github.io/r-shinylive/), with no server.

## Key findings

- **Release year is the strongest predictor.** Games from the 1990s average about 6.1, while games from 2020 average 7.3. Recent games are mostly rated by the enthusiasts who sought them out, so raw ratings flatter new releases.
- **Longer, more demanding games rate higher**, with ratings rising steadily with playing time and recommended age.
- **Raw differences can mislead.** Worker placement, deck building and variable player powers each look about half a point better on raw averages, but almost none of that survives adjusting for year and length. Roll-and-move stays clearly negative (about −0.5), while miniatures (+0.3) and wargames (+0.2) stay positive.
- **Ratings are partly predictable before release.** A random forest explains about half the variation in ratings on held-out games.

![Raw versus adjusted effect of each mechanic and theme](analysis/figures/mechanics-1.png)

## Models

Trained on 80% of games and evaluated on the remaining 3,050.

| Model | RMSE | R² |
|---|---|---|
| Baseline (always predict the average) | 0.843 | 0.00 |
| Lasso regression | 0.613 | 0.47 |
| **Random forest** | **0.574** | **0.54** |

The lasso is slightly less accurate but fully interpretable, so it powers the app. Its coefficients are exported to small CSV files, which keeps the app light enough to run in a browser using only base R and shiny.

## Read the full analysis

[`analysis/boardgame_ratings.md`](analysis/boardgame_ratings.md) walks through the data, the year effect, playing time and age, raw versus adjusted effects of mechanics and themes, model comparison, permutation importance and partial dependence.

## Limitations

These are associations, not causal effects: adding miniatures to a game will not by itself raise its rating. BoardGameGeek raters are a self-selected hobbyist community. Only the 25 most common mechanics and 20 most common themes are modelled, and game complexity ("weight") is not in this dataset.

## Repository structure

```
├── R/prepare_data.R                 downloads and cleans the raw data → data/games.csv
├── R/model.R                        features, lasso, random forest, evaluation, app export
├── analysis/boardgame_ratings.Rmd   full analysis (rendered to .md with figures)
├── app/app.R                        the Shiny app and its small data files
├── docs/                            the app exported with shinylive (served by GitHub Pages)
├── data/                            cleaned modelling data
└── assets/app-demo.gif
```

## Run it yourself

```r
install.packages(c("tidyverse", "glmnet", "ranger", "patchwork", "rmarkdown", "shiny", "shinylive"))

source("R/prepare_data.R")                               # build data/games.csv
rmarkdown::render("analysis/boardgame_ratings.Rmd")      # analysis + app files
shiny::runApp("app")                                     # run the app locally
shinylive::export("app", "docs")                         # rebuild the browser version
```

## Data

BoardGameGeek data via [TidyTuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-25) (25 January 2022), originally compiled from BoardGameGeek and shared on Kaggle.

---

*By [Juliet Asantewaa Sarpong](https://asantewaah.github.io), data scientist and PhD researcher in Statistics at the University of Edinburgh. Also a keen board gamer.*
