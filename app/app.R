# What would BoardGameGeek think of your game?
# Predicts a game's average rating from the lasso model in R/model.R and shows
# what drives the prediction. Uses only base R and shiny, so it loads quickly
# in the browser with shinylive (no extra packages to download).

library(shiny)

coefs   <- read.csv("coefficients.csv")
labels  <- read.csv("feature_labels.csv")
typical <- read.csv("typical_game.csv")
games   <- read.csv("games.csv", check.names = FALSE)

beta <- setNames(coefs$coefficient, coefs$feature)
typical_x <- setNames(typical$value, typical$feature)
mech <- labels[labels$type == "Mechanic", ]
cats <- labels[labels$type == "Category", ]
all_ratings <- games$average

ink <- "#1D1B4C"; gold <- "#F2A900"; grey <- "#9A99B8"

pretty_name <- c(year_c = "Year published", year_c2 = "Year published", log2_time = "Playing time",
                 min_age = "Minimum age", min_players = "Minimum players",
                 max_players = "Maximum players", n_mechanics = "Number of mechanics",
                 setNames(labels$term, labels$column))

build_features <- function(input) {
  x <- setNames(numeric(length(typical_x)), names(typical_x))
  yc <- (input$year - 2010) / 10
  x["year_c"] <- yc
  x["year_c2"] <- yc^2
  x["log2_time"] <- log2(input$time)
  x["min_age"] <- input$age
  x["min_players"] <- input$players[1]
  x["max_players"] <- min(input$players[2], 10)
  x["n_mechanics"] <- input$n_mech
  x[input$mechanics] <- 1
  x[input$categories] <- 1
  x
}

css <- "
body { background:#F6F5FB; color:#1D1B4C; font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; }
h1 { font-weight:800; font-size:2rem; margin:24px 0 4px; letter-spacing:-.01em; }
.lede { color:#5B5A7E; margin-bottom:20px; max-width:46em; }
.well { background:#fff; border:1px solid #DCDAEC; box-shadow:none; border-radius:12px; }
.score { background:#1D1B4C; color:#fff; border-radius:14px; padding:20px 24px; margin-bottom:16px; }
.score .big { font-size:3.2rem; font-weight:800; line-height:1; }
.score .big span { color:#F2A900; }
.score .sub { color:#B9B7E0; margin-top:6px; }
.panel { background:#fff; border:1px solid #DCDAEC; border-radius:12px; padding:14px 16px; margin-bottom:16px; }
.panel h4 { font-weight:700; margin-top:0; }
.checkbox-inline, .checkbox { font-size:.9rem; margin-top:2px; margin-bottom:2px; }
.shiny-options-group { columns:2; column-gap:12px; }
.irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background:#1D1B4C; border-color:#1D1B4C; }
table { font-size:.9rem; }
.note { color:#5B5A7E; font-size:.82rem; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  h1("What would BoardGameGeek think of your game?"),
  p(class = "lede", "Describe a board game and see the average rating it would be expected to get, what pushes it up or down, and real games like it. Built on 15,000 BoardGameGeek games."),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("year", "Year published", min = 1970, max = 2021, value = 2019, sep = ""),
      sliderInput("time", "Playing time (minutes)", min = 10, max = 360, value = 60, step = 5),
      sliderInput("age", "Minimum age", min = 3, max = 18, value = 10),
      sliderInput("players", "Player count", min = 1, max = 10, value = c(2, 4)),
      checkboxGroupInput("mechanics", "Mechanics", choices = setNames(mech$column, mech$term),
                         selected = c("mech_hand_management", "mech_set_collection")),
      sliderInput("n_mech", "Total number of mechanics (including others)", min = 1, max = 15, value = 2),
      checkboxGroupInput("categories", "Themes", choices = setNames(cats$column, cats$term),
                         selected = "cat_card_game")
    ),
    mainPanel(
      width = 8,
      uiOutput("score"),
      div(class = "panel", h4("What's driving the prediction"),
          p(class = "note", "Numeric settings are compared with a typical game; mechanics and themes show their effect when ticked. Gold pushes the rating up, grey pulls it down."),
          plotOutput("drivers", height = "300px")),
      div(class = "panel", h4("Real games like yours"), tableOutput("similar")),
      p(class = "note", "Predictions come from a lasso regression (test RMSE 0.61, R² 0.47). They describe what BoardGameGeek's hobbyist raters tend to like, not what would cause a better game. Data: TidyTuesday BoardGameGeek, January 2022.")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$mechanics, {
    k <- max(1, length(input$mechanics))
    if (input$n_mech < k) updateSliderInput(session, "n_mech", value = k)
  }, ignoreNULL = FALSE)

  x <- reactive(build_features(input))
  pred <- reactive(unname(beta["(Intercept)"] + sum(beta[names(x())] * x())))

  output$score <- renderUI({
    p <- pred()
    pct <- round(100 * mean(all_ratings < p))
    div(class = "score",
        div(class = "big", sprintf("%.1f", p), span(" / 10")),
        div(class = "sub", sprintf("Expected average rating. Higher than %d%% of well-known games; most games land within about ±0.6 of this.", pct)))
  })

  output$drivers <- renderPlot({
    # Numeric features are compared with a typical game; mechanics and themes
    # show their effect when selected.
    is_tag <- names(x()) %in% labels$column
    baseline <- ifelse(is_tag, 0, typical_x[names(x())])
    contrib <- beta[names(x())] * (x() - baseline)
    df <- data.frame(feature = pretty_name[names(contrib)], value = contrib)
    df <- aggregate(value ~ feature, df, sum)          # combine the two year terms
    df <- df[abs(df$value) > 0.005, ]
    df <- head(df[order(-abs(df$value)), ], 8)
    if (nrow(df) == 0) return(NULL)
    df <- df[nrow(df):1, ]
    par(mar = c(4.2, 12, 0.5, 1), family = "sans", col.axis = ink, col.lab = ink, fg = ink)
    lim <- max(0.3, max(abs(df$value))) * c(-1.05, 1.05)
    bp <- barplot(df$value, horiz = TRUE, names.arg = df$feature, las = 1, border = NA,
                  col = ifelse(df$value > 0, gold, grey), xlim = lim, cex.names = 1.05,
                  xlab = "Effect on predicted rating (points)", axes = FALSE)
    abline(v = pretty(lim), col = "#E8E6F2")
    barplot(df$value, horiz = TRUE, add = TRUE, border = NA, axes = FALSE, names.arg = rep("", nrow(df)),
            col = ifelse(df$value > 0, gold, grey))
    axis(1, at = pretty(lim), col = NA, col.ticks = NA)
    abline(v = 0, col = ink, lwd = 1.5)
  })

  output$similar <- renderTable({
    feats <- c(input$mechanics, input$categories)
    all_feats <- c(mech$column, cats$column)
    sel <- as.matrix(games[, all_feats]) %*% as.numeric(all_feats %in% feats)
    n_tags <- rowSums(games[, all_feats])
    jaccard <- sel / pmax(n_tags + length(feats) - sel, 1)
    time_gap <- abs(log2(games$playing_time) - log2(input$time))
    score <- jaccard - 0.15 * time_gap - 0.02 * abs(games$year - input$year) / 10
    top <- games[order(-score), ][1:6, ]
    data.frame(Game = top$name, Year = as.integer(top$year),
               Minutes = as.integer(top$playing_time),
               Players = paste0(top$min_players, "-", top$max_players),
               `BGG rating` = sprintf("%.1f", top$average), check.names = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%")
}

shinyApp(ui, server)