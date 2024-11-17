# Problem 2 - plotly
# a. Regenerate the plot using plotly 

library(ggplot2)
library(tidyverse)
library(plotly)

# This code is from the problem set 4 solutions of STATS 506 Lecture Note 

# Construct variables for the plot for each genre 
art <- read.csv("df_for_ml_improved_new_market.csv")
unique(art[, grep("^Genre", names(art))])
art$Genre___Others[art$Genre___Painting == 1] <- 0
unique(art[, grep("^Genre", names(art))])
art$genre <- "Photography"
art$genre[art$Genre___Print == 1] <- "Print"
art$genre[art$Genre___Sculpture == 1] <- "Sculpture"
art$genre[art$Genre___Painting == 1] <- "Painting"
art$genre[art$Genre___Others == 1] <- "Other"

# set the proportion of each genre in each year
yeargenre <- with(art, table(year, genre))
ygperc <- yeargenre/apply(yeargenre, 1, sum)
ygperc <- ygperc[, c("Painting", "Sculpture", "Photography", "Print", "Other")]


# Set x, y position for each genre 
xpaint <- ygperc[16,1]/2
xsculpt <- ygperc[16,2]/2 + ygperc[16,1]
xphoto <- ygperc[16,3]/2 + sum(ygperc[16,1:2])
xprint <- ygperc[16, 4]/2 + sum(ygperc[16, 1:3])

# regenerate the plot for the distribution of genre of sales 
ygpercm <- as.data.frame(ygperc)
ygpercm$year <- as.factor(ygpercm$year)
years <- unique(ygpercm$year)

p <- plot_ly(data = ygpercm, x = ~Freq, y = ~year, type = 'bar', name = ~genre,
             color = ~genre, orientation = 'h') |>
     layout(title = "Proportion of Genre of Art Sales", 
            xaxis = list(title = 'Proportion'), 
            yaxis = list(title = 'year',tickmode = 'array',tickvals = years),
            barmode = 'stack', showlegend = FALSE) |> 
     add_annotations(y = 2012, x = xpaint, text = "Paint", showarrow = FALSE,
                    font = list(color = 'white', size = 12)) |> 
     add_annotations(y = 2012, x = xsculpt, text = "Sculpt", showarrow = FALSE,
                    font = list(color = 'white', size = 12)) |> 
     add_annotations(y = 2012, x = xphoto, text = "Photograph", showarrow = FALSE,
                    font = list(color = 'white', size = 12))  |> 
     add_annotations(y = 2012, x = xprint, text = "Print", showarrow = FALSE,
                    font = list(color = 'white', size = 12)) |>
     add_segments(y = 2012, yend = 2012, x = 1, xend = 1,
                  line = list(color = 'white', width = 1)) |>
     add_annotations(y = 2010.4, x = 1.02, text = "Other", showarrow = FALSE,
                     textangle = 90, hjust = 0) |>
     add_annotations(x = 1, y = 2012.2, xref = "x", yref = "y", 
                  ax = 1.02, ay = 2011, axref = "x", ayref = "y",
                  arrowhead = 2, arrowsize = 1, arrowwidth = 2, 
                  showarrow = TRUE, text = "") 
p

# b. Generate an interactive plot with plotly 

# This code is from the problem set 4 solutions of STATS 506 Lecture Note 

#' @title Subset a vector to values above some percentile
#' @param vec A vector of values
#' @param percentile A percentile to identify
select_top_values <- function(vec, percentile) {
  val <- quantile(vec, percentile)
  return(vec[vec > val])
}

# save the top values for each year
save <- list()
for (y in unique(art$year)) {
  prices <- art[art$year == y, "price_usd"]
  save[[as.character(y)]] <-
    data.frame(year = y,
               price_usd = select_top_values(prices, .95))
}
arttop <- do.call(rbind, save)

# save the median values for each year
artmedian <- aggregate(art$price_usd, by = list(art$year),
                       FUN = median, na.rm = TRUE)
names(artmedian) <- c("year", "price_usd")

# save the median values for each year and genre
artmedian2 <- aggregate(art$price_usd, by = list(art$year, art$genre),
                        FUN = median, na.rm = TRUE)
names(artmedian2) <- c("year", "genre", "price_usd")
genres <- rev(unique(artmedian2$genre))

# save the quantile values for each year abd genre
art975 <- aggregate(art$price_usd, by = list(art$year, art$genre),
                    FUN = quantile, .975, na.rm = TRUE)
names(art975) <- c("year", "genre", "price_usd")

# Set a default plotly plot 
pp <- plot_ly()

# Add a change in the sales price in USD over time
pp <- pp |> add_trace(data = arttop, x = ~factor(year), y = ~price_usd, 
                      type = "box", boxpoints = "outliers",
                      marker = list(color = 'black'), line = list(color = 'black'),
                      visible = TRUE) |>
            add_trace(data = artmedian, x = ~factor(year), y = ~price_usd, 
                      type = "scatter", mode = "lines",
                      marker = list(color = 'rgba(0,0,0,0)'),
                      line = list(color = 'red', dash = 'dot', width = 2), 
                      name = "Median", visible = TRUE)

# Add a change in sales price over time by genre 
for (i in seq_along(genres)) {
  genre_data <- artmedian2[artmedian2$genre == genres[i], ]
  pp <- pp |> add_trace(data = genre_data, x = ~factor(year), y = ~price_usd, 
                        type = 'scatter', mode = 'lines',
                        name = paste("Median", genres[i]),
                        line = list(width = 3),
                        visible = FALSE)
}
for (i in seq_along(genres)) {
  genre_data <- art975[art975$genre == genres[i], ]
  pp <- pp |> add_trace(data = genre_data, x = ~factor(year), y = ~price_usd,
                        type = 'scatter', mode = 'lines',
                        name = paste("97.5% Percentile", genres[i]),
                        line = list(width = 3, dash = 'dash'),
                        visible = FALSE)
}

# Add update buttons
pp <- pp |> 
      layout(updatemenus = list(
         list(type = "buttons", 
          buttons = list(
           list(method = "update", 
            args = list(
              list(visible = c(rep(TRUE, 2), rep(FALSE, length(genres) * 2))), 
              list(title = "Changes in Top 5% of Prices")),
            label = "Overall"),
           list(method = "update",
            args = list(
              list(visible = c(rep(FALSE, 2), rep(TRUE, length(genres) * 2))), 
              list(title = "Changes in Price by Genre")),
            label = "by Genre")
          ))),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Price in USD"),
        legend = list(x = 0.1, y = 0.9, bgcolor = 'rgba(255, 255, 255, 0)',
                      bordercolor = 'rgba(0, 0, 0, 0)'))
pp

