# Problem 2 
# a. Regenerate the plot with plotly 

library(ggplot2)
library(tidyverse)
library(plotly)

# This plot is from the problem set 4 solutions of STATS 506 Lecture 
# Construct variables for the plot 

art <- read.csv("df_for_ml_improved_new_market.csv")
unique(art[, grep("^Genre", names(art))])
art$Genre___Others[art$Genre___Painting == 1] <- 0
unique(art[, grep("^Genre", names(art))])

art$genre <- "Photography"
art$genre[art$Genre___Print == 1] <- "Print"
art$genre[art$Genre___Sculpture == 1] <- "Sculpture"
art$genre[art$Genre___Painting == 1] <- "Painting"
art$genre[art$Genre___Others == 1] <- "Other"
table(art$genre)

#' @title Subset a vector to values above some percentile
#' @param vec A vector of values
#' @param percentile A percentile to identify
select_top_values <- function(vec, percentile) {
  val <- quantile(vec, percentile)
  return(vec[vec > val])
}

save <- list()
for (y in unique(art$year)) {
  prices <- art[art$year == y, "price_usd"]
  save[[as.character(y)]] <-
    data.frame(year = y,
               price_usd = select_top_values(prices, .95))
}

arttop <- do.call(rbind, save)
artmedian <- aggregate(art$price_usd, by = list(art$year),
                       FUN = median, na.rm = TRUE)
names(artmedian) <- c("year", "price_usd")
yeargenre <- with(art, table(year, genre))
ygperc <- yeargenre/apply(yeargenre, 1, sum)
ygperc <- ygperc[, c("Painting", "Sculpture", "Photography", "Print", "Other")]
ygpercm <- as.data.frame(ygperc)
ygpercm$genre <- factor(ygpercm$genre, levels = rev(unique(ygpercm$genre)))

# generate the plot

oldpar <- par(no.readonly=TRUE)
g <- ggplot(ygpercm, aes(y = Freq, x = year, fill = genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = NULL, x = NULL, title = "Proportion of Genre of Art Sales") +
  theme(legend.position = "off") +
  geom_text(data = ygpercm[ygpercm$year == 2012 & ygpercm$genre != "Other", ],
            aes(label = genre),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  # Add the Other label
  geom_segment(aes(xend = 16, yend = 1, x = 15, y = 1.02),
               arrow = arrow(length = unit(0.15, "inches")),
               linewidth = .5, color = "black") +
  annotate("text", x = 14.9, y = 1.02, label = "Other", hjust = 0, angle = 270)

# regenerate the plot 
ggplotly(g)


# b. Generate an interactive plot with plotly 

# A change in the sales price in USD over time
# Generate a plotly plot

p1 <- plot_ly(data = arttop, x = ~factor(year), y = ~price_usd, 
              type = "box", boxpoints = "outliers") |>
      add_trace(data = artmedian, x = ~year, y = ~price_usd, 
                type = "scatter", mode = "lines",
                line = list(dash = 'dot', width = 2), name = "Median") |>
      layout(title = "Changes in top 5% of prices",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Price in Million USD",
                          tickvals = seq(0, 1400000, by = 200000),
                          ticktext = paste(seq(0, 1.4, .2), "M", sep = "")),
            legend = list(x = 0.1, y = 0.9, bgcolor = 'rgba(255, 255, 255, 0)',   
                          bordercolor = 'rgba(0, 0, 0, 0)'))
p1

# A change in sales price over time by genre

# this code is from Problem 4 solutions of STATS 506 Lecture Note

artmedian2 <- aggregate(art$price_usd, by = list(art$year, art$genre),
                        FUN = median, na.rm = TRUE)
names(artmedian2) <- c("year", "genre", "price_usd")
art975 <- aggregate(art$price_usd, by = list(art$year, art$genre),
                    FUN = quantile, .975, na.rm = TRUE)
names(art975) <- c("year", "genre", "price_usd")
genres <- rev(unique(artmedian2$genre))


# Generate a plotly plot

p2 <- plot_ly() 

for (i in seq_along(genres)) {
  genre_data <- artmedian2[artmedian2$genre == genres[i], ]
  p2 <- p2 |> add_trace(data = genre_data, x = ~year, y = ~price_usd, 
                        type = 'scatter', mode = 'lines',
                        name = paste("Median", genres[i]),
                        line = list(color = i, width = 3))
}

# Plot 97.5% Percentile lines
for (i in seq_along(genres)) {
  genre_data <- art975[art975$genre == genres[i], ]
  p2 <- p2 |> add_trace(data = genre_data, x = ~year, y = ~price_usd,
                        type = 'scatter', mode = 'lines',
                        name = paste("97.5% Percentile", genres[i]),
                        line = list(color = i, width = 3, dash = 'dash'))
  }

# Customize layout
p2 <- p2 |> layout(title = "Changes in Price by Genre",
                   xaxis = list(title = "", 
                                tickvals = seq(1997, 2012),
                                ticktext = seq(1997, 2012)),
                   yaxis = list(title = "Price in Thousands USD",
                                tickvals = seq(0, 350000, by = 50000),
                                ticktext = paste(seq(0, 350, by = 50), "k", 
                                                 sep = "")),
                   legend = list(x = 0.01, y = 0.99, traceorder = "normal"))

p2

pp <- p1

for (i in seq_along(genres)) {
  genre_data <- artmedian2[artmedian2$genre == genres[i], ]
  pp <- pp |> add_trace(data = genre_data, x = ~year, y = ~price_usd, 
                        type = 'scatter', mode = 'lines',
                        name = paste("Median", genres[i]),
                        line = list(color = i, width = 3),
                        visible = FALSE)
}

# Plot 97.5% Percentile lines
for (i in seq_along(genres)) {
  genre_data <- art975[art975$genre == genres[i], ]
  pp <- pp |> add_trace(data = genre_data, x = ~year, y = ~price_usd,
                        type = 'scatter', mode = 'lines',
                        name = paste("97.5% Percentile", genres[i]),
                        line = list(color = i, width = 3, dash = 'dash'),
                        visible = FALSE)
}
pp
# Customize layout
pp <- pp |> layout(title = "Changes in Price by Genre",
                   xaxis = list(title = "", 
                                tickvals = seq(1997, 2012),
                                ticktext = seq(1997, 2012)),
                   yaxis = list(title = "Price in Thousands USD",
                                tickvals = seq(0, 350000, by = 50000),
                                ticktext = paste(seq(0, 350, by = 50), "k", 
                                                 sep = "")),
                   legend = list(x = 0.01, y = 0.99, traceorder = "normal"))

pp |> layout(updatemenus = list(
  list(
    y = 1,
    buttons = list(
      list(method = "update",
           args = list(list(visible =  list(TRUE, FALSE)),
                       list(yaxis = list(title = "Overall"))),
           label = "Overall"),
      
      list(method = "update",
           args = list(list(visible =  list(FALSE, TRUE)),
                       list(yaxis = list(title = "by Genre"))),
           label = "by Genre"))
  )))

