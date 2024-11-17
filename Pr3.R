# Problem 3 - data.table

library(data.table)
library(tidyverse)
library(nycflights13)

# Generate table for mean and median departure delay per origin airport
flights <- data.table(flights)
airports <- data.table(airports)
planes <- data.table(planes)

departure_delay_dt <- flights[, .(mean_delay = mean(dep_delay, na.rm = TRUE),
                                  med_delay = median(dep_delay, na.rm = TRUE),
                                  num_flights = .N,
                                  origin = origin),
                              by = origin
                             ][num_flights >= 10
                             ][airports, on = .(origin = faa), nomatch = NULL
                             ][, .(name, mean_delay, med_delay)
                             ][order(-mean_delay)]
print(departure_delay_dt, nrows = Inf)

# Generate table for mean and median arrival delay per destination airport 
arrival_delay_dt <- flights[, .(mean_delay = mean(arr_delay, na.rm = TRUE),
                                med_delay = median(arr_delay, na.rm = TRUE),
                                num_flights = .N,
                                dest = dest), 
                            by = dest
                           ][num_flights >= 10
                           ][ , faa := dest
                           ][ , -c("dest")]
arrival_delay_dt <- merge(arrival_delay_dt, airports, by = "faa", all.x = TRUE)
arrival_delay_dt <- arrival_delay_dt[, .(name, mean_delay, med_delay)
                                    ][order(-mean_delay)]
print(arrival_delay_dt, nrows = Inf)

# Determine aircraft model with the fastest average speed
fastest_model_dt <- flights[ , time := air_time / 60
                           ][ , mph := distance / time
                           ][planes, on = .(tailnum)
                           ][ , .(avgmph = mean(mph, na.rm = TRUE), 
                                  nflights = .N, model), by = model
                           ][order(-avgmph), .(model, avgmph, nflights)
                           ][1]
fastest_model_dt



