
library(data.table)
library(tidyverse)
library(nycflights13)

flights %>%
  group_by(origin) %>%
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            med_delay = median(dep_delay, na.rm = TRUE),
            numflights = n()) %>%
  ungroup() %>%
  filter(numflights >= 10) %>%
  rename(faa = origin) %>%
  left_join(airports, by = "faa") %>%
  select(name, mean_delay, med_delay) %>%
  arrange(desc(mean_delay))

flights %>%
  group_by(dest) %>%
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE),
            med_delay = median(arr_delay, na.rm = TRUE),
            numflights = n()) %>%
  ungroup() %>%
  filter(numflights >= 10) %>%
  rename(faa = dest) %>%
  left_join(airports, by = "faa") %>%
  mutate(name = coalesce(name, faa)) %>%
  select(name, mean_delay, med_delay) %>%
  arrange(desc(mean_delay)) %>%
  print(n = count(.))

flights %>%
  left_join(planes, by = "tailnum") %>%
  mutate(time = air_time/60,
         mph = distance/time) %>%
  group_by(model) %>%
  summarize(avgmph = mean(mph, na.rm = TRUE),
            nflights = n()) %>%
  arrange(desc(avgmph)) %>%
  slice(1)

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
departure_delay_dt

# Generate table for mean and median arrival delay per destination airport 
unique(flights$dest)
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

arrival_delay_dt

# Determine aircraft model with the fastest average speed
fastest_model_dt <- flights[ , time := air_time / 60
                           ][ , mph := distance / time
                           ][planes, on = .(tailnum)
                           ][ , .(avgmph = mean(mph, na.rm = TRUE), 
                                  nflights = .N, model), by = model
                           ][order(-avgmph), .(model, avgmph, nflights)
                           ][1]
fastest_model_dt



