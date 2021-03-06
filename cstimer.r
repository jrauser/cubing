library(jsonlite)
library(tidyverse)
library(slider)

dir <- "~/Downloads/"
latest <- list.files(dir, pattern="^cstimer") %>% sort() %>% tail(1)
json <- fromJSON(file.path(dir, latest))

properties <- json$properties
json$properties <- NULL
session_data <- fromJSON(properties$sessionData, simplifyDataFrame = T)


aon <- function(t, n) {
  stopifnot(length(t)==n)
  t<-sort(t)
  end<-n-1
  return(mean(t[2:end]))
}
aon(c(20,35,40,10,100), 5) == mean(c(20,35,40))

solve_tbl <- function(s) {
  tibble(penalty = s[[1]][1],
         time = s[[1]][2],
         scramble = s[[2]],
         comment = s[[3]],
         timestamp = s[[4]]) 
}

do_session <- function(x) {
  solves_df <- map_dfr(x, solve_tbl) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01T00:00'),
           time = ifelse(penalty < 0, NA, (time+penalty)/1000)) %>%
    mutate(ao5 = slide_dbl(time, aon, n=5, .before=4, .complete=T),
           ao12 = slide_dbl(time, aon, n=12, .before=11, .complete=T))
  
  return(solves_df)
}




dat <- tibble(sessionid=names(json), solves_json=json, nsolves=map_int(solves_json, length)) %>%
  filter(nsolves>0) %>%
  mutate(solves_tbl = map(solves_json, do_session)) %>%
  select(-solves_json, -nsolves) %>%
  unnest(solves_tbl) %>%
  mutate(ao100 = slide_dbl(time, aon, n=100, .before=99, .complete=T))
       



dat %>%
  filter(timestamp > "2022-01-01") %>%
  ggplot(aes(timestamp, time)) +
  geom_point() +
  geom_smooth(method="loess") + 
  ggtitle("All solves")

dat %>%
  filter(timestamp > "2022-01-01", !is.na(ao5)) %>%
  ggplot(aes(timestamp, ao5)) +
  geom_point() +
  geom_smooth(method="loess") + 
  ggtitle("All ao5s")

dat %>%
  filter(timestamp > "2022-01-01", !is.na(ao12)) %>%
  ggplot(aes(timestamp, ao12)) +
  geom_point() +
  geom_smooth(method="loess") + 
  ggtitle("All ao12s")

dat %>%
  filter(timestamp > "2022-03-15", !is.na(ao100)) %>%
  ggplot(aes(timestamp, ao100)) +
  geom_point() +
  geom_smooth(method="loess") + 
  ggtitle("All ao100s")

# session best stats

dat %>%
  filter(timestamp > "2022-01-01", !is.na(ao5)) %>%
  group_by(sessionid) %>%
  mutate(rank = rank(ao5)) %>%
  filter(rank==1) %>%
  ggplot(aes(timestamp, ao5)) +
  geom_point() +
  geom_smooth(method="loess") +
  ggtitle("Session best ao5")

dat %>%
  filter(timestamp > "2022-01-01", !is.na(ao12)) %>%
  group_by(sessionid) %>%
  mutate(rank = rank(ao12)) %>%
  filter(rank==1) %>%
  ggplot(aes(timestamp, ao12)) +
  geom_point() +
  geom_smooth(method="loess") +
  ggtitle("Session best ao12")

