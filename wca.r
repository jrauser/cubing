library(tidyverse)

results <- read_tsv("wca/WCA_export_Results.tsv")
head(results)

competitions <- read_tsv("wca/WCA_export_Competitions.tsv", quote="") %>%
  mutate(date=as.Date(paste(year, month, day, sep = "-")),
         month=as.Date(paste(year, month, "01", sep = "-"))) 
head(competitions)

comp_best_ao5 <- results %>%
  filter(eventId == "333", average > 0) %>%
  group_by(competitionId, personId) %>%
  summarize(best_average = min(average))


comp_quantiles <- comp_best_ao5 %>%
  group_by(competitionId) %>%
  summarize(ao5_p0 = quantile(best_average, 0.0),
            ao5_p10 = quantile(best_average, 0.1),
            ao5_p50 = quantile(best_average, 0.5),
            ao5_p90 = quantile(best_average, 0.9)) %>%
  inner_join(competitions %>% select(id, date, month), by=c("competitionId"="id"))

comp_quantiles %>%
  group_by(month) %>%
  summarize(min_ao5 = min(ao5_p0/100)) %>%
  ggplot(aes(month, min_ao5)) +
  geom_point() + geom_smooth()


comp_quantiles %>%
  group_by(month) %>%
  summarize(median_p10_ao5 = median(ao5_p10/100)) %>%
  ggplot(aes(month, median_p10_ao5)) +
  geom_point() + geom_smooth()

comp_quantiles %>%
  group_by(month) %>%
  summarize(median_p50_ao5 = median(ao5_p50/100)) %>%
  ggplot(aes(month, median_p50_ao5)) +
  geom_point() + geom_smooth()

comp_quantiles %>%
  group_by(month) %>%
  summarize(median_p90_ao5 = median(ao5_p90/100)) %>%
  ggplot(aes(month, median_p90_ao5)) +
  geom_point() + geom_smooth()
