# Gonna try to find a way to scrape which players were drafted
library(tidyverse)
library(cfbscrapR)
library(rvest)

# First things first (rest in peace uncle phil) let's read in the HTML table
# for the 2020 NFL draft from Wikipedia
draft_html <- read_html(url("https://en.wikipedia.org/wiki/2020_NFL_Draft#Player_selections"))

draft_table <- draft_html %>% html_table(fill = TRUE)
str(draft_table)

draft20 <- draft_table[[5]]
draft20[, 1] <- NULL
draft20$Notes <- NULL

draft20 <- draft20 %>%
  filter(!str_detect(Player, "forfeited"))

cfb_team_roster_data("Wisconsin")

find_departures <- function(team)
{
  roster <- cfb_team_roster_data(team)
  roster <- roster %>%
    mutate(name = paste(first_name, last_name, sep = " "))
  draftees <- draft20 %>%
    filter(College == team)
  
}

# Test to see if the scraper works, using Wisconsin as an example

wis_roster <- cfb_team_roster_data("Wisconsin")
wis_roster <- wis_roster %>%
  mutate(Player = paste(first_name, last_name, sep = " "))

wis_draftees <- draft20 %>%
  filter(College == "Wisconsin") %>%
  select(Player) %>%
  mutate(drafted = 1)

wis_roster <- wis_roster %>%
  left_join(wis_draftees, keep = TRUE)

wis_roster <- wis_roster %>%
  mutate(drafted = ifelse(is.na(drafted), 0, 1))



