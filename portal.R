# Try to scrape the transfer portal from 24/7
library(jsonlite)
library(rvest)
library(tidyverse)
library(cfbscrapR)
library(XML)



url <- "https://247sports.com/Season/2020-Football/TransferPortal/"

portal <- read_html(url)

# IMPORTANT
# The name will always be in the form 
# "//*[@id="page-content"]/div/section/section/div/ul/li[ROW NUMBER]/div[2]/a"
# Where ROW NUMBER is between 6 and the lowest value. 

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[2]/div[2]/a") %>%
  html_text()

name_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
name_xpath2 <- "]/div[2]/a"

# Scrape the player names from the site ####################
names <- c()
for(i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(name_xpath1, i, name_xpath2)) %>%
    html_text() -> player
  print(player)
  names <- c(names, player)
}


# Scrape the height / weights ###################
meas_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
meas_xpath2 <- "]/div[2]/div[1]"

measurements <- c()
for(i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(meas_xpath1, i, meas_xpath2)) %>%
    html_text() -> height
  print(height)
  measurements <- c(measurements, height)
}

# Scrape HS and transfer rating #####################
portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[1251]/div[2]/div[2]/span/span[6]/span") %>%
  html_text()

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[6]/div[2]/div[2]/span[2]/span[6]") %>%
  html_text()

trans_rating_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
trans_rating_xpath2 <- "]/div[2]/div[2]/span[1]/span[6]"

hs_rating_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
hs_rating_xpath2 <- "]/div[2]/div[2]/span[2]/span[6]"


trans_ratings <- c()
hs_ratings <- c()
for(i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(trans_rating_xpath1, i, trans_rating_xpath2)) %>%
    html_text() -> rating
  if(is.na(rating))
  {
    trans_ratings <- c(trans_ratings, "NA")
    hs_ratings <- c(hs_ratings, "NA")
  }
  else
  {
    if(str_detect(rating, "(T)"))
    {
      trans_ratings <- c(trans_ratings, rating)
      portal %>%
        html_node(xpath = paste0(hs_rating_xpath1, i, hs_rating_xpath2)) %>%
        html_text() -> rating
      hs_ratings <- c(hs_ratings, rating)
    }
    else
    {
      trans_ratings <- c(trans_ratings, "NA")
      hs_ratings <- c(hs_ratings, rating)
    }
  }
}

# Scrape Positions ################
portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[6]/div[3]") %>%
  html_text()

pos_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
pos_xpath2 <- "]/div[3]"

positions <- c()

for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(pos_xpath1, i, pos_xpath2)) %>%
    html_text() -> pos
  print(pos)
  positions <- c(positions, pos)
}

# Scrape predicted team(s) #############3
portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[3]/ul/li/img") %>%
  html_attr(name = "title")

predteam1_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
predteam1_xpath2 <- "]/ul/li/img"

pred_school1 <- c()
for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(predteam1_xpath1, i, predteam1_xpath2)) %>%
    html_attr(name = "title") -> school
  print(school)
  pred_school1 <- c(pred_school1, school)
}

# scrape probability for school one ###########

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[2]/ul/li/span") %>%
  html_text(trim = TRUE)

predteam_perc_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
predteam_perc_xpath2 <- "]/ul/li/span"

pred_school1_perc <- c()

for(i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(predteam_perc_xpath1, i, predteam_perc_xpath2)) %>%
    html_text(trim = TRUE) -> perc
  print(perc)
  pred_school1_perc <- c(pred_school1_perc, perc)
}

# Scrape the second predicted team  #########

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[7]/ul/li[2]/img") %>%
  html_attr(name = "alt")

predteam2_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
predteam2_xpath2 <- "]/ul/li[2]/img"

pred_school2 <- c()

for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(predteam2_xpath1, i, predteam2_xpath2)) %>%
    html_attr(name = "alt") -> school
  print(school)
  pred_school2 <- c(pred_school2, school)
}

# Get percentage for second school because idk why not ############
portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[7]/ul/li[2]/span") %>%
  html_text(trim = TRUE)

predteam2_perc_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
predteam2_perc_xpath2 <- "]/ul/li[2]/span"

pred_school2_perc <- c()

for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(predteam2_perc_xpath1, i, predteam2_perc_xpath2)) %>%
    html_text(trim = TRUE) -> perc
  print(perc)
  pred_school2_perc <- c(pred_school2_perc, perc)
}

# Scrape eligibility status ##################

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[2]/div[4]") %>%
  html_text(trim = TRUE)

elig_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
elig_xpath2 <- "]/div[4]"

elig_status <- c()

for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(elig_xpath1, i, elig_xpath2)) %>%
    html_text(trim = TRUE) -> status
  print(status)
  elig_status <- c(elig_status, status)
}

# Scrape Where the player is transfering from

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[6]/div[5]/a[1]/img") %>%
  html_attr(name = "title")

orig_school_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
orig_school_xpath2 <- "]/div[5]/a[1]/img"

orig_schools <- c()
for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(orig_school_xpath1, i, orig_school_xpath2)) %>%
    html_attr(name = "title") -> school
  print(school)
  orig_schools <- c(orig_schools, school)
}

# Last one: Scrape the school the players are transferring to ###################

portal %>%
  html_node(xpath = "//*[@id=\"page-content\"]/div/section/section/div/ul/li[2]/div[5]/a[2]/img") %>%
  html_attr(name = "title")

new_school_xpath1 <- "//*[@id=\"page-content\"]/div/section/section/div/ul/li["
new_school_xpath2 <- "]/div[5]/a[2]/img"

new_schools <- c()

for (i in 2:1251)
{
  portal %>%
    html_node(xpath = paste0(new_school_xpath1, i, new_school_xpath2)) %>%
    html_attr(name = "title") -> school
  print(school)
  new_schools <- c(new_schools, school)
}

# Now that we have all the data scraped we can piece them together for our data frame #######

df <- data.frame(names, measurements, trans_ratings, hs_ratings, positions, pred_school1,
                 pred_school1_perc, pred_school2, pred_school2_perc, elig_status,
                 orig_schools, new_schools)

# Now let's do a little data cleaning ########

df %>%
  filter(!is.na(names)) -> df

colnames(df) <- c("name", "measurements", "current_rating", "hs_rating", "position", "pred_school1",
                  "pred_school1_estimate", "pred_school2", "pred_school2_estimate", "eligibility",
                  "transferred_from", "transferred_to")

df <- df %>%
  mutate(measurements = str_replace(measurements, " ", ""))

df <- df %>%
  separate(col = measurements, into = c("height", "weight"), sep = " / ")

df <- df %>%
  mutate(current_rating = str_replace(current_rating, " \\(T\\)", ""),
         hs_rating = str_replace(hs_rating, " \\(HS\\)", ""),
         pred_school1_estimate = str_replace(pred_school1_estimate, "%", ""),
         pred_school2_estimate = str_replace(pred_school2_estimate, "%", ""))

df <- df %>%
  mutate_at(.vars = vars(weight, current_rating, hs_rating, pred_school1_estimate,
                         pred_school2_estimate),
            .funs = funs(as.numeric))

df <- df %>%
  mutate(pred_school1_estimate = pred_school1_estimate / 100,
         pred_school2_estimate = pred_school2_estimate / 100)

df %>%
  group_by(transferred_to) %>%
  summarize(count = n(),
            avg_rating = mean(current_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) %>%
  filter(count >= 3)

df %>%
  group_by(transferred_from) %>%
  summarize(count = n(),
            avg_rating = mean(current_rating, na.rm = TRUE)) %>%
  filter(count >= 3) %>%
  arrange(desc(avg_rating))





