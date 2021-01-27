
# install all the required packages ---------------------------------------
library(pdftools)
library(tm)
library(tidyverse)
library(stringr)
library(quanteda)
library(here)
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr)
library(stringr)
library(treemap)
library(ggplot2)
library(lubridate)
# load all news pdf files -------------------------------------------------

#get summary of all news articles include key word "Chinese American"
setwd("/Users/sunnyshao/Dropbox/dissertation/CHAPTER 2/articletidyup/news_chinese_american")
files <- list.files(pattern = "pdf$")
df <- data.frame(matrix(NA, nrow = 367, ncol = 5))
df <- df %>% rename(title = X1,
                    source = X2,
                    datetime = X3,
                    content = X4,
                    date = X5) %>%
  mutate(id = row_number())

#write a loop to clean up the data
i <- 1
for (i in 1:367) {
  print(i)
  if(i <= 367){
    raw <- pdf_text(files[i]) %>%
      readr::read_lines()
    n <- length(raw)

    title <- raw[1] %>% str_squish()

    temp <- data.frame(raw) %>%
      filter(str_detect(raw, "Copyright")) %>%
      unlist()
    source <- paste(temp, collapse = '')

    temp <- data.frame(raw[1:12]) %>%
      filter(str_detect(raw.1.12., "January|February|March|April|May|June|July|August|September|October|November|December")) %>%
      unlist()
    datetime <- paste(temp, collapse = '') %>% str_squish()

    temp <- data.frame(raw[(n-5):n]) %>%
      filter(str_detect(`raw..n...5..n.`, "Load-Date")) %>%
      unlist()
    date <- paste(temp, collapse = '') %>% str_squish()

    temp <- data.frame(raw) %>%
      mutate(id = row_number()) %>%
      mutate(mark1 = case_when(
        str_detect(raw, "Body") ~id,
        TRUE ~as.integer(0))) %>%
      mutate(mark2 = case_when(
        str_detect(raw, "Load-Date") ~id,
        TRUE ~as.integer(0))) %>%
      mutate(sum1 = sum(mark1),
             sum2 = sum(mark2)) %>%
      mutate(keep = case_when(
        id > sum1 & id < sum2 ~1,
        TRUE ~0)) %>%
      filter(keep == 1) %>%
      select(raw) %>% unlist()
    content <- paste(temp, collapse = '') %>% str_squish() %>%
      str_replace_all(",", "") %>%
      str_replace_all("\"", "")

    df[i,1] <- title
    df[i,2] <- source
    df[i,3] <- datetime
    df[i,4] <- content
    df[i,5] <- date
  }else{return(df)
  }
  i <- i + 1
}
rm(content, date, datetime, files, i, n, source, temp, title, raw)

#get summary of all news articles include key word "news_prop16"
setwd("/Users/sunnyshao/Dropbox/dissertation/CHAPTER 2/articletidyup/news_prop16")
files <- list.files(pattern = "pdf$")
df2 <- data.frame(matrix(NA, nrow = 353, ncol = 5))
df2 <- df2 %>% rename(title = X1,
                    source = X2,
                    datetime = X3,
                    content = X4,
                    date = X5) %>%
  mutate(id = row_number())

#write a loop to clean up the data
i <- 1
for (i in 1:353) {
  print(i)
  if(i <= 353){
    raw <- pdf_text(files[i]) %>%
      readr::read_lines()
    n <- length(raw)

    title <- raw[1] %>% str_squish()

    temp <- data.frame(raw) %>%
      filter(str_detect(raw, "Copyright")) %>%
      unlist()
    source <- paste(temp, collapse = '')

    temp <- data.frame(raw[1:12]) %>%
      filter(str_detect(raw.1.12., "January|February|March|April|May|June|July|August|September|October|November|December")) %>%
      unlist()
    datetime <- paste(temp, collapse = '') %>% str_squish()

    temp <- data.frame(raw[(n-5):n]) %>%
      filter(str_detect(`raw..n...5..n.`, "Load-Date")) %>%
      unlist()
    date <- paste(temp, collapse = '') %>% str_squish()

    temp <- data.frame(raw) %>%
      mutate(id = row_number()) %>%
      mutate(mark1 = case_when(
        str_detect(raw, "Body") ~id,
        TRUE ~as.integer(0))) %>%
      mutate(mark2 = case_when(
        str_detect(raw, "Load-Date") ~id,
        TRUE ~as.integer(0))) %>%
      mutate(sum1 = sum(mark1),
             sum2 = sum(mark2)) %>%
      mutate(keep = case_when(
        id > sum1 & id < sum2 ~1,
        TRUE ~0)) %>%
      filter(keep == 1) %>%
      select(raw) %>% unlist()
    content <- paste(temp, collapse = '') %>% str_squish() %>%
      str_replace_all(",", "") %>%
      str_replace_all("\"", "")

    df2[i,1] <- title
    df2[i,2] <- source
    df2[i,3] <- datetime
    df2[i,4] <- content
    df2[i,5] <- date
  }else{return(df2)
  }
  i <- i + 1
}
rm(content, date, datetime, files, i, n, source, temp, title, raw)

df <- df %>% mutate(search_term = "chinese american")
df2 <- df2 %>% mutate(search_term = "prop 16")


# merge two search results -----------------------------------------

final <- df %>% 
  rbind(df2) %>% 
  select(title, datetime, date, search_term) %>% 
  mutate(load_date = as.Date(str_remove(date, "Load-Date: "), 
                             format = "%B %d, %Y"),#remove extra string and format as date
         #extract month day, year format as date
         publish_date = as.Date(str_extract(datetime, pattern ="\\w{3,9} \\d{1,2}, \\d{4}"), 
                                format = "%B %d, %Y")) %>% 
  #set date based on published date if it is not available, use load date
  mutate(date = case_when(
    is.na(publish_date) == F ~publish_date,
    TRUE ~load_date))

#check NAs
table(is.na(final$date))

# summarize total articles per day ----------------------------------------
tbl_sum <- final %>% 
  mutate(count = 1) %>% 
  select(date, search_term, count) %>% 
  group_by(search_term, date) %>% 
  mutate(tot_article = sum(count)) %>% 
  ungroup() %>% 
  select(-count) %>% unique()

table(tbl_sum$search_term)

write_csv(tbl_sum, "/Users/sunnyshao/Dropbox/dissertation/CHAPTER 2/articletidyup/news_counts.csv")
