library(tidyverse)

# Entities.csv has some rows which would be parsed as entries with more that 3 columns.
# We need to fix this.
entities_raw <- read_lines("Data/Entities.csv")

wrong_rows <- entities_raw %>% 
    str_split(",") %>%
    map_int(length) %>% 
    `!=`(3) %>% 
    which()

# Removing the first commas in problematic rows fixes the issue
entities_raw[wrong_rows] <- entities_raw[wrong_rows] %>% 
    str_replace(",", "")

entities <- entities_raw[-1] %>% 
    str_split(",") %>% 
    do.call(what = rbind) %>% 
    as_tibble()

names(entities) <- str_split(entities_raw[1], ",") %>% unlist() %>% tolower()

# The first 505 rows are redundant. The second half contains true data
setequal(entities$company[1:505], entities$company[506:1010])
entities <- entities[506:1010,]

news_raw <- read_lines("Data/News.csv", locale = locale(encoding = "windows-1252"))

# We would separate `news_raw`` into two distinct vector. One contains company names
# and the other contains news details
company <- news_raw %>% 
    str_split("-", n = 2) %>% 
    map_chr(1) %>% 
    str_extract_all("\\w+") %>% 
    unlist()

# This checks that every company listed in `company` belongs to the set of
# companies in `entities`
setdiff(company, entities$company)

headline_and_summary <- news_raw %>% 
    str_split("-", n = 2) %>% 
    map_chr(2) %>% 
    str_remove(",*$") %>% 
    str_remove('"$')

news <- tibble(company, news_id = 1:length(news_raw), headline_and_summary)
