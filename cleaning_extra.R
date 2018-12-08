# I spent quite a lot of time trying to separate `headline_and_summary`
# column into two separate columns. I could do this for most of the entries
# but some remained impossible. Finally I gave up the task because not only
# it seemed impossible, but also not much fruitful. Nevertheless I include the
# code which would take us half-way
news_tidy1 <- news %>%
    filter(str_detect(headline_and_summary, "-NA$")) %>% 
    mutate(
        headline = str_remove(headline_and_summary, "-NA$"),
        summary  = NA_character_
    ) %>% 
    select(-headline_and_summary)

news_untidy1 <- news %>% 
    filter(!str_detect(headline_and_summary, "-NA$"))

news_tidy2 <- news_untidy1 %>% 
    filter(str_detect(headline_and_summary, "^[^-]*-[^-]*$")) %>% 
    separate(headline_and_summary, c("headline", "summary"), sep = "-")

news_untidy2 <- news_untidy1 %>% 
    filter(!str_detect(headline_and_summary, "^[^-]*-[^-]*$"))
