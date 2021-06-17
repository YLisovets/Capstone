library(readr)
library(corpus)
library(dplyr)
library(tidyr)
library(textclean)

setwd("C:/Users/User/Desktop/D/Capstone/Data/en_US")
clean_text <- function(text){
    text <- iconv(text, "latin1", "ASCII", sub = "")
    text %>%
        add_comma_space() %>%
        replace_html(symbol = FALSE) %>%
        replace_email() %>% 
        replace_url() %>% 
        replace_hash() %>% 
        replace_tag() %>% 
        #replace_emoticon(emoticon_dt = hash_emoticons) %>%
        #replace_names() %>% 
        replace_word_elongation() %>%
        replace_contraction() %>% 
        replace_kern() %>% 
        replace_word_elongation() %>%
        replace_money(replacement = "") %>%
        replace_date(replacement = "") %>% 
        replace_time(replacement = "") %>% 
        replace_ordinal() %>%
        #replace_number() %>% 
        replace_internet_slang() %>%
        replace_symbol() %>%
        replace_incomplete(replacement = " ") %>% 
        strip(char.keep = c("-", "'")) %>%
        #mgsub_regex("\\w*[0-9]+\\w*\\s*", " ") %>% 
        mgsub_regex("(\\w['-]\\w)|[[:punct:]]", "\\1", perl=TRUE) %>%
        mgsub_regex("\\bu\\b", "you") %>% 
        mgsub_regex("(.)\\1{2,}", "") 
    #replace_tokens("rt", "")
}

news <- read_lines("en_US.news.txt")
news_cleared <- clean_text(news)
news_cleared %>% write_rds(file = "news_cleared.rds")
rm(news)
news_cleared <-  read_rds("news_cleared.rds")
news_unigram <- term_stats(news_cleared, text_filter(drop_symbol = TRUE, drop_punct = FALSE, map_case = FALSE, 
                                                     drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")),
                           min_support = 2)
news_unigram %>% write_rds(file = "news_unigram.rds")
rm(news_unigram)
news_bigrams <- term_stats(news_cleared, ngrams = 2, types = TRUE, min_support = 2,
                           text_filter(drop_symbol = TRUE, map_case = FALSE,
                                       drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_bigrams <- news_bigrams %>% 
    select(-5)
news_bigrams %>% write_rds(file = "news_bigrams2.rds")
rm(news_bigrams)

news_trigrams_1 <- term_stats(news_cleared[1:330000], ngrams = 3, types = TRUE, min_count = 2,
                              text_filter(drop_symbol = TRUE, map_case = FALSE,
                                          drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_trigrams_2 <- term_stats(news_cleared[330001:660000], ngrams = 3, types = TRUE,min_count = 2,
                              text_filter(drop_symbol = TRUE, map_case = FALSE,
                                          drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_trigrams_3 <- term_stats(news_cleared[660001:1010242], ngrams = 3, types = TRUE, min_count = 2, 
                              text_filter(drop_symbol = TRUE, map_case = FALSE,
                                          drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_trigrams <- select(news_trigrams_1, -support) %>% 
    full_join(select(news_trigrams_2, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    full_join(select(news_trigrams_3, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count.x + count.y + count) %>% 
    select(-c(5:6))
news_trigrams %>% write_rds(file = "news_trigrams2.rds")
rm(news_trigrams, news_trigrams_1, news_trigrams_2, news_trigrams_3)
gc()

news_4grams_1 <- term_stats(news_cleared[1:330000], ngrams = 4, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_4grams_2 <- term_stats(news_cleared[330001:660000], ngrams = 4, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_4grams_3 <- term_stats(news_cleared[660001:1010242], ngrams = 4, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_4grams <- select(news_4grams_1, -support) %>% 
    full_join(select(news_4grams_2, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    full_join(select(news_4grams_3, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count.x + count.y + count) %>% 
    select(-c(6:7))
news_4grams %>% write_rds(file = "news_4grams2.rds")
print(object.size(news_4grams),unit="Mb")
rm(news_4grams_1, news_4grams_2, news_4grams_3)
gc()

news_5grams_1 <- term_stats(news_cleared[1:330000], ngrams = 5, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_5grams_2 <- term_stats(news_cleared[330001:660000], ngrams = 5, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_5grams_3 <- term_stats(news_cleared[660001:1010242], ngrams = 5, types = TRUE, min_count = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
news_5grams <- select(news_5grams_1, -support) %>% 
    full_join(select(news_5grams_2, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    full_join(select(news_5grams_3, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count +count.x + count.y) %>% 
    select(-c(7:8))
news_5grams %>% write_rds(file = "news_5grams2.rds")
print(object.size(news_5grams),unit="Mb")
rm(news_5grams, news_5grams_1, news_5grams_2, news_5grams_3)
rm(news, news_cleared)
gc()


blogs <- read_lines("en_US.blogs.txt")
blogs <- iconv(blogs, "latin1", "ASCII", sub = "")
blogs <- gsub("\\w*[0-9]+\\w*\\s*", "", blogs)

blogs_cleared <- clean_text(blogs)
blogs_cleared %>% write_rds(file = "blogs_cleared.rds")
blogs_unigram <- term_stats(blogs_cleared, text_filter(drop_symbol = TRUE, map_case = FALSE,
                                               drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")),
                            min_support = 2)
blogs_unigram %>% write_rds(file = "blogs_unigram.rds")
rm(blogs_unigram)
blogs_bigrams <- term_stats(blogs_cleared, ngrams = 2, types = TRUE, min_support = 2,
                            text_filter(drop_symbol = TRUE, map_case = FALSE,
                                        drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_bigrams <- blogs_bigrams %>% 
    select(-5)   
blogs_bigrams %>% write_rds(file = "blogs_bigrams2.rds")
rm(blogs_bigrams)
gc()

blogs_trigrams_1 <- term_stats(blogs_cleared[1:300000], ngrams = 3, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_trigrams_2 <- term_stats(blogs_cleared[300001:600000], ngrams = 3, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_trigrams_3 <- term_stats(blogs_cleared[600001:899288], ngrams = 3, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_trigrams <- select(blogs_trigrams_1, -support) %>% 
    full_join(select(blogs_trigrams_2, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    full_join(select(blogs_trigrams_3, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(5:6))
blogs_trigrams %>% write_rds(file = "blogs_trigrams2.rds")
print(object.size(blogs_trigrams),unit="Mb")
rm(blogs_trigrams, blogs_trigrams_1, blogs_trigrams_2, blogs_trigrams_3)
gc()

blogs_4grams_1 <- term_stats(blogs_cleared[1:300000], ngrams = 4, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_4grams_2 <- term_stats(blogs_cleared[300001:600000], ngrams = 4, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_4grams_3 <- term_stats(blogs_cleared[600001:899288], ngrams = 4, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_4grams <- select(blogs_4grams_1, -support) %>% 
    full_join(select(blogs_4grams_2, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    full_join(select(blogs_4grams_3, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(6:7))
blogs_4grams %>% write_rds(file = "blogs_4grams2.rds")
print(object.size(blogs_4grams),unit="Mb")
rm(blogs_4grams, blogs_4grams_1, blogs_4grams_2, blogs_4grams_3)
gc()

blogs_5grams_1 <- term_stats(blogs_cleared[1:300000], ngrams = 5, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_5grams_2 <- term_stats(blogs_cleared[300001:600000], ngrams = 5, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_5grams_3 <- term_stats(blogs_cleared[600001:899288], ngrams = 5, types = TRUE, min_count = 2,
                             text_filter(drop_symbol = TRUE, map_case = FALSE,
                                         drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
blogs_5grams <- select(blogs_5grams_1, -support) %>% 
    full_join(select(blogs_5grams_2, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    full_join(select(blogs_5grams_3, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(7:8))
blogs_5grams %>% write_rds(file = "blogs_5gram2s.rds")
print(object.size(blogs_5grams),unit="Mb")
rm(blogs_5grams, blogs_5grams_1, blogs_5grams_2, blogs_5grams_3)
rm(blogs, blogs_cleared)
gc()


twitter <- read_lines("en_US.twitter.txt")
twitter <- iconv(twitter, "latin1", "ASCII", sub = "")
twitter[256]
twitter_cleared <- clean_text(twitter)
rm(twitter)
twitter_cleared %>% write_rds(file = "twitter_cleared.rds")
twitter_cleared <-  read_rds("twitter_cleared.rds")
twitter_unigram <- term_stats(twitter_cleared, min_support = 2,
                              text_filter(drop_symbol = TRUE, map_case = FALSE,
                                          drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s"))                              )
twitter_unigram %>% write_rds(file = "twitter_unigram.rds")
twitter_bigrams <- term_stats(twitter_cleared, ngrams = 2, types = TRUE, min_support = 2,
                              text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                          drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_bigrams <- twitter_bigrams %>% 
    select(-5)   
twitter_bigrams %>% write_rds(file = "twitter_bigrams2.rds")
rm(twitter_bigrams, twitter_unigram)

twitter_trigrams_1 <- term_stats(twitter_cleared[1:800000], ngrams = 3, types = TRUE, min_count = 2,
                                 text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                             drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_trigrams_2 <- term_stats(twitter_cleared[800001:1600000], ngrams = 3, types = TRUE, min_count = 2,
                                 text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                             drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_trigrams_3 <- term_stats(twitter_cleared[1600001:2360148], ngrams = 3, types = TRUE, min_count = 2,
                                 text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                             drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_trigrams <- select(twitter_trigrams_1, -support) %>% 
    full_join(select(twitter_trigrams_2, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    full_join(select(twitter_trigrams_3, -support), by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(5:6))
twitter_trigrams %>% write_rds(file = "twitter_trigrams2.rds")
print(object.size(twitter_trigrams),unit="Mb")
rm(twitter_trigrams, twitter_trigrams_1, twitter_trigrams_2, twitter_trigrams_3)
gc()

twitter_4grams_1 <- term_stats(twitter_cleared[1:800000], ngrams = 4, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_4grams_2 <- term_stats(twitter_cleared[800001:1600000], ngrams = 4, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_4grams_3 <- term_stats(twitter_cleared[1600001:2360148], ngrams = 4, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_4grams <- select(twitter_4grams_1, -support) %>% 
    full_join(select(twitter_4grams_2, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    full_join(select(twitter_4grams_3, -support), by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(6:7))
twitter_4grams %>% write_rds(file = "twitter_4grams2.rds")
print(object.size(twitter_4grams),unit="Mb")
rm(twitter_4grams, twitter_4grams_1, twitter_4grams_2, twitter_4grams_3)
gc()


twitter_5grams_1 <- term_stats(twitter_cleared[1:800000], ngrams = 5, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE, map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_5grams_2 <- term_stats(twitter_cleared[800001:1600000], ngrams = 5, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_5grams_3 <- term_stats(twitter_cleared[1600001:2360148], ngrams = 5, types = TRUE, min_count = 2,
                               text_filter(drop_symbol = TRUE,  map_case = FALSE,
                                           drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
twitter_5grams <- select(twitter_5grams_1, -support) %>% 
    full_join(select(twitter_5grams_2, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    full_join(select(twitter_5grams_3, -support), by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(-c(7:8))
twitter_5grams %>% write_rds(file = "twitter_5grams2.rds")
print(object.size(twitter_5grams),unit="Mb")
rm(twitter_5grams_1, twitter_5grams_2, twitter_5grams_3)
rm(twitter_cleared)
gc()

news_unigrams <- read_rds("news_unigram.rds")
blogs_unigrams <- read_rds("blogs_unigram.rds")
twitter_unigrams <- read_rds("twitter_unigram.rds")
vocabulary <- news_unigrams %>% 
    full_join(blogs_unigrams, by = "term") %>% 
    full_join(twitter_unigrams, by = "term") %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>% 
    select(term, count) %>%
    filter(count > 5) %>% 
    arrange(desc(count))

vocabulary %>% write_rds(file = "vocabulary2.rds")
rm(news_unigrams, blogs_unigrams, twitter_unigrams)
gc()


news_bigrams <- read_rds("news_bigrams2.rds")
blogs_bigrams <- read_rds("blogs_bigrams2.rds")
twitter_bigrams <- read_rds("twitter_bigrams2.rds")
bigrams <- news_bigrams %>% 
    full_join(blogs_bigrams, by = c("term", "type1", "type2"), copy = FALSE) %>% 
    full_join(twitter_bigrams, by = c("term", "type1", "type2"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>%
    select(-c(4:5)) %>% 
    filter(count > 3) %>% 
    arrange(desc(count))
length(unique(bigrams$type1))
bigrams %>% write_rds(file = "bigrams2.rds")
print(object.size(bigrams),unit="Mb")
rm(news_bigrams, blogs_bigrams, twitter_bigrams, bigrams)
gc()


news_trigrams <- read_rds("news_trigrams2.rds")
blogs_trigrams <- read_rds("blogs_trigrams2.rds")
twitter_trigrams <- read_rds("twitter_trigrams2.rds")
trigrams <- news_trigrams %>% 
    full_join(blogs_trigrams, by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    full_join(twitter_trigrams, by = c("term", "type1", "type2", "type3"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>%
    select(-c(5:6)) %>% 
    filter(count > 2) %>% 
    arrange(desc(count))
trigrams %>% write_rds(file = "trigrams2.rds")
print(object.size(trigrams),unit="Mb")
rm(news_trigrams, blogs_trigrams, twitter_trigrams, trigrams)
gc()


news_4grams <- read_rds("news_4grams2.rds")
blogs_4grams <- read_rds("blogs_4grams2.rds")
twitter_4grams <- read_rds("twitter_4grams2.rds")
quadrugrams <- news_4grams %>% 
    full_join(blogs_4grams, by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    full_join(twitter_4grams, by = c("term", "type1", "type2", "type3", "type4"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>%
    select(-c(6:7)) %>% 
    filter(count > 1) %>% 
    arrange(desc(count))
quadrugrams %>% write_rds(file = "quadrugrams2.rds")
print(object.size(quadrugrams),unit="Mb")
rm(news_4grams, blogs_4grams, twitter_4grams)
gc()


news_5grams <- read_rds("news_5grams2.rds")
blogs_5grams <- read_rds("blogs_5gram2s.rds")
twitter_5grams <- read_rds("twitter_5grams2.rds")
five_grams <- news_5grams %>% 
    full_join(blogs_5grams, by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    full_join(twitter_5grams, by = c("term", "type1", "type2", "type3", "type4", "type5"), copy = FALSE) %>% 
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    mutate(count = count + count.x + count.y) %>%
    select(-c(7:8)) %>%
    arrange(desc(count))
five_grams %>% write_rds(file = "five_grams2.rds")
print(object.size(five_grams),unit="Mb")
rm(news_5grams, blogs_5grams, twitter_5grams)
gc()


bigrams <-  read_rds("bigrams2.rds")
bigrams_sort <- bigrams %>%
    arrange(type1)
names(bigrams_sort)[2] <- "phrase"
names(bigrams_sort)[3] <- "predict"
bigrams_sort %>% write_rds(file = "bigrams_sort2.rds")


trigrams <-  read_rds("trigrams2.rds")
trigrams_sort <- trigrams %>%
    filter(count > 3) %>% 
    unite(phrase, type1, type2, sep = " ") %>% 
    arrange(phrase)
names(trigrams_sort)[3] <- "predict"
length(unique(trigrams_sort$phrase))
trigrams_sort %>% write_rds(file = "trigrams_sort2.rds")
print(object.size(trigrams_sort),unit="Mb")
rm(trigrams, bigrams)


quadrugrams <-  read_rds("quadrugrams2.rds")
quadrugrams_sort <- quadrugrams %>%
    filter(count > 2) %>% 
    unite(phrase, type1, type2, type3, sep = " ") %>% 
    arrange(phrase)
names(quadrugrams_sort)[3] <- "predict"
length(unique(quadrugrams_sort$phrase))
quadrugrams_sort %>% write_rds(file = "quadrugrams_sort2.rds")
print(object.size(quadrugrams_sort),unit="Mb")


five_grams <- read_rds("five_grams2.rds")
five_grams_sort <- five_grams %>%
    filter(count > 2) %>% 
    unite(phrase, type1, type2, type3, type4, sep = " ") %>% 
    arrange(phrase)
names(five_grams_sort)[3] <- "predict"
length(unique(five_grams_sort$phrase))
five_grams_sort %>% write_rds(file = "five_grams_sort2.rds")
print(object.size(five_grams_sort),unit="Mb")
rm(five_grams, quadrugrams)

five_grams_sort <- read_rds("five_grams_sort2.rds")
quadrugrams_sort <- read_rds("quadrugrams_sort2.rds")
trigrams_sort <-  read_rds("trigrams_sort2.rds")
bigrams_sort <-  read_rds("bigrams_sort2.rds")
vocabulary <-  read_rds("vocabulary2.rds")


five_grams_final <- five_grams_sort %>% 
    left_join(select(quadrugrams_sort, term, count), by = c("phrase" = "term")) %>% 
    mutate(score = round(count.x / count.y, digits = 5)) %>% 
    select(-c(1,4,5)) %>%
    mutate_at(vars(score), funs(replace(., is.na(.), 1))) %>% 
    group_by(phrase) %>% 
    slice_head(n = 5)
print(object.size(five_grams_final),unit="Mb")
five_grams_final %>% write_rds(file = "five_grams_final2.rds")

quadrugrams_final <- quadrugrams_sort %>% 
    left_join(select(trigrams_sort, term, count), by = c("phrase" = "term")) %>% 
    mutate(score = round((count.x / count.y), digits = 5)) %>% 
    select(-c(1,4,5)) %>% 
    mutate_at(vars(score), funs(replace(., is.na(.), 1))) %>% 
    group_by(phrase) %>% 
    slice_head(n = 5)
quadrugrams_final %>% write_rds(file = "quadrugrams_final2.rds")
print(object.size(quadrugrams_final),unit="Mb")

trigrams_final <- trigrams_sort %>% 
    left_join(select(bigrams_sort, term, count), by = c("phrase" = "term")) %>% 
    mutate(score = round((count.x / count.y), digits = 5)) %>% 
    select(-c(1,4,5)) %>%
    mutate_at(vars(score), funs(replace(., is.na(.), 1))) %>% 
    group_by(phrase) %>% 
    slice_head(n = 5)
trigrams_final %>% write_rds(file = "trigrams_final2.rds")
print(object.size(trigrams_final),unit="Mb")

bigrams_final <- bigrams_sort %>% 
    left_join(vocabulary, by = c("phrase" = "term")) %>% 
    mutate(score = round((count.x / count.y), digits = 5)) %>% 
    select(-c(1,4,5)) %>% 
    mutate_at(vars(score), funs(replace(., is.na(.), 1))) %>%
    group_by(phrase) %>% 
    slice_head(n = 5)
bigrams_final %>% write_rds(file = "bigrams_final2.rds")
print(object.size(bigrams_final),unit="Mb")
