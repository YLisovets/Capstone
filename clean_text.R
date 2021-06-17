library(textclean)

clean_text <- function(text){
    text <- iconv(text, "latin1", "ASCII", sub = "")
    text %>%
        add_comma_space() %>%
        replace_html(symbol = FALSE) %>%
        replace_email() %>% 
        replace_url() %>% 
        replace_hash() %>% 
        replace_tag() %>% 
        replace_word_elongation() %>%
        replace_contraction() %>% 
        replace_kern() %>% 
        replace_word_elongation() %>%
        replace_money(replacement = "") %>%
        replace_date(replacement = "") %>% 
        replace_time(replacement = "") %>% 
        replace_ordinal() %>%
        replace_internet_slang() %>%
        replace_symbol() %>%
        replace_incomplete(replacement = " ") %>% 
        strip(char.keep = c("-", "'")) %>%
        mgsub_regex("(\\w['-]\\w)|[[:punct:]]", "\\1", perl=TRUE) %>%
        mgsub_regex("\\bu\\b", "you") %>%  # replace u with you
        mgsub_regex("(.)\\1{2,}", "")      # removes any words with 3 or more repeated letters
}
