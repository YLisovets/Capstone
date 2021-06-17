library(data.table)
library(corpus)

prediction <- function(text){
    phrase_input <- clean_text(text)
    tokens <- text_tokens(phrase_input, text_filter(text_filter(drop_symbol = TRUE, map_case = FALSE,
                                                                drop = c(tolower(lexicon::profanity_alvarez), "rt", "th", "st", "s")))
    )
    words_input <- unlist(tokens)
    words_count <- length(words_input)
    
    predict_1gr <- data.table(predict = vocabulary[1:5,1], score = round((vocabulary[1:5,2]/ sum(vocabulary[,2])),
                                                                         digits = 5), ngram = rep(1,5))
    
    if (words_count >= 4) {
        words_4_input <- words_input[(words_count-3):words_count]
        search_phrase <- str_c(words_4_input, collapse = " ")
        predict_5gr <- pentadgrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_5gr[, ngram := 5]
        
        words_3_input <- words_input[(words_count-2):words_count]
        search_phrase <- str_c(words_3_input, collapse = " ")
        predict_4gr <- quadrugrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_4gr[, ':=' (score= score *0.4, ngram = 4)]
        
        words_2_input <- words_input[(words_count-1):words_count]
        search_phrase <- str_c(words_2_input, collapse = " ")
        predict_3gr <- trigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_3gr[, ':=' (score= score *0.4^2, ngram = 3)]
        
        search_phrase <- words_input[words_count]
        predict_2gr <- bigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_2gr[, ':=' (score= score *0.4^3, ngram = 2)]
        
        predict_1gr[, score := score *0.4^4]
        
        prediction <- rbind(predict_5gr, predict_4gr, predict_3gr, predict_2gr, predict_1gr)
        setorder(prediction, -score)
        prediction <- prediction[,.SD[1], by=predict]
    }
    if (words_count == 3) {
        
        search_phrase <- str_c(words_input, collapse = " ")
        predict_4gr <- quadrugrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_4gr[, ngram := 4]
        
        words_2_input <- words_input[(words_count-1):words_count]
        search_phrase <- str_c(words_2_input, collapse = " ")
        predict_3gr <- trigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_3gr[, ':=' (score= score *0.4, ngram = 3)]
        
        search_phrase <- words_input[words_count]
        predict_2gr <- bigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_2gr[, ':=' (score= score *0.4^2, ngram = 2)]
        
        predict_1gr[, score := score *0.4^3]
        
        prediction <- rbind(predict_4gr, predict_3gr, predict_2gr, predict_1gr)
        setorder(prediction, -score)
        prediction <- prediction[,.SD[1], by=predict]
    }
    
    if (words_count == 2) {
        
        search_phrase <- str_c(words_input, collapse = " ")
        predict_3gr <- trigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_3gr[, ngram := 3]
        
        search_phrase <- words_input[words_count]
        predict_2gr <- bigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_2gr[, ':=' (score= score *0.4, ngram = 2)]
        
        predict_1gr[, score := score *0.4^2]
        
        prediction <- rbind(predict_3gr, predict_2gr, predict_1gr)
        setorder(prediction, -score)
        prediction <- prediction[,.SD[1], by=predict]
    }
    
    if (words_count == 1) {
        
        search_phrase <- words_input
        predict_2gr <- bigrams[.(search_phrase), .(predict, score), nomatch = 0L]
        predict_2gr[, ngram := 2]
        
        predict_1gr[, score := score *0.4]
        
        prediction <- rbind(predict_2gr, predict_1gr)
        setorder(prediction, -score)
        prediction <- prediction[,.SD[1], by=predict]
    }
    
    if (words_count == 0) {
        prediction <- predict_1gr
    }
    
    prediction[, ngram:= as.integer(ngram)]
    return(prediction[1:5])
}