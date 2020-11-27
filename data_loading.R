install_load_package <- function(x){
  if (!require(x, character.only = T)){
    install.packages(x, dep=T)
    if(!require(x, character.only = T)) stop("Package not found")
  }
}


install_load_package('tidyverse')
install_load_package('tokenizers')
install_load_package('stopwords')
install_load_package('wordcloud')
install_load_package('RColorBrewer')
install_load_package('lubridate')
install_load_package('cowplot')

df <- read_csv('debate_transcripts_v3_2020-02-26.csv')

df$debate_name <- as.factor(df$debate_name)
df$date <- as.Date(df$date,tryFormats = c("%m-%d-%Y"))


speakers <- list('Bernie Sanders','Joe Biden','Elizabeth Warren','Pete Buttigieg','Amy Klobuchar')

en_stopwords = stopwords('en')
en_stopwords <- append(
  en_stopwords,c('s','t','re','going','thank','one','ve','get','just','think','m','got','let','us','year','today',
                 'd','put','went','put','two','day','bring','move','see','doesn','didn','go','don','done','tell',
                 'came','says','lot','ll','know','said','actually','even'))

tokenize <- function(column){
  words <- tokenize_words(column, stopwords = en_stopwords, lowercase = T, 
                          strip_numeric = T, strip_punct = T)
  unlist(words)
}

make_wordcloud <- function(words,n){
  count_words <- as.data.frame(sort(table(words),decreasing = T))
  colnames(count_words) <- c('words','freq')
  wordcloud(filter(count_words, !words%in%en_stopwords)$words,
            filter(count_words, !words%in%en_stopwords)$freq,
            min.freq = n, colors = brewer.pal(8,'Set1'))
}
