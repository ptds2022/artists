Topsongslyrics <- function(name, tfgraph = FALSE, wordcloud = FALSE, lang, environment.return = TRUE) {
  name <- name %>%
    gsub("[^[:alnum:]]", "", .) %>%
    gsub(" ", "-", .) %>%
    str_to_lower()

  html <- read_html(paste0("https://genius.com/artists/", name))

  titles <- html %>%
    html_nodes(".mini_card-title") %>%
    html_text2() %>%
    as.data.frame()
  names <- html %>%
    html_nodes(".mini_card-subtitle") %>%
    html_text2() %>%
    as.data.frame()


  top10.songs <- cbind(names, titles)

  colnames(top10.songs) <- c("artist", "song")
  top10.songs$artist <- gsub("&", "and", top10.songs$artist)

  top10.songs$artist <- gsub("[^[:alnum:] ]", "", top10.songs$artist)
  top10.songs$song <- gsub("[^[:alnum:] ]", "", top10.songs$song)

  top10.songs$artist <- gsub(" ", "-", top10.songs$artist)
  top10.songs$song <- gsub(" ", "-", top10.songs$song)
  top10.songs$artist <- gsub("'", "", top10.songs$artist)
  top10.songs$song <- gsub("'", "", top10.songs$song)

  top10.songs$artist <- top10.songs$artist %>%
    iconv(., from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    iconv(., from = "ASCII//TRANSLIT", to = "UTF-8")
  top10.songs$song <- top10.songs$song %>%
    iconv(., from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    iconv(., from = "ASCII//TRANSLIT", to = "UTF-8")



  lyrics <- list()


  for (i in 1:10) {
    html <- try(read_html(paste0("https://genius.com/", top10.songs$artist[i], "-", top10.songs$song[i], "-lyrics")), silent = TRUE)

    if (class(html)[1] == "try-error") {
      lyrics[[i]] <- ""
    } else {
      html <- read_html(paste0("https://genius.com/", top10.songs$artist[i], "-", top10.songs$song[i], "-lyrics"))

      lyrics[[i]] <- html %>%
        html_nodes(".YYrds , .jvutUp") %>%
        html_text2() %>%
        gsub("\n", ". ", .) %>%
        gsub("\\[[^\\]]*\\]", "", ., perl = TRUE) %>%
        gsub("([[:upper:]])", " \\1", .) %>%
        gsub("  ", " ", .) %>%
        paste(., collapse = "") %>%
        gsub("-", " ", .) %>%
        gsub("'", " ", .) %>%
        paste(., collapse = "")
      names(lyrics[[i]]) <- top10.songs$song[i]
    }
  }



  lyrics.df <- lyrics %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()


  colnames(lyrics.df)[1] <- "lyrics"


  row.names(lyrics.df) <- top10.songs$song

  lyrics.df$Songs <- row.names(lyrics.df)



  corpus <- corpus(lyrics.df$lyrics)
  tk <- tokens(
    corpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE
  )
  tk <- tk %>%
    tokens_tolower() %>%
    tokens_remove(stop_words$word) %>%
    tokens_remove(stopwords(lang, source = "stopwords-iso"))
  tk <- tokens_replace(
    tk,
    pattern = hash_lemmas$token,
    replacement = hash_lemmas$lemma
  )
  dfm <- dfm(tk)
  tfidf <- dfm_tfidf(dfm)
  freq <- textstat_frequency(dfm)
  co <- fcm(tk,
            context = "document",
            tri = FALSE
  )

  if (tfgraph == TRUE) {
    graph <- freq %>%
      top_n(20, frequency) %>%
      ggplot(aes(
        x = reorder(feature, frequency),
        y = frequency
      )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("Frequency") +
      ylab("term")
    print(graph)
  }


  if (wordcloud == TRUE) {
    textplot_wordcloud(dfm)
  }
  if (environment.return == TRUE) {
    .GlobalEnv$lyrics.df <- lyrics.df
    .GlobalEnv$tk <- tk
    .GlobalEnv$corpus <- corpus
    .GlobalEnv$dfm <- dfm
    .GlobalEnv$tfidf <- freq
    .GlobalEnv$co <- co
  }

  return(lyrics.df)
}




TopAlbums <- function(name) {

  name <- name %>%
    gsub("[^[:alnum:]]", "", .) %>%
    gsub(" ", "-", .) %>%
    str_to_lower()

  html <- read_html(paste0("https://genius.com/artists/", name))

  year <- html %>%
    html_nodes(".vertical_album_card-release_date") %>%
    html_text2() %>%
    as.data.frame()
  album <- html %>%
    html_nodes(".vertical_album_card-info h3") %>%
    html_text2() %>%
    as.data.frame()


  TopAlbums <- cbind(year, album)
  colnames(TopAlbums) <- c("year", "album")
  TopAlbums$year <- as.numeric(TopAlbums$year)
  return(TopAlbums)
}