#' @title Top songs lyrics
#' @author Emile Paris
#' @param name The name of the artist of your choice.
#' @param tfgraph If True, returns a term-frequency graph.
#' @param wordcloud If True, returns a worldcloud.
#' @param lang Language of the lyrics.
#' @param environment.return If True, saves all computations in global environment.
#' @param local determine if the function is run locally or not.
#' @return returns the top 10 songs of your artist with the lyrics.
#' @import magrittr stringr rvest quanteda quanteda.textstats lexicon tidytext ggplot2 dplyr quanteda.textplots geniusr
#' @export
#' @examples
#'Topsongslyrics(name = "Lady Gaga",
#'                tfgraph = TRUE,
#'                wordcloud = TRUE,
#'                lang = "en",
#'                environment.return = TRUE,
#'                local = TRUE)
Topsongslyrics <- function(name, tfgraph = FALSE, wordcloud = FALSE, lang, environment.return = TRUE, local=TRUE) {

  if (local==TRUE){
    name <- name %>%
      gsub("[^ a-zA-Z0-9]", "", .) %>%
      gsub(" ", "-", .) %>%
      str_to_lower()

    feature <- "." <- frequency <- reorder <- NULL

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
  }
  else{


    lyrics.df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
      .[[1,1]] %>%
      get_artist_songs_df(
        .,
        sort = "popularity",
        include_features = FALSE,
        access_token = genius_token()
      )

    lyrics.df$lyrics <- ''

    for (i in seq(nrow(lyrics.df))){
      a <- get_lyrics_id(lyrics.df$song_id[i], access_token = genius_token())
      lyrics.df$lyrics[i] <- paste(a$line, collapse = " ")
    }
    lyrics.df <- lyrics.df[, c(2,8)]
    return(lyrics.df)
  }



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
    tokens_remove(tidytext::stop_words$word) %>%
    tokens_remove(stopwords(lang, source = "stopwords-iso"))
  tk <- tokens_replace(
    tk,
    pattern = lexicon::hash_lemmas$token,
    replacement = lexicon::hash_lemmas$lemma
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



#' @title Top Albums
#' @author Emile Paris
#' @param name The name of the artist of your choice.
#' @param local determine if the function is run locally or not.
#' @return returns the albums and the release date of your artist.
#' @import rvest stringr magrittr geniusr
#' @export
#' @examples
#'TopAlbums("Lana Del Rey", local = TRUE)
TopAlbums <- function(name, local = TRUE) {

  if(local==TRUE){
    name <- name %>%
      gsub("[^ a-zA-Z0-9]", "", .) %>%
      gsub(" ", "-", .) %>%
      str_to_lower()

    "." <- NULL

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
  }
  else {

    df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
      .[[1,1]] %>%
      get_artist_songs_df(
        .,
        sort =  "popularity",
        include_features = FALSE,
        access_token = genius_token()
      )
    TopAlbums <- data.frame(year= c('', '', "", "","","","", ""), album=c('', '', "", "","","","", ""))
    for (i in seq(nrow(df))){

      a <-df$song_id[i] %>% get_song(., access_token = genius_token())
      if(is.null(a$content$album$id)){
        TopAlbums$album[i] <- b$content$name
      }
      else{
        b <- a$content$album$id %>% get_album(., access_token = genius_token())
        TopAlbums$album[i] <- b$content$name
        TopAlbums$year[i] <- b$content$release_date
      }


    }
  }



  return(TopAlbums)
}
