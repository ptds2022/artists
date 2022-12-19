#' @title Top songs lyrics
#' @author Emile Paris
#' @param name The name of the artist of your choice.
#' @param tfgraph If True, returns a term-frequency graph.
#' @param wordcloud If True, returns a worldcloud.
#' @param lang Language of the lyrics.
#' @param environment.return If True, saves all computations in global environment.
#' @param local determine if the function is run locally or not.
#' @return returns a dataframe with the 10 top songs and the lyrics for a given  artist (name) speaking in a special language (lang). Possibility to print a Term frequency graph and a wordcloud. Also, possibility to return directly in the environment the df, token, corpus, dtm, freq, and covariance matrix.
#' @import magrittr stringr rvest quanteda quanteda.textstats lexicon tidytext ggplot2 dplyr quanteda.textplots geniusr
#' @export
#' @examples
#' \dontrun{
#' Topsongslyrics(name = "Lady Gaga",
#'                tfgraph = TRUE,
#'                wordcloud = TRUE,
#'                lang = "en",
#'                environment.return = TRUE,
#'                local = TRUE)
#' }
Topsongslyrics <- function(name, tfgraph = FALSE, wordcloud = FALSE, lang, environment.return = TRUE, local = TRUE) {
  if (local == TRUE) {
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
  } else {
    lyrics.df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
      .[[1, 1]] %>%
      get_artist_songs_df(
        .,
        sort = "popularity",
        include_features = FALSE,
        access_token = genius_token()
      )

    lyrics.df$lyrics <- ""

    for (i in seq(nrow(lyrics.df))) {
      a <- get_lyrics_id(lyrics.df$song_id[i], access_token = genius_token())
      lyrics.df$lyrics[i] <- paste(a$line, collapse = " ")
    }
    lyrics.df <- lyrics.df[, c(2, 8)]
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
  dfm <- quanteda::dfm(tk)
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
    b <- textplot_wordcloud(dfm)
  }
  if (environment.return == TRUE) {
    .GlobalEnv$lyrics.df <- lyrics.df
    .GlobalEnv$tk <- tk
    .GlobalEnv$corpus <- corpus
    .GlobalEnv$dfm <- dfm
    .GlobalEnv$tfidf <- freq
    .GlobalEnv$co <- co
  }

  my_list <- list("lyrics.df"=lyrics.df,"tk" = tk, "corpus"=corpus,"dfm"=dfm, "freq"=freq, "co"=co, "b"=b , "graph"=graph)
  return(my_list)


}



#' @title Top Albums
#' @author Emile Paris
#' @param name The name of the artist of your choice.
#' @param local determine if the function is run locally or not.
#' @return Return the name of the albums and the year of release of a specific artist (name).
#' @import rvest stringr magrittr geniusr
#' @export
#' @examples
#' \dontrun{
#' TopAlbums("Lana Del Rey", local = TRUE)
#' }
TopAlbums <- function(name, local = TRUE) {
  if (local == TRUE) {
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
  } else {
    df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
      .[[1, 1]] %>%
      get_artist_songs_df(
        .,
        sort = "popularity",
        include_features = FALSE,
        access_token = genius_token()
      )
    TopAlbums <- data.frame(year = c("", "", "", "", "", "", "", ""), album = c("", "", "", "", "", "", "", ""))
    for (i in seq(nrow(df))) {
      a <- df$song_id[i] %>% get_song(., access_token = genius_token())
      if (is.null(a$content$album$id)) {
        TopAlbums$album[i] <- b$content$name
      } else {
        b <- a$content$album$id %>% get_album(., access_token = genius_token())
        TopAlbums$album[i] <- b$content$name
        TopAlbums$year[i] <- b$content$release_date
      }
    }
  }



  return(TopAlbums)
}


#' @title Compare year
#' @author Emile Paris
#' @param name.x The name of the first artist of your choice.
#' @param name.y The name of the second artist of your choice.
#' @param local determine if the function is run locally or not.
#' @return return a density plot of the year of activity & popularity of the two artists.
#' @import rvest stringr magrittr plyr dplyr ggplot2 hrbrthemes plotly geniusr
#' @export
#' @examples
#' \dontrun{
#' compare.year("lady gaga", "lana del rey", local = TRUE)
#' }
compare.year <- function(name.x, name.y, local = TRUE) {

  name <- year <- NULL

  x <- TopAlbums(name.x, local)
  x$name <- name.x

  y <- TopAlbums(name.y, local)
  y$name <- name.y

  compare.df <- rbind(x, y)

  compare.df <- compare.df %>%
    group_by(name, year) %>%
    count()

  plot <- ggplot(data = compare.df, aes(x = year, group = name, fill = name)) +
    geom_density(adjust = 1.5, alpha = .4) +
    theme_ipsum()

  ggplotly(plot)
}





#' @title Compare lyrics
#' @author Emile Paris
#' @param name.x The name of the first artist of your choice.
#' @param name.y The name of the second artist of your choice.
#' @param language The language of the lyrics you want.
#' @return returns a plot that compares the lyrics between the two artists.
#' @import magrittr stringr rvest quanteda quanteda.textstats lexicon tidytext ggplot2 dplyr quanteda.textplots seededlda reshape2 geniusr
#' @export
#' @examples
#' \dontrun{
#' compare.lyrics("lady gaga", "lana del rey")
#' }
compare.lyrics <- function(name.x, name.y, language = 'en'){

  lyrics <- NULL

  a <- Topsongslyrics(name = name.x, tfgraph = TRUE, wordcloud = TRUE, lang = language, environment.return = FALSE)
  A <- tibble(name = name.x)
  A$lyrics <- a$lyrics.df %>% summarize(lyrics = paste(lyrics, collapse = ","))

  b <- Topsongslyrics(name = name.y, tfgraph = TRUE, wordcloud = TRUE, lang = language, environment.return = FALSE)
  B <- tibble(name = name.y)
  B$lyrics <- b$lyrics.df %>% summarize(lyrics = paste(lyrics, collapse = ","))

  compare <- rbind(A, B)

  cp <- corpus(compare$lyrics$lyrics)
  tk <- tokens(
    cp,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE
  )
  tk <- tk %>%
    tokens_tolower() %>%
    tokens_remove(tidytext::stop_words$word)

  dfm <- dfm(tk)

  keyness <- textstat_keyness(
    dfm,
    target = "text1"
  )

  # LDA
  lda <- textmodel_lda(
    x = dfm,
    k = 3
  )

  seededlda::terms(lda, 5)

  phi.long <- reshape2::melt(
    lda$phi,
    varnames = c("Topic", "Term"),
    value.name = "Phi"
  )

  theta.long <- reshape2::melt(
    lda$theta,
    varnames = c("Doc", "Topic"),
    value.name = "Theta"
  )


  # Return a list containing the keyness plot and the phi.long data frame
  my_list= list(keyness_plot = textplot_keyness(keyness, labelsize = 4), phi.long = phi.long, theta.long=theta.long)
  return(my_list)

}







#' @title Geniusdashboard
#' @author Emile Paris
#' @param artists a dataframe with one column containing the artists you want.
#' @param lang a dataframe with one column containing the abrevations of the languages you want (example: English must be "en").
#' @return returns a dashboard
#' @import shiny shinydashboard plotrix sentimentr RColorBrewer magrittr stringr rvest quanteda quanteda.textstats lexicon tidytext ggplot2 quanteda.textplots geniusr dplyr hrbrthemes plotly seededlda reshape2
#' @export
#' @examples
#' \dontrun{
#' Geniusdashboard(artists.df, lang.df)
#' }
Geniusdashboard <- function(artists, lang) {
  packages <- c("shiny", "shinydashboard", "magrittr", "plotrix", "sentimentr", "RColorBrewer", "stringr", "rvest", "quanteda", "quanteda.textstats", "lexicon", "tidytext", "ggplot2", "quanteda.textplots", "geniusr", "dplyr", "hrbrthemes", "plotly", "seededlda", "reshape2")
  lapply(packages, require, character.only = TRUE)
  stop_words <- hash_lemmas <- frequency <- reorder <- feature <- name <- year <- lyrics <- element_id <- Songs <- Total <- relfreq <- compare_artists <- Topic <- Phi <- Term <- Theta <- Doc <- NULL
  ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Text Analysis and Comparison Tool", titleWidth = 350),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Text Analysis", tabName = "text_analysis", icon = icon("file-text")),
        menuItem("Comparison", tabName = "comparison", icon = icon("bar-chart"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "text_analysis",
          fluidRow(
            sidebarPanel(
              # Inputs: name of artist and language
              selectInput("artist", "Name of Artist", choices = artists$Artist),
              selectInput("language", "Language", choices = lang$`stopwords::stopwords_getlanguages("stopwords-iso")`, selected = "en"),
              actionButton("analysis", "Analyze", icon = icon("music"))
            ),
            mainPanel(
              # Outputs: plots of text analysis results
              box(
                title = "Term Frequency Plot", status = "primary",
                plotOutput("tfidf_plot")
              ),
              box(
                title = "Wordcloud", status = "primary",
                plotOutput("wordcloud")
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  title = "Sentiment Analysis", id = "Tabset",
                  tabPanel(
                    "Average Sentiment", "Sentiment 1",
                    plotOutput("sentiment_by_song_plot")
                  ),
                  tabPanel(
                    "Sentiments by Song", "Sentiment 2",
                    plotOutput("sentiment_general_plot")
                  ),
                  tabPanel(
                    "Sentiments Globally", "Sentiment 3",
                    plotOutput("sentiment_general2_plot")
                  )
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "comparison",
          fluidRow(
            sidebarPanel(
              # Inputs: names of two artists and language
              selectInput("artist1", "Name of Artist 1", choices = artists$Artist, selected = "ABBA"),
              selectInput("artist2", "Name of Artist 2", choices = artists$Artist, selected = "Ariana Grande"),
              selectInput("language2", "Language", choices = lang$`stopwords::stopwords_getlanguages("stopwords-iso")`, selected = "en"),
              actionButton("analysis1", "Analyze", icon = icon("music"))
            ),
            mainPanel(
              # Outputs: plots of comparison results
              box(
                width = 12, title = "Popular albums Distribution", status = "primary",
                plotOutput("popularity")
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  title = "LDA - Theme Analysis", id = "Tabset LDA",
                  tabPanel(
                    "Theta plot", "LDA 1",
                    plotOutput("theta_plot")
                  ),
                  tabPanel(
                    "Phi plot", "LDA 2",
                    plotOutput("phi_plot")
                  )
                )
              ),
              box(
                width = 12, title = "Keyness plot", status = "primary",
                plotOutput("compare_year_plot")
              )
            )
          )
        )
      )
    )
  )



  # Define server logic
  server <- function(input, output) {
    options(shiny.trace = TRUE)
    # Placeholder function for text analysis of  artist
    Topsongslyrics <- function(name, tfgraph = FALSE, wordcloud = FALSE, lang, environment.return = TRUE, local = TRUE) {
      if (local == TRUE) {
        name <- name %>%
          gsub("[^ a-zA-Z0-9]", "", .) %>%
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
      } else {
        lyrics.df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
          .[[1, 1]] %>%
          get_artist_songs_df(
            .,
            sort = "popularity",
            include_features = FALSE,
            access_token = genius_token()
          )

        lyrics.df$lyrics <- ""

        for (i in seq(nrow(lyrics.df))) {
          a <- get_lyrics_id(lyrics.df$song_id[i], access_token = genius_token())
          lyrics.df$lyrics[i] <- paste(a$line, collapse = " ")
        }
        lyrics.df <- lyrics.df[, c(2, 8)]
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
        tokens_remove(stop_words$word) %>%
        tokens_remove(stopwords(lang, source = "stopwords-iso"))
      tk <- tokens_replace(
        tk,
        pattern = hash_lemmas$token,
        replacement = hash_lemmas$lemma
      )
      dfm <- quanteda::dfm(tk)
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
        b <- textplot_wordcloud(dfm)
      }
      if (environment.return == TRUE) {
        .GlobalEnv$lyrics.df <- lyrics.df
        .GlobalEnv$tk <- tk
        .GlobalEnv$corpus <- corpus
        .GlobalEnv$dfm <- dfm
        .GlobalEnv$tfidf <- freq
        .GlobalEnv$co <- co
      }

      my_list <- list("lyrics.df" = lyrics.df, "tk" = tk, "corpus" = corpus, "dfm" = dfm, "freq" = freq, "co" = co, "b" = b, "graph" = graph)
      return(my_list)
    }


    # Placeholder function for Album analysis
    TopAlbums <- function(name, local = FALSE) {
      if (local == TRUE) {
        name <- name %>%
          gsub("[^ a-zA-Z0-9]", "", .) %>%
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
      } else {
        df <- search_artist(name, n_results = 3, access_token = genius_token()) %>%
          .[[1, 1]] %>%
          get_artist_songs_df(
            .,
            sort = "popularity",
            include_features = FALSE,
            access_token = genius_token()
          )
        TopAlbums <- data.frame(year = c("", "", "", "", "", "", "", ""), album = c("", "", "", "", "", "", "", ""))
        for (i in seq(nrow(df))) {
          a <- df$song_id[i] %>% get_song(., access_token = genius_token())
          if (is.null(a$content$album$id)) {
            TopAlbums$album[i] <- b$content$name
          } else {
            b <- a$content$album$id %>% get_album(., access_token = genius_token())
            TopAlbums$album[i] <- b$content$name
            TopAlbums$year[i] <- b$content$release_date
          }
        }
      }



      return(TopAlbums)
    }
    # Placeholder function for comparison of two artist
    compare.year <- function(name.x, name.y, local = TRUE) {
      x <- TopAlbums(name.x, local)
      x$name <- name.x

      y <- TopAlbums(name.y, local)
      y$name <- name.y

      compare.df <- rbind(x, y)

      compare.df <- compare.df %>%
        group_by(name, year) %>%
        count()

      plot <- ggplot(data = compare.df, aes(x = year, group = name, fill = name)) +
        geom_density(adjust = 1.5, alpha = .4) +
        theme_ipsum()

      ggplotly(plot)
    }
    # Placeholder function for comparison of two artist

    compare.lyrics <- function(name.x, name.y, language) {
      a <- Topsongslyrics(name = name.x, tfgraph = TRUE, wordcloud = TRUE, lang = language, environment.return = FALSE)
      A <- tibble(name = name.x)
      A$lyrics <- a$lyrics.df %>% summarize(lyrics = paste(lyrics, collapse = ","))

      b <- Topsongslyrics(name = name.y, tfgraph = TRUE, wordcloud = TRUE, lang = language, environment.return = FALSE)
      B <- tibble(name = name.y)
      B$lyrics <- b$lyrics.df %>% summarize(lyrics = paste(lyrics, collapse = ","))

      compare <- rbind(A, B)

      cp <- corpus(compare$lyrics$lyrics)
      tk <- tokens(
        cp,
        remove_numbers = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_separators = TRUE
      )
      tk <- tk %>%
        tokens_tolower() %>%
        tokens_remove(stop_words$word) %>%
        tokens_remove(stopwords(language, source = "stopwords-iso"))

      dfm <- dfm(tk)

      keyness <- textstat_keyness(
        dfm,
        target = "text1"
      )

      # LDA
      lda <- textmodel_lda(
        x = dfm,
        k = 3
      )

      seededlda::terms(lda, 5)

      phi.long <- reshape2::melt(
        lda$phi,
        varnames = c("Topic", "Term"),
        value.name = "Phi"
      )

      theta.long <- reshape2::melt(
        lda$theta,
        varnames = c("Doc", "Topic"),
        value.name = "Theta"
      )


      # Return a list containing the keyness plot and the phi.long data frame
      my_list <- list(keyness_plot = textplot_keyness(keyness, labelsize = 4), phi.long = phi.long, theta.long = theta.long)
      return(my_list)
    }

    observeEvent(input$analysis, {
      # Reactive expression to perform text analysis of first artist
      withProgress(message = "Computing text analysis...", detail = "This may take a while...", value = 0, {
        artist_results <- reactive({
          Topsongslyrics(name = input$artist, lang = input$language, local = TRUE, environment.return = TRUE, tfgraph = TRUE, wordcloud = TRUE)
          # Update progress bar
        })
        incProgress(1 / 3)
      })


      # Output text analysis plots for first artist
      output$tfidf_plot <- renderPlot({
        withProgress(message = "Generating TF-IDF plot...", detail = "This may take a while...", value = 1 / 5, {
          incProgress(1 / 5)
          isolate(artist_results()$graph)
        })
        # Generate plot using results from artist1_results reactive expression
      })
      output$wordcloud <- renderPlot({
        withProgress(message = "Generating word cloud...", detail = "This may take a while...", value = 2 / 5, {
          incProgress(1 / 5)
          isolate(artist_results()$dfm %>% textplot_wordcloud())
          # Generate plot using results from artist1_results reactive expression
          # Update progress bar
        })
      })
      output$sentiment_by_song_plot <- renderPlot({
        withProgress(message = "Generating sentiment by song plot...", detail = "This may take a while...", value = 3 / 5, {
          # Generate plot using results from artist1_results reactive expression
          my_colors <- brewer.pal(11, "RdYlGn")
          my_color <- c(my_colors[1], my_colors[3], my_colors[7], my_colors[8], my_colors[11])
          sent <- isolate(sentiment_by(artist_results()$lyrics.df$lyrics))

          sent <- sent %>%
            arrange(element_id) %>%
            tibble()

          clplot(sent$element_id, sent$ave_sentiment, main = "Average sentiment in top songs", ylab = "Average sentiment Score", xlab = "Rank of the Song in Genius", lwd = 5, levels = c(seq(-1, 1, length.out = 5)), bty = "n", col = my_color, showcuts = T, )
          incProgress(1 / 5)
        })
      })
      output$sentiment_general_plot <- renderPlot({
        withProgress(message = "Generating sentiment plot...", detail = "This may take a while...", value = 4 / 5, {
          isolate(
            lyrics.tb <- as_tibble(
              data.frame(
                artist_results()$lyrics.df
              )
            )
          )

          lyrics.tok <- unnest_tokens(
            lyrics.tb,
            output = "word",
            input = "lyrics",
            to_lower = TRUE,
            strip_punct = TRUE,
            strip_numeric = TRUE
          )

          lyrics.sent <-
            inner_join(
              lyrics.tok,
              get_sentiments("nrc"),
              by = c("word" = "word")
            )


          # totals by document
          lyrics.sent.doc.total <-
            lyrics.sent %>%
            group_by(Songs) %>%
            dplyr::summarize(Total = n()) %>%
            ungroup()
          # Generate plot using results from artist_results reactive expression
          incProgress(1 / 5)
          left_join(
            lyrics.sent,
            lyrics.sent.doc.total
          ) %>%
            group_by(Songs, sentiment) %>%
            dplyr::summarize(
              n = n(),
              Total = unique(Total)
            ) %>%
            ungroup() %>%
            mutate(relfreq = n / Total) %>%
            ggplot(aes(
              x = sentiment,
              y = relfreq,
              fill = sentiment
            )) +
            geom_bar(stat = "identity", alpha = 0.8) +
            facet_wrap(~Songs) +
            coord_flip()
        })
      })

      output$sentiment_general2_plot <- renderPlot({
        withProgress(message = "Generating sentiment plot 2...", detail = "This may take a while...", value = 1, {
          isolate(
            lyrics.tb <- as_tibble(
              data.frame(
                artist_results()$lyrics.df
              )
            )
          )

          lyrics.tok <- unnest_tokens(
            lyrics.tb,
            output = "word",
            input = "lyrics",
            to_lower = TRUE,
            strip_punct = TRUE,
            strip_numeric = TRUE
          )

          lyrics.sent <-
            inner_join(
              lyrics.tok,
              get_sentiments("nrc"),
              by = c("word" = "word")
            )


          # totals by document
          lyrics.sent.doc.total <-
            lyrics.sent %>%
            group_by(Songs) %>%
            dplyr::summarize(Total = n()) %>%
            ungroup()
          incProgress(1 / 5)

          left_join(
            lyrics.sent,
            lyrics.sent.doc.total
          ) %>%
            group_by(Songs, sentiment) %>%
            dplyr::summarize(
              n = n(),
              Total = unique(Total)
            ) %>%
            ungroup() %>%
            mutate(relfreq = n / Total) %>%
            ggplot(aes(
              x = sentiment,
              y = relfreq,
              fill = sentiment
            )) +
            geom_bar(stat = "identity", alpha = 0.8) +
            coord_flip()
        })
      })
    })


    observeEvent(input$analysis1, {
      # Reactive expression to perform text analysis of second artist
      artist1_results <- reactive({
        Topsongslyrics(name = input$artist1, lang = input$language2, local = TRUE, environment.return = TRUE, tfgraph = TRUE, wordcloud = TRUE)
      })
      artist2_results <- reactive({
        Topsongslyrics(name = input$artist2, lang = input$language2, local = TRUE, environment.return = TRUE, tfgraph = TRUE, wordcloud = TRUE)
      })
      # Reactive expression to compare two artists
      comparison_results <- reactive({
        compare_artists(input$artist1, input$artist2, input$language)
      })

      plot_reactive <- reactive({
        isolate(
          name.x <- input$artist1
        )
        x <- TopAlbums(name.x, local = TRUE)
        x$name <- name.x
        isolate(
          name.y <- input$artist2
        )

        y <- TopAlbums(name.y, local = TRUE)
        y$name <- name.y

        compare.df <- rbind(x, y)

        compare.df <- compare.df %>%
          group_by(name, year) %>%
          count()

        plot <- ggplot(data = compare.df, aes(x = year, group = name, fill = name)) +
          geom_density(adjust = 1.5, alpha = .4) +
          theme_ipsum()
        plot
        # Generate plot using results from artist1_results reactive expression
      })

      compare_lyrics_reactive <- reactive({
        compare.lyrics(input$artist1, input$artist2, input$language2)
      })

      output$compare_year_plot <- renderPlot({
        withProgress(message = "Computing 1st Comparison plot...", detail = "This may take a while...", value = 0, {
          incProgress(1 / 4)
          isolate(
            compare_lyrics_reactive()$keyness_plot
          )
        })
      })
      output$popularity <- renderPlot({
        withProgress(message = "Computing 2nd Comparison plot...", detail = "This may take a while...", value = 2 / 4, {
          incProgress(1 / 4)
          isolate(
            plot_reactive()
          )
        })
      })

      output$phi_plot <- renderPlot({
        withProgress(message = "Computing 3rd Comparison plot...", detail = "This may take a while...", value = 3 / 4, {
          incProgress(1 / 4)
          isolate(
            compare_lyrics_reactive()$phi.long %>%
              group_by(Topic) %>%
              top_n(10, Phi) %>%
              ggplot(aes(reorder_within(Term, Phi, Topic), Phi)) +
              geom_col(show.legend = FALSE) +
              coord_flip() +
              facet_wrap(~Topic, scales = "free_y") +
              scale_x_reordered() +
              xlab("Term") +
              theme(
                axis.text.y = element_text(size = 12),
                strip.text = element_text(size = 12)
              )
          )
        })
      })

      output$theta_plot <- renderPlot({
        withProgress(message = "Computing the 4th Comparison plot...", detail = "This may take a while...", value = 1, {
          incProgress(1 / 4)
          isolate(
            compare_lyrics_reactive()$theta.long %>%
              group_by(Topic) %>%
              top_n(10, Theta) %>%
              ggplot(aes(reorder_within(Doc, Theta, Topic, fill = Topic), Theta)) +
              geom_col(show.legend = FALSE, aes(fill = Topic)) +
              coord_flip() +
              facet_wrap(~Topic, scales = "free_y") +
              scale_x_reordered() +
              xlab("Document") +
              theme(
                axis.text.y = element_text(size = 12),
                strip.text = element_text(size = 12)
              )
          )
        })
      })
    })
  }
  shinyApp(ui, server)
}
