<!-- badges: start -->
[![R-CMD-check](https://github.com/ptds2022/artists/workflows/R-CMD-check/badge.svg)](https://github.com/ptds2022/artists/actions)
<!-- badges: end -->

Install packages instructions at the end

# artists

In this repository you can download the artists package that provides the use of the following five functions:

- Topsongslyrics
- TopAlbum
- Compare.year
- Compare.lyrics
- Geniusdashboard

## Topsongslyrics

This function returns the top 10 songs of the artists you like with their lyrics. You also have the possibility to print a term frequency graph, a worlcloud and return directly in the environment the following variables: df, token, corpus, dtm, freq and covariance matrix.

## TopAlbum

This functions returns the name and the released year of the albums of the artists you like.

## Compare.year

This functions returns a density plot of the year of activity & popularity of two artists you like.

## Compare.lyrics

This function returns a plot that compares the lyrics between two artists you like.

## Geniusdashboard

This functions launches a shiny app dashboard with the following interactions:

- By choosing one artist of your choice the dashboard displays a term frequency plot, an average sentiment plot, a sentiments by song plot and a sentiments globally plot.
- By choosing two artists of your choice the dashboard displays between the two artists a comparison of a popular albums distribution, an LDA analysis with a theta plot and a phi plot, and a keyness plot.


Installing the package:

#> install.packages("devtools")

library(devtools)

install_github("ptds2022/artists")

library(artists)

#> Enjoy your new functions !
