test_that("Topsongslyrics returns a dataframe", {
  expect_is(Topsongslyrics(name = "Lady Gaga",
                             tfgraph = TRUE,
                             wordcloud = TRUE,
                             lang = "en",
                             environment.return = TRUE), "data.frame")
})

test_that("Topsongslyrics returns a dataframe of lenght 2", {
  expect_length(Topsongslyrics(name = "Lady Gaga",
                               tfgraph = TRUE,
                               wordcloud = TRUE,
                               lang = "en",
                               environment.return = TRUE), 2)
})

test_that("Topsongslyrics returns a dataframe of 10 rows", {
  expect_equal(nrow(Topsongslyrics(name = "Lady Gaga",
                               tfgraph = TRUE,
                               wordcloud = TRUE,
                               lang = "en",
                               environment.return = TRUE)), 10)
})

test_that("TopAlbums returns a dataframe", {
  expect_is(TopAlbums("Lana Del Rey"), "data.frame")
})

test_that("TopAlbums returns a dataframe of lenght 2", {
  expect_length(TopAlbums("Lana Del Rey"), 2)
})
