test_that("Topsongslyrics returns a list", {
  expect_type(Topsongslyrics(name = "Lady Gaga",
                             tfgraph = TRUE,
                             wordcloud = TRUE,
                             lang = "en",
                             environment.return = TRUE), "list")
})

test_that("Topsongslyrics returns a list of lenght 8", {
  expect_length(Topsongslyrics(name = "Lady Gaga",
                               tfgraph = TRUE,
                               wordcloud = TRUE,
                               lang = "en",
                               environment.return = TRUE), 8)
})

test_that("TopAlbums returns a dataframe", {
  expect_is(TopAlbums("Lana Del Rey"), "data.frame")
})

test_that("TopAlbums returns a dataframe of lenght 2", {
  expect_length(TopAlbums("Lana Del Rey"), 2)
})

test_that("Compare.year returns a list", {
  expect_type(compare.year("lady gaga", "lana del rey"), "list")
})

test_that("Compare.year returns a list of length 8", {
  expect_length(compare.year("lady gaga", "lana del rey"), 8)
})

test_that("Compare.lyrics returns a list", {
  expect_type(compare.lyrics("lady gaga", "lana del rey"), "list")
})

test_that("Compare.lyrics returns a list of length of 8", {
  expect_length(compare.lyrics("lady gaga", "lana del rey"), 3)
})


