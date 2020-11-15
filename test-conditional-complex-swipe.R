context("style & syntax of swipe_right")

# get syntax info of swipe function:
swipe_source <- parse(text = deparse(swipe_right))
swipe_parsed <- utils::getParseData(swipe_source)

test_that("all <else>-blocks have been removed", {
  expect_true(all(swipe_parsed[["token"]] != "ELSE"))
})
test_that("has at most 20 lines", {
  expect_lt(max(swipe_parsed[["line2"]]),
            20)
})
test_that("avoids superfluous <return> in final statement", {
  swipe_last_lines <- subset(swipe_parsed,
                             line1 >= max(line1) - 2)
  expect_false("return" %in% swipe_last_lines[["text"]])
})
