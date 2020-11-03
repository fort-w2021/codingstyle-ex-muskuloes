test_swipers <- list(
  sober_swiper = list(sober = TRUE, likes = c("penguins", "FORTRAN",  "casual sex")),
  drunk_swiper = list(sober = FALSE, likes = c("dogs", "yoga", "Markus Söder"))
)

test_profiles <- list(
  hot_1 = list(has_picture = TRUE, picture_attractive = TRUE,
               rather_weird = FALSE, likes = "penguins"),
  hot_2 = list(has_picture = TRUE, picture_attractive = TRUE,
               rather_weird = FALSE, likes = "Pokemon"),
  hotweird = list(has_picture = TRUE, picture_attractive = TRUE,
                  rather_weird = TRUE, likes = "penguins"),
  nothot_1 = list(has_picture = TRUE, picture_attractive = FALSE,
                  rather_weird = FALSE, likes = "Markus Söder"),
  nothot_2 = list(has_picture = TRUE, picture_attractive = FALSE,
                  rather_weird = FALSE, likes = "cats"),
  no_pic = list(has_picture = FALSE, picture_attractive = NA,
                likes = c("hamsters", "yoga")),
  no_likes = list(has_picture = TRUE, picture_attractive = TRUE,
                  rather_weird = FALSE, likes = NA)
)


for(swiper in names(test_swipers)) {
  for(profile in names(test_profiles)) {
    cat("Does", swiper, "swipe right on", profile, "?",
        try(swipe_right(test_swipers[[swiper]], test_profiles[[profile]]), silent = TRUE),
        "\n")
  }
}

