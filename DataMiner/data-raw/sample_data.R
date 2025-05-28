sample_data <- data.frame(
  id = 1:100,
  value = rnorm(100),
  group = sample(c("A","B","C"), 100, replace = TRUE)
)

usethis::use_data(sample_data, overwrite = TRUE)
