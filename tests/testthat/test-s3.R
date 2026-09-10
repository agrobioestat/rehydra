test_that("rehydra_analysis S3 methods return expected object types", {
  data(rehydra_data, package = "rehydra")

  fit <- summarize_rehydra(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(stomatal_conductance),
    segmentation_method = "auto"
  )

  expect_s3_class(fit, "rehydra_analysis")
  expect_s3_class(summary(fit), "tbl_df")
  expect_s3_class(generics::tidy(fit), "tbl_df")
  expect_s3_class(generics::augment(fit), "tbl_df")
  expect_s3_class(generics::glance(fit), "tbl_df")
  expect_s3_class(plot(fit), "ggplot")
})
