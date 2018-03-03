test_that("JSON Loading", {
  canvasSnippetFile <- system.file("extdata", "canvasSnippet.json", package = "GraspableDataAnalysis", mustWork = TRUE)
  canvasSnippetData <- importGraspableJson(canvasSnippetFile, version = "2.5.12", compress=F, debug=F )
  expect_equal(canvasSnippetData$trial_id[1], "_090ae6b93cebb8d0")
  expect_equal(canvasSnippetData$trial_id[100], "_0d2fe671340549a9")
  expect_equal(canvasSnippetData$trial_id[1000], "_4b3f7e03beec1b3a")
  expect_equal(canvasSnippetData$dur[149], 1087)
  expect_equal(canvasSnippetData$dur[1113], 153)
  expect_equal(canvasSnippetData$dur[2149], 885)
  expect_equal(class(canvasSnippetData), c("gm 2.5.12"
                                           , "tbl_df"
                                           , "tbl"
                                           , "data.frame"))
  canvasSnippetDataShort <- importGraspableJson(canvasSnippetFile, version = "2.5.12", compress=T, debug=F )
  expect_equal(canvasSnippetDataShort$context[1], "gm_canvas")
  expect_equal(canvasSnippetDataShort$canvasID[200], "_2733ace8704002c9")
  expect_equal(canvasSnippetDataShort$elementType[300], "derivation")
  
  expect_equal(canvasSnippetDataShort$old_state[300], "2*2*x^2*y^0")
  
})