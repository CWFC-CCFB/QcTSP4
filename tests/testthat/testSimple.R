#'
#' A series of simple tests
#'

restoreQcTSP4Data()

test_that("Testing nb rows in plots", {expect_equal(nrow(QcTSP4Data$plots), 109713)})
test_that("Testing nb rows in sites", {expect_equal(nrow(QcTSP4Data$sites), 109713)})
test_that("Testing nb rows in photoInterpretedStands", {expect_equal(nrow(QcTSP4Data$photoInterpretedStands), 109713)})
test_that("Testing nb rows in trees", {expect_equal(nrow(QcTSP4Data$trees), 3919833)})
test_that("Testing nb rows in studyTrees", {expect_equal(nrow(QcTSP4Data$studyTrees), 355123)})

plotList <- c(700412604, 700412701, 700412702)

selectedTrees <- extractArtemisFormatForMetaModelling(QcTSP4Data, plotList)
test_that("Testing nb rows in selectedTrees", {expect_equal(nrow(selectedTrees), 211)})
