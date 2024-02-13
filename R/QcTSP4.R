########################################################
# Clean database from TSP - fourth campaign - data.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: February 2024
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to QcTSP4!")
  packageStartupMessage("The QcTSP4 package provides a clean version of the TSP of the fourth campaign of Quebec provincial inventory.")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
}

.onDetach <- function(libpath) {
}


.loadPackageData <- function(filename) {
  return(readRDS(system.file(paste0("extdata/",filename,".Rds"), package = "QcTSP4")))
}

#'
#' Restore Quebec TSP Data in the Global Environment.
#'
#' @description This function call creates four data.frame objects that contain
#' the tree measurements.
#'
#' @details
#'
#' The resulting list encompasses five data.frame objects are: \cr
#' \itemize{
#' \item plots: the list of plots \cr
#' \item sites: some site information recorded in those plots \cr
#' \item photoInterpretedStands: some site information recorded through photo-interpretation \cr
#' \item trees: the talled trees \cr
#' \item studyTrees: the study trees (a subsample of trees) \cr
#' }
#'
#' @export
restoreQcTSP4Data <- function() {
  assign("QcTSP4Data", .loadPackageData("QcTSP4"), envir = .GlobalEnv)
}







