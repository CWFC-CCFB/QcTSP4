#'
#' Script to produce the data.frame objects
#' that contain the information from
#' the original database.
#' @author Mathieu Fortin - February 2024
#'

rm(list=ls())
options(scipen=999)

source("./compilation/utilityFunctions.R")

if (!require("RODBC")) {
  install.packages("RODBC")
  require("RODBC")
}

.driverinfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
.db <- file.path(getwd(), "compilation", "PET4.mdb") # database path
.path <- paste0(.driverinfo, "DBQ=", .db)
channel <- odbcDriverConnect(.path, rows_at_time = 1)
message("Here are the tables in the database:")
sqlTables(channel, tableType = "TABLE")$TABLE_NAME

plots <- sqlFetch(channel, "PLACETTE") #### 109 713 observations
sites <- sqlFetch(channel, "STATION_PE") #### 109 713 observations
photoInterpretedStands <- sqlFetch(channel, "PEE_ORI_SOND") #### 109 713 observations
trees <- sqlFetch(channel, "DENDRO_ARBRES") #### 3 919 833 observations
studyTrees <- sqlFetch(channel, "DENDRO_ARBRES_ETUDES") #### 355 123 observations

TSP4 <- list()
TSP4$plots <- plots
TSP4$sites <- sites
TSP4$photoInterpretedStands <- photoInterpretedStands
TSP4$trees <- trees
TSP4$studyTrees <- studyTrees

saveRDS(TSP4, file = file.path(getwd(),"inst","extdata", "QcTSP4.Rds"), compress = "xz")
