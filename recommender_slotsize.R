
library(RMySQL)
library(lpSolve)
source('../r_connections.R')


# ********  ACTUAL DATA  **********


query <- function(...)
  dbGetQuery(mychannel, ...)


sqlquery <- paste("SELECT 
                      imprep_id,
                      imprep_item,
                      CAST(imprep_replen * 253 AS UNSIGNED) AS imprep_replen,
                      CASE
                          WHEN grid_tier = 'L04' THEN imprep_gridvol
                          ELSE 0
                      END AS VOL_BIN,
                      CASE
                          WHEN grid_tier = 'L02' THEN imprep_gridvol
                          ELSE 0
                      END AS VOL_FLOW,
                      1 AS total
                  FROM
                      canada.impliedreplen_all
                          JOIN
                      canada.grids ON grid_whse = imprep_whse
                          AND imprep_build = grid_build
                          AND grid = imprep_grid
                          AND grid_length = imprep_griddep
                  WHERE
                      imprep_whse = 16
                  ORDER BY imprep_item , imprep_gridvol
                  LIMIT 1600", sep = "")
data <- query(sqlquery)

#calc available volume
sqlquery_usedvold <- paste("SELECT 
                      SUM(CASE
                          WHEN size_tier = 'L04' THEN size_gridvol
                          ELSE 0
                      END) AS USED_L04,
                      SUM(CASE
                          WHEN size_tier = 'L02' THEN size_gridvol
                          ELSE 0
                      END) AS USED_L02
                  FROM
                      canada.recom_slotsize
                          JOIN
                      canada.grids ON size_whse = grid_whse
                          AND size_build = grid_build
                          AND size_grid = grid
                          AND size_griddep = grid_length
                  WHERE size_whse = 16
                           ", sep = "")
data_usedvol <- query(sqlquery_usedvold)
used_L04 <- data_usedvol$USED_L04
used_L02 <- data_usedvol$USED_L02

# count unique items and set item count variable
df_uniq <- unique(data$imprep_item)
cnt_item <- length(df_uniq)

#dummy variable to restrict 1 grid per item is selected
type1Dummy <- as.data.frame.matrix(table(data$imprep_id, data$imprep_item))
data <- cbind(data, type1Dummy)



# constraints
totalItems <- cnt_item # limit selection to item count
flowVol <- (9623880 - used_L02) / 10  #flow location volume (95% of available capacity)
binVol <- (10211736 - used_L04) / 10 #bin location volume (85% of available capacity)
totalX <- c(rep(1,totalItems)) #each item must be selected once
rhs <- c(totalItems, flowVol, binVol, totalX)

# Direction vector
dirItem <- '=='
dirflow <- '<='
dirbin <- '<='
dirX <- c(rep('==',totalItems))
dir <- c(dirItem, dirflow, dirbin, dirX)

# Setup opt
obj <- data$imprep_replen
mat <- t(data.frame(data$total, data$VOL_FLOW,data$VOL_BIN, type1Dummy))


# Solver Setup
sol <- lpSolve::lp("min",   
                   objective.in = obj,
                   const.mat  = mat,
                   const.dir  = dir,
                   const.rhs  = rhs,
                   all.bin    = T
)

data$selected <- sol$solution
dfSolved <- data[data$selected == 1,]
dfSolved

daily_replen <- sum(dfSolved$imprep_replen) / 253
volused_flow <- sum(dfSolved$VOL_FLOW)
volused_bin <- sum(dfSolved$VOL_BIN)
