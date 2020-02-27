
library(RMySQL)
library(lpSolveAPI)
source('../r_connections.R')
source('RMySQL_UPdate.r')

mysql_schema <- 'nahsi'
whse <- 7


# ********  ACTUAL DATA  **********


query <- function(...)
  dbGetQuery(mychannel, ...)


sqlquery <- paste("SELECT 
    imprep_whse,
    imprep_build,
    1 AS imprep_level,
    1 AS imprep_zone,
    grid_tier,
    imprep_item,
    1 AS imprep_pkgu,
    grid,
    grid_length,
    CASE
        WHEN grid_tier = 'L04' THEN imprep_gridvol
        ELSE 0
    END AS VOL_BIN,
    CASE
        WHEN grid_tier = 'L02' THEN imprep_gridvol
        ELSE 0
    END AS VOL_FLOW,
    imprep_replen
FROM
    ",mysql_schema,".impliedreplen_all
        JOIN
    ",mysql_schema,".grids ON grid_whse = imprep_whse
        AND imprep_build = grid_build
        AND grid = imprep_grid
        AND grid_length = imprep_griddep
WHERE
    imprep_whse = ",whse," and grid_tier in ('L04','L02')
ORDER BY imprep_item , imprep_gridvol", sep = "")
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
                      ",mysql_schema,".recom_slotsize
                          JOIN
                      ",mysql_schema,".grids ON size_whse = grid_whse
                          AND size_build = grid_build
                          AND size_grid = grid
                          AND size_griddep = grid_length
                  WHERE size_whse = ",whse,"
                           ", sep = "")
data_usedvol <- query(sqlquery_usedvold)
used_L04 <- data_usedvol$USED_L04
used_L02 <- data_usedvol$USED_L02


flowVol <- (36481056)  #flow location volume (95% of available capacity)
binVol <- (68601971) #bin location volume (85% of available capacity)


nitems <- length(unique(data$imprep_item))
# Objective
obj <- data$imprep_replen

# Constraints
constraints <- list()

# Each month must appear once in the solution
for (item in unique(data$imprep_item)){
  constraints[[paste0('item', item)]] <- list(xt = as.numeric(data$imprep_item == item),
                                              type = "=",
                                              rhs = 1)
}

# Build model
lprec <- make.lp(0, ncol = nrow(data))
set.type(lprec, columns = seq(1,nrow(data)), type = "binary")



set.objfn(lprec, obj = obj)

for (constraint in constraints){
  add.constraint(lprec, xt = constraint$xt, type = constraint$type, rhs = constraint$rhs)
}
add.constraint(lprec, data[,"VOL_BIN"], "<=", rhs=binVol)
add.constraint(lprec, data[,"VOL_FLOW"], "<=", rhs=flowVol)
lp.control(lprec, sense="min")
lp.control(lprec, break.at.first = TRUE)
# Compute Solution
solve(lprec)

# Visualize solution
solution <- data[get.variables(lprec)==1,]
solution[,10] <- solution[,10] + solution[,11]  # add flow and bin volumes to one column
solution <- solution[,-11]  # drop flow vol column as now in bin column


 daily_replen <- sum(solution$imprep_replen)
 volused_flow <- sum(solution$VOL_FLOW)
 volused_bin <- sum(solution$VOL_BIN)
 write.lp(lprec, "filename.lp")


#write to mysql table
rmysql_update(mychannel,
              solution,
              'canada.recom_slotsize',
              verbose = FALSE)

library(xlsx)
write.xlsx(solution, "solution2.xlsx")
