
library(RMySQL)
library(lpSolveAPI)
source('../r_connections.R')
source('RMySQL_UPdate.r')


# ********  ACTUAL DATA  **********


query <- function(...)
  dbGetQuery(mychannel, ...)


sqlquery <- paste("SELECT 
    size_whse,
    size_build,
    size_level,
    size_zone,
    size_tier,
    size_item,
    size_pkgu,
    size_grid,
    size_griddep,
    size_gridvol,
    size_impreplen,
    CASE
        WHEN dist_walkdist = '12' THEN size_gridvol
        ELSE 0
    END AS dist12_vol,
    CASE
        WHEN dist_walkdist = '20' THEN size_gridvol
        ELSE 0
    END AS dist20_vol,
    CASE
        WHEN dist_walkdist = '28' THEN size_gridvol
        ELSE 0
    END AS dist28_vol,
    CASE
        WHEN dist_walkdist = '36' THEN size_gridvol
        ELSE 0
    END AS dist36_vol,
    CAST((dist_walkdist * SMTH_PCK_MN / 5280 / 3 * 19) * 10000
        AS UNSIGNED) AS cost_walk
FROM
    canada.recom_slotsize
        JOIN
    canada.grids ON grid_whse = size_whse
        AND grid_build = size_build
        AND grid = size_grid
        AND grid_length = size_griddep
        AND grid_tier = size_tier
        JOIN
    canada.walk_distance ON dist_whse = size_whse
        AND dist_build = size_build
        AND dist_tier = size_tier
        JOIN
    canada.demand ON WAREHOUSE = size_whse
        AND BUILDING = size_build
        AND ITEM_NUMBER = size_item
WHERE
    PACKAGE_TYPE = 'LSE'
        AND size_tier = 'L04'", sep = "")
data <- query(sqlquery)

dist_12 <- 3000000
dist_20 <- 3000000
dist_28 <- 3000000
dist_36 <- 3000000
# dist_8 <- 9623880  This is the same as flow volume so do not need this



nitems <- length(unique(data$size_item))
# Objective
obj <- data$cost_walk

# Constraints
constraints <- list()

# Each item must appear once in the solution
for (item in unique(data$size_item)){
  constraints[[paste0('item', item)]] <- list(xt = as.numeric(data$size_item == item),
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

add.constraint(lprec, data[,"dist12_vol"], "<=", rhs=dist_12)
add.constraint(lprec, data[,"dist20_vol"], "<=", rhs=dist_20)
add.constraint(lprec, data[,"dist28_vol"], "<=", rhs=dist_28)
add.constraint(lprec, data[,"dist36_vol"], "<=", rhs=dist_36)
lp.control(lprec, sense="min")
# lp.control(lprec, timeout = 100)
lp.control(lprec, break.at.first = TRUE)
# Compute Solution
solve(lprec)

# Visualize solution
solution <- data[get.variables(lprec)==1,]

 volused_dist12_vol <- sum(solution$dist12_vol)
 volused_dist20_vol <- sum(solution$dist20_vol)
 volused_dist28_vol <- sum(solution$dist28_vol)
 volused_dist36_vol <- sum(solution$dist36_vol)
 walkcost <- sum(solution$cost_walk)

# #write to mysql table
# rmysql_update(mychannel,
#               solution,
#               'canada.recom_slotsize',
#               verbose = FALSE)
# 
# library(xlsx)
# write.xlsx(solution, "solution.xlsx")
