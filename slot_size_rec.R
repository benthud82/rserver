# WORKING MODEL!!
# Compare to: https://docs.google.com/spreadsheets/d/1Yx3oSBagQbzYmVjjKRYbAMl7vTdGDFQHybM4fjzmMIA/edit#gid=0
library(RMySQL)
library(lpSolve)
library(reshape)
source('../r_connections.R')


i = 5 #number of items, can do this as a count

cost2 <- matrix(c(27,20,15,7,1,16,12,9,4,1,70,52,39,19,1,290,217,162,81,8,141,105,78,39,3),nrow = 5, byrow = TRUE)
vols <- matrix(c(10,20,50,100,200),nrow = 1, byrow = TRUE)

f.obj <- c(cost)

#constraints
# c1 <- cbind(diag(5), diag(5), diag(5), diag(5), diag(5)) #each item must have loc
c1 <- t(sapply(1:i, function(i) c(row(cost)) == i)) 
c2 <- rep(c(10,20,50,100,200), each=5) #sum of volume must be less than xvol

f.con<-rbind(c1, c2)
f.dir <- c(rep("=",5), "<=")
f.rhs <- c(rep(1,5), 300)


res <- lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=T)
res$solution
sol_matrix <- matrix(res$solution,nrow = 5)



# ***** ACTUAL DATA MODEL*******

query <- function(...)
  dbGetQuery(mychannel, ...)


sqlquery <- paste("SELECT 
          CAST(imprep_replen * 253 AS UNSIGNED) AS imprep_replen
        FROM
            canada.impliedreplen_all
        WHERE imprep_whse = 16
        ORDER BY imprep_item , imprep_gridvol
        ", sep = "")
data <- query(sqlquery)
cntrow = nrow(data)
data <- as.numeric(unlist(data))



cost <- matrix(data,nrow = cntrow/11, byrow = TRUE)

i <- nrow(cost)


f.obj <- c(cost)

c1 <- t(sapply(1:i, function(i) c(row(cost)) == i)) 
c2 <- rep(c(144,280,288,576,672,864,2640,3696,4752,7392,9504), each=i) #sum of volume must be less than xvol

f.con<-rbind(c1, c2)
f.dir <- c(rep("=",i), "<=")

f.rhs <- c(rep(1,i), 12605168)


res <- lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=T)
res$solution
sol_matrix <- matrix(res$solution,nrow = i)

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

