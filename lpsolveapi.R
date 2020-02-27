library(lpSolveAPI)

dataset <- data.frame(item=LETTERS[1:8], sales=c(1200, 5600, 450, 990, 1000, 560, 500, 2000), profit=c(120, 45, 0, -90, 80, 120, 23, 350))
nitems <- nrow(dataset)

# make lp   
lprec <- make.lp(0, ncol=nitems)
set.type(lprec, columns=seq.int(nitems), type="binary")

# set objective
lp.control(lprec, sense="max", bb.rule="gap", timeout=30)
set.objfn(lprec, obj=dataset[, "sales"]) 

# constraints
min_rel_profit <- 0.10 # min. 10% profit
add.constraint(lprec, dataset[, "profit"]-min_rel_profit*dataset[,"sales"], ">=", 0) # required profit
add.constraint(lprec, rep(1, nitems), "=", 4)  # four products

print(lprec)
solve(lprec)
dataset[get.variables(lprec)==1,]



# ***************************************


DF <- data.frame(Team=c(rep("Bears",5), rep("Jets",5), rep("49ers", 5)), Player=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O"), Role=c(rep(c("WR", "RB", "TE"),5)), Avgpts=c(22, 19, 30, 25, 20, 21, 26, 14, 21, 13, 11, 8, 4, 3, 5), Salary=c(930, 900, 1300, 970, 910, 920, 980, 720, 650, 589, 111, 1239, 145, 560, 780))

ncol <- nrow(DF) # of players in DF
nteams <- length(unique(DF$Team))
teams <- unique(DF$Team)

lp_rowpicker <- make.lp(ncol=(ncol+nteams))

obj_vals <- DF[, "Avgpts"]
set.objfn(lp_rowpicker, c(obj_vals, rep(0, nteams))) #dummy 0s for team variable
lp.control(lp_rowpicker,sense='max')
set.type(lp_rowpicker, columns=1:(ncol+nteams), type = "binary")
add.constraint(lp_rowpicker, xt=c(DF$Salary, rep(0, nteams)), type="<=", rhs=35000)
add.constraint(lp_rowpicker, xt=c(as.numeric(DF$Role=="WR"), rep(0, nteams)), type="=", rhs=1)
add.constraint(lp_rowpicker, xt=c(as.numeric(DF$Role=="RB"), rep(0, nteams)), type="=", rhs=1)
add.constraint(lp_rowpicker, xt=c(as.numeric(DF$Role=="TE"), rep(0, nteams)), type="=", rhs=1)

#3 players total
add.constraint(lp_rowpicker, xt=c(rep(1, ncol), rep(0, nteams)), type="=", rhs=3)

# add a constraint that every team must have between 3 and 6 players.
# put a dummy value of 3 in for each team
# if the flag for the team column is 0 then 3 players must be selected (each with a value of 1 in that team's column.
for (i in 1:nteams){
  team <- teams[i]
  add.constraint(lp_rowpicker, lhs=3, xt=c(as.numeric(DF$Team==team), rep(0, i-1), 3, rep(0, nteams-i)), type="<=", rhs=7)
}

# one team will not have the dummy value in the team column, forcing at     least 3 players picked from the same team to meet the lhs of the above constraint
add.constraint(lp_rowpicker, xt=c(rep(0, ncol), rep(1, nteams)), type="=", rhs=(nteams-1))

solve(lp_rowpicker)
get.objective(lp_rowpicker)
soln <- get.variables(lp_rowpicker)>0
solution <- DF[soln[0:ncol],]
print(solution[order(solution$Team),])
write.lp(lp_rowpicker, "filename.lp")





library(lpSolveAPI)

# Create data.table
dt <- data.frame(Item=c(rep("A",5), rep("B",5), rep("C",5), rep("D",5), rep("E",5)), Cost=c(27,20,15,7,1,16,12,9,4,1,70,52,39,19,1,290,217,162,81,8,141,105,78,39,3), Vol=c(10,20,50,100,200))
nitems <- length(unique(dt$Item))
# Objective
obj <- dt$Cost

# Constraints
constraints <- list()

# Each month must appear once in the solution
for (item in unique(dt$Item)){
  constraints[[paste0('item', item)]] <- list(xt = as.numeric(dt$Item == item),
                                                type = "=",
                                                rhs = 1)
}

# Build model
lprec <- make.lp(0, ncol = nrow(dt))
set.type(lprec, columns = seq(1,nrow(dt)), type = "binary")



set.objfn(lprec, obj = obj)

for (constraint in constraints){
  add.constraint(lprec, xt = constraint$xt, type = constraint$type, rhs = constraint$rhs)
}
add.constraint(lprec, dt[,3], "<=", rhs=350)
lp.control(lprec, sense="min")
# Compute Solution
solve(lprec)

# Visualize solution
solution <- dt[get.variables(lprec)==1,]
write.lp(lprec, "filename.lp")




# *******************************
# TEST DATA
# *******************************

DF <- data.frame(Item=c(rep("A",5), rep("B",5), rep("C",5), rep("D",5), rep("E",5)), Cost=c(27,20,15,7,1,16,12,9,4,1,70,52,39,19,1,290,217,162,81,8,141,105,78,39,3), Vol=c(10,20,50,100,200))
ncol <- nrow(DF) # of players in DF
nitems <- length(unique(DF$Item))
items <- unique(DF$Item)

lp_rowpicker <- make.lp(ncol=(ncol+nitems))

obj_vals <- DF[, "Cost"]
set.objfn(lp_rowpicker, c(obj_vals, rep(0, nitems))) #dummy 0s for team variable
lp.control(lp_rowpicker,sense='min')
set.type(lp_rowpicker, columns=1:(ncol+nitems), type = "binary")
add.constraint(lp_rowpicker, xt=c(DF$Vol, rep(0, nitems)), type="<=", rhs=280)

add.constraint(lp_rowpicker, xt=c(rep(1, ncol), rep(0, nitems)), type="=", rhs=5)

constraints <- list()
# Each item must appear once in the solution
for (i_item in items){
  constraints[[paste0('item', i_item)]] <- list(xt = as.numeric(DF$Item == i_item),
                                                type = "=",
                                                rhs = 1)
}

for (constraint in constraints){
  add.constraint(lp_rowpicker, xt = constraint$xt, type = constraint$type, rhs = constraint$rhs)
}


solve(lp_rowpicker)
get.objective(lp_rowpicker)
soln <- get.variables(lp_rowpicker)>0
solution <- DF[soln[0:ncol],]
print(solution[order(solution$Item),])
write.lp(lp_rowpicker, "filename.lp")


