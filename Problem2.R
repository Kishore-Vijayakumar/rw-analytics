

#############
##Problem2
#############

library(lpSolveAPI)
#Creating LP model for maximizing profit with 9 decision variable and 10 constraints
Cloth_Factory <- make.lp(9,9)

#Setting the objective function to maximize
lp.control(Cloth_Factory,sense="maximize")

#Setting the objective funciton
set.objfn(Cloth_Factory,c(15,11,15,10,6,10,25,21,25))

#Setting the constraints
#Maximum demand for Bloom C1
set.row(Cloth_Factory,1,c(1,1,1),indices = c(1,4,7))

#Maximum demand for Amber C2
set.row(Cloth_Factory,2,c(1,1,1),indices=c(2,5,8))

#Maximum demand for Leaf C3
set.row(Cloth_Factory,3,c(1,1,1),indices=c(3,6,9))

#min Cotton proportion in bloom C4
set.row(Cloth_Factory,4,c(0.5,-0.5,-0.5),indices=c(1,4,7))

#min Cotton proportion in amber C5
set.row(Cloth_Factory,5,c(0.4,-0.6,-0.6),indices=c(2,5,8))

#min Cotton proportion in leaf C6
set.row(Cloth_Factory,6,c(0.5,-0.5,-0.5),indices=c(3,6,9))

#min Wool proportion in bloom C7
set.row(Cloth_Factory,7,c(-0.4,0.6,-0.4),indices=c(1,4,7))

#min Wool proportion in amber C8
set.row(Cloth_Factory,8,c(-0.4,0.6,-0.4),indices=c(2,5,8))

#min Wool proportion in leaf C9
set.row(Cloth_Factory,9,c(-0.3,0.7,-0.3),indices=c(3,6,9))

#Setting the right hand side of the constraints
set.rhs(Cloth_Factory,c(4200,3200,3500,0,0,0,0,0,0))

#Setting the type of constraint
set.constr.type(Cloth_Factory,c("<=","<=","<=",">=",">=",">=",">=",">=",">="))

#Setting type to real
set.type(Cloth_Factory,c(1:9),"real")

#Setting the lower and upper bound
set.bounds(Cloth_Factory,lower = rep(0,9),upper = rep(Inf,9))

#Now solving the model
solve(Cloth_Factory)

#Printing the objective values
objvalue<-get.objective(Cloth_Factory)
objvalue

#Printing the variables
solution<-get.variables(Cloth_Factory)
solution
sum(solution[c(1,4,7)])
sum(solution[c(2,5,8)])
sum(solution[c(3,6,9)])

#From the above three codes we got the maximum production of each item under constraint.





#############
##Problem 3
#############

#Bidding for Giant
#Creating lp model with 6 constraints
Bidding_Problem<-make.lp(0,6)

lp.control(Bidding_Problem,sense="maximize")

#setting the objective function
set.objfn(Bidding_Problem,c(0,0,0,0,0,1))

#Setting the 6 constraints
add.constraint(Bidding_Problem, c(-1, -1, -1, -1, 1, 1),"<=",0)

add.constraint(Bidding_Problem, c(1, -1, -1, -1, 1, 1),"<=",0)

add.constraint(Bidding_Problem, c(1, 1, -1, -1, 1, 1),"<=",0)

add.constraint(Bidding_Problem, c(1, 1, 1, -1, 1, 1),"<=",0)

add.constraint(Bidding_Problem, c(1, 1, 1, 1, -1, 1),"<=",0)

add.constraint(Bidding_Problem, c(1,1,1,1,1,0),"=",1)

#Setting the limits
set.bounds(Bidding_Problem, lower = c(0, 0, 0, 0, 0, -Inf))

#Setting row name and column names
RowNames <- c("Row1","Row2","Row3","Row4","Row5","Row6")

ColNames <- c("x1","x2","x3","x4","x5","v")

dimnames(Bidding_Problem) <- list(RowNames, ColNames)

#Solving the model
solve(Bidding_Problem) 

get.objective(Bidding_Problem)

get.variables(Bidding_Problem)

get.constraints(Bidding_Problem)




# Player 2/Bidding for Sky #
#Creating lp model with 6 constraints
Bidding_Problem <- make.lp(0, 6) 

lp.control(Bidding_Problem, sense= "minimize") #  

#setting the objective function
set.objfn(Bidding_Problem, c(0, 0, 0, 0, 0, 1)) 

#Setting the 6 constraints
add.constraint(Bidding_Problem, c(-1, 1, 1, 1, 1, 1),">=",0)

add.constraint(Bidding_Problem, c(-1, -1, 1, 1, 1, 1),">=",0)

add.constraint(Bidding_Problem, c(-1, -1, -1, 1, 1, 1),">=",0)

add.constraint(Bidding_Problem, c(-1, -1, -1, -1, 1, 1),">=",0)

add.constraint(Bidding_Problem, c(1, 1, 1, 1, -1, 1),">=",0)

add.constraint(Bidding_Problem, c(1,1,1,1,1,0),"=",1)

#Setting the limits
set.bounds(Bidding_Problem, lower = c(0, 0, 0, 0, 0,-Inf))

#Setting row name and column names
RowNames <- c("Row1","Row2","Row3","Row4","Row5","Row6")

ColNames <- c("y1","y2","y3","y4","y5","v")

dimnames(Bidding_Problem) <- list(RowNames, ColNames)

#Solving the model
solve(Bidding_Problem) 

get.objective(Bidding_Problem)

get.variables(Bidding_Problem)

get.constraints(Bidding_Problem)