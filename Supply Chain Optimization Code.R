# Supply chain analytics code
# set the working directory where the data files are present
setwd("E:\\IITK\\Optimization Methods for Analytics\\project\\data")

# install the necessary libraries and then include it
library(readr)
library(readxl)
#library(lpSolve)
library(lpSolveAPI)

# start reading the data files and convert those to dataframes. Remove the unnecessary columns
variable_costs <- data.frame(read_excel("variable_costs.xlsx"))
rownames(variable_costs) <- variable_costs$Variable.Costs....Unit.
variable_costs <- variable_costs[, 2:6]
head(variable_costs)

freight_costs <- data.frame(read_excel("freight_costs.xlsx"))
rownames(freight_costs) <- freight_costs$Freight.Costs....Container.
freight_costs <- freight_costs[, 2:6]
head(freight_costs)

storage_costs <- data.frame(read_excel("storage_costs.xlsx"))
rownames(storage_costs) <- storage_costs$Storage.Costs....unit.
storage_costs <- storage_costs[, 2:3]
head(storage_costs)

fixed_costs <- data.frame(read_excel("fixed_costs.xlsx"))
rownames(fixed_costs) <- fixed_costs$...1
fixed_costs <- fixed_costs[, 2:3]
head(fixed_costs)

co2_emissions <- data.frame(read_excel("co2_emissions.xlsx"))
rownames(co2_emissions) <- co2_emissions$CO2.Emissions..kgs.
co2_emissions <- co2_emissions[, 2:6]
head(co2_emissions)

delivery_lead_times <-
  data.frame(read_excel("delivery_leadtime.xlsx"))
rownames(delivery_lead_times) <- delivery_lead_times$...1
delivery_lead_times <- delivery_lead_times[, 2:6]
head(delivery_lead_times)

capacity <- data.frame(read_excel("capacity.xlsx"))
rownames(capacity) <- capacity$Capacity..kUnits.month.
capacity <- capacity[, 2:3]
head(capacity)

demand <- data.frame(read_excel("demand.xlsx"))
rownames(demand) <- demand$X.Units.month.
#demand <- demand[,2:2]
head(demand)

# For further analysis - but missing data
delivery_deadlines <-
  data.frame(read_excel("delivery_deadlines.xlsx"))
rownames(delivery_deadlines) <- delivery_deadlines$...1
delivery_deadlines <- delivery_deadlines[, 2:6]
head(delivery_deadlines)

# Adding Freight costs to per unit variable costs since delivery details are not available in the data
# Freight costs are for 1000 units
total_variable_costs <- variable_costs + freight_costs / 1000
head(total_variable_costs)

# Both fixed costs and storage costs are in 1000 $
total_fixed_costs <- (fixed_costs + storage_costs) * 1000
head(total_fixed_costs)

# limit on max CO2 emissions permitted by a country
max_co2_emission_permitted <- 10000000000

# Model development
# formulate LP

# create a model with x constraints, y variables
# Decision variables
# Fixed costs per location
# Variable costs for manufacturing it across locations
countries <- c("USA", "Germany", "Japan", "Brazil", "India")
lowhi <- c("Low", "High")
lst <- c("")

# total length = 2*length(countries)^2 + 2*length(countries)
vi <- 1

for (c in countries) {
  for (lh in lowhi) {
    lst[vi] <- paste (c, lh, sep = "_Location_")
    vi <- vi + 1
  }
}


for (c1 in countries) {
  for (c2 in countries) {
    lst[vi] <- paste (c2, c1, sep = "_Production_")
    vi <- vi + 1
  }
}


# Removing the delivery variables due to lack of data
#for(c1 in countries){
#  for(c2 in countries) {
#    lst[vi] <- paste (c1, c2, sep="_Delivery_")
#    vi <- vi + 1
#  }
#}

# set objective function
obj_coeffs <- rep(0, 35)
for (c in countries) {
  for (lh in lowhi) {
    obj_coeffs[which(lst == paste (c, lh, sep = "_Location_"))] <-
      total_fixed_costs[c, lh]
  }
  for (c1 in countries) {
    obj_coeffs[which(lst == paste (c, c1, sep = "_Production_"))] <-
      total_variable_costs[c, c1]
  }
}

# set constraints
clist = list()
slist <- c()
rlist <- c()
nlist <- c()
lconstraint <- 1

# add location fixed constraints
for (c in countries) {
  cc <- rep(0, 35)
  for (lh in lowhi) {
    cc[which(lst == paste (c, lh, sep = "_Location_"))] <-
      -1000 * capacity[c, lh]
  }
  for (c1 in countries) {
    cc[which(lst == paste (c, c1, sep = "_Production_"))] <- 1
  }
  clist[[lconstraint]] <- cc
  slist[lconstraint] <- "<="
  rlist[lconstraint] <- 0
  nlist[lconstraint] <- paste("Location constraint for", c, sep = " ")
  lconstraint <- lconstraint + 1
}

# add production constraints
for (c in countries) {
  cc <- rep(0, 35)
  for (c1 in countries) {
    cc[which(lst == paste (c1, c, sep = "_Production_"))] <- 1
  }
  clist[[lconstraint]] <- cc
  slist[lconstraint] <- ">="
  rlist[lconstraint] <- demand[c, "Demand"]
  nlist[lconstraint] <-
    paste("Production constraint for", c, sep = " ")
  lconstraint <- lconstraint + 1
}

# add co2 emission constraints
for (c in countries) {
  cc <- rep(0, 35)
  for (c1 in countries) {
    cc[which(lst == paste (c1, c, sep = "_Production_"))] <-
      co2_emissions[c1, c]
  }
  clist[[lconstraint]] <- cc
  slist[lconstraint] <- "<="
  rlist[lconstraint] <- max_co2_emission_permitted
  nlist[lconstraint] <- paste("CO2 constraint for", c, sep = " ")
  lconstraint <- lconstraint + 1
}

# add delivery lead time constraints
#cc <- rep(0,60)
#for(c in countries){
#  for(c1 in countries){
#    cc[which(lst == paste (c, c1, sep="_Dellivery_"))] <- delivery_lead_times[c,c1]
#  }
#}
#clist[[lconstraint]] <- cc
#slist[lconstraint] <- "<="
#rlist[lconstraint] <- sum(delivery_deadlines)
#nlist[lconstraint] <- "Delivery lead time constraint"
#lconstraint <- lconstraint + 1


# develop the lp model
nconstraints <- length(rlist)
ndecisionvars <- length(lst)

lprec <- make.lp(nconstraints, ndecisionvars)

for (i in 1:ndecisionvars) {
  cc <- c()
  for (j in 1:nconstraints) {
    cc[j] <- clist[[j]][i]
  }
  set.column(lprec, i, cc)
}

# set objective function, constraints, names for constraints and columns
set.objfn(lprec, obj_coeffs)
set.constr.type(lprec, slist)
set.rhs(lprec, rlist)
dimnames(lprec) <- list(nlist, lst)

solve(lprec)
lprec


# write the final model to a file for easy browsing
write.lp(lprec, filename = "modelout.lp")

# identify the number of solutions
get.solutioncount(lprec)

# get the primal solution
get.primal.solution(lprec)

# dual solution
get.dual.solution(lprec)

# get the constraint coefficients
get.constraints(lprec)

# get the value of the objective function
get.objective(lprec)

# get the variable coefficients
print(cbind(lst,get.variables(lprec)))