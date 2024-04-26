# DynOpt Exam Winter 2020/2021

# Exercise 1 

maxcar <- 25
fixcost <- 20 #20 or 1
varcost <- 5
carrevenue <- 7
orders <- 0:maxcar
states <- 0:maxcar
getwd()
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
demand <- read.csv(paste(PATH,"/bookings.csv",sep=""))$demand
TT <- length(demand)
beta0 <- 0.99

# Compute the value function by backward induction
V <- matrix(-Inf, TT, length(states))

utility <- function(cc, cur_demand, cars_e){
  # no negative orders
  if (cc<0){
    return(-Inf)
    # no orders higher than space available
  }else if ((cc+cars_e) > maxcar){
    return(-Inf)
    # Calculate utility
  }else{
    return(min(cur_demand, cars_e + cc) * carrevenue - cc * varcost - min(cc,1) * fixcost)
  }
}

# Anchor the value function in the last period
# In the last period we order as many cars as necessary to fulfill the demand
for(i in states){
  V[TT,i+1] = utility(max(demand[TT] - i, 0), demand[TT], i)
}

# Backward recursion
for(tt in (TT-1):1){
  # Define the maximand
  maximand <- function(cc, cur_demand, cars_e, V){
    # Plug the Bellman equation
    # Instead of a value function, we have a value matrix 
    return(utility(cc, cur_demand, cars_e) + beta0 * V[[max(cars_e + cc - cur_demand + 1, 1)]]) # Prevent negative indices
  }
  # Instead of using R function "optimise" we compute for all
  # possible states and actions as they are finite and discrete
  for(i in states){
    objs <- rep(0,length(states))
    # for all allowed orders
    for(o in 0:(maxcar-i)){
      objs[o+1] = maximand(cc=o, cur_demand=demand[tt], cars_e=i, V=V[tt+1,])
    }
    # for all unallowed already -Inf, we choose the highest valued action
    V[tt, i+1] <- max(objs)
  }
}

# Now we simulate
carstore <- rep(NA, TT)
order <- rep(NA, TT)
carstore[1] <- 0 # We start with zero cars
profit <- rep(0,TT-1)

# Calculate for all periods except final period TT
for(tt in 1:(TT-1)){
  # Define the maximand
  maximand <- function(cc, cur_demand, cars_e, V){
    return(utility(cc, cur_demand, cars_e) + beta0 * V[[max(cars_e + cc - cur_demand + 1, 1)]])
  }
  objs <- rep(0,length(states))
  # for all allowed orders
  for(o in 0:(maxcar-carstore[tt])){
    objs[o+1] = maximand(cc=o, cur_demand=demand[tt], cars_e=carstore[tt], V=V[tt+1,])
  }
  # for all unallowed already -Inf, we choose the highest valued action
  order[tt] <- which.max(objs)-1
  # Calculate the profit for statistics
  profit[tt] <- min(demand[tt], carstore[tt] + order[tt]) * carrevenue - order[tt] *
    varcost - min(order[tt],1) * fixcost
  print(order)
  carstore[tt+1] <- max(carstore[tt] + order[tt] - demand[tt], 0)
}
# For TT we order as many cars as we need to fulfill the demand
order[TT] <- max(demand[TT] - (i-1), 0)

# Plotting the results
plot(1:TT,order, 
     t="o",
     xlab="period",
     ylim=c(0, 25)) # Plotting the order sizes
lines(1:TT,demand,    
      t="o",
      col="red")  # Add the demand stream to the plot
lines(1:TT,carstore,    
      t="o",
      col="blue") # Add the cars to the plot

# Calculating the total profit
sum(profit)