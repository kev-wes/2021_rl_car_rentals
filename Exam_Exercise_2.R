# DynOpt Exam Winter 2020/2021

# Exercise 2

maxcar <- 25
fixcost <- 20 # NOTE: This was 10 in the template! As there was no info in the taskbook, we reverted this to 20!
varcost <- 5
carrevenue <- 7
orders <- 0:maxcar
states <- 0:maxcar
beta0 <- 0.99
lambda = 15 #2 9 15

# Initial guess for value function
V <- Vnew <- rep(0, length(states))

utility <- function(cc, cur_demand, cars_e){
  # no negative orders
  if (cc<0){
    return(-Inf)
    # no orders higher than space available
  }else if ((cc+cars_e) > maxcar){
    return(-Inf)
  }else{
    return(min(cur_demand, cars_e + cc) * carrevenue - cc * varcost - min(cc,1) * fixcost)
  }
}
maximand <- function(cc, cars_e, V){
  
  # As the demand is rather finite, we compute for all possible demands
  # We assume that a demand cannot be higher than 25
  # As the Poisson distribution is asymptotic, the demand could potentially be over 25,
  # however, chances are infinitesimally small
  demand_new = seq(0,25)

  # Law of Motion
  cars_tilde = cars_e + cc - demand_new
  cars_tilde = replace(cars_tilde, cars_tilde<0, 0) # Remove negative values when demand is gt stock
  
  # future values weighted by probability
  probs = dpois(demand_new, lambda)
  probs = probs/sum(probs)
  # weigh future values by probability of occurence
  EV = sum(V[cars_tilde+1] * probs)

  # utilities for all possible demands 
  utils = rep(0,length(demand_new))
  for(d in 1:length(demand_new)){
    cur_util = utility(cc = cc, cur_demand = demand_new[d], cars_e = cars_e)
    utils[d] = cur_util
  }
  # weighted by probability
  util = sum(utils * probs)
  return(util + beta0*EV)
}

# Initialize iteration counter for printing only
ite = 0
# Value function iteration
repeat{
  # Initialize the new Value vector
  Vnew <- rep(0, length(states))
  for(i in states){
    objs <- rep(0, length(states))
    # for all allowed orders
    for(o in 0:(maxcar-i)){
      objs[o+1] = maximand(cc=o, cars_e=i, V=V)
    }
    Vnew[i+1] <- max(objs)
  }
  # Calculate deviation of contraction mapped value functions
  print(paste("Iteration", ite, "- Maximal deviation between V and Vnew is:", max(abs(Vnew-V))))
  # If the value vectors have a maximum absolute deviation of lt 1e-6,
  # the optimization is finished
  if(max(abs(Vnew-V)) < 1e-6){ 
    break
  }
  V <- Vnew
  # Raise counter for printing purposes only
  ite = ite + 1
}

# Compute the policy function for all states
c_opt <- rep(0, length(orders))
for(i in states){
  objs <- rep(0, length(states))
  # choose the optimal order by computing value of for each state
  for(o in 0:(maxcar-i)){
    objs[o+1] = maximand(cc=o, cars_e=i, V=V)
  }
  c_opt[i+1] <- which.max(objs)-1
}
# Print the policy function
print(c_opt)
# Plot the policy function
plot(c_opt)
lines(c_opt)

# Simulate for 50 periods
TT = 50
carstore <- rep(NA, TT)
order <- rep(NA, TT)
demand <- rep(NA, TT)
carstore[1] <- 0 # according to task
profit <- rep(0,TT)
for(tt in 1:(TT-1)){
  # Use the optimal policy for current stock
  order[tt] <- c_opt[carstore[tt]+1]
  demand[tt] = rpois(1, lambda)
  profit[tt] <- min(demand[tt], carstore[tt] + order[tt]) * carrevenue - order[tt] *
    varcost - min(order[tt],1) * fixcost
  carstore[tt+1] <- max(carstore[tt] + order[tt] - demand[tt], 0)
}

# Plot order, demand and stock
plot(1:TT,order, 
     t="o",
     xlab="period",
     ylim=c(0, 25))
lines(1:TT,demand,    
      t="o",
      col="red")  # Add the demand stream to the plot
lines(1:TT,carstore,    
      t="o",
      col="blue") # Add the cars to the plot