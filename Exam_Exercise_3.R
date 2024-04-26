# DynOpt Exam Winter 2020/2021

# Exercise 3

# a)
# Read data
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
load(paste(PATH,"/janestates.RData",sep=""))
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
load(paste(PATH,"/janerentals.RData",sep=""))

# b)
# Create neural network layers
sizes         <- c(17, 50, 1)
activs        <- c("input", "swish", "linear")

# c)
# Initialization
N_epo     = 200
TT        = 1096

order_cost= 5
park_cost = 6
revenue   = 10
maxcar    = 25
actions   = 0:25 # action space

gamma0    = 0.9                             # discount factor
epsilons  = pmin(0.05, 2/(log(2:(N_epo+1))^3)) # deviation 
alphas    = 0.00005/sqrt(log(2:(N_epo+2)))     # step size 

# We start with -Inf
best_mean_reward = -Inf
# Here the mean reward per each epoch can be stored
mean_rewards_over_epos = rep(0,N_epo)
# Create the neural networ based on b)
net_AVF       <- net.create(sizes, activs)

# Iterate over periods
for (epoch in 1:N_epo){
  alpha <- alphas[epoch]
  eps <- epsilons[epoch]
  cars_e <- cars_m <- rep(0,TT)
  rewards = rep(0, TT)
  action_store  <- rep(0,TT)
  
  old_action <-0 # We assume an action of ordering 0 cars for the first period
  start_cars <-0 #according to task
  cars_m[1]  <- start_cars
  cars_e[1]  <- start_cars
  janestates <- unname(janestates) # Transform into simple vector
  old_states <- c(janestates[1,], start_cars)
  old_AV     <- net.IO(net_AVF, data = c(old_states,old_action))
  
  for (i in 2:TT){
    # current day
    cars_m[i] <- cars_e[i-1] + old_action
    # evening of same day
    cars_e[i] <- max(cars_m[i] - rentals[i],0)
    # compute reward
    possible_rentals <- min(rentals[i],cars_m[i])
    reward           <- revenue * possible_rentals - order_cost * old_action - cars_e[i-1] * park_cost
    rewards[i]       <- reward
    # new state
    new_states        <- c(janestates[i,], (cars_e[i]-maxcar/2)/10)
    states_matrix     <- cbind(matrix(new_states, nrow = 26, ncol = length(new_states), byrow = TRUE), (actions-maxcar/2)/10)
    current_AFV       <- net.IO(net = net_AVF, data = states_matrix)
    
    # prohibited actions
    prohib_actions    <- which(cars_e[i] + actions > maxcar) # indices!
    if(length(prohib_actions) > 0){   # if some actions are prohibited
      permitted_actions <- actions[-prohib_actions] # actual actions, not indices!
      greedy_action     <- permitted_actions[which.max(current_AFV[permitted_actions+1])]# actual action, not index
      nongreedy_actions <- (1:26)[-c(prohib_actions, greedy_action+1)] # indices!
      if(length(nongreedy_actions) == 0){
        probs <- rep(0, length(actions))
      }else{
        probs <- rep(eps/length(nongreedy_actions), length(actions))
      }
      probs[prohib_actions] <- 0
      probs[greedy_action+1]  <- 1-eps
    }else{ # in case all actions are allowed
      greedy_action        <- which.max(current_AFV) # index!
      nongreedy_actions    <- (1:26)[-(greedy_action)] # index!
      probs                <- rep(eps/(length(actions)-1), length(actions))
      probs[greedy_action] <- 1-eps
    }
    
    # new action
    new_action      <- sample(actions, 1, prob = probs)
    action_store[i] <- new_action # Not an index!
    new_AV          <- current_AFV[new_action+1]
    # update ANN with gradient
    grad            <- net.gradient.RL(net_AVF, data = c(old_states, (old_action-maxcar/2)/10))
    delta           <- reward + gamma0 * new_AV - old_AV
    
    net_AVF         <- net.add.gradient.RL(net_AVF, grad = grad, stepsize = alpha * delta)
    
    # old_stuff <- new_stuff
    old_states      <- new_states
    old_action      <- new_action
    old_AV          <- new_AV
    
  }
  # If current epoch performs better than previous epochs, store NN
  mean_rewards_over_epos[epoch] = mean(rewards)
  if (mean(rewards) > best_mean_reward){
    best_net_AVF = net_AVF
    best_actions = action_store
    best_rewards = rewards
    # Also print out the good news
    print(paste("A better mean reward of ", mean(rewards), " (previously ", best_mean_reward, ") has been found in epoch ", epoch, "."))
    best_mean_reward = mean(rewards)
  }
  # Print every 10 epochs so we know that it's still alive
  if(epoch%%10 == 0){
    print(paste("Epoch ", epoch))
  }
}
# Store the best NN on disk
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
saveRDS(best_net_AVF,paste(PATH,"/our_net.rds",sep=""))

# Load the NN
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
net_AVF <- readRDS(paste(PATH,"/our_net.rds",sep=""))

# d) Simple heuristic
h_cars_e <- h_cars_m <- rep(0,TT)
h_rewards = rep(0, TT)
h_cars_m[1] = h_cars_e[1] = 0
for (i in 2:TT){
  # current day
  h_old_action = max(rentals[i-1] - h_cars_e[i-1], 0)
  h_cars_m[i] <- h_cars_e[i-1] + h_old_action
  # evening of same day
  h_cars_e[i] <- max(h_cars_m[i] - rentals[i],0)
  # compute reward
  h_possible_rentals <- min(rentals[i],h_cars_m[i])
  h_reward    <- revenue * h_possible_rentals - order_cost * h_old_action - h_cars_e[i-1] * park_cost
  h_rewards[i]<- h_reward
}
# Print the mean reward of rule of thumb
print(mean(h_rewards))

# Plot and compare RL with simple heuristic
plot(mean_rewards_over_epos)
lines(mean_rewards_over_epos)
abline(a=mean(h_rewards), b = 0, col="red")