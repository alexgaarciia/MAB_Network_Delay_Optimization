# Our goal is to minimize the delay, so we consider the negative delay as the
# "reward." Since bandit algorithms maximize reward, minimizing negative delay
# achieves the same.

################################################################################
#                       DEFINITION OF BASIC PARAMETERS
################################################################################
mu <- c(100, 80) # Means of the Gaussian distributions
sigma <- c(10, 30) # Standard deviations
n_trials <- 5000 # Number of trials
epsilon <- 0.7 # Exploration rate


################################################################################
#                               INITIALIZATION
################################################################################
arms <- length(mu)
counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path


################################################################################
#                                 SIMULATIONS
################################################################################
for(i in 1:n_trials){
  # Decide to explore or exploit
  if(runif(1) < epsilon){
    # Exploration: choose a random path
    chosen_arm <- sample(arms, 1)
  } else {
    # Exploitation: choose the best path based on average reward
    average_rewards <- rewards/pmax(counts, 1)
    chosen_arm <- which.max(average_rewards)
  }
  
  # Simulate the delay (reward) from the chosen path
  reward <- -rnorm(1, mu[chosen_arm], sigma[chosen_arm])
  
  # Update counts and rewards
  counts[chosen_arm] <- counts[chosen_arm] + 1
  rewards[chosen_arm] <- rewards[chosen_arm] + reward
}


################################################################################
#                                   RESULTS
################################################################################
average_rewards <- rewards/counts
cat("Counts of selections for each path:", counts, "\n")
cat("Average rewards (negative delay) for each path:", average_rewards, "\n")
cat("The best path is: Path", which.max(average_rewards), "\n")

