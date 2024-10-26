
# The script is a comparison tool for different multi-armed bandit (MAB) algorithms,
# such as Epsilon-Greedy, Thomson sampling, and EXP3.
# The main goal of this script is to evaluate the performance
# of these algorithms under various conditions by simulating actions (e.g., arms)
# and calculating metrics like regret or cumulative reward. The script used for 
# percentiles scenario where in case if the latency > threshold, the penalty equals to -1.
# Else it is equal to 0.

################################################################################
#                                  Libraries
################################################################################

library(readxl)
library(igraph)
library(simmer)

################################################################################
#                                  Functions
################################################################################

simmer_mg1 <- function(g, Capacity_Gbps, Load, PS_size, PS_weights, k_num, path_v, distance_n_times = 1){
  
  # Calculate total number of packets in the system
  N = sum(PS_size * PS_weights)
  
  # Calculate node capacity in Bps
  nodes_capacity_Bps = Capacity_Gbps * 1e9
  
  # Get edge IDs for the given path
  path_e <- sapply(2:length(path_v), function(i) {
    get.edge.ids(g, as.vector(c(path_v[i-1], path_v[i])))
  })
  
  # Calculate capacity per packet in Bps
  Capacity_ps = Capacity_Gbps * 1e9 / (8 * N)
  
  # Calculate traffic per packet in Bps
  traffic_ps = Capacity_ps * Load
  
  # Calculate service rate
  mu = 1 / Capacity_ps
  
  # Calculate variance of the number of packets in the system
  var_N <- sum(PS_size^2 * PS_weights) - N^2
  Cs2 <- var_N / (N^2)
  
  # Calculate theoretical queue delay for each link in the path
  theor_qeueu_delay_link_mg1 <- mu * Load / (1 - Load) * (1 + Cs2) / 2 + mu
  theor_qeueu_delay_mg1 <- theor_qeueu_delay_link_mg1*length(path_e)
  # Calculate theoretical propagation delay for each link in the path
  theor_prop_delay_link_mg1 <- 5e-6 * E(g)$Distance[path_e]*distance_n_times
  
  # Print total average delay
  cat("Total theoretical average delay", theor_qeueu_delay_mg1 + sum(theor_prop_delay_link_mg1), "s \n")
  
  # Initialize simulation environment
  env <- simmer()
  
  # Add resources for each node in the graph
  for (i in 1:length(V(g))) {
    env %>% add_resource(paste0("node_", i), 1) 
  }  
  
  # Create trajectory for each link in the path
  trajectory_name <- lapply(1:length(path_e), function(i) {
    trajectory() %>%
      seize(paste0("node_", path_v[i])) %>%
      timeout(function() 8 * sample(PS_size, size = 1, replace = TRUE, prob = PS_weights) / nodes_capacity_Bps) %>%
      release(paste0("node_", path_v[i])) %>%
      timeout(function() theor_prop_delay_link_mg1[i])
  }) %>% join()
  
  # Define arrival process
  env %>% add_generator("trajectory_name", trajectory_name, function() rexp(1, traffic_ps))
  
  # Run simulation
  env %>% run(until = (k_num+1e3) / traffic_ps)
  
  # Get spending times for all arrivals up to k_num
  all_arrivals_res <- data.frame(env %>%
                                   get_mon_arrivals(per_resource = FALSE) %>%
                                   transform(waiting_time_in_queue = round(end_time - start_time - activity_time)) %>%
                                   transform(spending_time = end_time - start_time))
  
  # Return spending times for k_num arrivals
  return(all_arrivals_res$spending_time[1:k_num])
  
}


################################################################################
#                                  M/M/1 e2e delay Simulation
################################################################################



path1_v = c(11, 10, 13, 04) 
path2_v = c(11, 09, 03, 01)  
path3_v = c(11, 12, 15, 20, 16)
path4_v = c(11, 9, 3, 2, 8, 7) 
arms = 4 # 4 paths

n_trials <- 10000 # Number of trials

PS_size=c((64+127)/2,(128+255)/2,(256+511)/2, (512+1023)/2, (1024+1513)/2, 1514, (1515+9100)/2)
PS_weights=c(33.2/100, 5.4/100, 3.3/100, 3.7/100, 34.6/100, 14.6/100, 5.2/100)
N = sum(PS_size*PS_weights)
N

topology_name = "Tokyo"
file_name_v2 <- "input_files/Metro_topology_full_Tokyo.xlsx"
nodes_info <- read_excel(file_name_v2, sheet = 1)
links_info <- read_excel(file_name_v2, sheet = 2)

national_nodes <- c()
regional_nodes <- c()

for (i in seq_along(nodes_info$node_code)) {
  if (nodes_info$node_code[i] == "HL2") {
    national_nodes <- c(national_nodes, i)
  }
  if (nodes_info$node_code[i] == "HL3") {
    regional_nodes <- c(regional_nodes, i)
  }
}

cat("National nodes:", national_nodes, "\n")
cat("Regional nodes:", regional_nodes, "\n")


# Building the graph:
g <- graph_from_data_frame(links_info, directed = TRUE, vertices = nodes_info)

##Calculations of the capacity in p/s
E(g)$Distance <- E(g)$distanceKm
E(g)$Definition <- paste0(as_edgelist(g)[,1],"->",as_edgelist(g)[,2])
E(g)$Capacity <- E(g)$capacityGbps*10^9/(8*N)


#Plot graph
V(g)$color <- "gray"
V(g)$color[national_nodes] <- "red"
V(g)$color[regional_nodes] <- "yellow"

deg <- degree(g, mode="all")
V(g)$size <- deg*1.5
l <- layout_nicely(g)
set.seed(24) #321,24

# Get edge ids for each path
path1_e <- sapply(2:length(path1_v), function(i) {
  get.edge.ids(g, as.vector(c(path1_v[i-1], path1_v[i])))
})
path2_e <- sapply(2:length(path2_v), function(i) {
  get.edge.ids(g, as.vector(c(path2_v[i-1], path2_v[i])))
})
path3_e <- sapply(2:length(path3_v), function(i) {
  get.edge.ids(g, as.vector(c(path3_v[i-1], path3_v[i])))
})
path4_e <- sapply(2:length(path4_v), function(i) {
  get.edge.ids(g, as.vector(c(path4_v[i-1], path4_v[i])))
})

E(g)$color <- "gray"
E(g)$color[c(path1_e,path2_e,path3_e,path4_e)] <- "red"
E(g)$width <- 1
E(g)$width[c(path1_e, path2_e, path3_e, path4_e)] <- 2  # You can adjust the width to your desired thickness

plot(g, edge.arrow.size=.3, vertex.label = V(g)$name, edge.curved=.5, layout=l)


k_num = n_trials
mg1_packets_path1 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.8, PS_size, PS_weights, k_num, path1_v, distance_n_times = 6)
mg1_packets_path2 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.5, PS_size, PS_weights, k_num, path2_v, distance_n_times = 6)
mg1_packets_path3 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.4, PS_size, PS_weights, k_num, path3_v, distance_n_times = 6)
mg1_packets_path4 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.1, PS_size, PS_weights, k_num, path4_v, distance_n_times = 6)
cat("Total simulated average delay path1", mean(mg1_packets_path1), "s \n")
cat("Total simulated average delay path2", mean(mg1_packets_path2), "s \n")
cat("Total simulated average delay path3", mean(mg1_packets_path3), "s \n")
cat("Total simulated average delay path4", mean(mg1_packets_path4), "s \n")

threshold = 1e-4 #s
set.seed(42)

# Calculate maximum density value across all paths
mg1_packets_path1 = mg1_packets_path1 * 1e6
mg1_packets_path2 = mg1_packets_path2 * 1e6
mg1_packets_path3 = mg1_packets_path3 * 1e6
mg1_packets_path4 = mg1_packets_path4 * 1e6

# Calculate maximum density value across all paths
max_y <- max(max(density(mg1_packets_path1)$y),
             max(density(mg1_packets_path2)$y),
             max(density(mg1_packets_path3)$y),
             max(density(mg1_packets_path4)$y))
max_x <- max(max(density(mg1_packets_path1)$x),
             max(density(mg1_packets_path2)$x),
             max(density(mg1_packets_path3)$x),
             max(density(mg1_packets_path4)$x))

# Plot densities for each path with adapted axis limits
plot(density(mg1_packets_path1), col = rgb(1, 0, 0, 0.5), lty = 2, lwd = 3, xlim = c(0, max_x), ylim = c(0, max_y), xlab = "Latency (us)", main = "")
lines(density(mg1_packets_path2), col = rgb(0, 1, 0, 0.5), lty = 2, lwd = 3)
lines(density(mg1_packets_path3), col = rgb(0, 0, 1, 0.5), lty = 2, lwd = 3)
lines(density(mg1_packets_path4), col = rgb(0.5, 0, 0.5, 0.5), lty = 2, lwd = 3)
abline(v = threshold*1e6, col = "black", lty = 1, lwd = 2)

# Add legends for each component
legend("topleft", legend=c("Path 1", "Path 2", "Path 3", "Path 4", expression(T[thres])),
       col=c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5), rgb(0.5, 0, 0.5, 0.5), "black"),
       lty=c(2, 2, 2, 2, 1), lwd=c(3, 3, 3, 3, 3), inset = c(0.01, 0.01), xpd = TRUE)

# Reset to a single plot
par(mfrow=c(1, 1))
grid()

mg1_packets_path1 = mg1_packets_path1 / 1e6
mg1_packets_path2 = mg1_packets_path2 / 1e6
mg1_packets_path3 = mg1_packets_path3 / 1e6
mg1_packets_path4 = mg1_packets_path4 / 1e6



n_trials = 1000 # Number of trials
tests = 10000
optimal_path = 3

################################################################################
#                                  Test 1 Basic Epsilon Greedy
################################################################################

epsilon <- 0.7 # Exploration rate


counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path


prob_opt_path_selected <- rep(0, n_trials)

for (test in 1:tests){
  all_average_penalties <- list() 
  counts <- numeric(arms)
  penalties <- numeric(arms)
  for(i in 1:n_trials){
    # Decide to explore or exploit
    if(runif(1) < epsilon){
      # Exploration: choose a random path
      chosen_arm <- sample(arms, 1)
    } else {
      # Exploitation: choose the best path based on average penalty
      average_penalties <- penalties/pmax(counts, 1)
      chosen_arm <- which.max(average_penalties)
    }
    
    # Simulate the delay (penalty) from the chosen path
    delay <- switch(chosen_arm,
                    sample(mg1_packets_path1,1),
                    sample(mg1_packets_path2,1),
                    sample(mg1_packets_path3,1),
                    sample(mg1_packets_path4,1)) 
    
    # Update counts and penalties
    penalty = -ifelse(delay > threshold, 1, 0)
    counts[chosen_arm] <- counts[chosen_arm] + 1
    penalties[chosen_arm] <- penalties[chosen_arm] + penalty
    
    all_average_penalties[[i]] <- penalties/counts
    # cat("Counts of selections for each path:", counts, "\n")
    # cat("Average rewards (negative delay) for each path:", average_rewards, "\n")
    # cat("The best path is: Path", which.max(average_rewards), "\n")
    if (optimal_path == which.max(penalties/counts)){
      
      prob_opt_path_selected[i] <- prob_opt_path_selected[i] + 1
    }
    
  }
}

prob_opt_path_selected_elipson_greedy_Basic <- prob_opt_path_selected/100

################################################################################
#                                  Test 2 Decaying Epsilon Greedy
################################################################################

counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path


prob_opt_path_selected <- rep(0, n_trials)

for (test in 1:tests){
  all_average_penalties <- list() 
  counts <- numeric(arms)
  penalties <- numeric(arms)
  for(i in 1:n_trials){
    # Decide to explore or exploit
    epsilon <- 1/sqrt(i)
    if(runif(1) < epsilon){
      # Exploration: choose a random path
      chosen_arm <- sample(arms, 1)
    } else {
      # Exploitation: choose the best path based on average penalty
      average_penalties <- penalties/pmax(counts, 1)
      chosen_arm <- which.max(average_penalties)
    }
    
    # Simulate the delay (penalty) from the chosen path
    delay <- switch(chosen_arm,
                    sample(mg1_packets_path1,1),
                    sample(mg1_packets_path2,1),
                    sample(mg1_packets_path3,1),
                    sample(mg1_packets_path4,1)) 
    
    # Update counts and penalties
    penalty = -ifelse(delay > threshold, 1, 0)
    counts[chosen_arm] <- counts[chosen_arm] + 1
    penalties[chosen_arm] <- penalties[chosen_arm] + penalty
    
    all_average_penalties[[i]] <- penalties/counts
    # cat("Counts of selections for each path:", counts, "\n")
    # cat("Average rewards (negative delay) for each path:", average_rewards, "\n")
    # cat("The best path is: Path", which.max(average_rewards), "\n")
    if (optimal_path == which.max(penalties/counts)){
      
      prob_opt_path_selected[i] <- prob_opt_path_selected[i] + 1
    }
    
  }
}

prob_opt_path_selected_elipson_greedy_Decaying <- prob_opt_path_selected/100


################################################################################
#                                  Test 3  Thompson Sampling
################################################################################


# Parameters

arms <- 4          # Number of paths
prob_opt_path_selected <- rep(0, n_trials)
temperature <- 1  # Temperature parameter for Softmax (controls exploration vs. exploitation)

# Function to calculate the softmax probabilities
softmax <- function(values, temperature) {
  exp_values <- exp(values / temperature)  # Scale rewards using the temperature parameter
  return(exp_values / sum(exp_values))  # Return probabilities
}


for (test in 1:tests){
  
  # Initialization for Thompson
  counts <- numeric(arms) # Track how many times each path has been selected
  penalties <- numeric(arms) # Accumulate total rewards (negative delays) for each path
  all_average_penalties <- list() 
  
  numbers_of_rewards_1 = integer(arms)
  numbers_of_rewards_0 = integer(arms)
  
  for (i in 1:n_trials) {
    max_random = 0
    for (arm in 1:arms) {
      random_beta = rbeta(n = 1,
                          shape1 = numbers_of_rewards_1[arm] + 1,
                          shape2 = numbers_of_rewards_0[arm] + 1)
      if (random_beta > max_random) {
        max_random = random_beta
        chosen_arm = arm
      }
    }
    
    delay = switch(chosen_arm,
                   sample(mg1_packets_path1, 1),
                   sample(mg1_packets_path2, 1),
                   sample(mg1_packets_path3, 1),
                   sample(mg1_packets_path4, 1))
    
    
    penalty = -ifelse(delay > threshold, 1, 0)
    counts[chosen_arm] <- counts[chosen_arm] + 1
    penalties[chosen_arm] <- penalties[chosen_arm] + penalty
    
    if (penalty == 0) {
      numbers_of_rewards_1[chosen_arm] = numbers_of_rewards_1[chosen_arm] + 1 #win
    } else {
      numbers_of_rewards_0[chosen_arm] = numbers_of_rewards_0[chosen_arm] + 1 #lose
    }
    
    all_average_penalties[[i]] <- penalties/counts
    average_penalties <- all_average_penalties[[i]]
  
    
    
    if (optimal_path == which.max(average_penalties)){
      
      prob_opt_path_selected[i] <- prob_opt_path_selected[i] + 1
    }
    
    
  }
}

prob_opt_path_selected_Thompson <- prob_opt_path_selected/100




################################################################################
#                                  Test 4 EXP3
################################################################################



# Parameters
arms <- 4          # Number of paths
prob_opt_path_selected <- rep(0, n_trials)


for (test in 1:tests){
  # Initialization
  weights <- rep(1, arms)  # Initialize weights for each arm to 1
  gamma <- 0.1  # Exploration parameter (between 0 and 1)
  probabilities <- rep(1 / arms, arms)  # Initial equal probability for each arm
  counts <- numeric(arms)  # Track how many times each path has been selected
  rewards <- numeric(arms)  # Accumulate total rewards (negative delays) for each path
  all_average_rewards <- list()
  average_rewards <- rep(0, arms)
  # EXP3 Algorithm
  for (i in 1:n_trials) {
    # Calculate probabilities for choosing each arm
    probabilities <- (1 - gamma) * (weights / sum(weights)) + gamma / arms
    
    # Choose an arm based on the computed probabilities
    chosen_arm <- sample(1:arms, 1, prob = probabilities)
    
    # Simulate reward (negative delay) from the chosen path
    delay <- switch(chosen_arm,
                    sample(mg1_packets_path1, 1),
                    sample(mg1_packets_path2, 1),
                    sample(mg1_packets_path3, 1),
                    sample(mg1_packets_path4, 1))
    
    reward = ifelse(delay > threshold, 0, 1)
    
    # Update counts and rewards
    counts[chosen_arm] <- counts[chosen_arm] + 1
    
    # Update the weights for the chosen arm
    estimated_reward <- reward / probabilities[chosen_arm]
    weights[chosen_arm] <- weights[chosen_arm] * exp((gamma * estimated_reward) / arms)
    average_rewards[chosen_arm] <- average_rewards[chosen_arm] + estimated_reward
    all_average_rewards[[i]] <- average_rewards
    if (optimal_path == which.max(average_rewards)){
      
      prob_opt_path_selected[i] <- prob_opt_path_selected[i] + 1
    }
    
  }
  
}

prob_opt_path_selected_exp3 <- prob_opt_path_selected/100




x <-3:n_trials


plot(x, prob_opt_path_selected_elipson_greedy_Basic[3:n_trials], type = "l", col = "blue", lwd = 2,
     ylim = c(0, 100), xlab = "Trials", ylab = "Probability of Optimal Path Selection in 10k times",
     main = "Comparison of MAB Algorithms")
lines(x, prob_opt_path_selected_Thompson[3:n_trials], col = "green", lwd = 2)
lines(x, prob_opt_path_selected_exp3[3:n_trials], col = "red", lwd = 2)
lines(x, prob_opt_path_selected_elipson_greedy_Decaying[3:n_trials], col = "gray", lwd = 2)

# Add legend
legend("bottomright", legend = c("Epsilon-Greedy","Decaying Epsilon", "Thompson", "EXP3"),
       col = c("blue", "gray","green", "red"), lty = 1, lwd = 2)
grid()

