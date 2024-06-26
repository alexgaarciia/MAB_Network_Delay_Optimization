# Our goal is to minimize the delay, so we consider the negative delay as the
# "reward." Since bandit algorithms maximize reward, minimizing negative delay
# achieves the same.



################################################################################
#                       Libraries
################################################################################

library(readxl)
library(igraph)
library(simmer)
library(reshape2)
library(ggplot2)



################################################################################
#                       Functions
################################################################################

## `simmer_mg1` function: Simulates an M/G/1 queuing system using the simmer package.
# Parameters:
#   - `g`: Graph object representing the network topology.
# - `Capacity_Gbps`: Capacity of the links in Gbps.
# - `Load`: Load of the system.
# - `PS_size`: Packet sizes.
# - `PS_weights`: Weights of packet sizes.
# - `k_num`: Number of packets to simulate.
# - `path_v`: Vector representing the path in the network.
# Returns:
#   - Vector of spending times for the specified number of packets.

simmer_mg1 <- function(g, Capacity_Gbps, Load, PS_size, PS_weights, k_num, path_v){
  
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
  theor_prop_delay_link_mg1 <- 5e-6 * E(g)$Distance[path_e]
  
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
  env %>% run(until = (k_num+1e4) / traffic_ps)
  
  # Get spending times for all arrivals up to k_num
  all_arrivals_res <- data.frame(env %>%
                                   get_mon_arrivals(per_resource = FALSE) %>%
                                   transform(waiting_time_in_queue = round(end_time - start_time - activity_time)) %>%
                                   transform(spending_time = end_time - start_time))
  
  # Return spending times for k_num arrivals
  return(all_arrivals_res$spending_time[1:k_num])
  
}


## `get_n_trial_convergence` function: Checks convergence based on average rewards.
# The convergence criterion is defined as follows:
#   $$ \text{Err} < \text{threshold} $$
#   where \(\text{Err} \) is calculated as: 
#   $$ \text{Err} = \frac{\sum_{i=1}^{n} |\overline{\text{Reward\_curr}}_i - \overline{\text{Reward\_prev\_n}}_i|}{|\overline{\text{Reward\_prev\_n}}_i|}$$
#   Here, \( \overline{\text{Reward\_curr}}_{i} \) represents the average rewards for the current trial, and \(\overline{\text{Reward\_prev\_n}}_{i}\) represents the mean of the previous \( n \) average rewards. 

# - `conv_num`: This variable represents the number of previous trials used for convergence checking. It specifies the number of previous average rewards to consider when checking for convergence.
# - `convergence_threshold`: This variable defines the threshold for convergence, maximum allowed variance between the current average rewards and the mean of the previous `conv_num` average rewards. If the variance falls below this threshold, the system is considered converged.
# - `converged`: This variable is a boolean flag that indicates whether the system has converged or not.

get_n_trial_convergence <- function(all_average_rewards, i_trial, conv_num, convergence_threshold) {
  
  # Check if the current trial is less than or equal to the convergence window
  if (i_trial < conv_num) {
    return(FALSE)  # Not converged yet
  }
  
  # Retrieve the average rewards for the current trial
  current_average_rewards <-  ifelse(is.na(all_average_rewards[[i_trial]]), 0, all_average_rewards[[i_trial]])
  
  # Retrieve the previous n average rewards (convergence window)
  prev_n_average_rewards <- all_average_rewards[(i_trial - conv_num):(i_trial - 1)]
  
  # Calculate the total sum of the previous n average rewards for each iteration
  total_sum <- colSums(do.call(rbind, lapply(prev_n_average_rewards, function(x) ifelse(is.na(x), 0, x))))
  
  # Calculate the mean of the previous n average rewards
  prev_n_mean <- total_sum / conv_num
  
  # Calculate the error of the current average rewards according the previous n mean
  error_perc <- sum(abs(current_average_rewards - prev_n_mean))/(abs(sum(prev_n_mean)))*1e2
  
  # Check if the error is less than the convergence threshold
  if (error_perc < convergence_threshold) {
    cat("Error =", error_perc, "% \n")
    cat("Converged at trial", i_trial, "\n")  # Print convergence message
    assign("n_trial_convergence", i_trial, envir = .GlobalEnv)
    converged = TRUE
    cat("Converged:",converged, "\n")
    return(TRUE)  # Converged
  } else {
    return(FALSE)  # Not converged yet
  }
}



################################################################################
#                       Definition of basic parameters
################################################################################

# Path configuration


# Parameter definition
n_trials <- 10000 # Number of trials
k_num <- 10000 #Number of packets to simulate
epsilon <- 0.7 # Exploration rate
PS_size=c((64+127)/2,(128+255)/2,(256+511)/2, (512+1023)/2, (1024+1513)/2, 1514, (1515+9100)/2)
PS_weights=c(33.2/100, 5.4/100, 3.3/100, 3.7/100, 34.6/100, 14.6/100, 5.2/100)
N = sum(PS_size*PS_weights)

#link capacity
Capacity_Gbps = 10 #Gbps 
Load = 0.8 



################################################################################
#                       Topology loading
################################################################################

topology_name = "Milano"
file_name_v2 <- "input_files/Metro_topology_full_Milano_v3.xlsx"
nodes_info <- read_excel(file_name_v2, sheet = 1)
links_info <- read_excel(file_name_v2, sheet = 2)

national_nodes <- which(nodes_info$node_code == "HL2")
regional_nodes <- which(nodes_info$node_code == "HL3")
local_nodes <- which(nodes_info$node_code == "HL4")

cat("National nodes:", national_nodes, "\n")
cat("Regional nodes:", regional_nodes, "\n")


# Building the graph:
g <- graph_from_data_frame(links_info, directed = TRUE, vertices = nodes_info)

## Calculations of the capacity in p/s
E(g)$Distance <- E(g)$distanceKm
E(g)$Definition <- paste0(as_edgelist(g)[,1],"->",as_edgelist(g)[,2])
E(g)$Capacity <- E(g)$capacityGbps*10^9/(8*N)


# Plot graph
V(g)$color <- "gray"
V(g)$color[national_nodes] <- "red"
V(g)$color[regional_nodes] <- "yellow"

deg <- degree(g, mode="all")
V(g)$size <- deg*1.5
l <- layout_nicely(g)
set.seed(321)


E(g)$color <- "gray"
plot(g, edge.arrow.size=.3, vertex.label = V(g)$name, edge.curved=.5, layout=l)

# Add legend
legend("topright", 
       c("HL2", "HL3s", "HL4s"), 
       pch = 19, 
       col = c("red", "yellow", "gray"), 
       bty = "n", 
       cex = 1.5, 
       pt.cex = 2)



################################################################################
#                       Simulation of entire topology
################################################################################

# Consider a network topology with multiple hierarchical levels, including HL4 (local) nodes and HL2 (national) nodes. The objective is to determine the optimal paths from each HL4 node to HL2 node that has the lowest the end-to-end delay.
# ## Define function *MAB_algorithm_av_delays*
# Function *MAB_algorithm_av_delays* that implements the MAB algorithm and calculates the best paths between nodes.

MAB_algorithm_av_delays <- function(mg1_packets, arms, conv_num, convergence_threshold, n_trials = 10000) {
  counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
  rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path
  converged_at_trial <- 0
  all_average_rewards <- list() 
  converged <- FALSE
  
  for(i in 1:n_trials){
    # Decide to explore or exploit
    if(runif(1) < epsilon){
      # Exploration: choose a random path
      chosen_arm <- sample(arms, 1)
    } else {
      # Exploitation: choose the best path based on average reward
      #average_rewards <- rewards/pmax(counts, 1)
      average_rewards <- rewards/pmax(counts, 1)
      chosen_arm <- which.max(average_rewards)
    }
    
    # Simulate the delay (reward) from the chosen path
    reward <- -sample(mg1_packets[, chosen_arm], 1)
    
    # Update counts and rewards
    counts[chosen_arm] <- counts[chosen_arm] + 1
    rewards[chosen_arm] <- rewards[chosen_arm] + reward
    
    all_average_rewards[[i]] <- rewards/counts
    
    if (get_n_trial_convergence(all_average_rewards, i, conv_num, convergence_threshold)) {
      break
    }
  }
  best_path <- which.max(average_rewards)
  return(best_path)
}


## Obtaining the best path with different convergence thresholds
# MAB algorithm obtains with different convergence thresholds, $5\%$ and $1\%$, to identify the best paths. The convergence threshold determines the maximum allowed variance between the current average rewards (negative delays) and the mean of the previous n (see below 25) average rewards. If the variance falls below the threshold, the system is considered converged, and the algorithm terminates.


# Initialize variables to store the best paths and delays
best_paths_v1 <- c()
best_paths_v2 <- c()
n_trial_convergence_v1 <- c()
n_trial_convergence_v2 <- c()
best_paths_real <- c()
conv_num = 20
local_nodes <- which(nodes_info$node_code == "HL4")#[1:5]
arms <- length(national_nodes)
real_delays <- matrix(0, nrow = arms, ncol = length(local_nodes))
convergence_thresholds <- c(10,3) #%


n = 1
# Calculate the best paths and delays for each HL4 node to all HL2 national nodes with errors of 1% and 5%
for (hl4 in local_nodes) {
  # Initialize variables to store the best paths and delays for this HL4 node
  
  # Calculate the delay for each path from this HL4 node to this HL2 node
  
  mg1_packets <- sapply(1:arms, function(i) {
    path_v <- shortest_paths(g, from = V(g)[hl4],to = V(g)[national_nodes[i]],output = c("vpath"))$vpath[[1]]
    return(simmer_mg1(g, Capacity_Gbps, Load, PS_size, PS_weights, k_num, path_v))
  })
  real_delays[,n] <- sapply(1:arms, function(i) mean(mg1_packets[,i]))
  
  # Calculate the best paths and delays for this HL4 node with errors of 1% and 5%
  best_paths_v1[n] <- MAB_algorithm_av_delays(mg1_packets, arms, conv_num, convergence_thresholds[1])
  n_trial_convergence_v1[n] <- n_trial_convergence
  best_paths_v2[n] <- MAB_algorithm_av_delays(mg1_packets, arms, conv_num, convergence_thresholds[2])
  n_trial_convergence_v2[n] <- n_trial_convergence
  best_paths_real[n] <- which.min(real_delays[,n])
  n = n + 1
}



################################################################################
#                       Results
################################################################################

## Printing the results
# Printing the best paths and corresponding delays from each HL4 node to all HL2 nodes, using the different convergence thresholds. Additionally, the real best paths determined through simulations, considering the true end-to-end delays.


# Print the best paths and delays
for (i in 1:length(local_nodes)) {
  cat("Best paths with ", convergence_thresholds[1],"% error from HL4 node", V(g)[local_nodes[i]]$name, ":", best_paths_v1[i], ", with ",convergence_thresholds[2],"% error: ", best_paths_v2[i],", best path:",best_paths_real[i],", with delay ", real_delays[best_paths_real[i],i], "s \n")
}

# Average number of trial for convergence 

cat("N trials for", convergence_thresholds[1], "%: ", mean(n_trial_convergence_v1))
cat("N trials for", convergence_thresholds[2], "%: ", mean(n_trial_convergence_v2))



################################################################################
#                       Heatmap plot
################################################################################

## Plotting the heatmaps

# The heatmap represents the average delay values between each pair of HL4 and HL2 nodes. The heatmap uses a color scale, where darker shades represent higher delays, and lighter shades represent lower delays. The chosen paths chosen are highlighted in frames with different convergence thresholds (5% and 1%) and the real best paths.
df <- melt(real_delays)

colnames(df) <- c("HL2s", "HL4s", "delay" )
# Create a matrix of TRUE/FALSE values
best_paths_v1_matrix <- matrix(FALSE, nrow = length(national_nodes), ncol = length(best_paths_v1))
# Set the corresponding position in the matrix to TRUE for each number
best_paths_v1_matrix[cbind(best_paths_v1, 1:length(best_paths_v1))] <- TRUE
# Display the matrix of TRUE/FALSE vectors
best_paths_v1_df <- melt(best_paths_v1_matrix)
df$best_paths_v1 <- best_paths_v1_df$value

# Create a matrix of TRUE/FALSE values
best_paths_v2_matrix <- matrix(FALSE, nrow = length(national_nodes), ncol = length(best_paths_v2))
# Set the corresponding position in the matrix to TRUE for each number
best_paths_v2_matrix[cbind(best_paths_v2, 1:length(best_paths_v2))] <- TRUE
# Display the matrix of TRUE/FALSE vectors
best_paths_v2_df <- melt(best_paths_v2_matrix)
df$best_paths_v2 <- best_paths_v2_df$value

# Create a matrix of TRUE/FALSE values
best_paths_real_matrix <- matrix(FALSE, nrow = length(national_nodes), ncol = length(best_paths_real))
# Set the corresponding position in the matrix to TRUE for each number
best_paths_real_matrix[cbind(best_paths_real, 1:length(best_paths_real))] <- TRUE
# Display the matrix of TRUE/FALSE vectors
best_paths_real_df <- melt(best_paths_real_matrix)
df$best_paths_real <- best_paths_real_df$value

# ggplot(df, aes(HL4s, HL2s)) +
#   geom_tile(aes(fill = delay*1e6)) +
#   geom_text(aes(label = round(delay*1e6 , 1))) +
#   scale_fill_gradient(low = "white", high = "gray10")


# Colors represent high delay (in dark gray) and low latency (in white).
# The red square highlights the path chosen with a 5% convergence thresholds,
# the blue square highlights the path chosen with a 1% convergence thresholds,
# and the black square highlights the real best path.


p <- ggplot(df, aes(HL4s, HL2s)) +
  geom_tile(aes(fill = delay * 1e6)) +
  geom_text(aes(label = round(delay * 1e6, 1), fontface = ifelse(best_paths_real, "bold", "plain"))) +
  scale_fill_gradient(low = "white", high = "gray30") +
  geom_tile(data = subset(df, best_paths_v1), aes(color = "10% Error"), linewidth = 3, fill = NA) +
  geom_tile(data = subset(df, best_paths_v2), aes(color = "3% Error"), linewidth = 2, fill = NA) +
  guides(fill = guide_legend(title = "Delay, us")) +
  scale_x_continuous(breaks = seq(1, length(local_nodes), 1)) +
  scale_y_continuous(breaks = seq(1, length(national_nodes), 1)) +
  scale_discrete_manual(
    aesthetics = "fontface",
    values = c("bold", "plain"),
    name = "Best Path",
    labels = c(expression(bold("TRUE")), "FALSE")
  ) +
  
  scale_color_manual(
    name = "Paths",
    values = c("10% Error" = "red", "3% Error" = "blue")
  ) +

  theme(
    legend.position = "right",
    legend.box = "vertical"
  )

# Create the comment text grob
#comment_grob <- textGrob("Bold: Real Best Path", x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
#                         hjust = 1.35, vjust = 12, gp = gpar(fontsize = 10, fontface = "bold"))

# Combine the plot and the comment using grid.arrange
#grid.arrange(ggplotGrob(p), comment_grob, ncol = 2, widths = c(8, 1))

# Display the plot
print(p)



################################################################################
#                       MAB performance 
################################################################################

# The average number of errors, which represents the percentage of cases where the best path chosen by MAB algorithm does not match the true best path determined through the simulations.


cat("Average Number of convergence threshold ", convergence_thresholds[1],"% average error:", sum(best_paths_v1 != best_paths_real)/length(best_paths_v1)*100, "%\n")
cat("Average Number of convergence threshold ", convergence_thresholds[2],"% average error:", sum(best_paths_v2 != best_paths_real)/length(best_paths_v2)*100, "%\n")

