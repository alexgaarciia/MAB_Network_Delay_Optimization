
library(readxl)
library(igraph)
library(simmer)


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
  env %>% run(until = (k_num+1e3) / traffic_ps)
  
  # Get spending times for all arrivals up to k_num
  all_arrivals_res <- data.frame(env %>%
                                   get_mon_arrivals(per_resource = FALSE) %>%
                                   transform(waiting_time_in_queue = round(end_time - start_time - activity_time)) %>%
                                   transform(spending_time = end_time - start_time))
  
  # Return spending times for k_num arrivals
  return(all_arrivals_res$spending_time[1:k_num])
  
}


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
    converged = TRUE
    cat("Converged:",converged)
    return(TRUE)  # Converged
  } else {
    return(FALSE)  # Not converged yet
  }
}



path1_v = c(11, 10, 13, 04) 
path2_v = c(11, 09, 03, 01)  
path3_v = c(11, 12, 15, 20, 16)
path4_v = c(11, 9, 3, 2, 8, 7) 
arms = 4 # 4 paths

n_trials <- 10000 # Number of trials
epsilon <- 0.7 # Exploration rate

PS_size=c((64+127)/2,(128+255)/2,(256+511)/2, (512+1023)/2, (1024+1513)/2, 1514, (1515+9100)/2)
PS_weights=c(33.2/100, 5.4/100, 3.3/100, 3.7/100, 34.6/100, 14.6/100, 5.2/100)
N = sum(PS_size*PS_weights)
N

topology_name = "Tokyo"
file_name_v2 <- "./input_files/Metro_topology_full_Tokyo.xlsx"
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



g <- graph_from_data_frame(links_info, directed = TRUE, vertices = nodes_info)

##Calculations of the capacity in p/s

E(g)$Distance <- E(g)$distanceKm
E(g)$Definition <- paste0(as_edgelist(g)[,1],"->",as_edgelist(g)[,2])
E(g)$Capacity <- E(g)$capacityGbps*10^9/(8*N)

##Plot graph

#plot graph
V(g)$color <- "gray"
V(g)$color[national_nodes] <- "red"
V(g)$color[regional_nodes] <- "yellow"

deg <- degree(g, mode="all")
V(g)$size <- deg*1.5
l <- layout_nicely(g)

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
set.seed(321)
plot(g, edge.arrow.size=.3, vertex.label = V(g)$name, edge.curved=.5, layout=l)

# Print the results
cat("Edge ids for path1:", path1_e, "\n")
cat("Edge ids for path2:", path2_e, "\n")
cat("Edge ids for path3:", path3_e, "\n")
cat("Edge ids for path4:", path4_e, "\n")

# Print the results
cat("Distance for path1:", sum(E(g)$Distance[path1_e]), "km \n")
cat("Distance for path2:", sum(E(g)$Distance[path2_e]), "km \n")
cat("Distance for path3:", sum(E(g)$Distance[path3_e]), "km \n")
cat("Distance for path4:", sum(E(g)$Distance[path4_e]), "km \n")

k_num = n_trials
mg1_packets_path1 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.8, PS_size, PS_weights, k_num, path1_v)
mg1_packets_path2 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.5, PS_size, PS_weights, k_num, path2_v)
mg1_packets_path3 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.4, PS_size, PS_weights, k_num, path3_v)
mg1_packets_path4 <- simmer_mg1(g, Capacity_Gbps = 10, Load = 0.1, PS_size, PS_weights, k_num, path4_v)
cat("Total simulated average delay path1", mean(mg1_packets_path1), "s \n")
cat("Total simulated average delay path2", mean(mg1_packets_path2), "s \n")
cat("Total simulated average delay path3", mean(mg1_packets_path3), "s \n")
cat("Total simulated average delay path4", mean(mg1_packets_path4), "s \n")

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
plot(density(mg1_packets_path1), col = rgb(1, 0, 0, 0.5), lty = 2, lwd = 2, xlim = c(0, max_x), ylim = c(0, max_y), main = "PDF M/G/1")
lines(density(mg1_packets_path2), col = rgb(0, 1, 0, 0.5), lty = 2, lwd = 2)
lines(density(mg1_packets_path3), col = rgb(0, 0, 1, 0.5), lty = 2, lwd = 2)
lines(density(mg1_packets_path4), col = rgb(0.5, 0, 0.5, 0.5), lty = 2, lwd = 2)

# Add legends for each component
legend("topright", legend=c("Path 1", "Path 2", "Path 3", "Path 4"),
       col=c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5), rgb(0.5, 0, 0.5, 0.5)),
       lty=c(2, 2, 2, 2), lwd=c(2, 2, 2, 2), inset = c(0.01, 0.01), xpd = TRUE)

# Reset to a single plot
par(mfrow=c(1, 1))
grid()


#arms <- length(mu)
counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path



converged_at_trial <- 0
conv_num <- 5
convergence_threshold <- 1 #%
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
  reward <- -switch(chosen_arm,
                    sample(mg1_packets_path1,1),
                    sample(mg1_packets_path2,1),
                    sample(mg1_packets_path3,1),
                    sample(mg1_packets_path4,1)) #-rnorm(1, mu[chosen_arm], sigma[chosen_arm])
  
  # Update counts and rewards
  counts[chosen_arm] <- counts[chosen_arm] + 1
  rewards[chosen_arm] <- rewards[chosen_arm] + reward
  
  all_average_rewards[[i]] <- rewards/counts
  
  if (get_n_trial_convergence(all_average_rewards, i, conv_num, convergence_threshold)) {
    break
  }
  
}


average_rewards <- rewards/counts
cat("Counts of selections for each path:", counts, "\n")
cat("Average rewards (negative delay) for each path:", average_rewards, "\n")
cat("The best path is: Path", which.max(average_rewards), "\n")



counts <- numeric(arms) # A vector initialized with zeros to track how many times each path has been selected
rewards <- numeric(arms) # A vector initialized with zeros to accumulate the total rewards (negative delays) obtained from each path

converged_at_trial <- 0
conv_num <- 5
convergence_threshold <- 1 #%
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
  reward <- -switch(chosen_arm,
                    sample(mg1_packets_path1,1),
                    sample(mg1_packets_path2,1),
                    sample(mg1_packets_path3,1),
                    sample(mg1_packets_path4,1)) #-rnorm(1, mu[chosen_arm], sigma[chosen_arm])
  
  # Update counts and rewards
  counts[chosen_arm] <- counts[chosen_arm] + 1
  rewards[chosen_arm] <- rewards[chosen_arm] + reward
  
  all_average_rewards[[i]] <- rewards/counts
  
  if (get_n_trial_convergence(all_average_rewards, i, conv_num, convergence_threshold)) {
    break
  }
  
}



