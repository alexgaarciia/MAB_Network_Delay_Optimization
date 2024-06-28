# MAB_Network_Delay_Optimization
The goal of this repository is to provide the code and resources for implementing and evaluating a reinforcement learning approach to optimize network delays using the Multi-Armed Bandit (MAB) model. 

The repository includes simulation scripts for different network topologies, such as the Tokyo metro network, to demonstrate the effectiveness of the MAB algorithm in minimizing network delays by exploring and exploiting different network paths.


## Files available in the repository
### Topology Scenarios and Test Cases:
The following files provide specific scenarios and test cases for different topologies:

- **tokyo_simulation.Rmd**: This file is used to solve and analyze the Tokyo-based topology scenario. The corresponding report is provided in **tokyo_simulation.pdf**.
- **milano_simulation.R**: This file is used to solve and analyze the Milano-based topology scenario.
- **mexico_simulation.R**: This file is used to solve and analyze the Mexico-based topology scenario.

### Datasets:
The following datasets are available in the `input_files` folder:

- **FlexScale_695Nodes.xlsx**: This dataset includes information about the nodes in the FlexScale topology, links between nodes, and details about equipment costs in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_Haul_topology_full_info_18_05_v3.xlsx**: This dataset contains information about the nodes in the Metro Haul topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_topology_full_Milano_v3.xlsx**: This dataset provides information about the nodes in the Milano topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_topology_full_Tokyo.xlsx**: This dataset includes information about the nodes in the Tokyo topology, links between nodes, associated services, and equipment cost details.
- **Metro_topology_MAN157.xlsx**: This dataset contains information about the nodes in the MAN157 topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).

### Heatmaps:
The heatmaps below represent the average delay values between each pair of HL4 and HL2 nodes. They provide a visual representation of the network delays, helping to identify the effectiveness of the Multi-Armed Bandit algorithm in optimizing these delays across different topologies. The following heatmaps are available in the `heatmaps` folder:

- **heatmap_Milano.pdf**: This heatmap shows the average delay values for the Milano topology.
- **heatmap_Mexico.pdf**: This heatmap displays the average delay values for the Mexico topology.

<p align = "center">
   <img src="https://github.com/alexgaarciia/MAB_Network_Delay_Optimization/blob/main/heatmaps/heatmap_Milano.png">
   <img src="https://github.com/alexgaarciia/MAB_Network_Delay_Optimization/blob/main/heatmaps/heatmap_Mexico.png">
</p>
