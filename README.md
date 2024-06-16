# MAB
The goal of this repository is to provide the code and resources for implementing and evaluating a reinforcement learning approach to optimize network delays using the Multi-Armed Bandit (MAB) model. 

The repository includes simulation scripts for different network topologies, such as the Tokyo metro network, to demonstrate the effectiveness of the MAB algorithm in minimizing network delays by exploring and exploiting different network paths.


## Files available in the repository
### Topology Scenarios and Test Cases:
The following files provide specific scenarios and test cases for different topologies:

- **milano_topology.R**, **milano_topology.Rmd**, **milano_topology.pdf**: These files are used to solve and analyze the Milano-based topology scenario.
- **test_bandit_algorithm.R**, **test_bandit_algorithm.Rmd**, **test_bandit_algorithm.pdf**, **test_bandit_algorithm.html**: These files are for optimizing network delays using bandit algorithms within the Tokyo topology.

### Datasets:
The following datasets are available in the `input_files` folder:

- **FlexScale_695Nodes.xlsx**: This dataset includes information about the nodes in the FlexScale topology, links between nodes, and details about equipment costs in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_Haul_topology_full_info_18_05_v3.xlsx**: This dataset contains information about the nodes in the Metro Haul topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_topology_full_Milano_v3.xlsx**: This dataset provides information about the nodes in the Milano topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).
- **Metro_topology_full_Tokyo.xlsx**: This dataset includes information about the nodes in the Tokyo topology, links between nodes, associated services, and equipment cost details.
- **Metro_topology_MAN157.xlsx**: This dataset contains information about the nodes in the MAN157 topology, links between nodes, and equipment cost details in two versions (EquipmentCost and EquipmentCost_v2).
