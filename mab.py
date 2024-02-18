"""
This file contains a basic scenario. The main goal of this project is to find the best path from one node to another
when there are two paths provided (both gaussian). It is also required to obtain the convergence episode.

ALL OF THIS MUST BE DONE USING MAB.
"""

# Import necessary libraries
import numpy as np


class GaussianBandit:
    """Represents the environment, with each "arm" corresponding to a path"""
    def __init__(self, means, stds):
        self.means = np.array(means)
        self.stds = np.array(stds)
        self.n_arms = len(means)


# Parameters
means = [100, 80]
stds = [10, 30]

# Simulation
bandit = GaussianBandit(means, stds)
print(bandit.n_arms)
