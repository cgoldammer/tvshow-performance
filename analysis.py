import pandas as pd
import numpy as np

import sys
import imp

import tvshow_functions as f

seed = 123
np.random.seed(seed)

# Right now, there's only a single parameter, the relative standard deviation of noise.
# in other words, it's the extent to which the outcome of a round is determined by luck
# or noise.

# If the impact of noise if relatively low, then some participants would consistently be at the
# top or bottom of the rankings. Thus we are using the following metric to estimate how important
# noise is: "The proportion of times that a non-bottom finisher finishes non-bottom in the next round,
# relative to random chance"

df_real = f.get_data()

share_top = df_real.groupby('episode').top.mean().mean()

rel_lift_data = f.relative_lift(df_real)

number = 10
std_agent_skill = 1
relative_std_noise = 2

parameters = {
  'number': number,
  'std_agent_skill': std_agent_skill,
  'relative_std_noise': relative_std_noise,
  'share_top': share_top
}

# Comparing the median lift in simulated data to the lift observed in the data
# if they are close, then we have set the parameter correctly.
rel_lift_data

number_runs = 50
lifts = [f.relative_lift(f.simulate_show(parameters)) for _ in range(number_runs)]
np.median(lifts)

