import pandas as pd
import numpy as np

import sys
import imp

import tvshow_functions as f


res = f.simulate_show(parameters)
res.head(20)






# Right now, there's only a single parameter, the relative standard deviation of noise.
# in other words, it's the extent to which the outcome of a round is determined by luck
# or noise.


# If the impact of noise if relatively low, then some participants would consistently be at the
# top or bottom of the rankings. Thus we are using the following metric to estimate how important
# noise is: "The proportion of times that a non-bottom finisher finishes non-bottom in the next round,
# relative to random chance"

episodes_remove = ['05', '07', '12.1', '12.2', '13']

df_raw = pd.read_csv("./data/season1.csv").set_index('Participant')
df_long = df_raw.stack(level=-1).reset_index()

df_long = df_long[df_long.result.notnull()]
df_long = df_long[~df_long.episode.isin(episodes_remove)]
df_long.episode.unique()

df_long['top'] = df_long.result.isin(results_top)
df_long['top_previous'] = df_long.groupby('agent').top.shift(1)

results_top = ['HIGH', 'WIN']

df_long.groupby('top_previous').top.mean()


share_top = df_long.groupby('episode').top.mean().mean()

res


pers_data = f.persistence(df_long)

rel_lift_data = pers_data[True] / pers_data[False]

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
parameters
lifts = [f.relative_lift(parameters) for _ in range(50)]
np.median(lifts)

