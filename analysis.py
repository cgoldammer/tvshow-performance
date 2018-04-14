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
df_real.season.unique()


rel_lift_data = f.relative_lift(df_real)

# The number of participants and the share that is in the top group
# is matched to the show data.
number = df_real.agent.nunique()
share_top = df_real.groupby('episode').top.mean().mean()

# This is the remaining parameter we are aiming to estimate.
relative_std_noise = 2

parameters = {
  'number': number,
  'relative_std_noise': relative_std_noise,
  'share_top': share_top
}

# Comparing the median lift in simulated data to the lift observed in the data
# if they are close, then we have set the parameter correctly.
rel_lift_data

number_runs = 50
lifts = [f.relative_lift(f.simulate_show(parameters)) for _ in range(number_runs)]
np.median(lifts)


df_real.shape

import statsmodels.formula.api as smf 

df_real = f.get_data()
df_real['top_previous'] = df_real.groupby('agent').top.shift(1)
df_real['bottom_previous'] = df_real.groupby('agent').bottom.shift(1)

smf.ols('I(top*1) ~ I(top_previous*1)', data=df_real[df_real.top_previous.notnull()]).fit().summary()
smf.ols('I(top*1) ~ I(bottom_previous*1)', data=df_real[df_real.bottom_previous.notnull()]).fit().summary()

smf.ols('I(bottom*1) ~ I(bottom_previous*1)', data=df_real[df_real.bottom_previous.notnull()]).fit().summary()
smf.ols('I(bottom*1) ~ I(top_previous*1)', data=df_real[df_real.top_previous.notnull()]).fit().summary()

smf.ols('I(top*1) ~ I(top_previous*1) + I(bottom_previous*1)', data=df_real[df_real.top_previous.notnull()]).fit().summary()
smf.ols('I(bottom*1) ~ I(top_previous*1) + I(bottom_previous*1)', data=df_real[df_real.top_previous.notnull()]).fit().summary()


