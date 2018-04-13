import pandas as pd
import numpy as np


def create_agents(parameters):
  return pd.Series(np.random.normal(0, parameters['std_agent_skill'], parameters['number']))


def simulate_round(parameters, agents):
  """Simulate a round and return the distribution of the result"""
  df = pd.DataFrame(agents, columns=['skill'])
  std_noise = parameters['std_agent_skill'] * parameters['relative_std_noise']
  df['noise'] = np.random.normal(0, std_noise, len(df))
  df['outcome'] = df.skill + df.noise
  df['rank'] = df.outcome.rank().astype(int)
  number = len(agents)
  df['top'] = df['rank'].div(number) >= 1- parameters['share_top']

  return df


def simulate_show(parameters):
    agent_skill = create_agents(parameters)

    results = []
    agents_after_round = agent_skill.index
    episode = 1
    while len(agents_after_round) > 1:
      result = simulate_round(parameters, agent_skill[agents_after_round])
      lowest_outcome = result.outcome.min()
      results_after_round = result.ix[result.outcome > lowest_outcome, ['skill', 'noise', 'outcome', 'rank', 'top']]
      results_after_round['episode'] = episode
      results_after_round['agent'] = results_after_round.index
      agents_after_round = results_after_round.index
      results.append(results_after_round)
      len(result)
      len(agents_after_round)
      episode += 1
    return pd.concat(results).sort_values(['episode', 'agent'])


def persistence(df):
  df['top_previous'] = df.groupby('agent').top.shift(1)
  return df.groupby('top_previous').top.mean()


def relative_lift(parameters):
  df_sim = simulate_show(parameters)
  pers_sim = persistence(df_sim)
  return pers_sim[True] / pers_sim[False]
