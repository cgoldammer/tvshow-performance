import pandas as pd
import numpy as np


def create_agents(parameters):
  """Returns a series with agent ids and skill"""
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
  """Simulate a show, returning a list of results from each round"""

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
    episode += 1
  return pd.concat(results).sort_values(['episode', 'agent'])


def persistence(df):
  """Calculating how often users are in top group depending on whether they were in that group last round"""
  df['top_previous'] = df.groupby('agent').top.shift(1)
  return df.groupby('top_previous').top.mean()


def relative_lift(data):
  """Calculating the relative lift in persistence"""
  pers_sim = persistence(data)
  return pers_sim[True] / pers_sim[False]


def get_data():
  df_raw = pd.read_csv("./data/season1.csv").set_index('Participant')
  df_long = df_raw.stack(level=-1).reset_index()
  df_long.columns = ['agent', 'episode', 'result']

  df_long = df_long[df_long.result.notnull()]

  episodes_remove = ['05', '07', '12.1', '12.2', '13']
  df_long = df_long[~df_long.episode.isin(episodes_remove)]
  df_long.episode.unique()

  results_top = ['HIGH', 'WIN']
  df_long['top'] = df_long.result.isin(results_top)

  return df_long
