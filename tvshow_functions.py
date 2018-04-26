import pandas as pd
import numpy as np
import statsmodels.formula.api as smf 


# The standard deviation of contestant skill. This is a normalization.
STD_AGENT_SKILL = 1


def create_contestants(parameters):
  """Returns a series with contestant ids and skill"""
  return pd.Series(np.random.normal(0, STD_AGENT_SKILL, parameters['number']))


def simulate_round(parameters, contestants):
  """Simulate a round and return the distribution of the result"""
  df = pd.DataFrame(contestants, columns=['skill'])
  std_noise = STD_AGENT_SKILL * parameters['relative_std_noise']
  df['noise'] = np.random.normal(0, std_noise, len(df))
  df['outcome'] = df.skill + df.noise
  df['rank'] = df.outcome.rank().astype(int)
  number = len(contestants)
  df['top'] = df['rank'].div(number) >= 1 - parameters['share_top']

  return df


def simulate_show(parameters):
  """Simulate a show, returning a list of results from each round"""

  contestant_skill = create_contestants(parameters)

  results = []
  contestants_after_round = contestant_skill.index
  episode = 1
  while len(contestants_after_round) > 1:
    result = simulate_round(parameters, contestant_skill[contestants_after_round])
    lowest_outcome = result.outcome.min()
    results_after_round = result.ix[result.outcome > lowest_outcome, ['skill', 'noise', 'outcome', 'rank', 'top']]
    results_after_round['episode'] = episode
    results_after_round['contestant'] = results_after_round.index
    contestants_after_round = results_after_round.index
    results.append(results_after_round)
    episode += 1
  return pd.concat(results).sort_values(['episode', 'contestant'])


def persistence(df):
  """Calculating how often users are in top group depending on whether they were in that group last round"""
  df['top_previous'] = df.groupby('contestant').top.shift(1)
  return df.groupby('top_previous').top.mean()


def relative_lift(data):
  """Calculating the relative lift in persistence"""
  pers_sim = persistence(data)
  return pers_sim[True] / pers_sim[False]


episodes_remove = {
  1: ['05', '07', '12.1', '12.2', '13'],
  2: ['06', '08', '10', '12', '14', '20']
}


SHOW_MASTERCHEF = 1
SHOW_BAKEOFF = 2

SEASONS = {
  SHOW_MASTERCHEF: range(1, 9),
  SHOW_BAKEOFF: range(1, 9)
}


COLUMN_ORDER = ['season', 'episode', 'contestant', 'result']

RESULTS = {
  SHOW_MASTERCHEF: {
    'top': ['HIGH', 'WIN', 'IMM'],
    'bottom': ['ELIM', 'LOW'],
    'low': ['LOW'],
    'out': ['ELIM']
  },
  SHOW_BAKEOFF: {
    'top': ['SB', 'FAVE'],
    'bottom': ['OUT', 'LOW'],
    'low': ['LOW'],
    'out': ['OUT']
  }
}
    


def get_data_bakeoff_for_season(season):
  df_raw = pd.read_csv("./data/Great British Bake Off/season%s.csv" % season).set_index('participant')
  df_long = df_raw.stack(level=-1).reset_index()
  df_long.columns = ['contestant', 'episode', 'result']
  df_long['season'] = season

  df_long = df_long[df_long.result.notnull()]

  return df_long[COLUMN_ORDER]

  
def get_data_masterchef_for_season(season):
  df_raw = pd.read_csv("./data/MasterChef/season%s.csv" % season).set_index('participant')
  df_long = df_raw.stack(level=-1).reset_index()
  df_long.columns = ['contestant', 'episode', 'result']
  df_long['season'] = season

  df_long = df_long[df_long.result.notnull()]

  # df_long = df_long[~df_long.episode.isin(episodes_remove.get(season, []))]
  return df_long[COLUMN_ORDER]


DATA_GETTERS = {
  SHOW_MASTERCHEF: get_data_masterchef_for_season,
  SHOW_BAKEOFF: get_data_bakeoff_for_season
}
    
def get_data(data_type):

  getter = DATA_GETTERS[data_type]
  df = pd.concat([getter(season) for season in SEASONS[SHOW_BAKEOFF]])

  results = RESULTS[data_type]

  for (name, values) in results.items():
    df[name] = df.result.isin(values)

  # Since participants are known only by their first name, we create a new
  # contestant id (defined on season + name) that uniquely identifies a person.
  contestant_ids = df[['season', 'contestant']].drop_duplicates()
  contestant_ids['contestant_id'] = range(len(contestant_ids))
  df = df.merge(contestant_ids, on=['season', 'contestant']).sort_values(['season', 'episode'])

  return df


def get_results(df):
  df = df[~df.episode.str.endswith('.1')].copy()
  df.shape

  df['number'] = df.groupby(['season', 'episode']).contestant.transform(lambda x: len(x))
  df['top_previous'] = df.groupby('contestant_id').top.shift(1)
  df['bottom_previous'] = df.groupby('contestant_id').bottom.shift(1)
  df['low_previous'] = df.groupby('contestant_id').low.shift(1)

  df_reg = df[df.bottom_previous.notnull()]

  smf.ols('I(out*1) ~ I(top_previous*1) + number', data=df_reg).fit().summary()
  smf.ols('I(out*1) ~ I(top_previous*1) + I(low_previous*1) + number', data=df_reg).fit().summary()

  df_reg.top.mean()

  mod_top = smf.ols('I(top*1) ~ I(top_previous*1) + I(low_previous*1) + number', data=df_reg).fit()
  mod_bottom = smf.ols('I(bottom*1) ~ I(top_previous*1) + I(low_previous*1) + number', data=df_reg).fit()

  res = pd.DataFrame(index=mod_top.params.index)
  res['beta_top'] = mod_top.params
  res['std_top'] = mod_top.params / mod_top.tvalues
  res['beta_bottom'] = mod_bottom.params
  res['std_bottom'] = mod_bottom.params / mod_bottom.tvalues

  results = {
    'means': df[['top', 'bottom']].mean(),
    'table': res
  }

  return results
