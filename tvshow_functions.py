import pandas as pd
import numpy as np


def create_agents(parameters):
  return pd.Series(np.random.normal(0, parameters['std_agent_skill'], parameters['number']))


def simulate_round(parameters, agents):
  """Simulate a round and return the distribution of the result"""
  df = pd.DataFrame(agents, columns=['skill'])
  std_noise = parameters['std_noise']
  df['noise'] = np.random.normal(0, std_noise)
  df['outcome'] = df.skill + df.noise

  df['rank'] = df.outcome.rank().astype(int)

  return df


def simulate_show(parameters):
    agent_skill = create_agents(parameters)

    results = []
    agents_after_round = agent_skill.index
    while len(agents_after_round) > 1:
      result = simulate_round(parameters, agent_skill[agents_after_round])
      lowest_outcome = result.outcome.min()
      agents_after_round = result[result.outcome > lowest_outcome].index
      results.append(agents_after_round)
      len(result)
      len(agents_after_round)
    return results

