import pandas as pd
import numpy as np

import sys
import imp

import tvshow_functions as f

number = 10
std_agent_skill = 1
std_noise = 1

parameters = {
  'number': number,
  'std_agent_skill': std_agent_skill,
  'std_noise': std_noise
}

f.simulate_show(parameters)



