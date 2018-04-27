# Overview

This repo provides standardized data on outcomes for the following reality TV shows:

- The Great British Bake Off (All seasons, 1-8)
- MasterChef (All seasons, 1-8)

The data provide the outcomes for each contestant and episode in CSV format. 

We also provide python code to clean and combine the data. For instance, you can obtained the cleaned data for MasterChef as follows:

```
>>> df = f.get_data(tvshow_functions.SHOW_MASTERCHEF)
>>> ## How many results are there?
>>> df.groupby('season').result.count()
season
1    127
2    236
3    244
4    335
5    320
6    297
7    280
8    360
Name: result, dtype: int64
```

In the resulting data, one row describes the outcome for a contestant in a single episode.

# Data collection

We've obtained the raw data from Wikipedia. For instance, you can find the outcomes for season 1 of MasterChef [here](https://en.wikipedia.org/wiki/MasterChef_(U.S._season_1) under the "Elimination Table". 

# Analytics

The code also provides some analyses that we use to make sense of the data, for instance the analysis of how persistent good and bad outcomes are.
