# Overview

This repo provides data on outcomes for the following reality TV shows:

- The Great British Bake Off (All seasons, 1-8)
- MasterChef (All seasons, 1-8)

The data provide the outcomes for each contestant and episode in CSV format. 

We also provide python code to clean and combine the data. For instance, you can obtained the cleaned data for MasterChef as follows:

```
df_masterchef = f.get_data(f.SHOW_MASTERCHEF)
```

Which results in a DataFrame in which one row describes the outcome for a contestant in a single episode.

# Data collection

We've obtained the raw data from Wikipedia. For instance, you can find the outcomes for season 1 of MasterChef [here](https://en.wikipedia.org/wiki/MasterChef_(U.S._season_1) under the "Elimination Table". 

# Analytics

The code also provides some analyses that we use to make sense of the data, for instance the analysis of how persistent good and bad outcomes are.
