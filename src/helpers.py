import pandas as pd
import numpy as np
from scipy.special import binom

def clean_dataframe(race_df, secs_col_list):
    """
    Clean df for use in glicko functions
    """
    # filter out non finishers
    race_df = race_df.query('position != "DNS"'
                           ).query('position != "DNF"'
                           ).query('position != "LAP"'
                           ).query('position != "DSQ"')
    
    # turn all times into integer seconds
    adj_df = race_df.apply(lambda x:  pd.to_timedelta(x, unit = 's').dt.seconds \
                           if x.name in secs_col_list else x)
    return adj_df

def one_vs_one(race_df, period):
    """
    Converts a race df of finish positions into a n(n-1)/2 length df
    of "one on one" races between competitors
    Also need the "period" of the race, starting from 1 with the first
    race of the series in 2009
    """
    # determine number of racers
    num_racers = race_df.shape[0]
    
    # determine num rows
    num_rows = int(binom(num_racers,2))
    
    # create the empty df to be filled in
    # created as per specs of R glicko fnc
    ovo_df = pd.DataFrame(data = {"time": (np.ones(num_rows)*period).astype(int),
                                  "player1": np.zeros(num_rows).astype(int),
                                  "player2": np.zeros(num_rows).astype(int),
                                  "result": (np.ones(num_rows)).astype(int)})
    
    # iterate through the df and fill in the playerID's
    # need to look at two rows at a time and fill in the appropriate
    # player 1 and player 2
    ovo_row = 0
    for i in range(num_racers - 1):
        for j in range(i+1, num_racers):
            # Player 1 ID
            ovo_df.iloc[ovo_row,1] = race_df.iloc[i, 1]
            # Player 2 ID
            ovo_df.iloc[ovo_row,2] = race_df.iloc[j, 1]
            ovo_row +=1
            
    return ovo_df