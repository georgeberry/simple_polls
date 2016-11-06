import json
import pandas as pd
import numpy as np
from datetime import datetime

"""
Got the polls, uniqify them!

Format them for modeling

Uniqify by:
    State / start date / end date / pollster

Output data frame like so:

State, Rep, Dem, Pollster, Observations, Start-day-of-year
"""

def convert_date_to_doy(datestr):
    return datetime.strptime(datestr, '%Y-%m-%d').timetuple().tm_yday

if __name__ == '__main__':
    polls_dict = {}

    with open('polls.json', 'r') as f:
        for line in f:
            j = json.loads(line)
            try:
                state, start_date, end_date, pollster = \
                    j['state'], j['start_date'], j['end_date'], j['pollster']
                polls_dict[(state, start_date, end_date, pollster)] = j
            except KeyError:
                continue
    df = pd.DataFrame(list(polls_dict.values()))
    df = df[np.isfinite(df['Dem'])]
    df = df[np.isfinite(df['Rep'])]
    df['dem_adv'] = df['Dem'] - df['Rep']
    df['doy'] = df['start_date'].apply(convert_date_to_doy)
    df.to_csv('polls.csv')
