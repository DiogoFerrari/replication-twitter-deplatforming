import sys; sys.path.append("..")
from __init__ import *
from __paths__ import *
from __constants__ import *
from pandasci import ps as ds
from distributed import Client
from os import listdir
from os.path import isfile, join
import pandas as pd
import numpy as np
import dask.dataframe as dd
import re
import pickle

# Selecting only dates after this date 
date_min_did = pd.to_datetime('2020-12-01')
date_max_did = pd.to_datetime('2021-01-20')
# Selecting only dates after this date 
date_min = '2019-09-01'
SAVE = False
SAVE = True


# * functions

def collect_user_ids(files):
    GROUP = {}
    for fn, label in files.items():
        print(f"...{fn}...")
        fn = PATH_DATA_RAW / fn
        GROUP[label] = []
        with open(fn) as file:
            for line in file:
                GROUP[label] += [line.strip()]
        GROUP[label] = set(GROUP[label])
    return GROUP

def get_daily_summaries(dfu, GROUP):
    outcomes = list((FN | NFN).keys()) + ['n']
    # 
    res = dd.from_pandas(pd.DataFrame(), 1)
    for group, user_ids in GROUP.items():
        print(f'Collecting daily totals, avg, and number of users for {group}...')
        rest = (
            dfu
            .query(f"user_id in {list(user_ids)}")
            .groupby(['date'])
        )
        tot    = rest[outcomes].sum()  
        avg    = rest[outcomes].mean()  
        nusers = rest[['user_id']].count().rename(columns={'user_id':'nusers'})
        rest=(
            dd.concat([tot.assign(stat='total'),
                       avg.assign(stat='avg')])
            .merge(nusers)
            .reset_index(drop=False)
            .assign(group=group)
        )
        res = dd.concat([res, rest])
    return res

def get_daily_percentages(dfu):
    pass

def overview(dfu, GROUP):
    dim=dfu.shape #this one takes a while
    print(f"Variables : {dim[1]}", flush=True)
    print(f"Users+days: {dim[0].compute():,.0f}", flush=True)
    if GROUP:
        for group,size in GROUP.items():
            print(f"Group: {group:30s} ; N. users: {len(size):7,.0f}", flush=True)

def get_tweet_retweet(df, newcol, fake, not_fake, case='did'):
    fns=get_tweet_retweet_idx(df, case)
    # 
    df[newcol] = np.nan
    df.loc[ fns, newcol] = df.loc[ fns, fake]
    df.loc[~fns, newcol] = df.loc[~fns, not_fake]
    return df

def get_tweet_retweet_idx(df, case):
    if case != 'daily totals':
        fns=df['nfns']==0
    else:
        fns= (df['group']=='nfns') | \
            (df['group']=='nfns_ha') |\
            (df['group']=='nfns_ma') |\
            (df['group']=='nfns_la')
    return fns


# * Loading groups

# all users 
# ---------
fn = PATH_DATA_RAW / "user_level_counts.tsv"
user_ids = ds.read_data(fn=fn, dtype=dtypes, big_data=True, usecols=['user_id'])
# 
# groups 
# ------
files = {
    "fn_sharers.txt"                 : 'fns',   
    "suspended_accounts.txt"         : "suspended",
    # 
    "high_activity_accounts.txt"     : "ha",
    "moderate_activity_accounts.txt" : "ma",
    "low_activity_accounts.txt"      : "la",
    # # 
    "qanon_accounts.txt"             : "qanon",
    "antivax_accounts.txt"           : 'av',
    "supersharers.txt"               : "ss1",
    "supersharers_alt.txt"           : 'ss5',
    # # 
    "A.txt"                          : 'A',
    "B.txt"                          : 'B',
    "D.txt"                          : 'D',
    "F.txt"                          : 'F'
}
# 
print('Collecting user ids from files...')
GROUP            = collect_user_ids(files)
print('done!')
# 
print('Creating user ids dictionary (nfns and by activity)...', end='')
GROUP['all']     = set(user_ids.user_id)
GROUP['nfns']    = GROUP['all'].difference(GROUP['fns'])
GROUP['nfns_ha'] = list(GROUP['nfns'].intersection(GROUP['ha']))
GROUP['nfns_ma'] = list(GROUP['nfns'].intersection(GROUP['ma']))
GROUP['nfns_la'] = list(GROUP['nfns'].intersection(GROUP['la']))
GROUP['A_ha']    = list(GROUP['A'].intersection(GROUP['ha']))
GROUP['B_ha']    = list(GROUP['B'].intersection(GROUP['ha']))
GROUP['D_ha']    = list(GROUP['D'].intersection(GROUP['ha']))
GROUP['F_ha']    = list(GROUP['F'].intersection(GROUP['ha']))# 
# 
GROUP['A_ma']    = list(GROUP['A'].intersection(GROUP['ma']))
GROUP['B_ma']    = list(GROUP['B'].intersection(GROUP['ma']))
GROUP['D_ma']    = list(GROUP['D'].intersection(GROUP['ma']))
GROUP['F_ma']    = list(GROUP['F'].intersection(GROUP['ma']))# 
# 
GROUP['A_la']    = list(GROUP['A'].intersection(GROUP['la']))
GROUP['B_la']    = list(GROUP['B'].intersection(GROUP['la']))
GROUP['D_la']    = list(GROUP['D'].intersection(GROUP['la']))
GROUP['F_la']    = list(GROUP['F'].intersection(GROUP['la']))# 
print('done!')
# 

# checking 
# --------
print('Number of users:')
for group, users in GROUP.items():
    print(f"{group:15s}: {len(users):,}")

# * Daily totals
# ** Deca

# loading
fn = PATH_DATA_RAW / "user_level_counts_decahose.tsv"
dfu = ds.read_data(fn=fn, dtype=dtypes, big_data=True, parse_dates=['date'])

print("\nCollecting Jul 2021 onwards totals from dask into pandas...")
dftot_deca = get_daily_summaries(dfu, GROUP)


# ** Panel 2016

fn=PATH_DATA_RAW / 'user_level_counts_2016.tsv'
dfu = ds.read_data(fn=fn, dtype=dtypes, big_data=True, parse_dates=['date'])
dfu = dfu.query(f"'2016-01-09'<=date<='2017-03-01'")
#
print(f"Computing daily totals...", end='')
# 
dfu['n']                       = dfu['fake_merged'] + dfu['not_fake']
dftot16                        = get_daily_summaries(dfu, GROUP)
# 
# dftot16['fake_pct']            = dftot16['fake_merged'] + dftot16['n']
# dftot16['fake_initiation_pct'] = dftot16['fake_merged_initiation'] + dftot16['n']
# dftot16['fake_rt_pct']         = dftot16['fake_merged_rt'] + dftot16['n']
print('done!')

# dftot16.date.max().compute()

# ** Panel 2019 and 2020-2021

# 2019 
# ----
fn = PATH_DATA_RAW / "user_level_counts_2019.tsv"
dfu = ds.read_data(fn=fn, dtype=dtypes, big_data=True, parse_dates=['date'])
# 
print("Collecting 2019 to Jun 2021 totals from dask into pandas...")
dftot19 = get_daily_summaries(dfu, GROUP)
print('done!')
# 
# 2020-2021 
# ---------
fn = PATH_DATA_RAW / "user_level_counts.tsv"
dfu = ds.read_data(fn=fn, dtype=dtypes, big_data=True, parse_dates=['date'])
# 
print("Collecting Jul 2021 onwards totals from dask into pandas...")
dftot20 = get_daily_summaries(dfu, GROUP)
print('done!')
# 
# Merging 
# -------
print(f"Concating 2019 and 2020 daily summaries...", end='')
dftot20 = dd.concat([dftot19, dftot20])
# 
# dftot20['fake_pct']            = dftot20['fake_merged'] + dftot20['n']
# dftot20['fake_initiation_pct'] = dftot20['fake_merged_initiation'] + dftot20['n']
# dftot20['fake_rt_pct']         = dftot20['fake_merged_rt'] + dftot20['n']
print('done!')

# checking (this takes a while to run)
# --------
# print('Checking min and max dates:')
# print('Merged:')
# print(dftot.date.min().compute())
# print(dftot.date.max().compute())
# print('2019:')
# print(dftot19.date.min().compute())
# print(dftot19.date.max().compute())
# print('2020:')
# print(dftot20.date.min().compute())
# print(dftot20.date.max().compute())
# print('Groups:')
# print(dftot.group.unique().compute())

# * User-level data (DiD)
# ** Merging groups

print("\nData for DiD analysis:")
fn = PATH_DATA_RAW / "user_level_counts.tsv"
dfu = ds.read_data(fn=fn, dtype=dtypes, big_data=True, parse_dates=['date'])
# 
print(f'Subsetting date range (from {date_min_did} to {date_max_did})...')
dfdid = dfu.query(f"'{date_min_did}'<=date<='{date_max_did}'")
# dfdid = ds.eDataFrame(dfdid.compute())
# 
for group, user_ids in GROUP.items():
    print(f'Merging group indicator for group *{group}* to user-level data ...', end='')
    dfgroup = pd.DataFrame({'user_id':list(user_ids), group:1})
    dfdid = dfdid.merge(dfgroup, how='left', on=['user_id'])
    dfdid[group] = dfdid[group].fillna(0)
    print('done!')

# ** Merging demographics 

fn = PATH_DATA_RAW / "user_demographics.csv"
dfd = ds.read_data(fn=fn, dtype=dtypes)
dfd
# 
vars = {'user_id'                                   :"user_id", 
        'voterbase_age'                             :'age',
        'voterbase_gender'                          :'gender',
        'voterbase_race'                            :'race',
        "tsmart_presidential_general_turnout_score" :'pres_gen',
        "tsmart_presidential_primary_turnout_score" :'pres_pri',
        "tsmart_partisan_score"                     :"ptyid"}
dfd=(dfd
     .rename(columns={"twProfileID":'user_id'})
     # .select_rows(query=f"user_id in {list(dfdid.user_id.values)}")
     .select_cols(names=vars)
     .drop_duplicates()
     .mutate({'female': lambda col: [1 if gender=='Female' else 0
                                     for gender in col['gender']],
              # 
              'white' : lambda col: [1 if race=='Caucasian' else 0
                                     for race in col['race']]
              }))
print('Merging demographics...', end='')
dfdid = dfdid.merge(dfd, how='left', on=['user_id'])
print('done!')

# print(dfdt.tab('race', 'white').to_string())
# print(dfdt.tab('gender', 'female').to_string())

# ** Merging dosage

print("Merging dosage...", end='')
path = PATH_DATA_RAW
fns = [f for f in listdir(path) if isfile(join(path, f))]
fns = [f for f in fns if 'dosage' in f ]
fns.sort()
# 
for fn in fns:
    dosage = re.sub(pattern='.txt', repl='', string=fn)
    fn = path / fn
    with open(fn) as file:
        dose = ds.eDataFrame({'user_id':file, dosage:1})\
                 .mutate({'user_id': lambda col: col['user_id'].str.strip()})
    print(f"Merging {dosage}...", end='')
    dfdid = dfdid.merge(dose, how='left', on=['user_id'])
    dfdid[dosage] = dfdid[dosage].fillna(0)
    print('done!')
# 
print('done!')

# ** Creating tweet and retweet

# # s = 'not_'
# # [c for c in dfdid.columns if s in c ]

print("Creating tweet and retweet (fake or not fake column for nfs/nfns)...", end='')
# 
dfdid['tweet']   = np.nan
dfdid['retweet'] = np.nan
dfdid['trt']     = np.nan
dfdid = dfdid.map_partitions(get_tweet_retweet, 'tweet'  ,'fake_merged_initiation','not_fake_initiation')
dfdid = dfdid.map_partitions(get_tweet_retweet, 'retweet','fake_merged_rt'        , 'not_fake_rt')
dfdid = dfdid.map_partitions(get_tweet_retweet, 'trt'    ,'fake_merged'           , 'not_fake')
#
print('done!')




# * saving

if SAVE:
    # Daily totals 
    # ------------
    fn = "panel-2016-daily-totals.csv"
    print(f'Saving panel data with daily totals ({fn})...', end='')
    dftot16.to_csv(PATH_DATA_FINAL / fn, sep=';', index=False, decimal='.')
    print('done!')
    # 
    fn = "panel-2020-daily-totals.csv"
    print(f'Saving panel data with daily totals ({fn})...', end='')
    dftot20.to_csv(PATH_DATA_FINAL/fn, sep=';', index=False, decimal='.')
    print('done!')
    # 
    fn = "decahose-daily-totals.csv"
    print(f'Saving panel data with daily totals ({fn})...', end='')
    dftot_deca.to_csv(PATH_DATA_FINAL/fn, sep=';', index=False, decimal='.')
    print('done!')
    # 
    # User-level information 
    # ----------------------
    fn = "panel-2020-user-level-did.csv"
    print(f'Saving panel user level data for DiD ({fn})...', end='')
    dfdid.to_csv(PATH_DATA_FINAL/fn, sep=';', index=False, decimal='.')
    print('done!')


# * done
print("All done!")
