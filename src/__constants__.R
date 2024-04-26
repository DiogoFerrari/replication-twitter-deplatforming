
# For DiD 
# -------
FN = c(
    'fake_merged'     = "Shared misinformation",
    'fake_initiation' = "Tweeted misinformation",
    'fake_rt'         = "Retweeted misinformation"
)
NFN = c(
    'not_fake'           = "Shared information",
    'not_fake_initiation'= "Tweeted information",
    'not_fake_rt'        = "Retweeted information",
    # 
    'not_fake_conservative'           = "Conservative URL",
    'not_fake_conservative_initiation'= "Conservative URL",
    'not_fake_conservative_rt'        = "Conservative URL",
    # 
    'not_fake_liberal'            = "Liberal URL",
    'not_fake_liberal_initiation' = "Liberal URL",
    'not_fake_liberal_rt'         = "Liberal URL",
    # 
    'not_fake_shopping'            = "Shared shopping information",
    'not_fake_shopping_initiation' = "Tweeted shopping information",
    'not_fake_shopping_rt'         = "Retweeted shopping information",
    # 
    'not_fake_sports'              = "Shared sports news",
    'not_fake_sports_initiation'   = "Tweeted sports news",
    'not_fake_sports_rt'           = "Retweeted sports news"
)
OUTCOMES = c(
    ## these are misinfo URL for fns and not-misinfo for nfns
    'tweet'   = "Tweets",           
    'retweet' = "Retweets",
    'trt'     = "Tweets or retweets",
    ## these are misinfo URL for both fns and nfns
    ## 'fake_initiation' = "Misinformation tweets",
    ## 'fake_rt'         = "Misinformation retweets",
    ## 'fake'            = "Misinformation tweets and retweets"
    'fake'            = "Shared misinformation",
    'fake_initiation' = "Tweeted misinformation",
    'fake_rt'         = "Retweeted misinformation"
    )
OUTCOMES.ROBUSTNESS.FN.LIST = c(
    'fake_grinberg_rb_initiation'  = "Tweeted misinformation (Grinberg RB)",
    'fake_grinberg_initiation'     = "Tweeted misinformation (Grinberg)",
    'fake_newsguard_initiation'    = "Tweeted misinformation (Newsguard)",
    'fake_grinberg_rb_rt'          = "Retweeted misinformation (Grinberg RB)",
    'fake_grinberg_rt'             = "Retweeted misinformation (Grinberg)",
    'fake_newsguard_rt'            = "Retweeted misinformation (Newsguard)"
    ## 
)
OUTCOMES.ROBUSTNESS.NFN = c(
    'not_fake_shopping_initiation' = "Tweeted shopping",
    'not_fake_sports_initiation'   = "Tweeted sports",
    'not_fake_shopping_rt'         = "Retweeted shopping",
    'not_fake_sports_rt'           = "Retweeted sports"
)
OUTCOMES.ALL = c(OUTCOMES, OUTCOMES.ROBUSTNESS.FN.LIST, OUTCOMES.ROBUSTNESS.NFN)
FN_GROUPS = c(
    ## 'all'           = "All groups",
    'fns'          = 'All Misinformation sharers',
    'ss1'           = "Supersharer 0.1%",
    'ss5'           = "Supersharer 5%",
    'qanon'         = "QAnon",
    'av'            = "Antivax",
    'tf'            = "Trump followers",
    ## 
    'suspended'     = "Deplatformed user",
    'followers'     = "Followers",
    'non_followers' = "Non-followers",
    'all'           = "All activity levels",
    'ha'            = 'High activity users',
    'la'            = 'Low activity users',
    'ma'            = 'Moderate activity users',
    ## 
    'A'     = " Trump-only followers",
    'B'     = "Followers",
    'D'     = "Followers (4+)",
    'F'     = 'Not-followers',
    'nfns'  = 'Not misinformation sharers',
    ## 
    'A_ha'     = " Trump-only followers (high activity)",
    'B_ha'     = "Followers (high activity)",
    'D_ha'     = "Followers (4+) (high activity)",
    'F_ha'     = 'Not-followers (high activity)',
    'nfns_ha'  = 'Not misinformation sharers (high activity)',
    ## 
    'A_ma'     = " Trump-only followers (moderate activity)",
    'B_ma'     = "Followers (moderate activity)",
    'D_ma'     = "Followers (4+) (moderate activity)",
    'F_ma'     = 'Not-followers (moderate activity)',
    'nfns_ma'  = 'Not misinformation smarers (moderate activity)',
    ## 
    'A_la'     = " Trump-only followers (low activity)",
    'B_la'     = "Followers (low activity)",
    'D_la'     = "Followers (4+) (low activity)",
    'F_la'     = 'Not-followers (low activity)',
    'nfns_la'  = 'Not misinformation sharers (low activity)'
    ## 'B'     = "Deplatformed followers",
    ## 'D'     = "Deplatformed followers (4+)",
    ## 'F'     = 'Not deplatformed followers'
    ## 
    ## 'A'     = " Trump-only followers (Group A)",
    ## 'B'     = "Deplatformed followers, including Trump followers (Group B)",
    ## 'C'     = "Deplatformed followers, excluding Trump followers (Group C)",
    ## 'D'     = "Four+ Deplatformed followers, including Trump followers (Group D)",
    ## 'E'     = "Four+ Deplatformed followers, excluding Trump followers (Group E)",
    ## 'F'     = 'Not deplatformed followers (Group F)'
    ## "others"        = 'Others'
    ## 
    ## 
    ## "All subgroups"   = 'all',
    ## 'Supersharers 1%' = 'ss1',
    ## 'Supersharers 5%' = 'ss5',
    ## 'QAnon'           = 'qanon',
    ## 'Antivax'         = 'av',
    ## "Others"          = 'others'
)
DEMOGRAPHICS = c(
    'voterbase_age'                             = 'Age',
    'female'                                    = 'Female',
    'race'                                      = "Race",
    'white'                                     = "White",
    'tsmart_partisan_score'                     = "Partisan score",
    'tsmart_presidential_general_turnout_score' = 'Turnout score (general)',
    'tsmart_presidential_primary_turnout_score' = 'Turnout score (primary)'
)
## 
CONSTANTS = c(
    "FN"  = FN,
    "NFN" = NFN,
    "FN_GROUPS" = FN_GROUPS,
    "DEMOGRAPHICS"= DEMOGRAPHICS
)
