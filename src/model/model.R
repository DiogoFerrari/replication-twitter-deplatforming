library(glue)
library(magrittr)
library(did)
library(psych)
library(scales)
library(ggrepel)
library(ggh4x)
library(gridExtra)
library(sandwich)
library(ComplexUpset)
library(furrr)
library(lmtest)
library(patchwork)
library(modelsummary)
library(rdrobust)
library(glue)
library(stringr)
library(writexl)
library(tidyverse)
## 
source("../__constants__.R")
source("../__settings__.R")
source("../__paths__.R")
## 
SAVE=FALSE
SAVE=TRUE
## ----
## parameters
jan01          = as.Date('2021-01-01')
jan06          = as.Date('2021-01-06')
jan06.17       = as.Date('2017-01-06')
jan12          = as.Date('2021-01-12')
jan12.17       = as.Date('2017-01-12')
dez01          = as.Date('2020-12-01')
jan29          = as.Date('2021-01-29')
## DiD
date_min_did = as.Date('2020-12-01')
date_max_did = as.Date('2021-01-20')
max_date     = as.Date('2021-02-28')
## RDD
cutoff_low        = as.Date('2021-01-06')
cutoff_high       = as.Date('2021-01-12')
cutoff.low.jan05  = as.Date('2021-01-05')
cutoff.high.jan05 = as.Date('2021-01-12')
cutoff.low.jan07  = as.Date('2021-01-07')
cutoff.high.jan07 = as.Date('2021-01-12')
cutoff.low.dez20  = as.Date('2020-12-20')
cutoff.high.dez20 = as.Date('2020-12-21')
cutoff.low.jan18  = as.Date('2021-01-18')
cutoff.high.jan18 = as.Date('2021-01-19')
## 
time.start <- Sys.time() # code time profile

## * functions

recoding <- function(df)
{
    df = (
        df 
        %>% mutate(date= as.Date(date),
                   pres_gen=as.numeric(pres_gen),
                   pres_pri=as.numeric(pres_pri),
                   ptyid=as.numeric(ptyid),
                   female=as.numeric(female)
                   )
        %>% group_by(date)
        %>% mutate(period = cur_group_id())
        %>% group_by(user_id)
        %>% mutate(id = cur_group_id())
        %>% ungroup(.) 
        %>% mutate() 
    )
    return(df)
}

collect_treat_groups <- function(df, treat, control, t0, t1)
{
    df["treat"]   = df[treat]
    df["control"] = df[control]
    df = (
        df
        %>% filter(treat==1 | control ==1) 
        %>% mutate(t = case_when(date>=!!t1 ~ 1,
                                 date<=!!t0 ~ 0),
                   d = case_when(treat  ==1 ~ 1,
                                 control==1 ~ 0)
                   )
    )
    return(df)
}

collect_did_data <- function(df, treat, control, outcomes, controls,
                             t1, t0, date_min_did , date_max_did)
{
    tab = (
        df
        %>% filter(date_min_did <= date & date<=date_max_did) 
        %>% filter(date <= t0 | t1 <= date ) 
        %>% collect_treat_groups(.,
                                 treat=treat, control=control,
                                 t1=t1, t0=t0) 
        %>% select(date, d,t,  treat, !!treat, control, !!control,
                   outcomes, controls, id)
    )
    return(tab)
}

estimate_non_parametric <- function(data, y)
{
    ## High activity users information
    ns = (
        data
        %>% select(date, t, d, id)
        %>% group_by(t, d)
        %>% summarise(nusers = length(unique(id)),
                      ndays = length(unique(date))) 
    )
    ns
    nonparametric = (
        data
        %>% select(date, t, d,  y=!!y)
        %>% group_by(t, d)
        %>% summarise(avg=mean(y),
                      tot=sum(y)) 
        %>% ungroup(.) 
        %>% left_join(., ns , by=c("d", "t")) 
        %>% mutate(tot_est= avg*(nusers*ndays))
    )
    y00 = nonparametric%>% filter(t==0 & d==0) %>% dplyr::pull(avg)
    y01 = nonparametric%>% filter(t==0 & d==1) %>% dplyr::pull(avg)
    y10 = nonparametric%>% filter(t==1 & d==0) %>% dplyr::pull(avg)
    y11 = nonparametric%>% filter(t==1 & d==1) %>% dplyr::pull(avg)
    nonparametric = (
        nonparametric
        %>% mutate(
                didnp = (y11 - y10) - (y01 - y00),
                `Ratio followers/non-followers (pre)`=y01/y00,
                `Ratio followers/non-followers (after)`=y11/y10,
                )
    )
    return(nonparametric)
}

save_reg_table <- function(tab, tableid, caption, depvars, SAVE)
{
    names(tab)=depvars
    res = modelsummary(tab,
                 ## estimate  = "{estimate}{stars}\n({conf.low}, {conf.high})",
                 statistic='({conf.low}, {conf.high}) (t={statistic}, p={p.value})',
                 stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                 vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                 coef_omit = "Intercept",
                 output='data.frame'
                 )%>%
        select(term, !!depvars)
    res$term[duplicated(res$term)] <- ""
    res = res %>% rename(!!caption:=term)
    if (SAVE) {
        fn=file.path(PATH_MAN_TABLES, glue('tab-{tableid}.csv'))
        write.table(x=res, file=fn, row.names=F, sep=';')
        fn=file.path(PATH_MAN_TABLES, glue('tab-{tableid}.xlsx'))
        write_xlsx(x=res, path=fn)
    }
    return(res)
}

get_daily_outcome <- function(group, outcome, stat, act)
{
    group = ifelse(act=='all', group, glue("{group}_{act}"))
    res = (
        dftot 
        %>% filter(stat==!!stat) 
        %>% filter(group==!!group)  
        %>% select(date, outcome=!!outcome, group) 
        %>% mutate(y = c(scale(outcome)),
                   y.type=!!outcome,
                   stat=stat,
                   act = act,
                   )
    )
    return(res)
}

get_events <- function(ndays, impeach, floyd, lafay, acb, insurr)
{
    impeach.seq = seq(from=as.Date(impeach)-ndays, to=as.Date(impeach)+ndays, 1) 
    floyd.seq   = seq(from=as.Date(floyd  )-ndays, to=as.Date(floyd  )+ndays, 1) 
    lafay.seq   = seq(from=as.Date(lafay  )-ndays, to=as.Date(lafay  )+ndays, 1) 
    acb.seq     = seq(from=as.Date(acb    )-ndays, to=as.Date(acb    )+ndays, 1) 
    insurr.seq  = seq(from=jan06-ndays, to=jan06+ndays, 1) 
    events = (
        tribble(
            ~event    ,~date      ,~date.event      , ~label ,
            "impeach" ,impeach.seq,impeach,  "Impeachment charges",
            "floyd"   ,floyd.seq  ,floyd  ,  "George Floyd murder",
            "lafay"   ,lafay.seq  ,lafay  ,  "Protest at Lafayette Sq.",
            "acb"     ,acb.seq    ,acb    ,  "ACB nominated",
            "jan06"   ,insurr.seq ,insurr ,  "Insurrection/Deplatforming"
        )  
        %>% mutate(label = factor(label, levels=c("Impeachment charges",
                                                  "George Floyd murder",
                                                  "Protest at Lafayette Sq.",
                                                  "ACB nominated",
                                                  "Insurrection/Deplatforming"))) 
        %>% unnest()
    )
    return(events)
}

complete_suspended <- function(df, group, outcomes, max_date)
{
    if (group=='suspended') {
        complete = tibble(date=seq(from=jan12, to=max_date, length=100)) 
        complete[outcomes] = 0
        df = (
            df 
            %>% filter(date<jan12)  
            %>% bind_rows(complete)
        )
    }
    return(df)
}

tidy_rdd <- function(res, y, stat=None, group=None)
{
    tmp = tibble(parameter=rownames(res$coef),
                 coef=c(res$coef),
                 se = c(res$se),
                 ci.low = c(res$ci[,1]),
                 ci.high = c(res$ci[,2]),
                 pv = c(res$pv) 
                 ) %>%
        mutate(group   = group,
               outcome = y,
               stat    = stat,
               group   = group
               )
    return(tmp)
}

rdd_report_details <- function(res)
{
  info <- list()
  
  for (i in 1:nrow(res)){
    est <- res$est[[i]]
    cutoff_date <- res$data[[i]] %>%
      filter(score == 0) %>%
      pull(date) %>%
      as.character() %>%
      .[1]
    
    info <- info %>%
        bind_rows(tibble(
            group        = res$group[i],
            ss           = sum(est$N),
            ss_before    = est$N[1],
            ss_after     = est$N[2],
            ss_effective = sum(est$N_h),
            bw_left      = est$bws["h", "left"],
            bw_right     = est$bws["h", "right"],
            cutoff_date  = as.Date(cutoff_date, "%Y-%m-%d")
        )
        )
  }
  summ = info %>%
      dplyr::summarise_all(list(mean)) 
  
  print(info, n=Inf, width=Inf) 
  cat("\n")
  cat("Average across groups:\n")
  print(t(summ))
}


## * loading

path = file.path(PATH_DATA_FINAL, "panel-2020-user-level-did.csv/")
fns = list.files(path=path, full.names = TRUE)
dfdid = lapply(fns, function(fn) read_delim(file=fn, delim=";")) %>% bind_rows(.)
dfdid = recoding(dfdid)
## 
## daily totals
## ------------
## 2016
path = file.path(PATH_DATA_FINAL, "panel-2016-daily-totals.csv/")
fns = list.files(path=path, full.names = TRUE)
dftot16 = lapply(fns, function(fn) read_delim(file=fn, delim=";",
                                            col_types = cols("group"=col_character())))
dftot16 = dftot16[lapply(dftot16, nrow)>0] %>% bind_rows()
dftot16 = dftot16 %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))
## 2020
path = file.path(PATH_DATA_FINAL, "panel-2020-daily-totals.csv/")
fns = list.files(path=path, full.names = TRUE)
dftot = lapply(fns, function(fn) read_delim(file=fn, delim=";",
                                            col_types = cols("group"=col_character())))
dftot = dftot[lapply(dftot, nrow)>0] %>% bind_rows()
## decahose
path = file.path(PATH_DATA_FINAL, "decahose-daily-totals.csv/")
fns = list.files(path=path, full.names = TRUE)
dftot.deca = lapply(fns, function(fn) read_delim(file=fn, delim=";",
                                            col_types = cols("group"=col_character())))
dftot.deca = dftot.deca[lapply(dftot.deca, nrow)>0] %>% bind_rows()
dftot.deca = dftot.deca %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))
## rename fake_merged* to fake*
## -----
dfdid   = dfdid %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))
dftot   = dftot %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))
dftot16 = dftot16 %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))
dftot.deca = dftot.deca %>% rename_all(list(~str_replace(string=., pattern="_merged", replacement="")))

#
## * Estimate
## ** SRD
## *** Main results

print("Estimaring the SRD (main)...")
## 
cutoffs = tibble::tribble(~cutoff.low     , ~cutoff.high,
                          cutoff_low      , cutoff_high,)
outcomes = c('fake_rt', 'fake_initiation') 
groups = c(
    'suspended',
    'F',
    'A',
    'B',
    'D',
    #,
    "all",
    'fns',
    'nfns',
    'supersharers1',
    'qanon',
    'antivax'      
)
max_date = dftot$date %>% max(., na.rm = T)
res.rdd.main = (
    dftot
    %>% select(date, group, stat, outcomes)
    %>% filter(group %in%  groups)  
    %>% nest(-group, -stat) 
    %>% left_join(., crossing(group=groups, outcome=outcomes) , by=c('group'))  
    %>% left_join(., crossing(group=groups, cutoffs) , by=c('group'))   
    %>% mutate(
            data = future_map2(.x=data, .y=group, function(.x, .y) 
                complete_suspended(.x, .y, !!outcomes, !!max_date) ),
            data = future_pmap(list(data=data,
                                    cutoff.low=cutoff.low,
                                    cutoff.high=cutoff.high),
                               function(data, cutoff.low, cutoff.high)
                                   data
                               %>% mutate(score = case_when(
                                              date <= cutoff.low  ~ as.integer(date - cutoff.low),
                                              date >= cutoff.high ~ -1*as.integer(cutoff.high - date)+1))
                               ),
            ##         ## 
            data = future_map(.x=data, function(.x) .x %>% drop_na()),
            est = future_map2(.x=data, .y=outcome, function(.x, .y)
                rdrobust(y=.x %>% pull(.y), x=.x %>% pull('score'), c=0, all=T)),
            summ = future_pmap(list(est, outcome, stat, group), function(est, outcome, stat, group)
                tidy_rdd(est, outcome, stat, group)  , .progress=TRUE)
            )
)
## 
print('done!')


## checking
## --------
## res.rdd.main
## res=res.rdd.main$est[[1]]
## res.rdd.main$summ[[1]]
## complete suspended
## group='suspended'
## cutoff = cutoff.low.jan07
## cutoff = cutoff.low.jan18
## cutoff = cutoff.low.dez20
## cutoff = cutoff_low
## print(glue("\n\nCutoff: {cutoff}") )
## (
##     res.rdd.main 
##     %>% filter(cutoff.low==cutoff) 
##     %>% filter(group==group)  
##     %>% pull(data) 
##     %>% extract2(1) 
##     %>% filter(date %in% seq(from=cutoff-4, to=cutoff+8, length=100) )  
## ) %>% print(., n=Inf, width=Inf) 


## *** Robustness to alternative dates

print("Estimaring the SRD models (robustness to date)...")
## 
cutoffs = tibble::tribble(
                      ~cutoff.low     , ~cutoff.high,
                      cutoff.low.jan05, cutoff.high.jan05,
                      cutoff.low.jan07, cutoff.high.jan07,
                      cutoff.low.jan18, cutoff.high.jan18,
                      cutoff.low.dez20, cutoff.high.dez20,
                      )
outcomes = c('fake_rt', 'fake_initiation')
groups = c(
    'suspended',
    'F',
    'A',
    'B',
    'D',
    #,
    "all",
    'fns',
    'nfns',
    'supersharers1',
    'qanon',
    'antivax'      
)
max_date = dftot$date %>% max(., na.rm = T)
res.rdd.robust.date = (
    dftot
    %>% select(date, group, stat, outcomes)
    %>% filter(group %in%  groups)  
    %>% nest(-group, -stat) 
    %>% left_join(., crossing(group=groups, outcome=outcomes) , by=c('group'))  
    %>% left_join(., crossing(group=groups, cutoffs) , by=c('group'))   
    %>% mutate(
            data = future_map2(.x=data, .y=group, function(.x, .y) 
                complete_suspended(.x, .y, !!outcomes, !!max_date) ),
            data = future_pmap(list(data=data,
                                    cutoff.low=cutoff.low,
                                    cutoff.high=cutoff.high),
                               function(data, cutoff.low, cutoff.high)
                                   ## data = future_map(.x=data, function(.x) 
                data
                %>% mutate(score = case_when(
                               date <= cutoff.low  ~ as.integer(date - cutoff.low),
                               date >= cutoff.high ~ -1*as.integer(cutoff.high - date)+1))
                ),
            ##         ## 
            data = future_map(.x=data, function(.x) .x %>% drop_na()),
            est = future_map2(.x=data, .y=outcome, function(.x, .y)
                rdrobust(y=.x %>% pull(.y), x=.x %>% pull('score'), c=0, all=T)),
            summ = future_pmap(list(est, outcome, stat, group), function(est, outcome, stat, group)
                tidy_rdd(est, outcome, stat, group)  , .progress=TRUE)
            ) 
)
## 
print('done!')

## checking
## --------
## res.rdd.robust.date

## *** Robustness to alternative outcomes


cutoffs = tibble::tribble(~cutoff.low     , ~cutoff.high,
                          cutoff_low      , cutoff_high)
## 
print("Estimaring the SRD models (robustness to outcome)...")
outcomes = c(names(OUTCOMES.ROBUSTNESS.FN.LIST), names(OUTCOMES.ROBUSTNESS.NFN))
groups = c(
    'suspended',
    'F',
    'A',
    'B',
    'D',
    #,
    "all",
    'fns',
    'nfns',
    'supersharers1',
    'qanon',
    'antivax'      
)
max_date = dftot$date %>% max(., na.rm = T)
res.rdd.robust.outcome = (
    dftot
    %>% select(date, group, stat, outcomes)
    %>% filter(group %in%  groups)  
    %>% nest(-group, -stat) 
    %>% left_join(., crossing(group=groups, outcome=outcomes) , by=c('group'))  
    %>% left_join(., crossing(group=groups, cutoffs) , by=c('group'))   
    %>% mutate(
            data = future_map2(.x=data, .y=group, function(.x, .y) 
                complete_suspended(.x, .y, !!outcomes, !!max_date) ),
            data = future_pmap(list(data=data,
                                    cutoff.low=cutoff.low,
                                    cutoff.high=cutoff.high),
                               function(data, cutoff.low, cutoff.high)
                                   ## data = future_map(.x=data, function(.x) 
                data
                %>% mutate(score = case_when(
                               date <= cutoff.low  ~ as.integer(date - cutoff.low),
                               date >= cutoff.high ~ -1*as.integer(cutoff.high - date)+1))
                ),
            ## 
            data = future_map(.x=data, function(.x) .x %>% drop_na()),
            est = future_map2(.x=data, .y=outcome, function(.x, .y)
                rdrobust(y=.x %>% pull(.y), x=.x %>% pull('score'), c=0, all=T)),
            summ = future_pmap(list(est, outcome, stat, group), function(est, outcome, stat, group)
                tidy_rdd(est, outcome, stat, group)  , .progress=TRUE)
            ) 
)
##
print('done!')

## checking
## --------
## res.rdd.robust.outcome 


## *** Merging

res.rdd = (
    res.rdd.main  
    %>% bind_rows(res.rdd.robust.outcome) 
    %>% bind_rows(res.rdd.robust.date)
)

## ** DiD
## *** Main

print('Estimating DiD (main)...')
## 
## Creating cases
## --------------
t0       = jan06
t1       = jan12
outcomes = c('retweet', 'tweet')
controls = c("ss5", "av", "qanon", "white", "female", "pres_gen", "pres_pri", "ptyid")
act      = c('', '_ha', '_ma', '_la')
res.did.main = (
    tribble (
        ~label        , ~control , ~treat ,~reference,
        'ToU effect'  , "F"      , 'A'    , "F"      ,
        'ToU effect'  , "F"      , 'B'    , "F"      ,
        'ToU effect'  , "F"      , 'D'    , "F"      ,
        'ToU effect'  , 'nfns'   , "A"    , 'nfns'   ,
        'ToU effect'  , 'nfns'   , "B"    , 'nfns'   ,
        'ToU effect'  , 'nfns'   , "D"    , 'nfns'   ,
        'ToU effect'  , 'nfns'   , "F"    , 'nfns'   ,
        ## 
        )
    %>% mutate(group = FN_GROUPS[treat])
    %>% crossing(act) 
    %>% mutate(
            control = glue("{control}{act}"),
            treat = glue("{treat}{act}")
        )
    %>% crossing(Controls=c("1", paste(controls, collapse=' + '))) 
    %>% crossing(outcome = outcomes) 
    %>% mutate(
            controlsl = ifelse (Controls=="1",  'without controls', "with controls"),
            formula   = glue("{outcome} ~ t*d + id + {Controls}"),
        ) 
)
## res.did.main
## estimating
## ----------
res.did.main$est  = NA
res.did.main$summ = NA
res.did.main$estnp = NA
for (i in 1:nrow(res.did.main))
{
    treat = res.did.main$treat[[i]]
    control = res.did.main$control[[i]]
    outcome = res.did.main$outcome[[i]]
    formula = res.did.main$formula[[i]]
    adj.vars.ind = res.did.main$controlsl[[i]]
    ## "outcome" below is placeholder to not select covars in the dft
    adj.vars = `if`(adj.vars.ind=='with controls', controls, outcome) 
    ## 
    cat(glue("({i}/{nrow(res.did.main)})",
             "DiD: Collecing data; {control}; {treat}; ",
             "{outcome}; {adj.vars.ind}..."))
    dft = collect_did_data(df           = dfdid,
                           treat        = treat,
                           control      = control,
                           outcomes     = outcome,
                           controls     = adj.vars,
                           t0           = t0,
                           t1           = t1,
                           date_min_did = date_min_did,
                           date_max_did = date_max_did)
    ## 
    cat(glue("estimating..."))
    est = lm(formula(formula), data=dft)
    estnp = estimate_non_parametric(dft, y=outcome)
    res.did.main$est[i]   = list(est)
    res.did.main$estnp[i]   = list(estnp)
    res.did.main$summ[i] = list(tidy(coeftest(est, vcov=vcovCL, cluster=~id), conf.int=T))
    cat('done!\n')
}
## 
print('done!')

## checking
## --------
## i = 1
## res.did.main$est[[i]]
## res.did.main$summ[[i]]

## *** Dosage

print('Estimating DiD (dosage)...')
## 
t0       = jan06
t1       = jan12
dosage   = dfdid %>% select(matches("dosage"))  %>% select(-matches("_0"))  %>% names()
outcomes = c('retweet', 'tweet')
controls = c("ss5", "av", "qanon", "white", "female", "pres_gen", "pres_pri", "ptyid")
act      = c('')
res.did.dosage = (
    tribble (
        ~label        , ~control ,
        'ToU effect'  , "F"      ,
        'ToU effect'  , "F"      ,
        'ToU effect'  , "F"      ,
        ) 
    %>% crossing(treat=dosage)
    %>% mutate(follow.trump = case_when(
                      str_detect(treat, pattern="_no_") ~ "No",
                   T ~ 'Yes'),
               dosage = parse_number(treat),
               )
    %>% crossing(act) 
    %>% mutate(
            control = glue("{control}{act}"),
            treat = glue("{treat}{act}")
        )
    %>% crossing(Controls=c("1", paste(controls, collapse=' + '))) 
    %>% crossing(outcome = outcomes) 
    %>% mutate(
            formula   = glue("{outcome} ~ t*d + id + {Controls}"),
            controlsl = ifelse (Controls=="1",  'without controls', "with controls"),
        ) 
)
## res.did.dosage
##
## estimating
## ----------
res.did.dosage$est  = NA
res.did.dosage$summ = NA

for (i in 1:nrow(res.did.dosage))
{
    treat = res.did.dosage$treat[[i]]
    control = res.did.dosage$control[[i]]
    outcome = res.did.dosage$outcome[[i]]
    formula = res.did.dosage$formula[[i]]
    adj.vars.ind = res.did.dosage$controlsl[[i]]
    adj.vars = `if`(adj.vars.ind=='with controls', controls, outcome) 
    ## 
    cat(glue("({i}/{nrow(res.did.dosage)})",
             "DiD: Collecing data; {control}; {treat}; ",
             "{outcome}; {adj.vars.ind}..."))
    dft = collect_did_data(df           = dfdid,
                           treat        = treat,
                           control      = control,
                           outcomes     = outcome,
                           controls     = adj.vars,
                           t0           = t0,
                           t1           = t1,
                           date_min_did = date_min_did,
                           date_max_did = date_max_did)
    ## 
    cat(glue("estimating..."))
    est = lm(formula(formula), data=dft)
    res.did.dosage$est[i]   = list(est)
    res.did.dosage$summ[i] = list(tidy(coeftest(est, vcov=vcovCL, cluster=~id), conf.int=T))
    cat('done!\n')
}
## 
print("done!")


## checking
## --------
## i = 1
## res.did.dosage$est[[i]]
## res.did.dosage$estnp[[i]]

## * Tables
## ** ------- Extended --------
## ** DONE table extended 1

## Lazer lab created

## ** DONE table extended 2

## Lazer lab created

## ** DONE ------- SI --------
## ** DONE table supplemental 1 to 3

tables = tibble::tribble(
                     ~treat,~act, ~tableid,
                     'A'   ,  '',   'supplemental-1',
                     'B'   ,  '',   'supplemental-2',
                     'D'   ,  '',   'supplemental-3',
                 )

cat(glue("Creating tables {paste0(tables$tableid, collapse=', ')}..."))
tab = (
    res.did.main
    %>% mutate(depvar = case_when(outcome=='tweet'~'Tweeted',
                                  outcome=='retweet'~'Retweeted'))
    %>% rowwise(.)
    %>% mutate(colname = glue("{depvar} ({controlsl})"))
    %>% ungroup(.) 
    %>% select(treat, act, est, colname)
    %>% nest(est, colname) 
    %>% left_join(., tables, by=c('treat', 'act'))  
    %>% drop_na(tableid) 
    %>% mutate(caption = glue("Table: {tableid}: {FN_GROUPS[str_replace(string=treat, pattern='_.*', replacement='')]}; ",
                              "{FN_GROUPS[str_replace(string=act, pattern='_', replacement='')]}"),
               caption = str_replace(string=caption, pattern=" NA$", replacement=" Combined")
               )
    ## ## ## ## saving
    ## ## ## ## ------
    %>% rowwise(.)
    %>% mutate(table = list(save_reg_table(data%>% dplyr::pull(est),
                                           tableid=tableid,
                                           caption=caption,
                                           depvars=data%>% dplyr::pull(colname),
                                           SAVE = SAVE
                                           ))
               )
)
print('done!\n')
tab


## checking
## --------
## print(tab$table[[1]])
## print(tab$table[[2]])
## print(tab$table[[3]])
## FN_GROUPS['B']
## tab$table[[2]]%>% as_tibble() %>% rename(str_replace(string=., pattern="", replacement="")) 
## tab$caption[[2]]
## ## models nested
## i=1
## tab$data[[i]] %>% dplyr::pull(est)
## ## sabe tables
## save_reg_table(tab$data[[i]] %>% dplyr::pull(est))
## ## all tables
## lapply(tab$data, function(tab) save_reg_table(tab%>% dplyr::pull(est)))

## ** DONE table supplemental 4

table = 'tab-supplemental-4'

fn_groups = FN_GROUPS
names(fn_groups) = glue("^{names(fn_groups)}$")
tab=(
    res.did.main 
    %>% filter(reference=='F') 
    %>% filter(Controls==1) 
    %>% mutate(depvar = case_when(outcome=='tweet'~'Tweeted',
                                  outcome=='retweet'~'Retweeted'))
    %>% select(act, depvar, estnp, group=treat)
    %>% unnest(estnp)
    %>% select(
            Group      = group,
            Activity   = act,
            Period     = t,
            Followers  = d,
            `N. Users` = nusers,
            `N. Days`  = ndays,
            Average    = avg,
            Total      = tot,
            depvar
        )
    %>% distinct(., .keep_all=TRUE) 
    %>% dplyr::mutate_if(.predicate=is.numeric, .funs=list(~round(., 2)))
    %>% unite('(Avg/Total)', c("Average", "Total"), sep='/')
    %>% tidyr::pivot_wider(id_cols=,
                           names_from=depvar,
                           values_from='(Avg/Total)')
    %>% mutate(Group = str_replace(string=Group, pattern="_..$", replacement="")
               %>% str_replace_all(., fn_groups)%>% str_trim(.),
               Activity = str_replace(string=Activity, pattern="_", replacement="")%>%
                   str_replace_all(string=., FN_GROUPS)%>% str_trim(.),
               Activity = case_when(Activity=='' ~ 'Combined',
                                    TRUE ~ Activity)
               ) 
    %>% distinct(., .keep_all=TRUE) 
)
tab %>% print(., n=Inf, width=Inf) 
if (SAVE) {
    fn=file.path(PATH_MAN_TABLES,glue('{table}.csv'))
    write.table(x=tab, file=fn,  sep=';', row.names = FALSE)
    fn=file.path(PATH_MAN_TABLES,glue('{table}.xlsx'))
    write_xlsx(x=tab, path=fn)
}

## ** DONE table supplemental 5

figure="pvalue"
path = PATH_OUTPUTS
## 
tab.long = (
    res.did.main  
    %>% filter(!str_detect(reference, pattern="nfns")) 
    %>% select(label, control, treat, reference, group, act, Controls, outcome, summ)  
    %>% unnest(summ) 
    %>% filter(term=='t:d')  
    %>% select(-estimate, -contains("std"), -statistic, -contains("conf"))  
    %>% mutate(p.adj.bonf = p.adjust(p.value, method = 'bonferroni'),
               p.adj.hoch = p.adjust(p.value, method = 'hochberg'), 
               p.adj.holm   = p.adjust(p.value, method = 'holm'), 
               p.adj.homm   = p.adjust(p.value, method = 'hommel'), 
               p.adj.bh     = p.adjust(p.value, method = 'BH'), 
               p.adj.by     = p.adjust(p.value, method = 'BY'), 
               ## p.adj.fdr    = p.adjust(p.value, method = 'fdr'), 
               ) 
    %>% pivot_longer(contains("p.adj"),
                     names_to='method', values_to="p.value.corr")  
    %>% labelled::set_value_labels(method=c(
                                       'Bonferroni'         = "p.adj.bonf",
                                       "Hochberg"           = "p.adj.hoch",
                                       "Holm"               = "p.adj.holm",
                                       "Hommel"             = "p.adj.homm",
                                       "Benjamini-Hochberg" = "p.adj.bh",
                                       "Benjamini-Yekutiel" = "p.adj.by"
                                            )) 
    %>% sjlabelled::as_label(method) 
)
tab.long
tab = (
    tab.long
    %>% select(-label, -term)
    %>% select(-reference, -control, -treat)
    ## %>% filter(p.value.corr<0.05) 
    %>% filter(p.value<0.05)  
    %>% pivot_wider(names_from=method,
                    values_from=p.value.corr)  
    %>% arrange(group , outcome, act)  
    %>% mutate(
            Controls = case_when(Controls=='1' ~ "Yes",
                                 T ~ 'No')
        ) 
    %>% rename(Raw=p.value)  
    ## %>% bind_rows(
    ##         .
    ##         %>% dplyr::summarise_all(list(~sum(.<0.05))) 
    ##         %>% select(-act, -Controls, -outcome)
    ##     )
)
## Count significant
tab = tab %>% bind_rows(tab %>%
                        dplyr::summarise_all(list(~sum(.<0.05))) %>%
                        select(-act, -Controls, -outcome, -group) %>%
                        mutate(group='Significant (N)')
                        )
tab %>% print(., n=Inf) 
x = "p.value"
y = "p.value.corr"
color    = NULL
fill     = 'method'
facet1   = NULL
facet2   = NULL
leg      = 'Method'
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = "DiD p-value (multiple test correction)"
xlab     = 'DiD p-value<0.05 (raw)'
g = (
    tab.long
    %>% filter(p.value<0.05) 
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0.05) ,linetype="dashed", col="red")
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill))
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), color='black')
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_color_grey(start = 0, end = .7, na.value="red") 
    + scale_shape_manual(values=c(20:26))
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggguides(ncol=3)
)
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(path, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(path, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=5 )}
    cat("done!\n")
}



## * Figures
## ** ------- Main paper --------
## ** DONE figure 1

theme_set(ggtheme2())
## 
figure = 'fig-1'
## 
## 
fake       = 'fake'
stat       = 'pct'
date_min   = as.Date('2020-09-01')
date_max   = as.Date('2021-01-29')
election   = as.Date('2020-11-03')
date_min16 = as.Date('2016-09-01')
date_max16 = as.Date('2017-01-29')
election16 = as.Date('2016-11-08')
## 
## table
## -----
tab = (
    dftot    
    %>% mutate(facet='2020 Election Cycle')
    %>% bind_rows(dftot16
                  %>% mutate(n=fake+not_fake,
                             facet='2016 Election Cycle')
                  %>% filter(date<'2017-02-02') ) 
    %>% rename(fake=!!fake) 
    %>% mutate(pct = fake/n) 
    %>% drop_na(pct) 
    ## 
    %>% filter(group %in% c('fns', 'all')) 
    %>% filter(date_min < date & date <= date_max |
               date_min16 < date & date <= date_max16)  
    ## 
    %>% mutate(group = factor(group, c('all', 'fns'),
                              c('All users (shared at least one URL of any kind)',
                                'Misinformation sharers only (shared at least one misinformation URL)')
                              ))
)
tab
## 
## plot
## ----
x = "date"
y = "pct"
color    = NULL
fill     = 'group'
facet1   = 'facet'
facet2   = NULL
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = "Percentage of misinformation shared"
xlab     = NULL
size     = 3.2
g = (
    tab
    %>% ggplot(.) 
    + geom_vline(aes(xintercept=jan06), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=jan12), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=election), linetype="solid", col="black")
    + geom_vline(aes(xintercept=jan06.17), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=jan12.17), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=election16), linetype="solid", col="black")
    #
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill))
    + geom_point(aes_string(x=x, y=y, fill=fill), size=2)
    ##
    + geom_text(aes_string(x=election16, y=Inf, label="'Election day'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    + geom_text(aes_string(x=jan06.17, y=Inf, label="'Jan 6th'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    + geom_text(aes_string(x=jan12.17, y=Inf, label="'Jan 12th'"),
                size=size, hjust=-.05, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    ## 
    + geom_text(aes_string(x=election, y=Inf, label="'Election day'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2020 Election Cycle')) 
    + geom_text(aes_string(x=jan06, y=Inf, label="'Jan 6th'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2020 Election Cycle')) 
    + geom_text(aes_string(x=jan12, y=Inf, label="'Jan 12th'"),
                size=size, hjust=-.05, vjust=1.5,
                data=data.frame(facet='2020 Election Cycle')) 
    ## 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 5),
                  data = . %>% filter(date<=jan06) ) 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 5),
                  data = . %>% filter(date>=jan12) ) 
    ## 
    + scale_x_date(date_labels="%b %d") 
    + scale_y_continuous(labels=percent_format(scale=100)) 
    + scale_colour_grey(start = 0, end = .7, na.value="red")
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    ## 
    + facet_wrap(glue("~ {facet1}"), ncol = , scales='free_x')
    ## 
    + ggguides()
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g
## 
if (SAVE) {
    ## table
    cat(glue("Saving table of figure {figure}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=10, height=4.5 )}
}
## 
## info
## ----
tab16   = tab %>% filter(date<'2020-01-01') 
avg16   = tab16 %>% filter(str_detect(group, pattern="All") & stat=='total')  %>% pull(n) %>% mean()
users16 = tab16 %>% filter(str_detect(group, pattern="All") & stat=='total') %>% pull(nusers) %>% mean()
tab20   = tab %>% filter(date>'2018-01-01') 
avg20   = tab20 %>% filter(str_detect(group, pattern="All") & stat=='total')  %>% pull(n) %>% mean()
users20 = tab20 %>% filter(str_detect(group, pattern="All") & stat=='total') %>% pull(nusers) %>% mean()
drop20 = (
    res.rdd.main
    %>% select(summ)
    %>% unnest(summ)
    %>% filter(stat=='total')  
    %>% filter(group=='all' | group=='fns' | group == 'suspended')    
    %>% filter(str_detect(parameter, pattern="Bias"))   
    %>% arrange(outcome, group )  
    %>% select(group, stat, outcome, coef)  
    %>% pivot_wider(id_cols=c("outcome", "stat"),
                    names_from=group,
                    values_from=coef) 
    %>% mutate(`s/a` = suspended/all,
               `s/fns` = suspended/fns)
)
print(drop20)
## 
## 
print(glue("\n
Information: 
## 
2016
----
Start date : {min(tab16$date)}
End date   : {max(tab16$date)}
Number of days : {max(tab16$date) - min(tab16$date)}
Average daily total number of shared (tweet+retweet): {comma(avg16)}
Average daily number of users: {comma(users16)}
## 
2020
----
Start date : {min(tab20$date)}
End date   : {max(tab20$date)}
Number of days : {max(tab20$date) - min(tab20$date)}
Average daily total number of shared (tweet+retweet): {comma(avg20)}
Average daily number of users: {comma(users20)}
## 
Drop (SRD):
") )
print(drop20)
## 
theme_set(ggtheme())


## ** DONE figure 2


theme_set(ggtheme2())
## 
figure = 'fig-2'
## parameters
## ----------
stat= 'total'
outcome = 'fake_rt'
groups = c(
    'suspended',
    'A',
    'B',
    'F'
)
ndays = 45
digits=1
date_min = jan06-days(ndays)
date_max = jan06+days(ndays)
## table
## -----
tab = (
    res.rdd.main
    %>% filter(stat==!!stat) 
    %>% filter(outcome==!!outcome) 
    %>% filter(group %in% groups)  
    ## 
    %>% select(summ, data)
    %>% unnest(summ)  
    %>% unnest(data)
    ## 
    %>% filter(date_min < date & date <= date_max) 
    %>% filter(str_detect(parameter, pattern="Bias-"))   
    %>% mutate(across(where(is.numeric), round, 1)) 
    %>% mutate(label = factor(group, groups, FN_GROUPS[groups]),
               est   = glue("Est: {coef} [{ci.low}, {ci.high}]" ),
               label = glue("{label} ({est})"),
               label = str_replace(string=label,
                                   pattern="Deplatformed user",
                                   replacement=" Deplatformed")
               )
)
tab
## tab
## plot
## ----
x = "date"
y = outcome
color    = NULL
fill     = NULL
facet1   = "label"
facet2   = NULL
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab = 'Total of Misinformation Retweeted'
xlab     = NULL
size=2
g = (
    tab
    %>% ggplot(.)
    + geom_point(aes_string(x=x, y=y, fill=fill), size=size)
    + geom_vline(aes(xintercept=jan06), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=jan12), linetype="dashed", col="black")
    + geom_smooth(aes_string(x=x, y=y, colour=color, fill=fill),
                  color='black',
                  method="lm", se=T, formula=y~poly(x, 5),
                  data=. %>% filter(date<jan06) ) 
    + geom_smooth(aes_string(x=x, y=y, colour=color, fill=fill),
                  color='black',
                  method="lm", se=T, formula=y~poly(x, 5),
                  data=. %>% filter(date>=jan12) ) 
    ##
    + scale_x_date(date_labels="%b %d") 
    ## 
    + facet_wrap(glue("~ {facet1}"), ncol = , scales='free')
    + ggguides()
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g
if (SAVE) {
   ## table
    cat(glue("Saving table of figure {figure}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    cat(glue("Saving figure {figure}...") )
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=10, height=5)}
}
theme_set(ggtheme())
## info
## ----
## rdd_report_details(res.rdd.main)

## ** DONE figure 3

figure = 'fig-3' # change it to create different plots based on parameters
## 
## parameters
## ----------
include.nfns = F         # include or not not misinformation sharers
outcome      = 'fake_rt' # outcome
stat         = 'total'   # statistics (total, avg, or pct)
fns.group1   = 'F'       # fns comparison group 1
fns.group2   = 'B'       # fns comparison group 2
act          = 'all'     # activity level ('all',  'ha', 'ma', or 'la')
ndays        = 9         # days before and after for the zoom-in plot
## 
## event dates
impeach = as.Date('2020-02-05')
floyd   = as.Date('2020-05-25')
lafay   = as.Date('2020-06-01')
acb     = as.Date('2020-09-26')
insurr  = jan06
events  = get_events(ndays, impeach, floyd, lafay, acb, insurr)
events %>% filter(event=='impeach')  %>% filter(date %in% c(min(date), max(date))) %>%  pull(date)
## 
## interval to split the top plot
date_min1 = as.Date('2019-12-01')
date_max1 = as.Date('2020-06-29')
date_min2 = as.Date('2020-07-01')
date_max2 = as.Date('2021-01-29')
## 
## Table
## -----
tab.nfns = ifelse(include.nfns,
                  list(get_daily_outcome(group='nfns',
                                         outcome=glue("not_{outcome}") ,
                                         stat=stat,
                                         act=act)),
                  data.frame()
                  )[[1]]
tab = (
    bind_rows(
        tab.nfns,
        get_daily_outcome(group=fns.group1, outcome=outcome, stat=stat, act=act),
        get_daily_outcome(group=fns.group2, outcome=outcome, stat=stat, act=act)  
    ) 
    %>% filter(date_min1 <= date & date <= date_max2) 
    %>% mutate(
            period = case_when(
                date_min1<=date & date<=date_max1 ~ "First",
                date_min2<=date & date<=date_max2 ~ "Second",
                )
        )  
    %>% drop_na(period) 
    %>% left_join(., events, by=c('date'='date'))  
    ## 
    %>% mutate(group = factor(group, names(FN_GROUPS), FN_GROUPS))
    %>% mutate(act = factor(act, names(FN_GROUPS), FN_GROUPS))
)
tab
## 
## plot
## ----
x        = "date"
y        = "y"
color    = NULL
fill     = 'group'
facet1   = 'period'
facet2   = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
hjust    = 1.05     # for the top plot event labels
vjust    = 2.1       # for the top plot event labels
size     = 3      # size of top plot event labels
xlab     = NULL
ylab     = 'Total (mis)information retweeted (std)'
leg      = tab$act[1]
ga = (
    tab
    %>% ggplot(.)
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill, linetype=fill))
    + geom_point(aes_string(x=x, y=y, fill=fill), size=1.5)
    + geom_vline(aes(xintercept=date.event), linetype="dashed", col="black")
    ## + geom_text_repel(aes_string(x=x, y="Inf", label="label"), 
    ##                   vjust=vjust, hjust=hjust, angle=0, size=size,
    ##                   show_legend=F, parse=F,
    ##                   direction='y',
    ##                   min.segment.length = Inf,
    ##                   colour="black", position=position_dodge(0),
    ##                   data= events %>%
    ##                       select(event, date=date.event, label) %>% 
    ##                       distinct(., .keep_all=TRUE) %>% 
    ##                       mutate(period = case_when(
    ##                                  date_min1<=date & date<=date_max1 ~ "First",
    ##                                  date_min2<=date & date<=date_max2 ~ "Second"),
    ##                              label=case_when(str_detect(label, pattern="Laf")~str_wrap(label,
    ##                                                                                        width=15),
    ##                                              T~label
    ##                                              )
    ##                              )
    ##                   ) 
    + geom_text(aes_string(x=x, y="Inf", label="label"), 
                vjust=vjust, hjust=hjust, angle=0, size=size,
                show_legend=F, parse=F,
                direction='y',
                min.segment.length = Inf,
                colour="black",
                position=position_dodge(0),
                      data= events %>%
                          filter(!str_detect(label, pattern="Laf"))  %>% 
                          select(event, date=date.event, label) %>% 
                          distinct(., .keep_all=TRUE) %>% 
                          mutate(period = case_when(
                                     date_min1<=date & date<=date_max1 ~ "First",
                                     date_min2<=date & date<=date_max2 ~ "Second"),
                                 label=case_when(str_detect(label, pattern="Laf")~str_wrap(label,
                                                                                           width=15),
                                                 T~label
                                                 )
                                 )
                      ) 
    + geom_text(aes_string(x=x, y="Inf", label="label"), 
                vjust=1.4, hjust=-0.05, angle=0, size=size,
                show_legend=F, parse=F,
                direction='y',
                min.segment.length = Inf,
                colour="black",
                position=position_dodge(0),
                      data= events %>%
                          filter(str_detect(label, pattern="Laf"))  %>% 
                          select(event, date=date.event, label) %>% 
                          distinct(., .keep_all=TRUE) %>% 
                          mutate(period = case_when(
                                     date_min1<=date & date<=date_max1 ~ "First",
                                     date_min2<=date & date<=date_max2 ~ "Second"),
                                 label=case_when(str_detect(label, pattern="Laf")~str_wrap(label,
                                                                                           width=15),
                                                 T~label
                                                 )
                                 )
                      ) 
    ## 
    + scale_linetype_manual(values=c(1,1,2))
    + scale_x_date(date_labels="%b %Y") 
    + scale_colour_grey(start = 0, end = .7, na.value="red") 
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    ## 
    + facet_wrap( paste("~", facet1), ncol = 1, scales='free')
    ## 
    + labs(
          x        = NULL,
          y        = ylab ,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    ## 
    + ggguides(ncol=3)
    + theme(strip.text.x=element_blank(),
            legend.title = element_text(size=10, face='italic'),
            legend.title.align = 0
            )
)
## 
facet1   = 'label'
degree=8
gb = (
    tab %>% drop_na(event) 
    %>% ggplot(.)
    + geom_vline(aes(xintercept=date.event), linetype="dashed", col="black")
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm", se=F, formula=y~poly(x, degree),
                  show.legend=F) 
    ## 
    + scale_colour_grey(start = 0, end = .7, na.value="red") 
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_linetype_manual(values=c(1,1,2))
    ## 
    + scale_x_date(breaks=c(impeach,
                            impeach+ndays,
                            impeach-ndays,
                            floyd,
                            ## floyd+ndays+3,
                            floyd-ndays,
                            lafay,
                            lafay+ndays,
                            ## lafay-ndays,
                            acb,
                            acb+ndays,
                            acb-ndays,
                            insurr,
                            insurr+ndays,
                            insurr-ndays
                            ),
                   date_labels="%b %d") 
    + facet_wrap( paste("~", facet1), ncol = 5, scales='free')
    + labs(
          x        = NULL,
          y        = NULL ,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggguides()
    + theme(
          axis.text.x  = element_text( size=7),
          axis.text.y  = element_text( size=7),
          strip.text.x=element_text(size=8, face='italic')
      )
)
## 
layout=matrix(c(1,1,1,1, 2,2,2,2), ncol=4, byrow = T)
g = grid.arrange(grobs = list(ga, gb), layout=layout, nrow=2,ncol=1,
                 heights=c(2.2, .8))
g
## 
print(glue("
Start date: {tab$date %>% min}
End date  : {tab$date %>% max}
"))
if (SAVE) {
    ## table
    cat(glue("\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("Saving figure {figure}...") )
    if (!include.nfns) {
        fn = c(glue("{figure}.pdf"),
               glue("{figure}.png"),
               glue("{figure}.jpg"))
    }else{
        fn = c(glue("{figure}-{fns.group1}-{fns.group2}-{act}-{outcome}-{stat}-with-nfns.pdf"),
               glue("{figure}-{fns.group1}-{fns.group2}-{act}-{outcome}-{stat}-nwith-fns.png"),
               glue("{figure}-{fns.group1}-{fns.group2}-{act}-{outcome}-{stat}-nwith-fns.jpg"))
    }
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8.5, height=5 )}
    cat("done!\n")
}


## ** DONE figure 4

theme_set(ggtheme2())
## 
figure = 'fig-4'
## 
## parameters
## ----------
y       = "fake_rt"
y       = "retweet"
treat   = "B"
control = "F"
## control = "nfns"
act     = 'ha'
## 
## tables
## ------
group_lab = c(
    treat  = unname(FN_GROUPS[treat]),
    control= unname(FN_GROUPS[control])
)
group_lab 
## 
tab = (
    dfdid
    %>% select(date, y=!!y, treat=!!treat, control=!!control, act=!!act) 
    %>% filter(act==1) 
    ## ## 
    %>% pivot_longer(cols=c(treat, control),
                     names_to='group', values_to = 'group_value')
    %>% filter((group=='treat' & group_value==1) |
               (group=='control' & group_value==1)) 
    %>% group_by(date, group)
    %>% summarise(y_raw = sum(y)) 
    %>% group_by(group)
    %>% mutate(y_std = c(scale(y_raw)))
    %>% ungroup(.) 
    %>% mutate(act = recode_factor(act, !!!FN_GROUPS)
               %>% as.character(.))
)
tab
## 
## counterfactual
## --------------
counter =(
    tab
    %>% filter(date<=jan06) 
    %>% group_by(act, group)
    %>% summarise(avg = mean(y_std)) 
    %>% pivot_wider(id_cols=act,
                    names_from=group,
                    values_from=avg)
    %>% mutate(f_minus_nf=treat-control)
    %>% select(act, f_minus_nf)
    %>% distinct(., .keep_all=TRUE) 
    %>% right_join(tab
                   %>% filter(date>=jan12) 
                   %>% filter(group=="control") 
                   %>% select(date, act, y_std) , by=c('act')) 
    %>% mutate(
            group=factor('Counterfactual', levels=c('Counterfactual',
                                                    FN_GROUPS[treat],
                                                    FN_GROUPS[control]
                                                    )),
            group = 'Counterfactual',
            y_std = y_std + f_minus_nf)
)
counter
tab = (
    tab
    %>% bind_rows(counter)
    %>% mutate(group = recode_factor(group, !!!group_lab)%>% as.character(.))
)
## 
## plot
## ----
x = "date"
y = "y_std"
color=NULL
fill='group'
facet1='act'
facet2=NULL
dodge=.1
size=2.5
counter_color="black"
fill_scale=c('black', 'gray', counter_color)
t1 = jan06
t2 = jan12
degree=1
names(fill_scale) = c(group_lab, 'Counterfactual')
ylab    = ifelse(treat=='nfns' | control=='nfns',
                 "Total (Mis)information Retweeted (std)",
                 "Total Misinformation Retweeted (std)"
                 )
g = (
    tab 
    %>% ggplot(.)
    + geom_vline(aes(xintercept=t1), linetype="dashed", col='black')
    + geom_vline(aes(xintercept=t2), linetype="dashed", col="black")
    ## 
    + geom_line(aes_string(x=x, y=y, color=fill),
                show.legend = FALSE,
                data=.%>% filter(group!='Counterfactual')
                )
    + geom_point(aes_string(x=x, y=y, fill=fill), size=size,
                show.legend = FALSE,
                 data=.%>% filter(group!='Counterfactual')
                 )
    ##
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm", se=T, formula=y~poly(x, degree), size=.5,
                  show.legend = FALSE,
                  alpha=.3,
                  data=.%>% filter(date<=t1 & group!='Counterfactual') ) 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm", se=T, formula=y~poly(x, degree), size=.5,
                  alpha=.3,
                  show.legend = FALSE,
                  ## show.legend = FALSE,
                  data=.%>% filter(date>=jan12 & group!='Counterfactual')) 
    ## counterfactual
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm",  formula=y~poly(x, degree), size=1,
                  ## fill=NA,
                  se=FALSE,
                  ## show.legend = FALSE, 
                  data=.%>% filter(date>=jan12 & group=='Counterfactual')) 
    ## 
    + scale_x_date(breaks=c(date_min_did, t1, t2), date_labels = '%b %d') 
    + scale_shape_manual(values=c(21, 22, 20))
    + scale_linetype_manual(values=c(2,1,1))
    + scale_color_manual(values=fill_scale)
    + scale_fill_manual(values=fill_scale)
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + labs(
          x        = NULL,
          y        = ylab,
          color    = NULL, 
          fill     = NULL,
          linetype = NULL,
          shape = NULL
      )
    + ggguides()
)
g
if (SAVE) {
    ## table
    cat(glue("\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("Saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8, height=3.5 )}
    cat("done!\n")
}
## 
## info
days = tab%>% select(date)%>% distinct(., .keep_all=TRUE) %>% nrow()
glue("\n
Start date: { tab$date%>% min}
End date  : { tab$date%>% max}
Days      : {days}
")
## 
theme_set(ggtheme())

## ** DONE figure 5 and extended 5

theme_set(ggtheme2())
## 
## parameters
## ----------
## figure      = see below, based on 'case'
cases  = 'with controls' # 'with controls'
cases  = 'without controls' # 'with controls'
cases  = c('with controls', 'without controls')
digits = 4
for (case in cases)
{
    ## table
    ## -----
    if (case == 'with controls') {
        figure = 'fig-extended-5'
        path = PATH_MAN_FIGURES 
    }else{
        figure = 'fig-5'
        path = PATH_MAN_FIGURES
    }
    tab = (
        res.did.main 
        %>% filter(str_detect(control, pattern="^F")) 
        %>% select(act, group, outcome, control, treat, controlsl, summ)
        %>% unnest(summ)
        ## 
        %>% filter(term=='t:d') 
        %>% filter(str_detect(outcome, pattern="tweet|retweet")) 
        %>% filter(controlsl==case) 
        ## ## 
        %>% mutate(act = factor(act,
                                levels=c('', '_ha', '_ma', '_la'),
                                labels=c("Combined", 'High', 'Moderate', 'Low')),
                   ## 
                   outcome = factor(outcome,
                                    levels=c('tweet', 'retweet'),
                                    labels=c('Tweeted misinformation', 'Retweeted misinformation')),
                   ## 
                   label = case_when(p.value<=0.05 ~ glue('{round(estimate,digits)}***'),
                                     TRUE ~ as.character(round(estimate, digits)))
                   )
    )
    tab
    ## 
    ## plot
    ## ----
    x = "act"
    y = "estimate"
    color=NULL
    fill='group'
    facet1='outcome'
    facet2='control'
    ylab=glue('Difference-in-differences Effect\n{case}')
    xlab='User Activity Level'
    fn = '*** significant at 0.05'
    dodge=.3
    g = (
        tab
        %>% ggplot(.)
        + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red", alpha=.5)
        + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                                   color=fill), width=.05, size=.7,
                        position=position_dodge(dodge))
        + geom_point(aes_string(x=x, y=y, fill=fill),
                     position=position_dodge(dodge))
        + geom_text(aes_string(x=x, y=y, label="label", color=fill), size=3,
                    check_overlap = T, hjust=-.1,
                    position=position_dodge(dodge))
        + scale_colour_grey(start = 0, end = .6, name="", na.value="red") 
        + scale_fill_grey(start = 0, end = .6, name="", na.value="red") 
        ## + facet_wrap( paste("~", facet1), ncol = 2, scales='free_x')  
        + facet_wrap( paste("~", facet1), ncol = 2)  
        + ggguides(ncol=3)
        + labs(
              x        = xlab,
              y        = ylab,
              color    = NULL, 
              fill     = NULL,
              linetype = NULL,
              shape    = NULL,
              title    = NULL,
              subtitle = NULL,
              caption  = fn
          )
    )
    g
    ## 
    if (SAVE) {
        ## table
        cat(glue("\nSaving table of figure {figure}...") )
        fn = file.path(path, glue("{figure}"))
        write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
        write_xlsx(x=tab, path=glue("{fn}.xlsx"))
        ## 
        cat(glue("Saving figure {figure}...") )
        fn  = c(glue("{figure}.pdf"),
                glue("{figure}.png"),
                glue("{figure}.jpg"))
        fns = file.path(path, fn)
        for (fn in fns){ggsave(g, filename=fn, width=8, height=3.5 )}
    }
    ## 
    ## info
    ## ----
    group = c('F_ha', 'D_ha')
    outcome = "fake_rt"
    ndays = date_max_did - jan12
    ## 
    dftot%>% names 
    tab = (
        dftot
        %>% filter(stat=='total')
        %>% select(date, group, stat, n, nusers, outcome=!!outcome)
        %>% filter(group %in% !!group)
        %>% mutate(
                date = as.Date(date),
                outcome = as.numeric(outcome),
                period = case_when(date_min_did <= date & date <= jan06 ~ 'Before',
                                   jan12 <= date & date <= date_max_did ~ 'After'
                                   )
            )
        %>% drop_na(period)  
        %>% group_by(period, group)
        %>% summarise(
                mean = mean(outcome),
                ndays = max(date) - min(date),
                nusers_avg = mean(nusers, na.rm=T),
                date_init = min(date),
                date_end = max(date),
                .groups='drop'
            )
    )
    tab
    ## 
    tab %>% print(., n=Inf, width=Inf) 
    tabt = tab%>% filter(group %in% !!group) %>% arrange(group  )%>% print()
    (
        tabt 
        %>% select(period, group, mean)
        %>% pivot_wider(id_cols=group,
                        names_from=period,
                        values_from=mean)
        %>% mutate(reduction_rate = 100*(1 - After/Before))
        %>% print(., n=Inf, width=Inf) 
    )
}
theme_set(ggtheme())

## samples sizes
## -------------
case   = 'with controls' # 'with controls'
case   = 'without controls' # 'with controls'
tabn = (
    res.did.main   
    %>% filter(controlsl==!!case)  
    %>% filter(reference=="F")  
    %>% filter(outcome=='tweet') 
    %>% mutate(n = future_map_chr(.x=est, function(.x) scales::comma(nrow(model.frame(.x))))) 
    %>% select(-label, -summ, -estnp, -est, -outcome, -formula, -Controls)  
    %>% mutate(act = case_when(
                   act==''~'1. combined',
                   act=='_ha'~'2. high',
                   act=='_ma'~'3. moderate',
                   act=='_la'~'4. low',
                   ))
    %>% arrange(group, act) 
) %>% print(., n=Inf, width=Inf) 
cat(glue("\n---- {case} ----\n") )
(
    tabn  
    %>% select(group, act, n)  
    %>% rowwise(.)
    %>% mutate(cases = act %>%
                   str_replace(string=., pattern="[0-9]\\. ", replacement="") %>%
                   glue(": {n}")
               ) 
    %>% select(group, cases) 
    %>% nest(-group)  
    %>% mutate(n = future_map_chr(.x=data, function(.x) paste(.x %>% pull(cases), collapse = ', ') ),
               n = glue("{group}: {n}") )  
    %>% pull(n) 
    %>% paste(., collapse = '; ')
)

## ** DONE figure 6


theme_set(ggtheme2())
## 
figure = 'fig-6'
## 
date_min <- as.Date('2020-11-01')
date_max <- as.Date('2021-01-29')
groups <- c(
    'fns',
    'F',
    'B',
    'A',
    'ss1',
    'qanon'
)
vars <- c('date' = 'date',
          'nusers' = 'Number of users')
tab <- dftot %>%
  filter(date >= date_min & date <= date_max) %>%
  filter(group %in% groups) %>%
  mutate(group = factor(group, groups, FN_GROUPS[groups]))  %>% 
  select(date, group, nusers)
tab
## 
x = "date"
y = "nusers"
color    = NULL
fill     = NULL
facet1   = 'group'
facet2   = NULL
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = NULL
xlab     = NULL
degree=4
size=1.5
ylab='Number of active users'
g = (
    tab
    %>% ggplot(.)
    + geom_vline(aes(xintercept=jan06), linetype="dashed", col='black')
    + geom_vline(aes(xintercept=jan12), linetype="dashed", col="black")
    ## 
    + geom_point(aes_string(x=x, y=y, fill=fill), size=size,
                show.legend = FALSE)
    ##
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm", se=T, formula=y~poly(x, degree), size=1,
                  show.legend = FALSE,
                  color='black',
                  alpha=.3,
                  data=.%>% filter(date<=jan06) ) 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill, linetype=fill),
                  method="lm", se=T, formula=y~poly(x, degree), size=1,
                  alpha=.3,
                  color='black',
                  show.legend = FALSE,
                  ## show.legend = FALSE,
                  data=.%>% filter(date>=jan12)) 
    ## 
    ## + scale_x_date(breaks=c(date_min_did, t1, t2), date_labels = '%b %d') 
    + scale_shape_manual(values=c(21, 22, 20))
    + scale_linetype_manual(values=c(2,1,1))
    + scale_color_manual(values=fill_scale)
    + scale_fill_manual(values=fill_scale)
    + facet_wrap( paste("~", facet1), ncol = , scales='free')  
    + labs(
          x        = NULL,
          y        = ylab,
          color    = NULL, 
          fill     = NULL,
          linetype = NULL,
          shape = NULL
      )
    + ggguides()
)
g
if (SAVE) {
    ## table
    cat(glue("\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("Saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=4 )}
    cat("done!\n")
}
##
theme_set(ggtheme())

## ** ------- Extended --------
## ** DONE figure extended 1

theme_set(ggtheme2())
## 
figure = 'fig-extended-1'
## 
digits   = 4
tab = (
    res.did.dosage
    %>% select(act, outcome, control, treat, controlsl, summ, dosage, follow.trump)
    %>% unnest(summ)
    ## 
    %>% filter(term=='t:d') 
    %>% filter(str_detect(outcome, pattern="tweet")) 
    %>% filter(controlsl=='without controls') 
    ## ## 
    %>% mutate(act = factor(act,
                            levels=c('', '_ha', '_ma', '_la'),
                            labels=c("Combined", 'High', 'Moderate', 'Low')),
               ## 
               outcome = factor(outcome,
                                levels=c('tweet', 'retweet'),
                                labels=c('Tweeted misinformation', 'Retweeted misinformation')),
               ## 
               label = case_when(p.value<=0.05 ~ glue('{round(estimate,digits)}***'),
                                 TRUE ~ as.character(round(estimate, digits))
                                 )
               )
)
tab
## 
x = "dosage"
y = "estimate"
color=NULL
fill='follow.trump'
facet1='outcome'
facet2=NULL
ylab=glue('Difference-in-differences Effect')
xlab='Number of Deplatformed Accounts the Non-deplatformed Misinformation Sharers Followed'
fn = 'X+: followed X or more'
leg_title = 'Trump follower?'
dodge=.3
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red", alpha=.5)
    + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high",
                               color=fill), width=.05, size=.7,
                    position=position_dodge(dodge))
    + geom_point(aes_string(x=x, y=y, fill=fill),
                 position=position_dodge(dodge))
    + scale_x_continuous(breaks=1:10, labels=glue("{1:10}+")) 
    + scale_colour_grey(start = 0, end = .6,  na.value="red") 
    + scale_fill_grey(start = 0, end = .6,  na.value="red") 
    + facet_wrap( paste("~", facet1), ncol = 2, scales='free_x')  
    + ggguides(ncol=3)
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg_title, 
          fill     = leg_title,
          linetype = NULL,
          shape    = NULL,
          title    = NULL,
          subtitle = NULL,
          caption  = fn
      )
)
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_FIGURES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=4.5 )}
    cat("done!\n")
}
## 
theme_set(ggtheme())


## samples sizes
## -------------
case   = 'without controls' # 'with controls'
tabn = (
    res.did.dosage
    %>% filter(controlsl==!!case)  
    %>% filter(outcome=='tweet') 
    %>% mutate(n = future_map_chr(.x=est, function(.x) scales::comma(nrow(model.frame(.x))))) 
    %>% select(-label, -summ,  -est, -outcome, -formula, -Controls)  
    %>% arrange(follow.trump, dosage) 
) %>% print(., n=Inf, width=Inf) 
cat(glue("\n---- {case} ----\n") )
(
    tabn  
    %>% rowwise(.)
    %>% nest(-follow.trump)  
    %>% mutate(n = future_map_chr(.x=data, function(.x)
        glue("{.x$dosage}: {.x$n}")  %>% paste(., collapse = '; ')),
        n = glue("Follow Trump ({follow.trump}): {n}") 
        )  
    %>% pull(n) 
    %>% paste(., collapse = '; ')
)

## ** DONE figure extended 2, 7, and 8; and supplemental 3 and 4

figures = c("fig-extended-2",       # fig-S3a
            "fig-extended-7",       # fig-S4a
            "fig-extended-8",       # fig-S4b
            ##
            "fig-supplemental-3",  # fig-S3b
            "fig-supplemental-4"   # fig-S3c
            )
groups = c(
    'suspended',
    'F',
    'A',
    'B',
    'D'
)
for (figure in figures)
{
    ## parameters
    ## ----------
    ## 
    figures = tribble(
        ~figure,  ~stat           , ~cutoff.low      , ~ outcomes                    , ~groups,
        "fig-extended-2"  , c('avg', 'total'), cutoff_low       , c('fake_initiation','fake_rt'), groups,
        "fig-extended-7"  , c('avg', 'total'), cutoff.low.dez20 , c('fake_initiation','fake_rt'), groups,
        "fig-extended-8"  , c('avg', 'total'), cutoff.low.jan18 , c('fake_initiation','fake_rt'), groups,
        ## 
        "fig-supplemental-3",c('avg','total'),cutoff.low.jan05 , c('fake_initiation','fake_rt'), groups,
        "fig-supplemental-4",c('avg','total'),cutoff.low.jan07 , c('fake_initiation','fake_rt'), groups,
        ## 
        ) %>% mutate(outcomesl = future_map(.x=outcomes, function(.x) OUTCOMES.ALL[.x]))
    outcomes  = figures %>% filter(figure==!!figure)  %>% pull(outcomes) %>% extract2(1) 
    outcomesl = figures %>% filter(figure==!!figure)  %>% pull(outcomesl) %>% extract2(1) 
    stat      = figures %>% filter(figure==!!figure)  %>% pull(stat) %>% extract2(1) 
    cutoff.low = figures %>% filter(figure==!!figure)  %>% pull(cutoff.low) %>% extract2(1) 
    ## 
    ## table
    ## -----
    tab = (
        ## res.rdd
        res.rdd
        %>% filter(cutoff.low == !!cutoff.low ) 
        %>% filter(outcome == !!outcomes) 
        %>% filter(group %in% groups)  
        %>% filter(stat %in% !!stat) 
        %>% select(-outcome,-stat, -group)
        ## ## 
        %>% unnest(summ) 
        %>% filter(str_detect(parameter, pattern="Bias-"))  
        ## ## 
        %>% mutate(across(where(is.numeric), round, 2),
                   facet = case_when(group=='suspended' ~ 'Deplatformed Users',
                                     T ~ 'Not Deplatformed Users'),
                   group = factor(group, groups, FN_GROUPS[groups]),
                   stat  = factor(stat, c('avg', 'total'), c('Average', 'Total')),
                   outcome = factor(outcome, outcomes, OUTCOMES.ALL[outcomes]),
                   stars   = case_when(pv<0.05 ~ "***", T~''),
                   label   = glue("{coef}{stars}") 
                   ) 
        %>% select(-est, -data)
    )
    if (cutoff.low==cutoff.low.jan18) {
        tab = tab %>% filter(group!=FN_GROUPS['suspended']) 
    }
    ## 
    ## plot
    ## ----
    x = "group"
    y = "coef"
    facet1   = 'facet'
    facet2   = 'stat'
    color    = NULL
    fill     = "outcome"
    label    = 'label'
    leg      = glue("Cutoff: {format(cutoff.low, format='%B %d, %Y')}") 
    title    = NULL
    subtitle = NULL
    caption  = '*** significant at 0.05'
    fn       = '*** significant at 0.05'
    dodge    = .3
    ylab="SRD effects on\nMisinformation Circulation"
    xlab     = NULL
    g = (
        tab
        %>% ggplot(.)
        + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
        + geom_errorbar(aes_string(x=x, ymin="ci.low", ymax="ci.high", color=fill),
                        width=.05, position = position_dodge(dodge)) 
        + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                     position = position_dodge(dodge))
        + geom_text(aes_string(x=x, y=y, label=label, color=fill),
                    size=3, hjust=-.2, width=.05, position = position_dodge(dodge)) 
        ## 
        + scale_shape_manual(values=c(21,22))
        + scale_colour_grey(start = 0, end = .6, na.value="red") 
        + scale_fill_grey(start = 0, end = .6, na.value="red") 
        + scale_x_discrete(labels = scales::wrap_format(15))
        ## 
        + ggguides()
        + labs(
              x        = xlab,
              y        = ylab,
              color    = leg, 
              fill     = leg,
              linetype = leg,
              shape    = leg,
              title    = title,
              subtitle = subtitle,
              caption  = caption
          )
        + theme(strip.placement = "outside")
    )
    ## facet
    if (cutoff.low!=cutoff.low.jan18) {
        g = g + facet_grid2(glue("{facet2} ~ {facet1}"), scales='free',
                            independent='all',switch="y") +
            force_panelsizes(cols = c(1, 4), rows = 1, respect = T)
    }else{
        g = g + facet_grid2(glue("{facet2} ~ ."), scales='free',
                            independent='all',switch="y")
    }
    g
    if (SAVE) {
        if (str_detect(figure, pattern="supplemental")) {
            path = PATH_MAN_FIGURES 
        }else{
            path = PATH_MAN_FIGURES 
        }
        ## table
        cat(glue("\nSaving table of figure {figure}...") )
        fn = file.path(path, glue("{figure}"))
        write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
        write_xlsx(x=tab, path=glue("{fn}.xlsx"))
        cat("done!\n")
        ## 
        cat(glue("Saving figure {figure}...") )
        fn  = c(glue("{figure}.pdf"),
                glue("{figure}.png"),
                glue("{figure}.jpg"))
        fns = file.path(path, fn)
        for (fn in fns){ggsave(g, filename=fn, width=10, height=6 )}
        cat("done!\n")
    }
}


## ** DONE figure extended 3

figure <- 'fig-extended-3'
## 
date_min <- as.Date('2020-11-01')
date_max <- as.Date('2021-01-29')
## 
groups <- list(
  suspended = "Deplatformed users",
  F = FN_GROUPS["F"],
  B = FN_GROUPS["B"],
  A = FN_GROUPS["A"],
  ss1 = 'Super sharers (0.1%)',
  qanon = 'QAnon accounts'
)
vars <- c(
  "date" = 'date',
  "group" = 'group',
  "not_fake_conservative_rt" = 'Conservative URL',
  "not_fake_liberal_rt" = 'Liberal URL'
)
## 
tab <- (
    dftot 
    %>% filter(stat == 'total') 
    %>% filter(date >= date_min & date <= date_max)  
    %>% filter(group %in% names(groups)) 
    %>% select(names(vars))
    %>% pivot_longer(cols = c("not_fake_conservative_rt", "not_fake_liberal_rt"),
                     names_to = 'ideo', values_to = 'total') 
    %>% mutate(ideo = factor(ideo, names(NFN), NFN),
               group = factor(group, names(groups), groups)
               )
)
tab
## 
x <- "date"
y <- "total"
fill <- NULL
facet1 <- "ideo"
facet2 <- "group"
fn <- NULL
ylab <- 'Total number of non-misinformation URL retweeted'
alpha <- 1
colorshade <- 'gray70'
colorfit <- 'gray30'
leg = NULL
title=NULL
subtitle = NULL
caption = NULL
g = (
    tab
    %>% ggplot(.)
    + geom_point(aes_string(x=x, y=y, fill=fill), size=2)
    + geom_smooth(aes_string(x=x, y=y),
                     method="lm", se=T, formula="y~poly(x, 5)",
                     color=colorfit,
                     fill=colorshade,
                     data=. %>% filter(date<=jan06)) 
    + geom_smooth(aes_string(x=x, y=y),
                     method="lm", se=T, formula="y~poly(x, 5)",
                     color=colorfit,
                     fill=colorshade,
                     data=. %>% filter(date>=jan12)) 
    ## # 
    + geom_vline(aes_string(xintercept=jan06),
                    linetype="dashed", col="black")
    + geom_vline(aes_string(xintercept=jan12),
                    linetype="dashed", col="black")
    + facet_grid(glue("{facet2} ~ {facet1}"), scales='free') 
    + ggguides()
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_FIGURES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=8, height=10 )}
    cat("done!\n")
}


## ** DONE figure extended 4

figure = 'fig-extended-4'
## 
treat='D'
treat_lab=unname(FN_GROUPS[treat])
## 
control = 'F'
control_lab= unname(FN_GROUPS[control])
## 
ylab = "Total Misinformation Retweeted (std)"
##
## 
tab = (
    dfdid
    %>% filter(date>date_min_did ) 
    %>% select(date,
               y_rt=fake_rt,
               y_init=fake_initiation,
               treat=!!treat,
               control=!!control,
               ha, ma, la)
    %>% filter(ha==1 | ma==1 | la==1) 
    ## ## 
    %>% pivot_longer(cols=c(treat, control),
                     names_to='group', values_to = 'group_value')
    %>% filter((group=='treat' & group_value==1) |
               (group=='control' & group_value==1)) 
    ## ## 
    %>% pivot_longer(cols=c(ha, ma, la),
                     names_to='act', values_to = 'act_value')
    %>% filter((act=='ha' & act_value==1) |
               (act=='ma' & act_value==1) |
               (act=='la' & act_value==1)
               ) 
    %>% group_by(date, act, group)
    %>% summarise(
            y_raw_init = sum(y_init),
            y_raw_rt = sum(y_rt),
            ) 
    %>% group_by(act, group)
    %>% mutate(
            y_std_rt = c(scale(y_raw_rt)),
            y_std_init = c(scale(y_raw_init)),
            )
    %>% ungroup(.) 
    %>% mutate(group = recode_factor(group, !!!c(treat=treat_lab,
                                                 control=control_lab
                                                 ))%>% as.character(.))
    ## %>% mutate(group = str_replace_all(group, !!!c(treat=unname(treat_lab),
    ##                                                control=unname(control_lab)
    ##                                                ))%>% as.character(.))
    %>% mutate(act = recode_factor(act, !!!c(ha='High activity users',
                                             ma='Moderate activity users',
                                             la='Low activity users'
                                             )))
    %>% pivot_longer(c(y_std_rt, y_std_init),
                     names_to = 'y', values_to = 'yval')
    %>% mutate(y = recode_factor(y, !!!c(y_std_init='Tweeted',
                                         y_std_rt='Retweeted'
                                         )))
)
tab

## 
## 
x = "date"
y = "yval"
color=NULL
fill='group'
facet1='act'
facet2='y'
dodge=.1
size=1.5
counter_color="darkred"
fill_scale=c('black', 'gray60')
names(fill_scale) = c(treat_lab, control_lab)
t0=jan06
g = (
    tab
    %>% ggplot(.)
    ## 
    + geom_line(aes_string(x=x, y=y, color=fill, linetype=fill))
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                 size=size)
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 1), size=.5,
                  show.legend = FALSE,
                  data=.%>% filter(date<=t0)) 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 1), size=.5,
                  show.legend = FALSE,
                  data=.%>% filter(date>=jan12)) 
    + geom_vline(aes(xintercept=t0), linetype="dashed", col='black')
    + geom_vline(aes(xintercept=jan12), linetype="dashed", col="black")
    ## 
    + scale_x_date(breaks=c(date_min_did, t0, jan12), date_labels = '%b %d') 
    + scale_shape_manual(values=c(21, 22))
    + scale_linetype_manual(values=c(1,1))
    + scale_color_manual(values=fill_scale)
    + scale_fill_manual(values=fill_scale)
    + ggh4x::facet_grid2(paste(facet1, " ~ ", facet2), scales='free',
                         independent='all') 
    + labs(
          x        = NULL,
          y        = ylab,
          color    = NULL, 
          fill     = NULL,
          linetype = NULL,
          shape    = NULL,
          title    = NULL,
          subtitle = NULL,
          caption  = NULL
      )
    + ggguides()
)
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_FIGURES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=7 )}
    cat("done!\n")
}



## ** DONE figure extended 5

## See figure 5

## ** DONE figure extended 6

figure = 'fig-extended-6'
## parameters
## ----------
groups = c(
    #,
    'F',
    'A',
    'B',
    'D'
)
outcomes  = names(OUTCOMES.ROBUSTNESS.NFN)
cutoff.low = cutoff_low
stat = c('total', 'avg')
## 
## table
## -----
tab = (
    ## res.rdd
    res.rdd 
    %>% filter(outcome %in%  !!outcomes) 
    %>% filter(group %in% groups)  
    %>% filter(stat %in% !!stat) 
    %>% select(-outcome,-stat, -group)
    ## ## ## 
    %>% unnest(summ) 
    %>% filter(str_detect(parameter, pattern="Bias"))  
    ## ## ## 
    %>% mutate(
            ## across(where(is.numeric), round, 2),
            facet = case_when(group=='suspended' ~ 'Deplatformed Users',
                              T ~ 'Not Deplatformed Users'),
            group = factor(group, groups, FN_GROUPS[groups]),
            stat  = factor(stat, c('avg', 'total'), c('Average', 'Total')),
            outcome = factor(outcome, outcomes, OUTCOMES.ALL[outcomes]),
            ) 
    %>% select(-data, -est)
)
tab
## 
## plot
## ----
x = "group"
y = "coef"
facet1   = 'facet'
facet2   = 'stat'
color    = NULL
fill     = "outcome"
label    = 'label'
leg      = glue("Cutoff: {format(cutoff.low, format='%B %d, %Y')}") 
title    = NULL
subtitle = NULL
caption  = '*** significant at 0.05'
fn       = '*** significant at 0.05'
dodge    = .3
ylab     = "SRD effect estimates on\nMisinformation Circulation"
xlab     = NULL
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="ci.low", ymax="ci.high", color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                 position = position_dodge(dodge))
    ## 
    + scale_shape_manual(values=c(21, 22, 23, 24))
    + scale_colour_grey(start = 0, end = .6, na.value="red") 
    + scale_fill_grey(start = 0, end = .6, na.value="red") 
    + scale_x_discrete(labels = scales::wrap_format(15))
    ## 
    + facet_grid2(glue("{facet2} ~ ."), scales='free',
                        independent='all',switch="y")
    ## 
    + ggguides(ncol=3)
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + theme(strip.placement = "outside")
)
## facet
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of {figure}...") )
    fn = file.path(PATH_MAN_FIGURES , glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES , fn)
    for (fn in fns){ggsave(g, filename=fn, width=10, height=6 )}
    cat("done!\n")
}

## rdd_report_details(res.rdd.robust.date)

## ** DONE ------- SI --------
## ** DONE figure supplemental 1

figure = 'fig-supplemental-1'
groups <- c(
  'suspended',
  'F',
  'B',
  'A',
  'qanon',
  'ss5',
  'ha',
  'ma',
  'la'
)
tab = (
    dfdid
    %>% select(groups, "user_id")
    %>% distinct(user_id, .keep_all=TRUE) 
    %>% drop_na()  
    %>% select(-user_id) 
    %>% rename_at(.vars=groups, ~paste(FN_GROUPS[groups]) )
)
g = upset(tab, intersect=names(tab), min_size=500,
    base_annotations=list(
        'Intersection size'=intersection_size(
            text = list(angle=45, vjust=-.3, hjust=0, color='black'))
        )
    ) 
if (SAVE) {
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg")
            )
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=9, height=6 )}
}


## ** DONE figure supplemental 2

figure = 'fig-supplemental-2'
## 
fake       = 'fake'
stat       = 'pct'
date_min   = as.Date('2020-09-01')
date_max   = as.Date('2021-01-29')
election   = as.Date('2020-11-03')
date_min16 = as.Date('2016-09-01')
date_max16 = as.Date('2017-01-29')
election16 = as.Date('2016-11-08')
## 
## table
## -----
tab = (
    dftot    
    %>% mutate(facet='2020 Election Cycle')
    ## 2016
    %>% bind_rows(dftot16
                  %>% mutate(n=fake+not_fake,
                             facet='2016 Election Cycle')
                  %>% filter(date<'2017-02-02') )  
    ## deca
    %>% bind_rows(dftot.deca
                  %>% mutate(n=fake+not_fake,
                             facet='2020 Election Cycle (Decahose)'))  
    ## 
    %>% rename(fake=!!fake) 
    %>% mutate(pct = fake/n) 
    %>% drop_na(pct) 
    ## 
    %>% filter(group %in% c('fns', 'all')) 
    %>% filter(date_min < date & date <= date_max |
               date_min16 < date & date <= date_max16)  
    ## 
    %>% mutate(group = factor(group, c('all', 'fns'),
                              c('All users (shared at least one URL of any kind)',
                                'Misinformation sharers only (shared at least one misinformation URL)')
                              ))
)
tab
## 
## plot
## ----
x = "date"
y = "pct"
color    = NULL
fill     = 'group'
facet1   = 'facet'
facet2   = NULL
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = "Percentage of misinformation shared"
xlab     = NULL
size     = 3
g = (
    tab
    %>% ggplot(.) 
    + geom_vline(aes(xintercept=jan06), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=jan12), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=election), linetype="solid", col="black")
    + geom_vline(aes(xintercept=jan06.17), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=jan12.17), linetype="dashed", col="black")
    + geom_vline(aes(xintercept=election16), linetype="solid", col="black")
    #
    + geom_line(aes_string(x=x, y=y, group=fill, color=fill))
    + geom_point(aes_string(x=x, y=y, fill=fill), size=2)
    ##
    + geom_text(aes_string(x=election16, y=Inf, label="'Election day'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    + geom_text(aes_string(x=jan06.17, y=Inf, label="'Jan 6th'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    + geom_text(aes_string(x=jan12.17, y=Inf, label="'Jan 12th'"),
                size=size, hjust=-.05, vjust=1.5,
                data=data.frame(facet='2016 Election Cycle')) 
    ## 
    + geom_text(aes_string(x=election, y=Inf, label="'Election day'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet=c('2020 Election Cycle',
                                        '2020 Election Cycle (Decahose)' ))) 
    + geom_text(aes_string(x=jan06, y=Inf, label="'Jan 6th'"),
                size=size, hjust=1.1, vjust=1.5,
                data=data.frame(facet=c('2020 Election Cycle',
                                        '2020 Election Cycle (Decahose)' ))) 
    + geom_text(aes_string(x=jan12, y=Inf, label="'Jan 12th'"),
                size=size, hjust=-.05, vjust=1.5,
                data=data.frame(facet=c('2020 Election Cycle',
                                        '2020 Election Cycle (Decahose)' ))) 
    ## 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 5),
                  data = . %>% filter(date<=jan06) ) 
    + geom_smooth(aes_string(x=x, y=y, colour=fill, fill=fill),
                  method="lm", se=T, formula=y~poly(x, 5),
                  data = . %>% filter(date>=jan12) ) 
    ## 
    + scale_x_date(date_labels="%b %d", breaks = '2 months') 
    + scale_y_continuous(labels=percent_format(scale=100)) 
    + scale_colour_grey(start = 0, end = .7, na.value="red")
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    ## 
    + facet_wrap(glue("~ {facet1}"), ncol = , scales='free_x')
    ## 
    + ggguides()
    + labs(
          x        = NULL,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
)
g
## 
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_FIGURES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=NA, height=NA )}
    cat("done!\n")
    for (fn in fns){ggsave(g, filename=fn, width=10, height=4.5 )}
}


## info
## ----
tab16   = tab %>% filter(date<'2020-01-01') 
avg16   = tab16 %>% filter(str_detect(group, pattern="All") & stat=='total')  %>% pull(n) %>% mean()
users16 = tab16 %>% filter(str_detect(group, pattern="All") & stat=='total') %>% pull(nusers) %>% mean()
tab20   = tab %>% filter(date>'2018-01-01') 
avg20   = tab20 %>% filter(str_detect(group, pattern="All") & stat=='total')  %>% pull(n) %>% mean()
users20 = tab20 %>% filter(str_detect(group, pattern="All") & stat=='total') %>% pull(nusers) %>% mean()
drop20 = (
    res.rdd.main
    %>% select(summ)
    %>% unnest(summ)
    %>% filter(stat=='total')  
    %>% filter(group=='all' | group=='fns' | group == 'suspended')    
    %>% filter(str_detect(parameter, pattern="Bias"))   
    %>% arrange(outcome, group )  
    %>% select(group, stat, outcome, coef)  
    %>% pivot_wider(id_cols=c("outcome", "stat"),
                    names_from=group,
                    values_from=coef) 
    %>% mutate(`s/a` = suspended/all,
               `s/fns` = suspended/fns)
)
print(drop20)


print(glue("\n
Information: 

2016
----
Start date : {min(tab16$date)}
End date   : {max(tab16$date)}
Number of days : {max(tab16$date) - min(tab16$date)}
Average daily total number of shared (tweet+retweet): {comma(avg16)}
Average daily number of users: {comma(users16)}

2020
----
Start date : {min(tab20$date)}
End date   : {max(tab20$date)}
Number of days : {max(tab20$date) - min(tab20$date)}
Average daily total number of shared (tweet+retweet): {comma(avg20)}
Average daily number of users: {comma(users20)}

Drop (SRD):
") )
print(drop20)

## ** DONE figure supplemental 3

## figure extended 1 to 3 and supplemental 3 and 4

## ** DONE figure supplemental 4

## figure extended 1 to 3 and supplemental 3 and 4

## ** DONE figure supplemental 5

figure = 'fig-supplemental-5'
## parameters
## ----------
groups = c(
    'F',
    'A',
    'B',
    'D'
)
outcomes  = names(OUTCOMES.ROBUSTNESS.FN.LIST )
cutoff.low = cutoff_low
stat = c('total', 'avg')
## 
## table
## -----
tab = (
    res.rdd 
    %>% filter(outcome %in%  !!outcomes) 
    %>% filter(group %in% groups)  
    %>% filter(stat %in% !!stat) 
    %>% select(-outcome,-stat, -group)
    ## ## 
    %>% unnest(summ) 
    %>% filter(str_detect(parameter, pattern="Bias-"))  
    ## ## 
    %>% mutate(
            facet = case_when(group=='suspended' ~ 'Deplatformed Users',
                              T ~ 'Not Deplatformed Users'),
            group = factor(group, groups, FN_GROUPS[groups]),
            stat  = factor(stat, c('avg', 'total'), c('Average', 'Total')),
            outcome = factor(outcome, outcomes, OUTCOMES.ROBUSTNESS.FN.LIST[outcomes]),
            stars   = case_when(pv<0.05 ~ "***", T~''),
            label   = glue("{coef}{stars}") 
        ) 
    %>% select(-data, -est)
)
tab 
## 
## plot
## ----
x = "group"
y = "coef"
facet1   = 'facet'
facet2   = 'stat'
color    = NULL
fill     = "outcome"
label    = 'label'
leg      = glue("Cutoff: {format(cutoff.low, format='%B %d, %Y')}") 
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .3
ylab="SRD effect estimates on\nMisinformation Circulation"
xlab     = NULL
fn='*** significant at 0.05'
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, ymin="ci.low", ymax="ci.high", color=fill),
                    width=.05, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill, color=fill),
                 position = position_dodge(dodge))
    ## 
    + scale_shape_manual(values=c(21, 22, 23, 24, 25, 1))
    + scale_colour_grey(start = 0, end = .6, na.value="red") 
    + scale_fill_grey(start = 0, end = .6, na.value="red") 
    + scale_x_discrete(labels = scales::wrap_format(15))
    ## 
    + facet_grid2(glue("{facet2} ~ ."), scales='free',
                        independent='all',switch="y")
    ## 
    + ggguides(ncol=3)
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + theme(strip.placement = "outside")
)
## facet
g
if (SAVE) {
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(PATH_MAN_FIGURES, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    ## 
    cat(glue("saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(PATH_MAN_FIGURES, fn)
    for (fn in fns){ggsave(g, filename=fn, width=10, height=6 )}
    cat("done!\n")
}

## rdd_report_details(res.rdd)

## * done

print(sessionInfo())
print(version)
print(difftime(Sys.time(), time.start, units='mins'))
cat('\nAll done!\n')


