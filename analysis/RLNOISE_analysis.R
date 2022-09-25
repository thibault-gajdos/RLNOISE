rm(list=ls(all=TRUE))## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/RLNOISE/analysis/')
load('summary_indiv.rdata') ## summary.all
load( 'pred_all.rdata') ## pred.all
load( 'accuracy.rdata') ## accuracy
load('obs.rdata') ## obs.all

pred.all <- pred.all %>%
      relocate(c(model, subjID), .before = 1)
  ## create an outlier variable
  ## = 1 if max(Rhat)>1.2 (fit did not converge), 0 otherwise
  outlier <- summary.all %>%
      group_by(model, subjID) %>%
      mutate(Rmax = max(Rhat, na.rm = T)) %>%
      mutate(outlier = ifelse(Rmax>1.1, 1,0)) %>%
      filter(param == 'alpha') %>%
      select(model, subjID, outlier) %>%
      ungroup()
  out  <- outlier %>%
      group_by(model) %>%
      summarise(outlier = sum(outlier, na.rm = TRUE)) %>%
      pivot_wider(names_from = model, values_from = outlier)
print(kable(out))

pred.all <- pred.all %>%
    relocate(c(model, subjID), .before = 1)
## create an outlier variable
## = 1 if max(Rhat)>1.12 (fit did not converge), 0 otherwise
outlier <- summary.all %>%
    group_by(model, subjID) %>%
    mutate(Rmax = max(Rhat, na.rm = T)) %>%
    mutate(outlier = ifelse(Rmax>1.1, 1,0)) %>%
    filter(param == 'alpha') %>%
    select(model, subjID, outlier) %>%
    ungroup()
out  <- outlier %>%
    group_by(model) %>%
    summarise(outlier = sum(outlier, na.rm = TRUE)) %>%
    pivot_wider(names_from = model, values_from = outlier)
print(kable(out))

a <- accuracy %>%
    rowwise() %>%
    mutate(acc = mean(c_across(starts_with("V")), na.rm = TRUE), .keep = "unused") %>%
    ungroup()
accuracy <- merge(a, outlier)

accuracy.summary <- accuracy %>%
    filter(outlier == 0) %>%
    group_by(model) %>%
    summarise(acc = mean(acc, na.rm = TRUE)*100)
a.summary <- accuracy.summary %>%
  pivot_wider(names_from = model, values_from = acc)
print(kable(a.summary, digits = 2))

d <- summary.all %>%
  mutate(k = case_when(
	   model == 'rw' ~ 4,
	   model == 'rw_noise' ~ 5
	 )) %>%
    group_by(model, subjID) %>%
    mutate(Rmax = max(Rhat, na.rm = T)) %>%
    mutate(outlier = ifelse(Rmax>1.1, 1,0)) %>%
    filter(outlier == 0) %>%
    mutate(AIC = -2*l + 2*k) %>%
    ungroup()

aic.summary <- d %>%
  group_by(model) %>%
  summarise(AIC = mean(AIC)) %>%
  pivot_wider(names_from = model, values_from = AIC) 
print(kable(aic.summary, digits = 1))

d.param <- d %>%
  filter(outlier == 0) %>%
  select(model, param, mean) %>%
  group_by(param, model) %>%
  summarise(mean = mean(mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = param, values_from = mean) 
print(kable(d.param, digit = 2))

d.param <- d %>%
  filter(outlier == 0) %>%
  select(model, param, mean) %>%
  group_by(param, model) %>%
  summarise(median = median(mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = param, values_from = median) 
print(kable(d.param, digit = 2))