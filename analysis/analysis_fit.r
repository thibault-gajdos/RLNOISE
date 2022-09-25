rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/IGT/data/')



## * VSE
## ** Model simulation vs data
## *** Mice
load('./cluster/fit_mice_vse_noise.rdata')
load('./cluster/data_mice/data_mice.rdata')
sim_mice_vse = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_mice_vse = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

vse.mice <- fit.mice
for (i in c(1:40)){
    x <- table(extract(fit.mice)$y_pred[,i,])/20000
    sim_mice_vse[i,1] <- i
    sim_mice_vse[i,2:5] <- x
}
save(sim_mice_vse, file = 'sim_mice_vse.rdata')

for (i in c(1:40)){
    x <- table(data.mice[data.mice$subjID == i,]$choice)
    data_mice_vse[i,1] <- i
    data_mice_vse[i,2:5] <- x
}
save(data_mice_vse, file = 'data_mice_vse.rdata')

sim <- sim_mice_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_mice_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_mice_vse %>%
    mutate(type = 'sim')
dd <- data_mice_vse %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot

## *** Human
load('./cluster/fit_human_vse_noise.rdata')
load('./cluster/data_human/data_human.rdata')
sim_human_vse = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_human_vse = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

vse.human <- fit.human
for (i in c(1:40)){
    x <- table(extract(fit.human)$y_pred[,i,])/20000
    sim_human_vse[i,1] <- i
    sim_human_vse[i,2:5] <- x
}
save(sim_human_vse, file = 'sim_human_vse.rdata')

for (i in c(1:40)){
    x <- table(data.human[data.human$subjID == i,]$choice)
    data_human_vse[i,1] <- i
    data_human_vse[i,2:5] <- x
}
save(data_human_vse, file = 'data_human_vse.rdata')

sim <- sim_human_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_human_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_human_vse %>%
    mutate(type = 'sim')
dd <- data_human_vse %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot

## *** all
load('./cluster/fit_all_vse_noise.rdata')
load('./cluster/data_all/data_all.rdata')
sim_all_vse = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_all_vse = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

for (i in c(1:57)){
    x <- table(extract(fit.human)$y_pred[,i,])/20000
    sim_all_vse[i,1] <- i
    sim_all_vse[i,2:5] <- x
}
save(sim_all_vse, file = 'sim_all_vse.rdata')
data.all <- data.all %>% filter(study == 'Steingroever2011')
for (i in c(1:57)){
    x <- table(data.all[data.all$subjID == i,]$choice)
    data_all_vse[i,1] <- i
    data_all_vse[i,2:5] <- x
}
save(data_all_vse, file = 'data_all_vse.rdata')

sim <- sim_all_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_all_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_all_vse %>%
    mutate(type = 'sim')
dd <- data_all_vse %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot


## ** Model comparison

## *** mice

load('./cluster/data_mice/data_mice.rdata')
load('./cluster/fit_mice_vse_noise.rdata')
vse.mice.noise <- fit.mice
N = 40
k = 6*(N+2)
log_lik = loo::extract_log_lik(vse.mice.noise, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_vse_noise = -2*l  + 2*k

load('./cluster/fit_mice_vse.rdata')
vse.mice <- fit.mice
N = 40
k = 5*(N+2)
log_lik = loo::extract_log_lik(vse.mice, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_vse = -2*l  + 2*k

load('./cluster/fit_mice_pvl_lambda.rdata')
pvl.mice <- fit.mice
N =40
k = 4*(N+2)
log_lik = loo::extract_log_lik(pvl.mice, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_pvl_lambda = -2*l  + 2*k

load('./cluster/fit_mice_pvl_lambda_noise.rdata')
pvl.mice <- fit.mice
N = 40
k = 5*(N+2)
log_lik = loo::extract_log_lik(pvl.mice, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_pvl_lambda_noise = -2*l  + 2*k

load('./cluster/fit_mice_pvl.rdata')
pvl.mice <- fit.mice
N = 40
k = 3*(N+2)
log_lik = loo::extract_log_lik(pvl.mice, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_pvl = -2*l  + 2*k

load('./cluster/fit_mice_pvl_noise.rdata')
pvl.mice <- pvl.mice
N = 40
k = 4*(N+2)
log_lik = loo::extract_log_lik(pvl.mice, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC_pvl_noise = -2*l  + 2*k


AIC_vse_noise
AIC_vse

AIC_pvl_lambda_noise
AIC_pvl_lambda

AIC_pvl_noise
AIC_pvl



## *** human
load('./cluster/data_human/data_human.rdata')
load('./cluster/fit_human_vse_noise.rdata')
vse.human.noise <- fit.human
N = nrow(data.human)
k = 6*(N+2)
log_lik = loo::extract_log_lik(vse.human.noise, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC.noise = -2*l  + 2*k

load('./cluster/fit_human_vse.rdata')
vse.human <- fit.human
N = nrow(data.human)
k = 5*(N+2)
log_lik = loo::extract_log_lik(vse.human, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC = -2*l  + 2*k

AIC.noise
AIC

## *** all
load('./cluster/data_all/data_all.rdata')
load('./cluster/fit_all_vse_noise.rdata')
vse.all.noise <- fit.human
N = nrow(data.all %>% filter(study=='Steingroever2011'))
k = 6*(N+1)
log_lik = loo::extract_log_lik(vse.all.noise, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC.noise = -2*l  + 2*k

load('./cluster/fit_all_vse.rdata')
vse.all <- fit.human
N = nrow(data.all)
k = 5*(N+1)
log_lik = loo::extract_log_lik(vse.all, parameter_name = "log_lik", merge_chains = TRUE)
l <- loo::elpd(log_lik)$estimates[[1]]
AIC = -2*l  + 2*k

AIC.noise
AIC

## ** Model results
## *** VSE
load('./cluster/fit_mice_vse_noise.rdata')
vse.mice <- fit.mice
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_gamma', 'mu_phi', 'mu_zeta')
coef.mice.vse <- summary(vse.mice, pars = params)$summary
save(coef.mice.vse, file = 'coef_mice_vse.rdata')
launch_shinystan(vse.mice)


load('./cluster/fit_human_vse_noise.rdata')
vse.human <- fit.human
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_gamma', 'mu_phi', 'mu_zeta')
coef.human.vse <- summary(vse.human, pars = params)$summary
save(coef.human.vse, file = 'coef_human_vse.rdata')
launch_shinystan(vse.human)


load('./cluster/fit_all_vse_noise.rdata')
vse.all <- fit.human
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_gamma', 'mu_phi', 'mu_zeta')
coef.all.vse <- summary(vse.all, pars = params)$summary
save(coef.all.vse, file = 'coef_all_vse.rdata')
launch_shinystan(vse.all)

load(file = 'coef_mice_vse.rdata')
load(file = 'coef_human_vse.rdata')
load(file = 'coef_all_vse.rdata')
print(coef.mice.vse, digits = 3)
print(coef.human.vse, digits = 3)
print(coef.all.vse, digits = 3)

## *** PVL
load('./cluster/fit_mice_pvl_noise.rdata')
pvl.mice <- fit.mice
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_zeta')
coef.mice.pvl <- summary(pvl.mice, pars = params)$summary
save(coef.mice.pvl, file = 'coef_mice_pvl.rdata')
launch_shinystan(pvl.mice)


load('./cluster/fit_human_pvl_noise.rdata')
pvl.human <- fit.human
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_zeta')
coef.human.pvl <- summary(pvl.human, pars = params)$summary
save(coef.human.pvl, file = 'coef_human_pvl.rdata')
launch_shinystan(pvl.human)


load('./cluster/fit_all_pvl_noise.rdata')
pvl.all <- fit.human
params <- c('mu_alpha', 'mu_A', 'mu_cons', 'mu_zeta')
coef.all.pvl <- summary(pvl.all, pars = params)$summary
save(coef.all.pvl, file = 'coef_all_pvl.rdata')
launch_shinystan(pvl.all)

load(file = 'coef_mice_vse.rdata')
load(file = 'coef_human_vse.rdata')
load(file = 'coef_all_vse.rdata')
print(coef.mice.pvl, digits = 3)
print(coef.human.pvl, digits = 3)
print(coef.all.pvl, digits = 3)


## * PVL
## ** Model simulation vs data
## *** Mice
load('./cluster/fit_mice_pvl_noise.rdata')
load('./cluster/data_mice/data_mice.rdata')
sim_mice_pvl = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_mice_pvl = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

pvl.mice <- fit.mice
x <- extract(fit.mice)
n <-  nrow(x$y_pred[,1,])
for (i in c(1:40)){
    xi <- table(x$y_pred[,i,])/n
    sim_mice_pvl[i,1] <- i
    sim_mice_pvl[i,2:5] <- xi
}
save(sim_mice_pvl, file = 'sim_mice_pvl.rdata')

for (i in c(1:40)){
    x <- table(data.mice[data.mice$subjID == i,]$choice)
    data_mice_pvl[i,1] <- i
    data_mice_pvl[i,2:5] <- x
}
save(data_mice_pvl, file = 'data_mice_pvl.rdata')

sim <- sim_mice_pvl %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_mice_pvl %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_mice_pvl %>%
    mutate(type = 'sim')
dd <- data_mice_pvl %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot

## *** Human
load('./cluster/fit_human_pvl_noise.rdata')
load('./cluster/data_human/data_human.rdata')
sim_human_pvl = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_human_pvl = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

pvl.human <- fit.human
x <- extract(pvl.human)
n = nrow(x)
for (i in c(1:40)){
    xi <- table(x$y_pred[,i,])/n
    sim_human_pvl[i,1] <- i
    sim_human_pvl[i,2:5] <- xi
}
save(sim_human_pvl, file = 'sim_human_pvl.rdata')

for (i in c(1:40)){
    x <- table(data.human[data.human$subjID == i,]$choice)
    data_human_vse[i,1] <- i
    data_human_vse[i,2:5] <- x
}
save(data_human_vse, file = 'data_human_vse.rdata')

sim <- sim_human_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_human_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_human_vse %>%
    mutate(type = 'sim')
dd <- data_human_vse %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot

## *** all
load('./cluster/fit_all_vse_noise.rdata')
load('./cluster/data_all/data_all.rdata')
sim_all_vse = data.frame(subj = numeric(), d1 = numeric(),
                          d2 = numeric(),  d3 = numeric(), d4 = numeric())
data_all_vse = data.frame(subj = numeric(), d1 = numeric(), d2 = numeric(),
                           d3 = numeric(),  d4 = numeric())

for (i in c(1:57)){
    x <- table(extract(fit.human)$y_pred[,i,])/20000
    sim_all_vse[i,1] <- i
    sim_all_vse[i,2:5] <- x
}
save(sim_all_vse, file = 'sim_all_vse.rdata')
data.all <- data.all %>% filter(study == 'Steingroever2011')
for (i in c(1:57)){
    x <- table(data.all[data.all$subjID == i,]$choice)
    data_all_vse[i,1] <- i
    data_all_vse[i,2:5] <- x
}
save(data_all_vse, file = 'data_all_vse.rdata')

sim <- sim_all_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))
data <- data_all_vse %>%
    summarise(d1 = mean(d1), d2 = mean(d2), d3 = mean(d3), d4 = mean(d4))

sim
data

d <- sim_all_vse %>%
    mutate(type = 'sim')
dd <- data_all_vse %>%
    mutate(type = 'data')
d <- rbind(d,dd)
plot <- ggplot(d, aes(x=subj, y = d1, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot
plot <- ggplot(d, aes(x=subj, y = d2, fill=type)) +
    geom_col(width=.8, position=position_dodge(.8))
plot


## * PVL lambda individual
## ** Mice
fit_mice_indiv_pvl = data.frame(subj = numeric(), alpha = numeric(), A = numeric(),
                       cons = numeric(),  lambda = numeric(),
                       zeta = numeric(),
                       alpha_R = numeric(), A_R = numeric(),
                       cons_R = numeric(),  lambda_R = numeric(), zeta_R = numeric(), divergent = numeric())
for (i in c(1:40)){
    file_name = paste('./cluster/sub_',i,'_mice_pvl_delta_lambda_noise.rdata', sep = '')
    load(file_name)
    params  <- c('alpha', 'A', 'cons', 'lambda', 'zeta')
    result <-  summary(fit)
    coef <- summary(fit, pars = params)$summary
    sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
    fit_mice_indiv_pvl  <- fit_mice_indiv_pvl %>%
        add_row(subj =i, alpha = coef[1,1], A = coef[2,1], cons = coef[3,1],
                            lambda = coef[4,1],  zeta = coef[5,1], alpha_R = coef[1,10],
                            A_R =  coef[2,10], cons_R = coef[3,10], lambda_R = coef[4,10],
                            zeta_R = coef[5,10],
                divergent = sum(sapply(sampler_params, function(x) sum(x[, "divergent__"]))))

}

save(fit_mice_indiv_pvl, file = 'fit_mice_indiv_pvl.rdata')

## ** human
fit_human_indiv_pvl = data.frame(subj = numeric(), alpha = numeric(), A = numeric(),
                       cons = numeric(),  lambda = numeric(),
                       zeta = numeric(),
                       alpha_R = numeric(), A_R = numeric(),
                       cons_R = numeric(),  lambda_R = numeric(), zeta_R = numeric(), divergent = numeric())
for (i in c(1:40)){
    file_name = paste('./cluster/sub_',i,'_human_pvl_lambda_noise.rdata', sep = '')
    load(file_name)
    params  <- c('alpha', 'A', 'cons', 'lambda', 'zeta')
    result <-  summary(fit)
    coef <- summary(fit, pars = params)$summary
    sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
    fit_human_indiv_pvl  <- fit_human_indiv_pvl %>%
        add_row(subj =i, alpha = coef[1,1], A = coef[2,1], cons = coef[3,1],
                            lambda = coef[4,1],  zeta = coef[5,1], alpha_R = coef[1,10],
                            A_R =  coef[2,10], cons_R = coef[3,10], lambda_R = coef[4,10],
                            zeta_R = coef[5,10],
                divergent = sum(sapply(sampler_params, function(x) sum(x[, "divergent__"]))))

}

save(fit_human_indiv_pvl, file = 'fit_human_indiv_pvl.rdata')

## ** all
fit_all_indiv_pvl = data.frame(subj = numeric(), alpha = numeric(), A = numeric(),
                       cons = numeric(),  lambda = numeric(),
                       zeta = numeric(),
                       alpha_R = numeric(), A_R = numeric(),
                       cons_R = numeric(),  lambda_R = numeric(), zeta_R = numeric(), divergent = numeric())
for (i in c(1:40)){
    file_name = paste('./cluster/sub_',i,'_all_pvl_lambda_noise.rdata', sep = '')
    load(file_name)
    params  <- c('alpha', 'A', 'cons', 'lambda', 'zeta')
    result <-  summary(fit)
    coef <- summary(fit, pars = params)$summary
    sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
    fit_all_indiv_pvl  <- fit_all_indiv_pvl %>%
        add_row(subj =i, alpha = coef[1,1], A = coef[2,1], cons = coef[3,1],
                            lambda = coef[4,1],  zeta = coef[5,1], alpha_R = coef[1,10],
                            A_R =  coef[2,10], cons_R = coef[3,10], lambda_R = coef[4,10],
                            zeta_R = coef[5,10],
                divergent = sum(sapply(sampler_params, function(x) sum(x[, "divergent__"]))))

}

save(fit_all_indiv_pvl, file = 'fit_all_indiv_pvl.rdata')


mice <- fit_mice_indiv_pvl %>%
    mutate(spec = 'mice') %>%
    filter(A>.01, alpha_R<1.2, A_R<1.2, cons_R<1.2, lambda_R < 1.2, zeta_R<1.2, divergent == 0)
human <- fit_human_indiv_pvl %>%
    mutate(spec = 'human') %>%
    filter(A>.01, alpha_R<1.2, A_R<1.2, cons_R<1.2, lambda_R < 1.2, zeta_R<1.2, divergent == 0)
all <- fit_all_indiv_pvl %>%
    mutate(spec = 'human_all') %>%
    filter(A> .01, alpha_R<1.2, A_R<1.2, cons_R<1.2, lambda_R < 1.2, zeta_R<1.2, divergent == 0)
mice
human
all

mean(mice$zeta)
mean(human$zeta)
mean(all$zeta)

d<- rbind(mice, human)
d <- rbind(d, all)

p <- ggplot(data = d %>% filter(divergent == 0), aes(x = subj, y = zeta, color = spec)) +
    geom_point()
p



## * data description
load('./cluster/data_all/data_all.rdata')
load('./cluster/data_mice/data_mice.rdata')
load('./cluster/data_human/data_human.rdata')
data.mice <- data.mice %>%
    group_by(subjID) %>%
    mutate(trial = row_number()) %>%
    ungroup()
plot.human <- ggplot(data = data.human, aes(x = trial, y = choice)) +
    geom_line() +
    facet_wrap( ~ subjID , scales = 'free')
plot.steingroever <- ggplot(data = data.all %>% filter(study == 'Steingroever2011'), aes(x = trial, y = choice)) +
    geom_line() +
    facet_wrap( ~ subjID , scales = 'free')
plot.mice <- ggplot(data = data.mice, aes(x = trial, y = choice)) +
    geom_line() +
    facet_wrap( ~ subjID , scales = 'free')
plot.human
plot.steingroever
plot.mice
unique(data.all$index)
d <- data.all %>% filter(index == 'Wetzels')
head(d)
write.csv(data.all, file = 'data_all.csv')

N <- nrow(choice) ## number of subjects
T <- ncol(choice) ## max number of trials by subject
N <- nrow(choice) ## number of subjects
T <- ncol(choice) ## max number of trials by subject





result_fit_EU = data.frame(subj = numeric(), alpha = numeric(), A = numeric(),
                       cons = numeric(),  zeta = numeric(),
                       alpha_R = numeric(),  A_R = numeric(),
                       cons_R = numeric(),  zeta_R = numeric(), divergent = numeric())
for (i in c(1:40)){
    file_name = paste('sub_',i,'EU.rdata', sep = '')
    load(file_name)
    params <- c('alpha', 'A', 'cons', 'zeta') ##params <- c('alpha', 'A', 'cons', 'lambda', 'zeta')
    result <-  summary(fit.human)
    coef <- summary(fit.human, pars = params)$summary
    sampler_params <- get_sampler_params(fit.human, inc_warmup = FALSE)
    result_fit_EU  <- result_fit_EU %>%
        ## add_row(subj =i, alpha = coef[1,1], A = coef[2,1], cons = coef[3,1],
        ##                     lambda = coef[4,1],  zeta = coef[5,1], alpha_R = coef[1,10],
        ##                     A_R =  coef[2,10], cons_R = coef[3,10], lambda_R = coef[4,10],
        ##                     zeta_R = coef[5,10])
        add_row(subj = i, alpha = coef[1,1], A = coef[2,1], cons = coef[3,1],
                              zeta = coef[4,1], alpha_R = coef[1,10],
                            A_R =  coef[2,10], cons_R = coef[3,10],
                zeta_R = coef[4,10],
                divergent = sum(sapply(sampler_params, function(x) sum(x[, "divergent__"]))))

}
result_fit_human <- result_fit_EU
save(result_fit_human, file = 'result_fit_human_EU.rdata')

load('obs.rdata')
obs <- obs.all %>% filter(study == 'mice', subjID == 1)
load('./cluster/output/vse_noise_mice_1.rdata')
launch_shinystan(fit)
