## Create the following data
## summary.all (summary_indiv.rdata): parametres des fits par sujet et modèle
## pred.all (pred_all.rdata): predictions des fits par essai, sujet et modèle (choix le plus fréquent)
## obs.all (obs.rdata): observations des choix par essai et sujet
## accuracy (accuracy.rdata) (compute prediction accuracy (1 = correct, 0 = incorrect)
## by trial for each subject and model)
rm(list=ls(all=TRUE))  ## efface les données

source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/RLNOISE/analysis/')

load('../data/data_RLNOISE.rdata')
models <- c('rw', 'rw_noise')
subjects = c(1:29)
## predictions and fitted parameters
params.rw = c('A',  'cons')
params.rw.noise = c('A',  'cons', 'zeta')
mat = matrix(ncol = 0, nrow = 0)
summary.all = data.frame(mat)
pred.all = data.frame(mat)
for (m in models){
    if (m == 'rw'){
            params = params.rw
        }else {
            params = params.rw.noise
        }
    for (i in subjects){        
        data.name <- paste('../',m,'/results/',m,'_',i,'.rds', sep='')
        fit <- readRDS(data.name)
        print(data.name)


## prediction
        pred <- extract(fit)$y_pred %>%
                           as.data.frame() %>%
                           summarise_all(mlv, method = 'mfv') %>%
                           mutate(model = m, subjID = i)
        pred.all <-  bind_rows(pred.all, pred[1,])
        
        ## log_lik and parameters
        log_lik = loo::extract_log_lik(fit, parameter_name = "log_lik", merge_chains = TRUE)
        l <- loo::elpd(log_lik)$estimates[[1]]
        sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
        div = sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
        su <- as.data.frame(summary(fit, pars = params)$summary) %>%
            rownames_to_column() %>%
            rename(param = rowname) %>%
            mutate(l = l,  model = m, subjID = i, divergent = div)
        summary.all <-  bind_rows(summary.all, su)
    }
}



save(summary.all, file = 'summary_indiv.rdata')
save(pred.all, file = 'pred_all.rdata')


## observed choices
mat = matrix(ncol = 0, nrow = 0)
obs.all = data.frame(mat)

load('../data/data_RLNOISE.rdata')
for (i in subjects){
    obs <- data %>%
        filter(subjID == i)
    obs_vector  <- t(as.vector(obs$choice))
    obs <- as.data.frame(obs_vector) %>%
        mutate(subjID = i)
    if (nrow(obs.all) == 0){
        obs.all <- obs
    }else{
        obs.all <-  bind_rows(obs.all, obs)
    }
}
save(obs.all, file = 'obs.rdata')

## * Accuracy
pred.all <- pred.all %>%
    relocate(c(model,  subjID), .before = 1)


## compute prediction accuracy (1 = correct, 0 = incorrect)
## by trial for each subject and model
accuracy <- pred.all %>% filter(V1 ==  -1) ## create empty dataframe
obs <- obs.all %>% select(starts_with('V'))
for (m in unique(pred.all$model)){
    pred <- pred.all %>%
        filter(model == m) %>%
        select(starts_with('V'))
    diff <- as.data.frame(pred == obs)
    diff <- add_column(diff, model = m,  subjID = obs.all$subjID, .before = 1)
    accuracy <- bind_rows(accuracy, diff)
}
save(accuracy, file = 'accuracy.rdata' )
