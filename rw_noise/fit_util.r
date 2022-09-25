fit_indiv <- function (data, subject, model, delta = .99, iter = 10000, warmup = 5000){
    rstan_options(auto_write = FALSE)
    d <- data %>%
        filter(subjID == subject)
    data_list <- list(
        T        = nrow(d),
        choice   = d$choice,
        gain  = d$gain,
        loss = abs(d$loss)
    )
    modelFile <- paste('./stan/',model,'.stan', sep = '')
    fit <- stan(modelFile,
                data = data_list,
                iter = iter,
                warmup = warmup,
                chains = 4,
                cores = 4,
                init =  'random',
                seed = 12345,
                control = list(adapt_delta = delta,  max_treedepth = 12)
                )
    file_name = paste('./results/',model,'_',subject,'.rds', sep = '')
    save(fit, file = file_name)
    saveRDS(fit, file = file_name)
}

