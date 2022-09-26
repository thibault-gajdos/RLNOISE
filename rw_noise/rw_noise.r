rm(list=ls(all=TRUE))  ## efface les données
source('~/R_lib.r')
source('fit_util.r')
##source('~/thib/projects/tools/R_lib.r')
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
load('../data/data_RLNOISE_2.rdata')

fit_indiv(data = data, subject = as.numeric(args[1]), model = 'rw_noise')
