rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/RLNOISE/data/')

data <- read.csv(file="./RLNOISE_exp1/RLNOISE_exp1_S01_data.csv", header=FALSE, sep=",")
names(data) <- c('fbtype','choice','reward1','reward2','trl','cue')
data$subjID = 1
for (i in 2:30){
    if (i<10){subj = paste("0",i,sep ='')}
    else{subj = as.character(i)}
    filename =paste("./RLNOISE_exp1/RLNOISE_exp1_S",subj,"_data.csv",sep="")
    d <- read.csv(file=filename, header=FALSE, sep=",")
    names(d) <- c('fbtype','choice','reward1','reward2','trl','cue')
    d$subjID = i
    data <- rbind(data,d)
}
data <- data %>%
    mutate(gain = ifelse(choice == 1, reward1, reward2)) %>%
    mutate(loss = 0) %>%
    filter(fbtype == 1) %>% ##partial condition
    select(subjID, choice, gain, loss)
save(data, file = "data_RLNOISE.rdata")
