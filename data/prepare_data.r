rm(list=ls(all=TRUE))  ## efface les données
source('~/thib/projects/tools/R_lib.r')
setwd('~/thib/projects/RLNOISE/data/')

data <- read.csv(file="./RLNOISE_exp2/RLNOISE_exp2_S01_data.csv", header=FALSE, sep=",")
names(data) <- c('fbtype','choice','reward1','reward2','trl')
data$subjID = 1
for (i in c(c(2:13),c(15:30))){
    if (i<10){subj = paste("0",i,sep ='')}
    else{subj = as.character(i)}
    filename =paste("./RLNOISE_exp2/RLNOISE_exp2_S",subj,"_data.csv",sep="")
    d <- read.csv(file=filename, header=FALSE, sep=",")
    names(d) <- c('fbtype','choice','reward1','reward2','trl')
    d$subjID = i
    data <- rbind(data,d)
}
## replace subject 14 (missing) by subject 29
data <- data %>%
    mutate(subjID = ifelse(subjID == 30, 14, subjID))
data.full <- data 
data <- data %>%
    mutate(gain = ifelse(choice == 1, reward1, reward2)) %>%
    filter(fbtype == 1) %>% ##partial condition
    select(subjID, choice, gain, loss)
save(data, file = "data_RLNOISE_2.rdata")
save(data.full, file = "data_RLNOISE_2_full.rdata")
