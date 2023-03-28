#go nogo processing data

setwd("E:/Jana/Adore/data_analysis/data")
.libPaths('C:/Program Files/R/R-4.2.3/library')


#install.packages('rJava')
#install.packages('xlsxjars')
#install.packages('xlsx')
#install.packages('plyr')
#install.packages('plotrix')
#install.packages('yarrr')
#install.packages('tidyr')

library(rJava)
library(xlsxjars)
library(xlsx)
library(plyr)
library(plotrix)
library(yarrr)
library(tidyr)

#create go nogo dataframe

go_nogo_data <- data.frame(id = NA,
                            ses = NA,
                            block_1A_acc = NA,
                            block_1A_go_acc = NA,
                            block_1A_nogo_acc = NA,
                            block_1B_acc = NA,
                            block_1B_go_acc = NA,
                            block_1B_nogo_acc = NA,
                            block_2A_acc = NA,
                            block_2A_go_acc = NA,
                            block_2A_nogo_acc = NA,
                            block_2B_acc = NA,
                            block_2B_go_acc = NA,
                            block_2B_nogo_acc = NA,
                            go_acc = NA,
                            nogo_acc = NA,
                            total_acc = NA,
                            total_go_trials = NA,
                            total_nogo_trials = NA,
                            correct_go_total = NA,
                            correct_nogo_total = NA,
                            wrong_go_total = NA,
                            wrong_nogo_total = NA,
                            go_trials_1A = NA,
                            nogo_trials_1A = NA,
                            correct_go_1A = NA,
                            correct_nogo_1A = NA,
                            wrong_go_1A = NA,
                            wrong_nogo_1A = NA,
                            go_trials_1B = NA,
                            nogo_trials_1B = NA,
                            correct_go_1B = NA,
                            correct_nogo_1B = NA,
                            wrong_go_1B = NA,
                            wrong_nogo_1B = NA,
                            go_trials_2A = NA,
                            nogo_trials_2A = NA,
                            correct_go_2A = NA,
                            correct_nogo_2A = NA,
                            wrong_go_2A = NA,
                            wrong_nogo_2A = NA,
                            go_trials_2B = NA,
                            nogo_trials_2B = NA,
                            correct_go_2B = NA,
                            correct_nogo_2B = NA,
                            wrong_go_2B = NA,
                            wrong_nogo_2B = NA,
                            block_1A_RT = NA,
                            block_1B_RT = NA,
                            block_2A_RT = NA,
                            block_2B_RT = NA,
                            total_RT = NA
                            )


data_path <- 'E:/Jana/Adore/data_analysis/data'
list_subj <- dir(data_path, pattern = 'sub*')

rowcount <- 1

#list_ses <- dir(paste(data_path,list_subj[1],sep='/'))

for (i in 1:length(list_subj)){
  
  list_ses <- dir(paste(data_path,list_subj[i],sep='/'))
  
  for (j in 1:length(list_ses)){
    
    file_path <- paste(data_path, 
                       list_subj[i],
                       list_ses[j],
                       sep = '/')
    
    go_nogo_file <- dir(file_path, 
                        pattern = 'go_nogo_new*')
    
    
    #data from csv file in temporary dataframe
    data_temp <- read.csv(paste(file_path, go_nogo_file, sep = '/'), 1)
    
    #data_temp <- read.csv('E:/Jana/Adore/data_analysis/data/subADBE04/ses-1/subADBE04 ses-1 go_nogo_new.csv')
    
    #id
    go_nogo_data[rowcount, 'id'] <- list_subj[i]
    
    #session
    go_nogo_data[rowcount, 'ses'] <- list_ses[j]
    
    #numbers
    #total numbers
    go_nogo_data[rowcount,'total_go_trials'] <- nrow(data_temp[data_temp$solution == 'space',])
    go_nogo_data[rowcount, 'total_nogo_trials'] <- nrow(data_temp[data_temp$solution == 'none',])
    go_nogo_data[rowcount, 'correct_go_total'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 1,])
    go_nogo_data[rowcount, 'correct_nogo_total'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 1,])
    go_nogo_data[rowcount, 'wrong_go_total'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 0,])
    go_nogo_data[rowcount, 'wrong_nogo_total'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 0,])
    
    #Block 1A numbers
    go_nogo_data[rowcount, 'go_trials_1A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$blocknr == '1A',])
    go_nogo_data[rowcount, 'nogo_trials_1A'] <- nrow(data_temp[data_temp$solution == 'none'& data_temp$blocknr == '1A',])
    go_nogo_data[rowcount, 'correct_go_1A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 1 & data_temp$blocknr == '1A',])
    go_nogo_data[rowcount, 'correct_nogo_1A'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 1 & data_temp$blocknr == '1A',])
    go_nogo_data[rowcount, 'wrong_go_1A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 0 & data_temp$blocknr == '1A',])
    go_nogo_data[rowcount, 'wrong_nogo_1A'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 0 & data_temp$blocknr == '1A',])
    
    #Block 1B numbers
    go_nogo_data[rowcount, 'go_trials_1B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$blocknr == '1B',])
    go_nogo_data[rowcount, 'nogo_trials_1B'] <- nrow(data_temp[data_temp$solution == 'none'& data_temp$blocknr == '1B',])
    go_nogo_data[rowcount, 'correct_go_1B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 1 & data_temp$blocknr == '1B',])
    go_nogo_data[rowcount, 'correct_nogo_1B'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 1 & data_temp$blocknr == '1B',])
    go_nogo_data[rowcount, 'wrong_go_1B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 0 & data_temp$blocknr == '1B',])
    go_nogo_data[rowcount, 'wrong_nogo_1B'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 0 & data_temp$blocknr == '1B',])
    
    #Block 2A numbers
    go_nogo_data[rowcount, 'go_trials_2A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$blocknr == '2A',])
    go_nogo_data[rowcount, 'nogo_trials_2A'] <- nrow(data_temp[data_temp$solution == 'none'& data_temp$blocknr == '2A',])
    go_nogo_data[rowcount, 'correct_go_2A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 1 & data_temp$blocknr == '2A',])
    go_nogo_data[rowcount, 'correct_nogo_2A'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 1 & data_temp$blocknr == '2A',])
    go_nogo_data[rowcount, 'wrong_go_2A'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 0 & data_temp$blocknr == '2A',])
    go_nogo_data[rowcount, 'wrong_nogo_2A'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 0 & data_temp$blocknr == '2A',])
    
    #Block 2B numbers
    go_nogo_data[rowcount, 'go_trials_2B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$blocknr == '2B',])
    go_nogo_data[rowcount, 'nogo_trials_2B'] <- nrow(data_temp[data_temp$solution == 'none'& data_temp$blocknr == '2B',])
    go_nogo_data[rowcount, 'correct_go_2B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 1 & data_temp$blocknr == '2B',])
    go_nogo_data[rowcount, 'correct_nogo_2B'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 1 & data_temp$blocknr == '2B',])
    go_nogo_data[rowcount, 'wrong_go_2B'] <- nrow(data_temp[data_temp$solution == 'space' & data_temp$correct == 0 & data_temp$blocknr == '2B',])
    go_nogo_data[rowcount, 'wrong_nogo_2B'] <- nrow(data_temp[data_temp$solution == 'none' & data_temp$correct == 0 & data_temp$blocknr == '2B',])
    
    
    #accuracies
    #total accuracies
    go_nogo_data[rowcount, 'total_acc'] <- sum(data_temp$correct) / 288
    go_nogo_data[rowcount, 'go_acc'] <- go_nogo_data[rowcount, 'correct_go_total'] / go_nogo_data[rowcount, 'total_go_trials']
    go_nogo_data[rowcount, 'nogo_acc'] <- go_nogo_data[rowcount, 'correct_nogo_total'] / go_nogo_data[rowcount, 'total_nogo_trials']
    
    #block 1A accuracies
    go_nogo_data[rowcount, 'block_1A_acc'] <- (go_nogo_data[rowcount, 'correct_go_1A'] + go_nogo_data[rowcount, 'correct_nogo_1A']) / 72
    go_nogo_data[rowcount, 'block_1A_go_acc'] <- go_nogo_data[rowcount, 'correct_go_1A'] / go_nogo_data[rowcount, 'go_trials_1A']
    go_nogo_data[rowcount, 'block_1A_nogo_acc'] <- go_nogo_data[rowcount, 'correct_nogo_1A'] / go_nogo_data[rowcount, 'nogo_trials_1A']
    
    #block 1B accuracies
    go_nogo_data[rowcount, 'block_1B_acc'] <- (go_nogo_data[rowcount, 'correct_go_1B'] + go_nogo_data[rowcount, 'correct_nogo_1B']) / 72
    go_nogo_data[rowcount, 'block_1B_go_acc'] <- go_nogo_data[rowcount, 'correct_go_1B'] / go_nogo_data[rowcount, 'go_trials_1B']
    go_nogo_data[rowcount, 'block_1B_nogo_acc'] <- go_nogo_data[rowcount, 'correct_nogo_1B'] / go_nogo_data[rowcount, 'nogo_trials_1B']
    
    #block 2A accuracies
    go_nogo_data[rowcount, 'block_2A_acc'] <- (go_nogo_data[rowcount, 'correct_go_2A'] + go_nogo_data[rowcount, 'correct_nogo_2A']) / 72
    go_nogo_data[rowcount, 'block_2A_go_acc'] <- go_nogo_data[rowcount, 'correct_go_2A'] / go_nogo_data[rowcount, 'go_trials_2A']
    go_nogo_data[rowcount, 'block_2A_nogo_acc'] <- go_nogo_data[rowcount, 'correct_nogo_2A'] / go_nogo_data[rowcount, 'nogo_trials_2A']
    
    #block 2B accuracies
    go_nogo_data[rowcount, 'block_2B_acc'] <- (go_nogo_data[rowcount, 'correct_go_2B'] + go_nogo_data[rowcount, 'correct_nogo_2B']) / 72
    go_nogo_data[rowcount, 'block_2B_go_acc'] <- go_nogo_data[rowcount, 'correct_go_2B'] / go_nogo_data[rowcount, 'go_trials_2B']
    go_nogo_data[rowcount, 'block_2B_nogo_acc'] <- go_nogo_data[rowcount, 'correct_nogo_2B'] / go_nogo_data[rowcount, 'nogo_trials_2B']
    
    
    
    rowcount <- rowcount + 1
    }
  }
    
    
 condition <- read.csv('E:/Jana/Adore/data_analysis/ADoRe_allocation_list_UNBLIND - Copy.csv')
 go_nogo_data <- go_nogo_data[order(go_nogo_data$id, go_nogo_data$ses),]
 condition <- condition[order(condition$id, condition$ses),]
 go_nogo_data <- merge(condition, go_nogo_data, by = c('id','ses'), all = TRUE)
 
 write.csv(go_nogo_data, "E:/Jana/Adore/data_analysis/go_nogo_data.csv", row.names=FALSE)