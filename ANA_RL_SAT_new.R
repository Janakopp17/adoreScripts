## reinforcing rank 1 (80%)
## 2 (70%) 
## 3 (60%)
## 4 (40%)
## 5 (30%) 
## 6 (20%)
## UPDATE: 20.02.2023 The images linked to the reinforcing ranks are DIFFERENT EVERY TIME!
## UPDATE: 15.03.2023 Adding the retrieval of the stimuli images each reinforcement strengths are linked to 
## Test data have inconsistent image-reinforcement link from train data. Discard the Stim_number in test data and use the image to identify the reinforcer

## stimuli number (reinforcement ranking) in testing phase is not consistent

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

data_path <- 'E:/Jana/Adore/data_analysis/data'
list_subj <- dir(data_path, pattern = 'sub*')
# list_ses <- c('ses-1','ses-2','ses-3')


# synthesized data set Reward Learning -------------------------------------


syndata_RL <- data.frame(id = NA, 
                         ses = NA,
                         train_acc_overall = NA,
                         train_acc_AB = NA,
                         train_acc_CD = NA,
                         train_acc_EF = NA,
                         test_acc_Aset = NA,
                         test_acc_Bset = NA,
                         test_acc_overall = NA,
                         test_RT_Aset = NA,
                         test_RT_Bset = NA,
                         test_RT_overall = NA
                         
)


test_data_all <- data.frame("id"=NA,
                            "ses"=NA,
                            "train_acc_overall"=NA,
                            "train_acc_AB"=NA,
                            "train_acc_CD"=NA,
                            "train_acc_EF"=NA,
                            "test_acc_Aset"=NA,
                            "test_acc_Bset"=NA,
                            "test_acc_overall"=NA,
                            "test_RT_Aset"=NA,
                            "test_RT_Bset"=NA,
                            "test_RT_overall"=NA)


image_reinforce <- data.frame('id'='',
                              'ses' = '',
                              'Reinf1_80' = '',
                              'Reinf2_70' = '',
                              'Reinf3_60' = '',
                              'Reinf4_40' = '',
                              'Reinf5_30' = '',
                              'Reinf6_20' = '')


SAT <- data.frame('id'='',
                  'ses' = '',
                  'SATpre_80' = '',
                  'SATpre_70' = '',
                  'SATpre_60' = '',
                  'SATpre_40' = '',
                  'SATpre_30' = '',
                  'SATpre_20' = '',
                  'SATpost_80' = '',
                  'SATpost_70' = '',
                  'SATpost_60' = '',
                  'SATpost_40' = '',
                  'SATpost_30' = '',
                  'SATpost_20' = ''
                )




# Calculation Reward learning ---------------------------------------------

row_count <- 1

for (i in 1:length(list_subj)){
  
  list_ses <- dir(paste(data_path,list_subj[i],sep='/'))
  
  
  for (j in 1:length(list_ses)){
    
    ## =========== Set up ============ ##
    
    ## Feed in ID & Ses number
    
    syndata_RL[row_count,'id'] <- list_subj[i]
    syndata_RL[row_count,'ses'] <- list_ses[j]
    
    image_reinforce[row_count,'id'] <- list_subj[i]
    image_reinforce[row_count,'ses'] <- list_ses[j]
    
    SAT[row_count,'id'] <- list_subj[i]
    SAT[row_count,'ses'] <- list_ses[j]
    
    
    ## Set up file path
    
    file_path <- paste(data_path, 
                       list_subj[i],
                       list_ses[j],
                       sep = '/')
    
    PST_train_file <- dir(file_path, 
                          pattern = 'train.csv')
    
    PST_test_file <- dir(file_path, 
                         pattern = 'test.csv')
    
    SATpre_file <- dir(file_path, 
                         pattern = 'SATpre*') 
    #Make sure only include .csv and the final file in the folder
    
    SATpost_file <- dir(file_path, 
                       pattern = 'SATpost*') 
    #Make sure only include .csv and the final file in the folder
    
    
    ## ========= train data ============ ##

    train <- read.csv(paste(file_path, PST_train_file, sep = '/'), 1)
    
    train$stimuli <- NA
    train$stimuli[which(train$left_stim_number == 1 | train$left_stim_number == 6)] <- 
      'AB'
    train$stimuli[which(train$left_stim_number == 2 | train$left_stim_number == 5)] <- 
      'CD'
    train$stimuli[which(train$left_stim_number == 3 | train$left_stim_number == 4)] <- 
      'EF'
    
    ## Retrieve linked images to the reinforcers
    
    Reinforcer1 <- unique(train$left_stim[which(train$left_stim_number == 1)])
    Reinforcer2 <- unique(train$left_stim[which(train$left_stim_number == 2)])
    Reinforcer3 <- unique(train$left_stim[which(train$left_stim_number == 3)])
    Reinforcer4 <- unique(train$left_stim[which(train$left_stim_number == 4)])
    Reinforcer5 <- unique(train$left_stim[which(train$left_stim_number == 5)])
    Reinforcer6 <- unique(train$left_stim[which(train$left_stim_number == 6)])
    
    image_reinforce$Reinf1_80[row_count] <- Reinforcer1
    image_reinforce$Reinf2_70[row_count] <- Reinforcer2
    image_reinforce$Reinf3_60[row_count] <- Reinforcer3
    image_reinforce$Reinf4_40[row_count] <- Reinforcer4
    image_reinforce$Reinf5_30[row_count] <- Reinforcer5
    image_reinforce$Reinf6_20[row_count] <- Reinforcer6
    
    
    ## Calculate accuracy by pair
    
    syndata_RL$train_acc_overall[row_count] <- 
      (count(train$accuracy[which(train$accuracy == 1)])[1,2] / 360)*100
    
    syndata_RL$train_acc_AB[row_count] <- 
      (count(train$accuracy[which(train$stimuli == 'AB' &
                                    train$accuracy == 1)])[1,2] / 120)*100
    
    syndata_RL$train_acc_CD[row_count] <- 
      (count(train$accuracy[which(train$stimuli == 'CD' &
                                    train$accuracy == 1)])[1,2] / 120)*100
    
    syndata_RL$train_acc_EF[row_count] <- 
      (count(train$accuracy[which(train$stimuli == 'EF' &
                                    train$accuracy == 1)])[1,2] / 120)*100
    
    
    ## ============= test data ============= #
    

    test <- read.csv(paste(file_path, PST_test_file, sep = '/'), 1)
    
    
    ## Link the CORRECT reinforcement strengths back to training data (Discard Stim_number)
    
    image_list <- image_reinforce[row_count,]
    
    test$CORR_left_stim_number <- ''
    test$CORR_right_stim_number <- ''
    
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf1_80)] <- 1
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf2_70)] <- 2
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf3_60)] <- 3
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf4_40)] <- 4
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf5_30)] <- 5
    test$CORR_left_stim_number[which(test$left_stim_name == image_list$Reinf6_20)] <- 6
    
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf1_80)] <- 1
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf2_70)] <- 2
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf3_60)] <- 3
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf4_40)] <- 4
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf5_30)] <- 5
    test$CORR_right_stim_number[which(test$right_stim_name == image_list$Reinf6_20)] <- 6
    
    
    ## Label AB sets
    
    test$stimuli <- 'others'
    
    test$stimuli[which(test$CORR_left_stim_number == 1 |
                         test$CORR_right_stim_number == 1)] <- 'Aset' #all pairs with A

    test$stimuli[which(test$CORR_left_stim_number == 6 |
                         test$CORR_right_stim_number == 6)] <- 'Bset' #all pairs with B

    test$stimuli[which(test$CORR_left_stim_number == 1 &
                         test$CORR_right_stim_number == 6)] <- 'others' #exclude AB pairs
    
    test$stimuli[which(test$CORR_left_stim_number == 6 &
                         test$CORR_right_stim_number == 1)] <- 'others' #exclude AB pairs
    
    
    ## Accuracy of AB sets
    
    Aset_count <- count(test$trial_accuracy[which(test$stimuli == 'Aset')])
    
    num_Aset <- sum(Aset_count$freq)
    num_Aset_corr <- Aset_count$freq[which(Aset_count$x == 1)]
    
    if (is.na(num_Aset_corr) == FALSE){
      
      syndata_RL$test_acc_Aset[row_count] <- 
        (num_Aset_corr / num_Aset) * 100
    } else {
      
      syndata_RL$test_acc_Aset[row_count] <- 0
      
    }
    
    
    Bset_count <- count(test$trial_accuracy[which(test$stimuli == 'Bset')])
    
    num_Bset <- sum(Bset_count$freq)
    num_Bset_corr <- Bset_count$freq[which(Bset_count$x == 1)]
    
    if (is.na(num_Bset_corr) == FALSE){
      
      syndata_RL$test_acc_Bset[row_count] <- 
        (num_Bset_corr / num_Bset) * 100
    } else {
      
      syndata_RL$test_acc_Bset[row_count] <- 0
      
    }
    
    
    ## Overall accuracy 
    
    syndata_RL$test_acc_overall[row_count] <- 
      (length(test$trial_accuracy[which(test$trial_accuracy == 1)]) / 180)*100
    
    print(c(num_Aset, num_Aset_corr, num_Bset, num_Bset_corr))
    
    
    
    ## =========== SAT data ========= ##
    
    SATpre <- read.csv(paste(file_path, SATpre_file, sep = '/'), 1)
    SATpost <- read.csv(paste(file_path, SATpost_file, sep = '/'), 1)
    
    colnames(SATpre)[1:6] <- c('image1', 'image2', 'image3', 'image4', 'image5', 'image6')
    colnames(SATpost)[1:6] <- c('image1', 'image2', 'image3', 'image4', 'image5', 'image6')
    
    
    # image80 <- paste('image', substring(image_list$Reinf1_80,12,12), sep = '')
    image80 <- as.numeric(substring(image_list$Reinf1_80,12,12))
    image70 <- as.numeric(substring(image_list$Reinf2_70,12,12))
    image60 <- as.numeric(substring(image_list$Reinf3_60,12,12))
    image40 <- as.numeric(substring(image_list$Reinf4_40,12,12))
    image30 <- as.numeric(substring(image_list$Reinf5_30,12,12))
    image20 <- as.numeric(substring(image_list$Reinf6_20,12,12))
    
    
    SAT$SATpre_80[row_count] <- SATpre[1,image80]
    SAT$SATpre_70[row_count] <- SATpre[1,image70]
    SAT$SATpre_60[row_count] <- SATpre[1,image60]
    SAT$SATpre_40[row_count] <- SATpre[1,image40]
    SAT$SATpre_30[row_count] <- SATpre[1,image30]
    SAT$SATpre_20[row_count] <- SATpre[1,image20]
    
    SAT$SATpost_80[row_count] <- SATpost[1,image80]
    SAT$SATpost_70[row_count] <- SATpost[1,image70]
    SAT$SATpost_60[row_count] <- SATpost[1,image60]
    SAT$SATpost_40[row_count] <- SATpost[1,image40]
    SAT$SATpost_30[row_count] <- SATpost[1,image30]
    SAT$SATpost_20[row_count] <- SATpost[1,image20]
    
    
    row_count <- row_count +1
    
  }
  
}



# Coding ------------------------------------------------------------------

condition <- read.csv('E:/Jana/Adore/data_analysis/ADoRe_allocation_list_UNBLIND - Copy.csv')

syndata_RL <- syndata_RL[order(syndata_RL$id, syndata_RL$ses),]
condition <- condition[order(condition$id, condition$ses),]
syndata_RL <- merge(condition, syndata_RL, by = c('id','ses'), all = TRUE)
head(syndata_RL)
write.csv(syndata_RL, "E:/Jana/Adore/data_analysis/final_dataframe.csv", row.names=FALSE)
