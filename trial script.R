
#trial script

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

for (i in 1:length(list_subj)){
  
  list_ses <- dir(paste(data_path,list_subj[i],sep='/'))

  for (j in 1:length(list_ses)){
    
    file_path <- paste(data_path, 
                       list_subj[i],
                       list_ses[j],
                       sep = '/')
    
    go_nogo_file <- dir(file_path, 
                       pattern = 'go no go*')
    
    
      #data from csv file in temporary dataframe
      data_temp <- read.csv(paste(file_path, go_nogo_file, sep = '/'), 1)

      #dataframe for imported columns and rows

      data_semifinal <- data.frame(blocknr = NA,
                                  trialnr = NA,
                                  solution = NA,
                                  correct = NA,
                                  RT = NA
                                  )

      row_count <- 1
      
      z <- 1
      
      for (x in 20:91){
  
          data_semifinal[row_count,'correct'] <- data_temp[x , 'resp_trial_Test1A.corr']
          data_semifinal[row_count,'RT'] <- data_temp[x , 'resp_trial_Test1A.rt']
          data_semifinal[row_count,'solution'] <- data_temp[x , 'corrAns_test1A']
          data_semifinal[row_count,'blocknr'] <- '1A'
          data_semifinal[row_count,'trialnr'] <- z
  
          z <- z + 1  
          row_count <- row_count + 1}

      z <- 1
      for (y in 93:164){
          data_semifinal[row_count,'correct'] <- data_temp[y , 'resp_trial_Test1B.corr']
          data_semifinal[row_count,'RT'] <- data_temp[y , 'resp_trial_Test1B.rt']
          data_semifinal[row_count,'solution'] <- data_temp[y , 'corrAns_test1B']
          data_semifinal[row_count,'blocknr'] <- '1B'
          data_semifinal[row_count,'trialnr'] <- z
  
          z <- z + 1  
          row_count <- row_count + 1}

      z <- 1

      for (w in 166:237){
          data_semifinal[row_count,'correct'] <- data_temp[w , 'resp_trial_Test2A.corr']
          data_semifinal[row_count,'RT'] <- data_temp[w , 'resp_trial_Test2A.rt']
          data_semifinal[row_count,'solution'] <- data_temp[w , 'corrAns_test2A']
          data_semifinal[row_count,'blocknr'] <- '2A'
          data_semifinal[row_count,'trialnr'] <- z
  
          z <- z + 1
          row_count <- row_count + 1}

      z <- 1

      for (v in 239:310){
          data_semifinal[row_count,'correct'] <- data_temp[v , 'resp_trial_Test2B.corr']
          data_semifinal[row_count,'RT'] <- data_temp[v , 'resp_trial_Test2B.rt']
          data_semifinal[row_count,'solution'] <- data_temp[v , 'corrAns_test2B']
          data_semifinal[row_count,'blocknr'] <- '2B'
          data_semifinal[row_count,'trialnr'] <- z
  
          z <- z + 1
          row_count <- row_count + 1}
          subjectname <- paste(list_subj[i], list_ses[j], "go_nogo_new.csv")
          
          write.csv(data_semifinal, paste(file_path, subjectname, sep = '/'), row.names=FALSE)
  }
}

