#go nogo data analysis

setwd("E:/Jana/Adore/data_analysis/data")
.libPaths('C:/Program Files/R/R-4.2.3/library')


#install.packages('rJava')
#install.packages('xlsxjars')
#install.packages('xlsx')
#install.packages('plyr')
#install.packages('plotrix')
#install.packages('yarrr')
#install.packages('tidyr')
#install.packages('gridExtra')
#install.packages('car')
#install.packages('rstatix')
#install.packages("pgirmess")



library(rJava)
library(xlsxjars)
library(xlsx)
library(plyr)
library(plotrix)
library(yarrr)
library(tidyr)
library(gridExtra)
library(car)
library(rstatix)
library(pgirmess)

#read in the data
#gonogo data
go_nogo_data <- read.csv('E:/Jana/Adore/data_analysis/go_nogo_data.csv')
go_nogo_data$cond <- factor(go_nogo_data$cond)
levels(go_nogo_data$cond)
#reward learning data
reward_learning_data <- read.csv('E:/Jana/Adore/data_analysis/final_dataframe.csv')
reward_learning_data$cond <- factor(reward_learning_data$cond)
levels(reward_learning_data$cond)

#test assumptions
#nogo accuracy
x <- c(go_nogo_data$nogo_acc)
#outliers
boxplot(x, main = "Boxplot")
#normality
hist(x)
qqnorm(x, main = "Normal Q-Q plot")
shapiro.test(x)
#homogeneity of variances
leveneTest(go_nogo_data$nogo_acc, go_nogo_data$cond)

#negative reward learning Bset accuracy
x <- c(reward_learning_data$test_acc_Bset)
#outliers
boxplot(x, main = "Boxplot")
#normality test
hist(x)
qqnorm(x, main = "Normal Q-Q plot")
shapiro.test(reward_learning_data$test_acc_Bset)
#homogenity of variances
leveneTest(reward_learning_data$test_acc_Bset, reward_learning_data$cond)

#reward learning test accuracy overall
x <- c(reward_learning_data$test_acc_overall)
hist(x)
shapiro.test(reward_learning_data$test_acc_overall)
leveneTest(reward_learning_data$test_acc_overall, reward_learning_data$cond)


#test accumptions condition wise
#create separate data for each condition
gonogo_acute = go_nogo_data[go_nogo_data$cond == 'ACUTE', ]
gonogo_plac = go_nogo_data[go_nogo_data$cond == 'PLAC', ]
gonogo_daily = go_nogo_data[go_nogo_data$cond == 'DAILY', ]

reward_acute = reward_learning_data[reward_learning_data$cond == 'ACUTE', ]
reward_plac = reward_learning_data[reward_learning_data$cond == 'PLAC', ]
reward_daily = reward_learning_data[reward_learning_data$cond == 'DAILY', ]



#nogo accuracy anova
res<-anova_test(data=go_nogo_data,dv=nogo_acc,wid=id,within=cond) 
get_anova_table(res)

#bset accuracy anova
resu <- anova_test(data = reward_learning_data ,dv = test_acc_Bset , wid = id , within = cond) 
get_anova_table(resu)

#nogo accuracy friedman test 
friedman.test(go_nogo_data$nogo_acc, go_nogo_data$cond, go_nogo_data$id)

#bset accuracy friedman test
friedman.test(reward_learning_data$test_acc_Bset, reward_learning_data$cond, reward_learning_data$id)

#nogo accuracy post hoc paired t test
pwc <-  pairwise_t_test(go_nogo_data, nogo_acc ~ cond, paired = TRUE,p.adjust.method = "bonferroni")
pwc

#bset accuracy post hoc paired ttest
pwcc <-  pairwise_t_test(reward_learning_data, test_acc_Bset ~ cond, paired = TRUE,p.adjust.method = "bonferroni")
pwcc

#nogo accuracy post hoc friedman
friedmanmc(go_nogo_data$nogo_acc, go_nogo_data$cond, go_nogo_data$id)

#test accuracy overall anova
resuuuu <- anova_test(data = reward_learning_data ,dv = test_acc_overall , wid = id , within = cond) 
get_anova_table(resuuuu)

#go errors
x <- c(go_nogo_data$wrong_go_total)
hist(x)
shapiro.test(go_nogo_data$wrong_go_total)
#non parametric test for the very much not normal distributed go error data
friedman.test(go_nogo_data$wrong_go_total, go_nogo_data$cond, go_nogo_data$id)

#go accuracy friedman test
friedman.test(go_nogo_data$go_acc, go_nogo_data$cond, go_nogo_data$id)

#correct nogo total
shapiro.test(go_nogo_data$correct_nogo_total)
x <- c(go_nogo_data$correct_nogo_total)
hist(x)
friedman.test(go_nogo_data$correct_nogo_total, go_nogo_data$cond, go_nogo_data$id)
shapiro.test(gonogo_acute$correct_nogo_total)
shapiro.test(gonogo_daily$correct_nogo_total)
shapiro.test(gonogo_plac$correct_nogo_total)
ressu <- anova_test(data = go_nogo_data ,dv = correct_nogo_total , wid = id , within = cond) 
get_anova_table(ressu)

#no outliers
gonogo_copy <- go_nogo_data
namevectorindices <- which(gonogo_copy$nogo_acc < 0.42)
namevector <- unique(gonogo_copy[namevectorindices,'id'])

for(value in namevector) {
  gonogo_copy = gonogo_copy[gonogo_copy$id != value, ]
}
#nogo accuracy without outliers
x <- c(gonogo_copy$nogo_acc)
hist(x)
boxplot(x, main = "Boxplot")
resuu <- anova_test(data = gonogo_copy ,dv = nogo_acc , wid = id , within = cond) 
get_anova_table(resuu)
friedman.test(gonogo_copy$nogo_acc, gonogo_copy$cond, gonogo_copy$id)
