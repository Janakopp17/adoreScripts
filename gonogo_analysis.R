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


library(rJava)
library(xlsxjars)
library(xlsx)
library(plyr)
library(plotrix)
library(yarrr)
library(tidyr)
library(gridExtra)
library(car)

go_nogo_data <- read.csv('E:/Jana/Adore/data_analysis/go_nogo_data.csv')

x <- c(go_nogo_data$block_1A_nogo_acc)
#boxplot(x, main = "Boxplot")
qqnorm(x, main = "Normal Q-Q plot")
shapiro.test(go_nogo_data$block_1A_nogo_acc)

leveneTest(go_nogo_data$nogo_acc, go_nogo_data$cond)
levels(go_nogo_data$cond)
go_nogo_data$cond <- factor(go_nogo_data$cond)

#leveneTest(mydata$Parch, mydata$Embarked)
