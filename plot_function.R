setwd('C:/Users/wangqn/Desktop/R')
library(tidyverse)
library(rpart)
library(rpart.plot)
library(gbm)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(gridExtra)
library(pROC)

Attribution_data <- read.csv('IBM.csv', header = TRUE)

#Plot bar diagram
#Relationship between gender and attribution
plotBar <- function(dataset, Gender, bar_diagram){
  bar_diagram <- ggplot(data = dataset, aes(x = Gender)) +
    geom_bar(aes(fill = Attrition), position = "fill") +
    theme_economist() +
    labs(title = "Gender VS Attrition", x = "Gender", y = "Ratio")
  bar_diagram
}

plotBar(Attribution_data, Gender, bar_diagram)

#Plot density diagram
#Relationship between age and attribution
plotDensity <- function(dataset, Age, density_diagram){
  density_diagram <- ggplot(data = dataset, aes(x = �..Age)) +
    geom_density(aes(fill = Attrition), alpha = 0.8) +
    theme_economist() +
    labs(title = "Age VS Attrition", x = "Age", y = "") +
    scale_x_continuous(breaks = seq(18, 60, 6))
  density_diagram
}

plotDensity(Attribution_data,Age, bar_diagram)

#Plot box diagram
#Relationship between employee input-output and attribution
plotBox <- function(dataset, JobInvolvementr1, MonthlyIncome, density_diagram){
  dataset$JobInvolvement1 <- as.character(dataset$JobInvolvement)
  boxdiagram <- ggplot(data = dataset, aes(x = JobInvolvement1, y = MonthlyIncome)) +
    geom_boxplot(aes(fill = Attrition)) +
    theme_economist() +
    labs(title = "JobInvolvement and  MonthlyIncome", x = "JobInvolvement", y = "MonthlyIncome")
  boxdiagram
}

plotBox(Attribution_data,JobInvolvementr1, MonthlyIncome, density_diagram)