# missing_data.R
# This R file is for the project of course:
# Advanced Software Paradigms (CSCI 6221, Summer 2020)
# Computer Science @ George Washington University
# Author: Siyu Zhang
# ------------------------------------------------------------------
# This file includes functions that use to deal with missing value in the data
# ------------------------------------------------------------------

# set path
setwd('C:/Users/Administrator/Desktop/data')

# libraries
library(mice)
library(VIM)
library(imputeTS)

loadData = function(filename){
    # input: the test results file
    # output: the results as a dataframe
    return (read.csv(filename))   
}

missing_data = loadData('missing_data.csv')
X = missing_data[,-length(missing_data)]
y = missing_data[,length(missing_data)]

# del missing value
delMissing = function(data){
    new_data = data[complete.cases(data),]
}

# fill with the mean value according to the complete
meanFill = function(data){

    for (i in 1:length(data)){
        options(digits=3) # set percision
        feature = data[,i] # get feature vector 
        average = mean(feature[!is.na(feature)]) # get mean value
        feature[is.na(feature)] = average # fill 
        data[,i] = feature
    }
    return (data)
}

# fill with the mean value by zero
zeroFill = function(data){

    for (i in 1:length(data)){
        feature = data[,i] # get feature vector 
        feature[is.na(feature)] = 0 # fill 
        data[,i] = feature
    }
    return (data)
}

# fill with the mean value by the number assigned by users
numFill = function(data,num){

    for (i in 1:length(data)){
        feature = data[,i] # get feature vector 
        feature[is.na(feature)] = num # fill 
        data[,i] = feature
    }
    return (data)
}

# use a random number sample from the complete data
randomFill = function(data){

    for (i in 1:length(data)){
        options(digits=3)
        feature = data[,i] # get feature vector 
        maxValue = max(feature[!is.na(feature)]) # get max value
        minValue = min(feature[!is.na(feature)]) # get min value

        # generate random numbers
        randomNums = runif(length(feature[is.na(feature)]),min=minValue,max=maxValue) 
        feature[is.na(feature)] = randomNums # fill 
        data[,i] = feature
    }
    return (data)
}

# fill the data by inference the distribution
multImp = function(data){
    # Impute
    # 'pmm' stands for Predictive Mean Match
    # 'polyreg' stands for Multinomial Logistic Regression
    imp = mice(missing_data)
    newdata = complete(imp, 1)   
    return(newdata)
}

# a matrix that contains information about missing data
# Fisrtly, leave the last row for the time being
# The first column indicates how many samples extist in the current situation
# The last column indicates how many eigenvalues are missing in the current situation
# If this feature is missing in the current situation, its value is 0; otherwise is 1
# The last row counts the number of missing features and the total number 

# --------------------------------------------------------------------------
# observe the missvalue 
obMissingValues = function(data){
    impute = mice(data)
    stripplot(impute, pch = 20, cex = 1.2)
}
#md.pattern(X)
#md.pairs(X)
#marginplot(X)
#aggr(X,prop = TRUE, numbers=TRUE)
