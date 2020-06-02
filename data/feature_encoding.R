# feature_encoding.R
# This R file is for the project of course:
# Advanced Software Paradigms (CSCI 6221, Summer 2020)
# Computer Science @ George Washington University
# Author: Siyu Zhang
# ------------------------------------------------------------------
# This file includes functions that use to encode the data into numberic type
# ------------------------------------------------------------------

# libraries
library(Boruta)
library(mlbench)
library(caret)

# set path
setwd('C:/Users/Administrator/Desktop/data')


numEncode = function(data,featureName = colnames(data)){
    # encode data with number
    # from 1 to the number of the species
    print('start to encode')

    for (feature in featureName){
        # record the name of the features
        names = list() # an array to store the names 
        # Search through the sample
        for (kind in data[,feature]){
            if (!(kind %in% names)){
                names = c(names, kind) # add new name
            }
        }
        # encoding data
        labels = c(1:length(names)) # create label
        data[,feature] = factor(data[,feature], levels = names, labels = labels)
    }

    # return new data
    print('finish encoding')
    return (data)
}

loadData = function(filename){
    # input: the test results file
    # output: the results as a dataframe
    return (read.csv(filename))   
}

# ------------------------------------------------------------------
# test
data = loadData('raw_data.csv')
X = data[,-length(data)]
y = data[,length(data)]
features_name = colnames(data)

b = features_name[length(features_name)]
a = numEncode(data,featureName = b)
#print(a)

# get feature
X = a[,-length(data)]
y = a[,length(data)] 

# Feature Selection
boruta = Boruta(y ~ X,data = a, doTrace = 2)

print(boruta)
# plot the figure
#plot(boruta)
#plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# matrix 
print(attStats(boruta))


