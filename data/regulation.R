# regulation.R
# This R file is for the project of course:
# Advanced Software Paradigms (CSCI 6221, Summer 2020)
# Computer Science @ George Washington University
# Author: Siyu Zhang
# ------------------------------------------------------------------
# This file includes functions that use to normalize the data
# ------------------------------------------------------------------

# set path
setwd('C:/Users/Administrator/Desktop/data')

# load data
loadData = function(filename){
    # input: the test results file
    # output: the results as a dataframe
    return (read.csv(filename))   
}

data = loadData('raw_data.csv')
X = data[-length(data)]
y = data[length(data)]

# mean normalization
MeanNorm = function(data){
    # range from [0,1]
    n = length(data)
    for(i in 1:n){
        # get parameters
        maxValue = max(data[,i])
        minValue = min(data[,i])
        average = mean(data[,i])

        # normalize the data
        data[,i] = (data[,i] - average) / (maxValue-minValue)
    }
    return (data) # Return the processed value
}

# min max normalization
MinMaxNorm = function(data,a,b){
    # range from [a,b]
    n = length(data)
    for(i in 1:n){
        # get parameters
        maxValue = max(data[,i])
        minValue = min(data[,i])
        average = mean(data[,i])

        # normalize the data
        data[,i] = (data[,i] - average) / (maxValue-minValue)
        data[,i] = data[,i]*(b-a) + a
    }
    return (data) # Return the processed value
}

# z-score normalization
zScoreNorm = function(data){
    # Gaussian distribution (0,1)

    n = length(data)
    for(i in 1:n){
        # get parameter
        average = mean(data[,i])

        # normalize the data
        data[,i] = (data[,i]-average) / var(data[,i])
    }
    return (data) # Return the processed value
}

# centralization 
Centralization = function(data){
    # this function only change the mean rather than normalize the data
    n = length(data)
    for(i in 1:n){
        # get parameter
        average = mean(data[,i])

        data[,i] = data - average
    }
    return (data) # Return the processed value
}
