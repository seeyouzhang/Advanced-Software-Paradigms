# set path
setwd('C:/Users/Administrator/Desktop/data')

loadData = function(filename){
    # input: the test results file
    # output: the results as a dataframe
    return (read.csv(filename))   
}

data = loadData('raw_data.csv')
X = data[-length(data)]
y = data[length(data)]

MeanNorm = function(data){
    # range from [0,1]
    n = length(data)
    for(i in 1:n){
        # get parameters
        maxValue = max(data[,i])
        minValue = min(data[,i])
        average = mean(data[,i])

        data[,i] = (data[,i] - average) / (maxValue-minValue)
    }
    return (data) # Return the processed value
}

MinMaxNorm = function(data,a,b){
    # range from [a,b]
    n = length(data)
    for(i in 1:n){
        # get parameters
        maxValue = max(data[,i])
        minValue = min(data[,i])
        average = mean(data[,i])

        data[,i] = (data[,i] - average) / (maxValue-minValue)
        data[,i] = data[,i]*(b-a) + a
    }
    return (data) # Return the processed value
}

zScoreNorm = function(data){
    # Gaussian distribution (0,1)

    n = length(data)
    for(i in 1:n){
        # get parameter
        average = mean(data[,i])

        data[,i] = (data[,i]-average) / var(data[,i])
    }
    return (data) # Return the processed value
}

Centralization = function(data){

    n = length(data)
    for(i in 1:n){
        # get parameter
        average = mean(data[,i])

        data[,i] = data - average
    }
    return (data) # Return the processed value
}
