# libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# set path
setwd('C:/Users/Administrator/Desktop/data')

# get other source
source(feature_encoding)

# test data 
loadData = function(filename){
    # input: the test results file
    # output: the results as a dataframe
    return (read.csv(filename))   
}

data = loadData('raw_data.csv')
X = data[,-length(data)]
y = data[,length(data)]

FeatureSelection = function(data,target){
    # Feature Selection
    boruta = Boruta(y ~ X,data = data, 
                    doTrace = 2)
    print(boruta)
    # plot the figure
    plot(boruta, las = 2, cex.axis = 0.7)
    #plotImpHistory(boruta)

    # matrix 
    print(attStats(boruta))

}

