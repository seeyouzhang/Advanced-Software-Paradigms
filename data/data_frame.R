# set path
setwd('C:/Users/Administrator/Desktop/data')

raw_data = read.csv('raw_data.csv')

y = raw_data[,length(raw_data)]
X = raw_data[,-length(raw_data)]

MergeData = function(data1,data2,combine_by = 'row'){
    # combine data1 and data2 by column or row
    
    # combine data frame by column
    if (combine_by = 'col'){
        return(cbind(data1, data2))
    }
    # combine data frame by row
    else{
        return(rbind(data1, data2))
    }
    
    return (data)
    
}

delData = function(data,row_names = NULL, col_names = NULL,
                   row_index = NULL, col_index = NULL){
    # Users can delete data by names or index
    
    # if data is empty, return NULL
    if (!data){
        return (NULL)
    }
    
    # the way delete data by names
    if (row_names | col_names){
        
        # row names
        if (row_names) {
            data = data[-row_names,]
        }
        
        # column names
        if (col_names) {
            data = data[,-col_names]
        }
    }
    
    # the way delete data by index
    if (row_index | col_index){
        
        # row index
        if (row_index) {
            data = data[-row_index,]
        }
        
        # column index
        if (col_index) {
            data = data[,-col_index]
        }
    }
    # return data
    return (data)
}

searchData = function(data,row_names = NULL, col_names = NULL,
                   row_index = NULL, col_index = NULL){
    # Users can search data by names or index
    
    # if data is empty, return NULL
    if (!data){
        return (NULL)
    }
    
    # the way search data by row_names and col_names
    if (row_names & col_names){
        return (data[row_names,col_names])
    }
    
    # the way search data by row_index and col_names
    else if (row_index & col_names){
        return (data[row_index,col_names])
    }
    
    # the way search data by row_names and col_index
    else if (row_names & col_index){
        return (data[row_names,col_index])
    }    

    # the way search data by row_index and col_index
    else if (row_index & col_index){
        return (data[row_index,col_index])
    }
    
    # if search parametres is illegal, return NULL
    else{
        return (NULL)
    }
}

changeData = function(data,
                      row_index = NULL, col_index = NULL,
                      new_data){
    # Users can replace data 
    
    # if data is empty, return NULL
    if (!data){
        return (NULL)
    }
    
    data[row_index, col_index] = new_data
    return (data)
}
