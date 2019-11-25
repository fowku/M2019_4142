# Subset data by rows and columns, execute calculations for columns.
# Returns list of subsetted dataframe and column calculated list
subsetDataFrame <- function(dataFrame, rowSelection, columnSelection, calculationFunction = sum) {
  subsettedDataFrame <- dataFrame[rowSelection, columnSelection]
  listSubsettedDataFrame <- subsettedDataFrame
  
  subsettedDataFrame %>% sapply(function(x) {calculateColumn(x, calculationFunction)}) %>% 
    list(subsettedDataFrame) %>% 
    return()
}

# Calculates sum for numerical columns, and frequency table for strings
calculateColumn <- function(element, calculationFunction) {
  if (class(element) == 'numeric' || class(element) == 'integer') {
     return(matrix(element) %>% calculationFunction()) # ask a question about pipe thing, it seems to be working incorrectly
  }
  
  if (class(element) == 'character' || class(element) == 'logical' || class(element) == 'factor') {
    element %>% table() %>% return()
  }
}

gapminder %>% subsetDataFrame(1:30, 1:5, mean) %>% print()