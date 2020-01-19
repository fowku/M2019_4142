library(tidyverse)
library(gapminder)

# Subset data by rows and columns, execute calculations for columns.
# Returns list of subsetted dataframe and column calculated list
subsetDataFrame <- function(dataFrame, rowSelection, columnSelection, calculationFunction = sum) {
  subsettedDataFrame <- dataFrame %>% 
    select(columnSelection) %>% 
    slice(rowSelection)
    
  listSubsettedDataFrame <- subsettedDataFrame
  
  subsettedDataFrame %>% sapply(function(x) {calculateColumn(x, calculationFunction)}) %>%
    list(subsettedDataFrame) %>%
    return()
}

# Calculates sum for numerical columns, and frequency table for strings
calculateColumn <- function(element, calculationFunction) {
  if (is.numeric(element) || is.integer(element)) {
      return(matrix(element) %>% calculationFunction())
  }

  if (is.character(element) || is.logical(element) || is.factor(element)) {
    element %>% table() %>% return()
  }
}

gapminder %>% subsetDataFrame(1:30, 1:5, mean) %>% print()
