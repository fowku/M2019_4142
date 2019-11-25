# Subset data by rows and columns, execute calculations for columns.
# Returns list of subsetted dataframe and column calculated list
subsetDataFrame <- function(dataFrame, rowSelection, columnSelection) {
  subsettedDataFrame <- dataFrame[rowSelection, columnSelection]
  
  list(subsettedDataFrame, subsettedDataFrame %>% calculateColumns()) %>% return()
}

# Calculates sum for numerical columns, and frequency table for strings
calculateColumns <- function(dataFrame) {
  calculatedResult <- list()
  
  for (column in names(dataFrame)) {
    if (class(dataFrame[[column]]) == 'numeric') {
      calculatedResult[[column]] <- sum(dataFrame[[column]])
      next
    }
    
    if (class(dataFrame[[column]]) == 'character' || class(dataFrame[[column]]) == 'logical' || class(dataFrame[[column]]) == 'factor') {
      calculatedResult[[column]] <- table(dataFrame[[column]])
      next
    }
  }
  
  return(calculatedResult)
}

gapminder %>% subsetDataFrame(1:30, 1:5) %>% print()