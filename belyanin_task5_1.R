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
    if (is.numeric(dataFrame[[column]])) {
      calculatedResult[[column]] <- sum(dataFrame[[column]])
      next
    }
    
    if (is.character(dataFrame[[column]]) || is.logical(dataFrame[[column]]) || is.factor(dataFrame[[column]])) {
      calculatedResult[[column]] <- table(dataFrame[[column]])
      next
    }
  }
  
  return(calculatedResult)
}

gapminder %>% subsetDataFrame(1:30, 1:5) %>% print()