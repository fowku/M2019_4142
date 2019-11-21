# Subset data by rows and columns, execute calculations for columns.
# Returns list of subsetted dataframe and column calculated list
subsetDataFrame <- function(dataFrame, rowSelection, columnSelection, calculationFunction = sum) {
  subsettedDataFrame <- dataFrame[rowSelection, columnSelection]
  listSubsettedDataFrame <- subsettedDataFrame

  return( list(subsettedDataFrame, sapply( subsettedDataFrame, function(x) {calculateColumn(x, calculationFunction)} ) ) )
}

# Calculates sum for numerical columns, and frequency table for strings
calculateColumn <- function(element, calculationFunction) {
  if (class(element) == 'numeric') {
    return(calculationFunction(matrix(element)))
  }

  if (class(element) == 'character' || class(element) == 'logical' || class(element) == 'factor') {
    return(table(element))
  }
}

# print(subsetDataFrame(gapminder, 1:30, 1:5, sum))

subsetDataFrame(gapminder, 1:30, 1:5, sum)