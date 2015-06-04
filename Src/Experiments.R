#Inicjalizacja ?rodowiska
source("DEStandard.R")
source("DEModified.R")
source("DEModified2.R")
library(cec2013)

#Warto?ci minimalne dla funkcji benchmarku CEC2013
OPTIMALS = c(seq(-1400, -100, 100), seq(100, 1400, 100))



buildResultRow <- function(populationSize, dimensionsCount, attemptsCount, benchmarkNumber, optimizingFunction)
{
  rawResults = seq(1, attemptsCount)
  
  optimizedFunctionError <- function(x) {return(abs(cec2013(benchmarkNumber, x) - OPTIMALS[benchmarkNumber]))}
  
  for(i in 1 : attemptsCount)
    rawResults[i] = optimizingFunction(populationSize, optimizedFunctionError, dimensionsCount)
  
  resultRow = c(benchmarkNumber, min(rawResults), max(rawResults), mean(rawResults), median(rawResults), sd(rawResults))
  
  return(resultRow)
  
}

buildResultsTable <- function(populationSize, dimensionsCount, attemptsCount, optimizingFunction, methodName, fromFunc, toFunc)
{
  resultsTable <- c("Func", "Best", "Worst", "Mean", "Median", "Standard deviation")
  
  print(methodName)
  print("Starting...")
  
  for(i in fromFunc : toFunc)
  {
    resultsTable <- rbind(resultsTable, buildResultRow(populationSize, dimensionsCount, attemptsCount, i, optimizingFunction))
    print(paste(c(i - fromFunc + 1, "/", toFunc - fromFunc + 1), collapse = " "))
  }
  
  print("Finished...")
  
  fileName <- paste(c(methodName, "from", fromFunc, "to", toFunc, ".txt"), collapse = "_")
  write(t(resultsTable), file = fileName, ncolumns = 6, append = FALSE, sep = "\t")
  print(resultsTable)

}