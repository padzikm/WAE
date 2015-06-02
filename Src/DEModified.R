#Funkcja realizuj¹ca zmodyfikowany algorytm ewolucji ró¿nicowej
#iterationsCount - liczba iteracji
#populationSize - rozmiar populacji
#benchmarkNumber - która funkcja z benchmarku CEC2013 jest analizowana
#dimensionsCount - liczba wymiarów dla jakiej obliczamy funkcjê
#middleResults - ile wyników poœrednich zwracamy, ¿eby narysowaæ póŸniej krzyw¹ zbie¿noœci
#return - tablicê, która ma middleResults + 1 wierszy. Ka¿dy wiersz reprezentuje
#         aktualnie najlepszy punkt populacji. Tablica ma dimensionsCount + 1 
#         kolumn. W pierwszych kolumnach s¹ wspó³rzêdne punktu. W ostatniej wartoœæ
#         funkcji celu. Ostatni wiersz to ostateczny wynik optymalizacji.


deModified <- function(iterationsCount, populationSize, benchmarkNumber,
                       dimensionsCount, middleResults)
{
  Ff <- 0.8
  Cr <- 0.9
  
  population <- matrix(runif(populationSize*dimensionsCount, -100, 100), populationSize, dimensionsCount)
  populationResults <- cec2013(benchmarkNumber, population)
  partialResults <- NULL
  freq <- floor(iterationsCount / middleResults)
  iteration <- 1
  
  while(iteration <= iterationsCount)
  {
    avg <- sum(populationResults) / populationSize
    
    newPopulation <- NULL
    
    for(i in 1:populationSize)
    {
      P <- sample(c(1:populationSize), 2)
      
      M <- avg + Ff*(population[P[1],] - population[P[2],])
      
      O <- NULL
      
      for(j in 1:dimensionsCount)
      {
        a <- runif(1)
        
        if(a < Cr)
        {
          O[j] = M[j]
        }
        else
        {
          O[j] = population[i,j]
        }
      }
      
      result <- cec2013(benchmarkNumber, O)
      
      if(result < populationResults[i])
      {
        populationResults[i] <- result
        newPopulation <- rbind(newPopulation, O)
      }
      else
      {
        newPopulation <- rbind(newPopulation, population[i,])
      }
    }
    
    population <- newPopulation
    
    currentBest <- which.min(populationResults)
    
    if(iteration %% freq == 0)
    {
      partialResults <- rbind(partialResults, c(population[currentBest,], populationResults[currentBest]))
    }
    
    iteration <- iteration + 1
  }
  
  currentBest <- which.min(populationResults)
  
  partialResults <- rbind(partialResults, c(population[currentBest,], populationResults[currentBest]))
  
  return (partialResults);
}