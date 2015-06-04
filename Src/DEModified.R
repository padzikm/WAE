#Funkcja realizuj?ca zmodyfikowany algorytm ewolucji r??nicowej


deModified <- function(populationSize, optimizedFunction, dimensionsCount)
{
  Ff <- 0.8
  Cr <- 0.9
  MaxFES <- 10000 * dimensionsCount
  FES <- 0

  population <- matrix(runif(populationSize*dimensionsCount, -100, 100), populationSize, dimensionsCount)
  populationResults <- optimizedFunction(population)
  FES <- FES + populationSize
  
  while(FES < MaxFES && min(populationResults) > 10e-8)
  {
  
    avg <- NULL
    
    for(k in 1:dimensionsCount)
    {
      avg[k] <- sum(population[,k]) / populationSize
    }
    
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
      
      result <- optimizedFunction(O)
      FES <- FES + 1
      
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
  }
  
  return (round(min(populationResults), 9))
}