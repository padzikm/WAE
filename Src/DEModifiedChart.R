#Funkcja realizuj?ca zmodyfikowany algorytm ewolucji r??nicowej


deModifiedChart <- function(populationSize, optimizedFunction, dimensionsCount, iterations)
{
  Ff <- 0.8
  Cr <- 0.9

  population <- matrix(runif(populationSize*dimensionsCount, -100, 100), populationSize, dimensionsCount)
  populationResults <- optimizedFunction(population)
  
  chart = c("It", "Max", "Avg", "Min")
  iteration <- 0
  
  while(iteration < iterations)
  {
    iteration <- iteration + 1
    
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
    
    if((iteration %% 50) == 0)
    {
      chart <- rbind(chart, c(iteration, max(populationResults), mean(populationResults), min(populationResults)))
      print(c(iteration, max(populationResults), mean(populationResults), min(populationResults)))
    }
    
    population <- newPopulation
    
  }
  
  return(chart)
}