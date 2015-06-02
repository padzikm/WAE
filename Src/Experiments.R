#Inicjalizacja œrodowiska
source("DEStandard.R")
source("DEModified.R")
source("DEModified2.R")
library(cec2013)


#Funkcja realizuj¹ca pojedynczy eksperyment
#iterationsCount - liczba iteracji
#populationSize - rozmiar populacji
#benchmarkNumber - która funkcja z benchmarku CEC2013 jest analizowana
#dimensionsCount - liczba wymiarów dla jakiej obliczamy funkcjê
#middleResults - ile wyników poœrednich zwracamy, ¿eby narysowaæ póŸniej krzyw¹ zbie¿noœci
#return - tablicê, która ma middleResults + 1 wierszy. Ka¿dy wiersz reprezentuje
#         aktualnie najlepszy punkt populacji. Tablica ma dimensionsCount + 1 
#         kolumn. W pierwszych kolumnach s¹ wspó³rzêdne punktu. W ostatniej wartoœæ
#         funkcji celu. Ostatni wiersz to ostateczny wynik optymalizacji.


runExperiment <- function(iterationsCount, populationSize, benchmarkNumber,
                          dimensionsCount, middleResults)
{
  optimizedFunction <- function(x) {return(cec2013(benchmarkNumber, x))}
  
  print(deStandard(iterationsCount, populationSize, optimizedFunction,
                   dimensionsCount, middleResults))
  
  print(deModified(iterationsCount, populationSize, optimizedFunction,
                   dimensionsCount, middleResults))
  
  print(deModified2(iterationsCount, populationSize, optimizedFunction,
                   dimensionsCount, middleResults))
}