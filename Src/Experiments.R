#Inicjalizacja �rodowiska
source("DEStandard.R")
source("DEModified.R")
library(cec2013)


#Funkcja realizuj�ca pojedynczy eksperyment
#iterationsCount - liczba iteracji
#populationSize - rozmiar populacji
#benchmarkNumber - kt�ra funkcja z benchmarku CEC2013 jest analizowana
#dimensionsCount - liczba wymiar�w dla jakiej obliczamy funkcj�
#middleResults - ile wynik�w po�rednich zwracamy, �eby narysowa� p�niej krzyw� zbie�no�ci
#return - tablic�, kt�ra ma middleResults + 1 wierszy. Ka�dy wiersz reprezentuje
#         aktualnie najlepszy punkt populacji. Tablica ma dimensionsCount + 1 
#         kolumn. W pierwszych kolumnach s� wsp�rz�dne punktu. W ostatniej warto��
#         funkcji celu. Ostatni wiersz to ostateczny wynik optymalizacji.


runExperiment <- function(iterationsCount, populationSize, benchmarkNumber,
                          dimensionsCount, middleResults)
{
  print(deStandard(iterationsCount, populationSize, benchmarkNumber,
                   dimensionsCount, middleResults))
  
  print(deModified(iterationsCount, populationSize, benchmarkNumber,
                   dimensionsCount, middleResults))
  
}