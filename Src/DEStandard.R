#Funkcja realizuj¹ca standardowy algorytm ewolucji ró¿nicowej
#iterationsCount - liczba iteracji
#populationSize - rozmiar populacji
#optimizedFunction - funkcja celu
#dimensionsCount - liczba wymiarów dla jakiej obliczamy funkcjê
#middleResults - ile wyników poœrednich zwracamy, ¿eby narysowaæ póŸniej krzyw¹ zbie¿noœci
#return - tablicê, która ma middleResults + 1 wierszy. Ka¿dy wiersz reprezentuje
#         aktualnie najlepszy punkt populacji. Tablica ma dimensionsCount + 1 
#         kolumn. W pierwszych kolumnach s¹ wspó³rzêdne punktu. W ostatniej wartoœæ
#         funkcji celu. Ostatni wiersz to ostateczny wynik optymalizacji.


deStandard <- function(iterationsCount, populationSize, optimizedFunction,
                          dimensionsCount, middleResults)
{
  #¯eby wygodnie siê liczy³o CEC populacja mo¿e byæ macierz¹. Ka¿dy wiersz to
  #jeden punkt populacji. Kolumny to kolejne wymiary.
  population <- matrix(0, populationSize, dimensionsCount)
  
  #Tak siê oblicza wartoœæ funkcji celu.
  #Parametr to macierz z punktami do wyznaczenia wartoœci funkcji celu.
  #Zwracana wartoœæ to wektor z kolejnymi wartoœciami funkcji celu.
  singleResult <- optimizedFunction(population)
  
  return(0)
}