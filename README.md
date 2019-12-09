### Study Project R
jest skryptem w języku R do czyszczenia danych o pożyczkach, które to dane po oczyszczeniu i ewentualnym uzupełnieniu zostaną użyte do budowy modelu uczenia maszynowego.
#### Author
Andriy Shyrokoryadov.
#### Zależności / wymagane biblioteki
Skrypt korzysta z bibliotek:
 - tidyverse
 - dplyr
 - sqldf
#### Przed uruchomieniem skryptu
 - upewnij się, że masz zainstalowane wymagane biblioteki
 - upewnij się, że ustawiłeś katalog roboczy i zawiera on pliki z danymi o pożyczkach
 - używając polecenia *options* ustaw niezbędny format liczb
#### Opis struktury skryptu
Skrypt zawiera jedną funkcję *clean_loan_stats_file* w której są wywoływane poszczególne funkcje wykonujące pojedyncze operacje na danych. Nazwa każdej funkcji ma opisowy charakter, aby można było się domyśleć jaką operację wykonuje. W ramach skryptu została utworzona funkcja do wyświetlania komunikatów na konsoli z podaniem czasu.
#### Opis działania skryptu
1. pobieramy nazwy kolumn;
2. pobieramy typ danych kolumn;
3. pobieramy kolumny z danymi numerycznymi;
4. pobieramy kolumny z danymi kategorycznymi;
5. pobieramy podsumowanie dla danych numerycznych (min, 1stQ, mediana, średnia, 3rdQ, max, liczba wartości unikalnych)
6. pobieramy statystyki wartości NA dla każdej kolumny (nazwa kolumny, typ danych, liczba wartości NA, liczba wartości nie NA, współczynnik liczby wartośći NA do ogólnej liczby danych);
7. wybieramy kolumny ze współczynnikiem NA większym niż 75%;
8. wybieramy kolumny ze współczynnikiem NA mnieszym lub równym 75%;
9. scalamy statystyki NA dla wartości numerycznych ze statystykami danych typu numerycznego;
10. pobieramy nazwę kolumny z danymi numerycznymi, medianę i średnią dla wartości numerycznych;
11. pobieramy z dataset'u z punktu 10 tylko kolumny z medianą i średnią - w ten sposób będziemy mogli wykryć dane zduplikowane;
12. wyświetlamy w konsoli dane zduplikowane;
13. dodajemy nazwy kolumn z danymi zduplikowanymi do listy z nazwami kolumn do usunięcia;
14. dodajemy do listy z nazwami kolumn do usunięcia nazwy kolumn z danymi typu 'numeric', 'integer', 'logical' gdzie współczynnik NA jest większy niż 95%;
15. pobieramy nazwy kolumn z wartościami kategorycznymi gdzie są podane daty lub zakres wartości jest zbyt szeroki i dodajemy te nazwy do listy z nazwami kolumn do usunięcia;
16. w danych numerycznych gdzie współczynnik wartości NA jest w przedziale od 10% do 95% zamieniamy wartości NA na maksymalną wartość dla tej kolumny - analiza pokazała, że jest to liczba dni od jakiegoś zdarzenia. Jeżeli zdarzenie nie miało miejsca, to do zestawu danych została dodana wartość NA. W takim przypadku należy potraktować wartość NA jako zdarzenie które było bardzo dawno temu czyli zamieniamy na wartość maksymalną dla tej kolumny;
17. w danych numerycznych gdzie współczynnik wartości NA jest mniejszy niż 10% zamieniamy wartości NA na minimalną wartość dla tej kolumny;
18. zamieniamy puste wartości kategoryczne na wartość która nadaje się na domyślną dla danej kolumny;
19. dane są gotowe do użycia w modelu;   
