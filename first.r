print("hello")

# 1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

funkcja <- function(x, y)
  {
    if (n %% m == 0)
        {
         rezultat = "Liczba podzielna"
        } 
    else 
        {
        rezultat = "Liczba nie podzielna"
        }
    return(rezultat)
  }

n = 2
m = 2

wynik <- funkcja(n,m)
print(wynik)

# 2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#    Drugą połowę przejechał ze średnią prędkością 90 km/h.
#    Jaka była średnia prędkość pociągu.


#droga jest rózna do poprawy zastowoć wzór na predkość

v1 <- 120 = s/t1
v2 <- 90  = s/t2

# 2s/t1+t1 = 2/(t1/s+t2/s) = 2/(1/120+1/90) = 2/(3/360+4/360) = 2/(7/360) = 2*(360/7)

120*90
360/120
360/90

srednia_predkosc
c <- 2*(360/7)
c

# 3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#    Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

dane <- read.csv(file = 'C:/Users/Admin/Documents/Podyplomowe Big Data/Filmy zajecia/II R cw/Projekt 1/dane/dane.csv', header = TRUE, sep =";" )

head(dane)

head(dane[1])
head(dane[2])

cor(dane[2],dane[1])
# jest korelacja miedzy wzrostem i wagą 
# wraz ze wzrostem wagi zwieksza sie wzrost osoby


# 4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#    stworzDataFrame <- function(ile=1)
#    W pierwszym wierszu użytkownik podaje nazwy kolumn. 
#    W kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. 
#    Ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)


stworzDataFrame <- function(ile=1)
{
  podane_nazwy_kolumn = list()
  wartosci_ramek = list()
  if(ile < 1)
  {
    ile = 1
  }
  podane_nazwy_kolumn <- readline("Podaj nazwy kolumn oddzielone przecinkami")
  print(podane_nazwy_kolumn)
  podane_nazwy_kolumn<-as.list(el(strsplit(podane_nazwy_kolumn, ",")))
  df<-podane_nazwy_kolumn
  ilosc_kolumn<- length(podane_nazwy_kolumn)
  cat("### Tworzysz ramke z nastepujaca liczba kolumn: ", ilosc_kolumn)
  print("\n")
  
  result <- matrix(ncol=ilosc_kolumn, nrow=ile)
  
  for(i in 1:ile){
    cat("### Tworzysz wiersz nr: ", i)
    for(y in 1:ilosc_kolumn){
      cat("### Wprowadz dane do kolumny: ", y)
      result[i,y] <- readline("Podaj wartosc dla wiersza")
    }
  }
  new_df<- rbind(df,result)
  new_df
}




stworzDataFrame(2)


# zaad 5
# pierewszy argument nowj funkcji z ilu plków bierzmy dane ( 3 plik)

# pobrać wszystkie dataframe 

# wybrać daną kolumnę 

# wykonac obliczenia

#liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",
#DlaIluPlikow=1){



#...}



CountFromFiles <- function(sciezka, nazwaKolumny, jakaFunkcja, DlaIluPlikow)
{
  pliki <- list.files(sciezka)
  result = 0
  result_list <- c()
  
  if(DlaIluPlikow>length(pliki)){
    print("There is no so many files in dir")
  }else{
    for(i in 1:DlaIluPlikow){
      cat("Aggregated files \n : ", pliki[i], "\n")
      plik <- pliki[i]
      
      path <- paste(sciezka,plik,sep="/")
      
      DataFrame <- read.csv(path,sep = ",", header = TRUE, na.strings = c("","NA"), stringsAsFactors=FALSE)
      
      DataFrame <- na.omit(DataFrame[[nazwaKolumny]])
      if(is.numeric(DataFrame) == TRUE){
        if(jakaFunkcja == "mean"){
          result_list[i] <- mean(DataFrame)
          
        }else if(jakaFunkcja == "median"){
          result_list[i] <- median(DataFrame)
          
        } else if(jakaFunkcja == "min"){
          result_list[i] <- min(DataFrame)
          
        }else{
          result_list[i] <- max(DataFrame)
        }
      }else{
        print("Column is not numeric")
      }
    }
    if(jakaFunkcja == "mean"){
      result <- mean(result_list)
    }else if(jakaFunkcja == "median"){
      result <- median(result_list)
    } else if(jakaFunkcja == "min"){
      result <- min(result_list)
    }else{
      result <- max(result_list)
    }
  }
  cat("Wynik dzialania to : ",result)
}
CountFromFiles("./smogKrakow","X142_pressure","mean",4)
CountFromFiles("./smogKrakow","X142_humidity","median",12)






































