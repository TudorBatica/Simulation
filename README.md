# Simularea unui atelier auto
Proiectul are ca scop simularea unui atelier auto care repara si curata masinile clientilor.

## Descrierea problemei
### Prezentare generala
Toti clientii atelierului trec prin acelasi proces: intai masina lor este reparata, apoi masina este spalata.  
Deci, sistemul de servire a clientilor poate fi privit ca un sistem de tip coada cu doua servere in serie, unde primul server are scopul de a repara masinile, iar al doilea server are scopul de a curata masinile. 
### Ipoteze 
- Atelierul este deschis 12 ore pe zi
- La finalul programului, atelierul nu mai accepta clienti noi in sistem
- Ambele servere sunt active pe intregul program si nu iau pauze
- Este de asteptat ca repararea unei masini sa dureze mai mult decat curatarea acesteia
- De obicei, primele doua ore din program sunt cele mai libere, iar atelierul se aglomereaza spre terminarea programului
- Clientii nou-veniti vor pleca daca sunt mai mult de 15 persoane in asteptare la primul server(cel ce se ocupa de reparatii)

### Obiective
- Determinarea timpului minim, timpului mediu si timpului maxim petrecut de clienti in atelier
- Numarul mediu de clienti serviti intr-o zi
- Numarul mediu de clienti pierduti intr-o zi

## Datele problemei
### Sosirea clientilor
Clientii noi sosesc conform unui proces Poisson neomogen cu functia de intensitate  
  
![lambda](lambda_fun.png)  
  
### Timpul de servire
Timpurile de servire ale serverului 1 si serverului 2 sunt date de doua variabile aleatoare Y1, respectiv Y2. Cele doua variabile aleatoare au asociate urmatoarele functii de repartitie: 
   
![y1](y1.png)  
![y2](y2.png)  

## Rezolvarea problemelor teoretice 
Inainte de a trece la implementarea efectiva a simularii atelierului, este necesara determinarea modelelor teoretice pentru simularea anumitor componente ale sistemului.  
Printre problemele ce necesita rezolvare matematica, se regaseste simularea variabilelor aleatoare de repartitii Poisson si normala. In acest scop, ne vom folosi doar de variabile aleatoare de repartitie uniforma.

### Simularea variabilei aleatoare de repartitie Poisson
Aceasta variabila aleatoare este necesara pentru determinarea timpului de servire al primului server.  

Pentru generarea acesteia, ne folosim de faptul ca o variabila aleatoare Poisson de parametru lambda este data de numarul de evenimente secventiale care au loc in timp lambda, unde perioadele temporale dintre evenimente sunt variabile aleatoare exponentiale de rata unitara.  
  
Matematic, acest lucru se transcrie astfel:  
  
![poisson](poisson.png) 

Implementand modelul matematic in limbajul R, obtinem urmatoarea functie: 
```r
customRPois <- function(lambda) {
  p = 0
  mysum = 0
  while(1) {
    unif = runif(1)
    mysum = mysum + log(1 - unif)
    if(mysum * -1 >= lambda) {
      return(p)
    }
    p = p + 1
  }
}
```   
   
Generand 100 de observatii folosind `customRPois(lambda = 7.3)` si 100 de observatii folosind functia din R `rpois(lambda = 7.3)`, obtinem urmatoarele repartitii:  
   
![customRPois](Rplot.png)  
![RPois](Rplot01.png)

De asemenea, analizand media si varianta, observam urmatoarele:  
| Functia folosita  | Media | Varianta|
| -------------     |:-----:| :-----:|
| `rpois`           | 7.17  | 8.44 |
| `customRPois`     | 7.5   | 8.47 |


### Simularea variabilei aleatoare de repartitie normala
Aceasta variabila aleatoare este necesara pentru determinarea timpului de servire al celui de al doilea server. 
  
Pentru generarea acesteia, ne folosim de algoritmul Box Muller. Cu ajutorul acesteia, putem simula o variabila aleatoare de repartie normala, cu media 0 si deviatia standard 1.

![normala](normala.png) 

Implementarea corespunzatoare in limbajul R:
```r
customRNorm <- function(sd, mean) {
  U.1 <- runif(1)
  U.2 <- runif(1)
  theta <- 2*pi*U.1
  E <- -log(U.2)
  R <- sqrt(2*E)
  X <- R*cos(theta)
  return(sd*X + mean)
}
```

Pentru testare, generam 100 de observatii folosind `customRNorm` cu media 2 si deviatia standard 1 si 100 de observatii, cu aceeasi medie si varianta, folosind functia din R `rnorm`.

Rezultatele sunt:

![rNorm](Rplot02.png)  
![customRNorm](Rplot04.png)

| Functia folosita  | Media | Varianta|
| -------------     |:-----:| :-----:|
| `customRNorm`     | 2.08  | 1.01 |
| `rnorm`     | 2.07   | 0.91 |

### Simularea sosirii clientilor
Asa cum este mentionat in sectiunea [Datele problemei](#datele-problemei), sosirea clientilor se face dupa un proces Poisson neomogen, ce modeleaza ipoteza ca primele doua ore sunt adesea cele mai libere, iar atelierul se aglomereaza spre sfarsitul programului.  
In implementare, simularea sosirii clientilor se efectueaza folosind urmatoarele doua functii:  

```r
## Functia de intensitate lambda
lambda_fun <- function(t) {
  if(t <= 2) {
    return(2)
  }
  return ((2*t)/log(t))
}

## Proces Poisson neomogen ce determina 
## ora aparitiei urmatorului client
generateTs <- function(s) {
  lambda = 9.66
  t = s
  while (1) {
    u1 = runif(1)
    u2 = runif(1)
    t = t - log2(u1)/lambda
    if(u2 <= lambda_fun(t) / lambda) {
      return(t)
    }
  }
}
```
Observam ca, in implementarea functiei `generateTs`, este folosit un parametru `lambda = 9.66`.  
Aceasta este o constanta ce este mai mare decat functia de intensitate intr-un moment de timp t, pentru orice t intre 0 si 12. 

Analizand graficul functiei de intensitate(figura de mai jos), se observa ca valoarea maxima este in `t = 12`. 
  
![lambda(t)](lambda_fun_plot.png)

Cum `lambda(12) = 24/ln(12) = 9.6583`, putem alege parametrul `lambda = 9.66`.

Folosind functia `generateTs` pentru generarea tuturor momentelor de timp la care sosesc clienti noi intr-o zi, obtinem:  

![sosire](sosite.png)

## Rezultate obtinute

In continuare, sunt prezentate rezultatele obtinute in urma simularilor. 

### Timpul minim, timpul mediu si timpul maxim petrecut de clienti in sistem

Pentru determinarea acestori timpi, apelam intai functia `simulateOneDay`. Aceasta intoarce urmatoarele date:
- momentele de timp la care au sosit clientii noi in sistem
- momentele de timp la care clientii au ajuns la serverul 2
- momentele de timp la care clientii au iesit din sistem
- momentele de timp la care au fost pierduti clienti

Rezultatul returnat il vom transmite functiei `waitingTimes`, care va extrage timpii doriti. Obtinem urmatoarele rezultate:

|   | Timpul minim | Timpul mediu | Timpul maxim |
|---|:------------:|:------------:|:------------:|
| Sistem  | 0.35  | 2.26 |  3.87 |
| Server 1| 0.2  | 1.97 |  3.7 |
| Server 2| 0.05   | 0.29 |  1 |  

*<b>Observatie</b>: Timpii sunt exprimati in ore. Timpii pentru serverele 1 si 2 reprezinta suma dintre timpul de asteptare si servirea efectiva.*

### Numarul mediu de clienti serviti/pierduti intr-o zi

Folosind functia `avgClientsStats(noOfDays)`, putem calcula numarul mediu de clienti serviti(care ies din sistem) si numarul mediu de clienti pierduti intr-o zi.  
Functia simuleaza mai multe zile pentru indeplinirea acestui task.  
Vom selecta numarul de zile ca fiind 365.  
Rezultatul obtinut este:
| Nr mediu de clienti serviti/zi | Nr mediu de clienti pierduti/zi |
|:-----:| :-----:|
| 34.93  | 3.56 |  

*<b>Observatie</b>: Printr-o zi se intelege un program intreg de lucru, de 12 ore.*

### Analiza clientilor pierduti

Analizand o oarecare zi simulata, observam ca atelierul pierde clienti la urmatoarele momente de timp: `10.84, 10.94, 11.01, 11.57`.  

Analizand momentele plecarii primului client, pe parcursul a 365 zile, folosind functia `lostClientsStats`, reies urmatoarele date:

| Cea mai devreme | Medie | Cea mai tarzie |
|:-----:| :-----:|:-----:|
| 6.42  | 10.29 | 11.9 |

*<b>Observatie</b>: Momentele de timp sunt exprimate in numarul de ore trecute de la inceperea programului.*

## Concluzii

Asa cum a fost formulat si in ipoteza, timpul mediu de servire la server-ul 1 este mult mai mare comparativ cu cel al server-ului 2. Prin urmare, pentru a putea servi mai multi clienti, este necesar ca atelierul sa dubleze serverul 1.  

Daca dublarea primului server nu este posibila pe toata durata zilei, se poate incerca dublarea doar in jumatatea a doua a programului de lucru, intrucat aceasta este perioada in care coada de la primul server creste considerabil si duce adesea si la pierderea clientilor.