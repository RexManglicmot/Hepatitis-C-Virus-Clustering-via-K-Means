Hepatitis C Virus K-Means Clustering
================
Rex Manglicmot

-   <a href="#status-continuing-working-document"
    id="toc-status-continuing-working-document">Status: Continuing Working
    Document</a>
-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#loading-the-libraries" id="toc-loading-the-libraries">Loading
    the Libraries</a>
-   <a href="#loading-the-data" id="toc-loading-the-data">Loading the
    Data</a>
-   <a href="#cleaning-the-data" id="toc-cleaning-the-data">Cleaning the
    Data</a>
-   <a href="#exploratory-data-analysis"
    id="toc-exploratory-data-analysis">Exploratory Data Analysis</a>
-   <a href="#k-means" id="toc-k-means">K-Means</a>
-   <a href="#limitations" id="toc-limitations">Limitations</a>
-   <a href="#conclusion" id="toc-conclusion">Conclusion</a>
-   <a href="#inspirationn-for-this-project"
    id="toc-inspirationn-for-this-project">Inspirationn for this project</a>

## Status: Continuing Working Document

## Introduction

The target attribute for classification is Category (blood donors
vs. Hepatitis C (including its progress (‘just’ Hepatitis C, Fibrosis,
Cirrhosis).

All attributes except Category and Sex are numerical. The laboratory
data are the attributes 5-14.

1)  X (Patient ID/No.)
2)  Category (diagnosis) (values: ‘0=Blood Donor’, ‘0s=suspect Blood
    Donor’, ‘1=Hepatitis’, ‘2=Fibrosis’, ‘3=Cirrhosis’)
3)  Age (in years)
4)  Sex (f,m)
5)  ALB
6)  ALP
7)  ALT
8)  AST
9)  BIL
10) CHE
11) CHOL
12) CREA
13) GGT
14) PROT

## Loading the Libraries

``` r
library(tidyverse)
```

## Loading the Data

``` r
#laod data into an url object
url <- ('https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv')

#put data into an object
data_orig <- read.csv(url)

#view strucuture
str(data_orig)
```

    ## 'data.frame':    615 obs. of  14 variables:
    ##  $ X       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Category: chr  "0=Blood Donor" "0=Blood Donor" "0=Blood Donor" "0=Blood Donor" ...
    ##  $ Age     : int  32 32 32 32 32 32 32 32 32 32 ...
    ##  $ Sex     : chr  "m" "m" "m" "m" ...
    ##  $ ALB     : num  38.5 38.5 46.9 43.2 39.2 41.6 46.3 42.2 50.9 42.4 ...
    ##  $ ALP     : num  52.5 70.3 74.7 52 74.1 43.3 41.3 41.9 65.5 86.3 ...
    ##  $ ALT     : num  7.7 18 36.2 30.6 32.6 18.5 17.5 35.8 23.2 20.3 ...
    ##  $ AST     : num  22.1 24.7 52.6 22.6 24.8 19.7 17.8 31.1 21.2 20 ...
    ##  $ BIL     : num  7.5 3.9 6.1 18.9 9.6 12.3 8.5 16.1 6.9 35.2 ...
    ##  $ CHE     : num  6.93 11.17 8.84 7.33 9.15 ...
    ##  $ CHOL    : num  3.23 4.8 5.2 4.74 4.32 6.05 4.79 4.6 4.1 4.45 ...
    ##  $ CREA    : num  106 74 86 80 76 111 70 109 83 81 ...
    ##  $ GGT     : num  12.1 15.6 33.2 33.8 29.9 91 16.9 21.5 13.7 15.9 ...
    ##  $ PROT    : num  69 76.5 79.3 75.7 68.7 74 74.5 67.1 71.3 69.9 ...

## Cleaning the Data

``` r
#make copy for cleaning
data <- data_orig

#view any NAs
sum(is.na(data))
```

    ## [1] 31

``` r
#which rows have NAs
which(is.na(data), arr.ind = TRUE)
```

    ##       row col
    ##  [1,] 604   5
    ##  [2,] 542   6
    ##  [3,] 546   6
    ##  [4,] 547   6
    ##  [5,] 569   6
    ##  [6,] 570   6
    ##  [7,] 571   6
    ##  [8,] 572   6
    ##  [9,] 577   6
    ## [10,] 582   6
    ## [11,] 583   6
    ## [12,] 584   6
    ## [13,] 585   6
    ## [14,] 586   6
    ## [15,] 591   6
    ## [16,] 593   6
    ## [17,] 604   6
    ## [18,] 614   6
    ## [19,] 615   6
    ## [20,] 541   7
    ## [21,] 122  11
    ## [22,] 320  11
    ## [23,] 330  11
    ## [24,] 414  11
    ## [25,] 425  11
    ## [26,] 434  11
    ## [27,] 499  11
    ## [28,] 585  11
    ## [29,] 591  11
    ## [30,] 604  11
    ## [31,] 591  14

``` r
#clean data all in one go
data <- data %>%
  #get rid of variables that are not needed for Kmeans via dplyr
  select(-c(X, Category, Sex)) %>%
  #rename columns to lower case
  rename_all(tolower) %>%
  #deletes NAs
  drop_na()
```

``` r
#check data for any NA's or characters
summary(data)
```

    ##       age             alb             alp              alt        
    ##  Min.   :23.00   Min.   :14.90   Min.   : 11.30   Min.   :  0.90  
    ##  1st Qu.:39.00   1st Qu.:38.80   1st Qu.: 52.50   1st Qu.: 16.40  
    ##  Median :47.00   Median :41.90   Median : 66.20   Median : 22.70  
    ##  Mean   :47.42   Mean   :41.62   Mean   : 68.12   Mean   : 26.58  
    ##  3rd Qu.:54.00   3rd Qu.:45.10   3rd Qu.: 79.90   3rd Qu.: 31.90  
    ##  Max.   :77.00   Max.   :82.20   Max.   :416.60   Max.   :325.30  
    ##       ast              bil              che              chol      
    ##  Min.   : 10.60   Min.   :  0.80   Min.   : 1.420   Min.   :1.430  
    ##  1st Qu.: 21.50   1st Qu.:  5.20   1st Qu.: 6.930   1st Qu.:4.620  
    ##  Median : 25.70   Median :  7.10   Median : 8.260   Median :5.310  
    ##  Mean   : 33.77   Mean   : 11.02   Mean   : 8.204   Mean   :5.391  
    ##  3rd Qu.: 31.70   3rd Qu.: 11.00   3rd Qu.: 9.570   3rd Qu.:6.080  
    ##  Max.   :324.00   Max.   :209.00   Max.   :16.410   Max.   :9.670  
    ##       crea              ggt             prot      
    ##  Min.   :   8.00   Min.   :  4.5   Min.   :44.80  
    ##  1st Qu.:  68.00   1st Qu.: 15.6   1st Qu.:69.30  
    ##  Median :  77.00   Median : 22.8   Median :72.10  
    ##  Mean   :  81.67   Mean   : 38.2   Mean   :71.89  
    ##  3rd Qu.:  89.00   3rd Qu.: 37.6   3rd Qu.:75.20  
    ##  Max.   :1079.10   Max.   :650.9   Max.   :86.50

``` r
#check the class. We need quant variables
lapply(data,class)
```

    ## $age
    ## [1] "integer"
    ## 
    ## $alb
    ## [1] "numeric"
    ## 
    ## $alp
    ## [1] "numeric"
    ## 
    ## $alt
    ## [1] "numeric"
    ## 
    ## $ast
    ## [1] "numeric"
    ## 
    ## $bil
    ## [1] "numeric"
    ## 
    ## $che
    ## [1] "numeric"
    ## 
    ## $chol
    ## [1] "numeric"
    ## 
    ## $crea
    ## [1] "numeric"
    ## 
    ## $ggt
    ## [1] "numeric"
    ## 
    ## $prot
    ## [1] "numeric"

Let’s Explore

## Exploratory Data Analysis

## K-Means

## Limitations

## Conclusion

## Inspirationn for this project
