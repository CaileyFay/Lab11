Lab 11 - Grading the professor, Pt. 2
================
Insert your name here
Insert date here

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
```

    ## Warning: package 'infer' was built under R version 4.5.2

    ## Warning: package 'parsnip' was built under R version 4.5.2

    ## Warning: package 'rsample' was built under R version 4.5.2

``` r
library(openintro)
library(readxl)
library(titanic)

titanic3 <- read_excel("data/titanic3.xls",
    col_types = c("numeric", "numeric", "text",
        "text", "numeric", "numeric", "numeric",
        "text", "numeric", "text", "text",
        "text", "text", "text"))


data("titanic_train")
data("titanic_test")
```

## Exercise 1

There are 1309 observations and 14 variables in the Titanic3 dataset.

``` r
# Rename columns to lowercase
names(titanic3) <- names(titanic3) %>% tolower()
names(titanic_train) <- names(titanic_train) %>% tolower()
names(titanic_test) <- names(titanic_test) %>% tolower()

# Convert pclass to factor

titanic_train <- titanic_train %>%
  mutate(pclass_ord = factor(pclass, ordered = TRUE, levels = c(3, 2, 1)))

titanic_test <- titanic_test %>%
  mutate(pclass_ord = factor(pclass, ordered = TRUE, levels = c(3, 2, 1)))

titanic3 <- titanic3 %>%
  mutate(pclass_ord = factor(pclass, ordered = TRUE, levels = c(3, 2, 1)))

glimpse(titanic3)
```

    ## Rows: 1,309
    ## Columns: 15
    ## $ pclass     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ survived   <dbl> 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0,…
    ## $ name       <chr> "Allen, Miss. Elisabeth Walton", "Allison, Master. Hudson T…
    ## $ sex        <chr> "female", "male", "female", "male", "female", "male", "fema…
    ## $ age        <dbl> 29.0000, 0.9167, 2.0000, 30.0000, 25.0000, 48.0000, 63.0000…
    ## $ sibsp      <dbl> 0, 1, 1, 1, 1, 0, 1, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ parch      <dbl> 0, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,…
    ## $ ticket     <chr> "24160", "113781", "113781", "113781", "113781", "19952", "…
    ## $ fare       <dbl> 211.3375, 151.5500, 151.5500, 151.5500, 151.5500, 26.5500, …
    ## $ cabin      <chr> "B5", "C22 C26", "C22 C26", "C22 C26", "C22 C26", "E12", "D…
    ## $ embarked   <chr> "S", "S", "S", "S", "S", "S", "S", "S", "S", "C", "C", "C",…
    ## $ boat       <chr> "2", "11", NA, NA, NA, "3", "10", NA, "D", NA, NA, "4", "9"…
    ## $ body       <chr> NA, NA, NA, "135", NA, NA, NA, NA, NA, "22", "124", NA, NA,…
    ## $ home.dest  <chr> "St Louis, MO", "Montreal, PQ / Chesterville, ON", "Montrea…
    ## $ pclass_ord <ord> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…

``` r
summary(titanic3)
```

    ##      pclass         survived         name               sex           
    ##  Min.   :1.000   Min.   :0.000   Length:1309        Length:1309       
    ##  1st Qu.:2.000   1st Qu.:0.000   Class :character   Class :character  
    ##  Median :3.000   Median :0.000   Mode  :character   Mode  :character  
    ##  Mean   :2.295   Mean   :0.382                                        
    ##  3rd Qu.:3.000   3rd Qu.:1.000                                        
    ##  Max.   :3.000   Max.   :1.000                                        
    ##                                                                       
    ##       age              sibsp            parch          ticket         
    ##  Min.   : 0.1667   Min.   :0.0000   Min.   :0.000   Length:1309       
    ##  1st Qu.:21.0000   1st Qu.:0.0000   1st Qu.:0.000   Class :character  
    ##  Median :28.0000   Median :0.0000   Median :0.000   Mode  :character  
    ##  Mean   :29.8811   Mean   :0.4989   Mean   :0.385                     
    ##  3rd Qu.:39.0000   3rd Qu.:1.0000   3rd Qu.:0.000                     
    ##  Max.   :80.0000   Max.   :8.0000   Max.   :9.000                     
    ##  NA's   :263                                                          
    ##       fare            cabin             embarked             boat          
    ##  Min.   :  0.000   Length:1309        Length:1309        Length:1309       
    ##  1st Qu.:  7.896   Class :character   Class :character   Class :character  
    ##  Median : 14.454   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   : 33.295                                                           
    ##  3rd Qu.: 31.275                                                           
    ##  Max.   :512.329                                                           
    ##  NA's   :1                                                                 
    ##      body            home.dest         pclass_ord
    ##  Length:1309        Length:1309        3:709     
    ##  Class :character   Class :character   2:277     
    ##  Mode  :character   Mode  :character   1:323     
    ##                                                  
    ##                                                  
    ##                                                  
    ## 

## Exercise 2

*Provide your answer here.*  
Add code chunks as needed.

``` r
# Add your R code here
```

## Additional Exercises

*Repeat the format above for additional exercises.*
