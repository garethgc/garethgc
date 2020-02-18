Assignment 1
================
Gareth
2/16/2020

## Setup of data and libraries

``` r
library(tidyverse)
library(lubridate)
library(e1071)
library(forcats)
library(skimr)
library(scales)
```

``` r
sales <- readRDS(here::here("data/sales.rds"))
```

``` r
# from https://stackoverflow.com/a/25635740
manual_mode <- function(x, na.rm = FALSE) { # we don't use 'mode' as a function name because it already exists
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
```

## Part 1: Summarizing the data

``` r
summary(sales)
```

    ##      month                town                      flat_type    
    ##  Min.   :2015-01-01   Length:79100       3 ROOM          :20187  
    ##  1st Qu.:2016-02-01   Class :character   4 ROOM          :32476  
    ##  Median :2017-03-01   Mode  :character   5 ROOM          :19189  
    ##  Mean   :2017-01-19                      2 ROOM          :  855  
    ##  3rd Qu.:2018-02-01                      EXECUTIVE       : 6337  
    ##  Max.   :2018-12-01                      1 ROOM          :   32  
    ##                                          MULTI-GENERATION:   24  
    ##     block           street_name          storey_range   floor_area_sqm  
    ##  Length:79100       Length:79100       04 TO 06:18618   Min.   : 31.00  
    ##  Class :character   Class :character   07 TO 09:17223   1st Qu.: 76.00  
    ##  Mode  :character   Mode  :character   10 TO 12:15086   Median : 96.00  
    ##                                        01 TO 03:14456   Mean   : 97.59  
    ##                                        13 TO 15: 7078   3rd Qu.:112.00  
    ##                                        16 TO 18: 2994   Max.   :280.00  
    ##                                        (Other) : 3645                   
    ##              flat_model    lease_commence_date remaining_lease
    ##  Model A          :24184   Min.   :1966        Min.   :47.00  
    ##  Improved         :20017   1st Qu.:1984        1st Qu.:66.00  
    ##  New Generation   :12325   Median :1992        Median :73.00  
    ##  Premium Apartment: 8250   Mean   :1992        Mean   :74.03  
    ##  Simplified       : 3692   3rd Qu.:2001        3rd Qu.:83.00  
    ##  Apartment        : 3231   Max.   :2016        Max.   :97.00  
    ##  (Other)          : 7401                                      
    ##   resale_price    
    ##  Min.   : 160000  
    ##  1st Qu.: 338000  
    ##  Median : 408888  
    ##  Mean   : 439794  
    ##  3rd Qu.: 505000  
    ##  Max.   :1185000  
    ## 

The summary table above shows the central tendencies across the multiple
sets of variables within the data, both continuous and discrete.

### Summary of Floor Area sq/m

``` r
sales %>% 
  group_by(town) %>% 
  skim(floor_area_sqm)
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 79100      |
| Number of columns                                | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | town       |

Data summary

**Variable type:
numeric**

| skim\_variable   | town            | n\_missing | complete\_rate |   mean |    sd | p0 |   p25 | p50 |    p75 | p100 | hist  |
| :--------------- | :-------------- | ---------: | -------------: | -----: | ----: | -: | ----: | --: | -----: | ---: | :---- |
| floor\_area\_sqm | ANG MO KIO      |          0 |              1 |  83.85 | 20.59 | 44 |  68.0 |  75 |  92.00 |  178 | ▇▇▃▁▁ |
| floor\_area\_sqm | BEDOK           |          0 |              1 |  88.58 | 24.50 | 44 |  67.0 |  84 | 105.00 |  176 | ▇▇▅▂▁ |
| floor\_area\_sqm | BISHAN          |          0 |              1 | 106.59 | 23.97 | 63 |  85.5 | 105 | 121.00 |  199 | ▇▇▇▂▁ |
| floor\_area\_sqm | BUKIT BATOK     |          0 |              1 |  92.74 | 24.82 | 59 |  69.0 |  92 | 104.00 |  165 | ▇▅▅▁▂ |
| floor\_area\_sqm | BUKIT MERAH     |          0 |              1 |  86.11 | 23.01 | 31 |  67.0 |  88 | 100.00 |  157 | ▁▇▇▅▁ |
| floor\_area\_sqm | BUKIT PANJANG   |          0 |              1 | 104.57 | 19.31 | 47 |  91.0 | 103 | 120.00 |  155 | ▁▃▇▃▁ |
| floor\_area\_sqm | BUKIT TIMAH     |          0 |              1 | 110.33 | 26.28 | 63 |  91.0 | 104 | 128.00 |  154 | ▆▃▇▇▇ |
| floor\_area\_sqm | CENTRAL AREA    |          0 |              1 |  82.98 | 17.83 | 41 |  67.0 |  85 |  95.00 |  138 | ▃▅▇▃▁ |
| floor\_area\_sqm | CHOA CHU KANG   |          0 |              1 | 110.73 | 18.70 | 46 | 103.0 | 108 | 122.25 |  215 | ▁▇▅▁▁ |
| floor\_area\_sqm | CLEMENTI        |          0 |              1 |  84.85 | 21.52 | 44 |  67.0 |  82 |  92.00 |  163 | ▇▇▅▂▁ |
| floor\_area\_sqm | GEYLANG         |          0 |              1 |  82.63 | 26.28 | 40 |  60.0 |  82 |  95.00 |  160 | ▇▆▆▂▁ |
| floor\_area\_sqm | HOUGANG         |          0 |              1 | 101.65 | 24.98 | 47 |  84.0 | 103 | 120.00 |  177 | ▃▅▇▂▁ |
| floor\_area\_sqm | JURONG EAST     |          0 |              1 |  96.31 | 25.72 | 43 |  71.0 |  94 | 120.00 |  173 | ▆▇▇▃▁ |
| floor\_area\_sqm | JURONG WEST     |          0 |              1 | 101.74 | 22.33 | 37 |  90.0 | 104 | 112.00 |  159 | ▁▃▇▅▁ |
| floor\_area\_sqm | KALLANG/WHAMPOA |          0 |              1 |  85.76 | 24.83 | 42 |  65.0 |  84 | 103.00 |  280 | ▇▆▁▁▁ |
| floor\_area\_sqm | MARINE PARADE   |          0 |              1 |  86.05 | 22.02 | 42 |  65.0 |  82 |  88.00 |  147 | ▁▇▃▃▁ |
| floor\_area\_sqm | PASIR RIS       |          0 |              1 | 123.09 | 18.17 | 55 | 105.0 | 123 | 143.00 |  190 | ▁▇▇▆▁ |
| floor\_area\_sqm | PUNGGOL         |          0 |              1 |  99.04 | 13.63 | 46 |  92.0 |  94 | 110.00 |  149 | ▁▁▇▆▁ |
| floor\_area\_sqm | QUEENSTOWN      |          0 |              1 |  81.99 | 22.00 | 41 |  65.0 |  76 |  95.00 |  156 | ▃▇▆▂▁ |
| floor\_area\_sqm | SEMBAWANG       |          0 |              1 | 106.97 | 15.85 | 40 |  93.0 | 110 | 115.00 |  143 | ▁▁▇▇▃ |
| floor\_area\_sqm | SENGKANG        |          0 |              1 | 101.31 | 16.72 | 44 |  91.0 |  96 | 110.00 |  145 | ▁▁▇▆▂ |
| floor\_area\_sqm | SERANGOON       |          0 |              1 | 100.30 | 27.18 | 44 |  83.0 |  99 | 121.00 |  165 | ▆▇▇▅▅ |
| floor\_area\_sqm | TAMPINES        |          0 |              1 | 105.28 | 23.96 | 45 |  84.0 | 104 | 122.00 |  168 | ▂▆▇▆▂ |
| floor\_area\_sqm | TOA PAYOH       |          0 |              1 |  85.66 | 24.24 | 38 |  67.0 |  77 | 104.00 |  166 | ▁▇▃▂▁ |
| floor\_area\_sqm | WOODLANDS       |          0 |              1 | 107.43 | 24.14 | 45 |  92.0 | 103 | 121.00 |  192 | ▂▇▇▂▁ |
| floor\_area\_sqm | YISHUN          |          0 |              1 |  92.26 | 22.97 | 40 |  74.0 |  90 | 104.00 |  181 | ▅▇▆▁▁ |

Going deeper into the variables, the size of resale flats are better
summarised across towns, regardless of flat type, to attain a
distribution via skim. As it can be seen, with reference to the towns,
there are ranges of distributions, with most being left skewed. This
would mean that the make-out of resale flats transacted during 2015 to
2018 were mostly, middle to small flats.

### Closer look into Flat size distribution

``` r
ggplot(sales, aes(x = floor_area_sqm)) + 
  geom_histogram(binwidth = 10)
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Overall distribution for flat sizes show a relatively normal
distribution, indicative of the large numer of samples provided, as
compared to the earlier skim which looked at individual towns.

### Normal distribution curve for flat sizes

``` r
ggplot(sales, aes(x = floor_area_sqm)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  stat_function(fun = dnorm, args = list(mean = mean(sales$floor_area_sqm), sd = sd(sales$floor_area_sqm)))
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Summary of Resale prices

``` r
sales %>% 
  group_by(town) %>% 
skim(resale_price)
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 79100      |
| Number of columns                                | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | town       |

Data summary

**Variable type:
numeric**

| skim\_variable | town            | n\_missing | complete\_rate |     mean |        sd |     p0 |      p25 |    p50 |    p75 |    p100 | hist  |
| :------------- | :-------------- | ---------: | -------------: | -------: | --------: | -----: | -------: | -----: | -----: | ------: | :---- |
| resale\_price  | ANG MO KIO      |          0 |              1 | 416431.0 | 158873.99 | 185000 | 300002.2 | 355000 | 480000 |  980000 | ▇▅▂▁▁ |
| resale\_price  | BEDOK           |          0 |              1 | 412097.6 | 137080.67 | 190000 | 303000.0 | 375000 | 490000 |  935000 | ▇▇▃▂▁ |
| resale\_price  | BISHAN          |          0 |              1 | 630955.3 | 177020.68 | 239000 | 510000.0 | 610000 | 756444 | 1180000 | ▂▇▆▃▁ |
| resale\_price  | BUKIT BATOK     |          0 |              1 | 384389.6 | 120810.66 | 210000 | 283000.0 | 360000 | 443000 |  890000 | ▇▆▂▁▁ |
| resale\_price  | BUKIT MERAH     |          0 |              1 | 555016.8 | 195955.09 | 168000 | 368000.0 | 570000 | 724000 | 1150000 | ▆▆▇▅▁ |
| resale\_price  | BUKIT PANJANG   |          0 |              1 | 420138.4 | 105791.04 | 200000 | 340000.0 | 400000 | 476000 |  840000 | ▃▇▃▁▁ |
| resale\_price  | BUKIT TIMAH     |          0 |              1 | 711795.2 | 188986.76 | 320000 | 591250.0 | 715944 | 875000 | 1058000 | ▅▃▇▇▅ |
| resale\_price  | CENTRAL AREA    |          0 |              1 | 653313.3 | 244789.54 | 210000 | 430000.0 | 600000 | 890000 | 1168000 | ▅▇▂▇▂ |
| resale\_price  | CHOA CHU KANG   |          0 |              1 | 390059.2 |  78115.99 | 208000 | 338000.0 | 370000 | 425000 |  900000 | ▅▇▂▁▁ |
| resale\_price  | CLEMENTI        |          0 |              1 | 459451.4 | 171497.65 | 220000 | 325000.0 | 400000 | 564750 | 1068000 | ▇▅▃▁▁ |
| resale\_price  | GEYLANG         |          0 |              1 | 428974.3 | 180173.43 | 160000 | 280000.0 | 375000 | 566000 |  998000 | ▇▆▃▂▁ |
| resale\_price  | HOUGANG         |          0 |              1 | 426292.7 | 127262.12 | 208000 | 338000.0 | 398000 | 480000 | 1000000 | ▆▇▂▁▁ |
| resale\_price  | JURONG EAST     |          0 |              1 | 424409.3 | 140867.13 | 200000 | 305000.0 | 393000 | 515000 |  900000 | ▇▇▅▂▁ |
| resale\_price  | JURONG WEST     |          0 |              1 | 397135.2 |  95285.98 | 175000 | 330000.0 | 392000 | 460000 |  790000 | ▃▇▆▁▁ |
| resale\_price  | KALLANG/WHAMPOA |          0 |              1 | 494093.4 | 194536.56 | 195000 | 325000.0 | 454888 | 645000 | 1185000 | ▇▅▅▂▁ |
| resale\_price  | MARINE PARADE   |          0 |              1 | 540409.4 | 176761.02 | 180000 | 402000.0 | 483888 | 618000 |  950000 | ▁▇▃▂▂ |
| resale\_price  | PASIR RIS       |          0 |              1 | 483993.6 |  95818.30 | 220000 | 410000.0 | 460000 | 540000 |  808000 | ▁▇▆▂▁ |
| resale\_price  | PUNGGOL         |          0 |              1 | 449492.3 |  68701.38 | 243000 | 408000.0 | 440000 | 485000 |  870000 | ▁▇▂▁▁ |
| resale\_price  | QUEENSTOWN      |          0 |              1 | 549037.4 | 211832.30 | 185000 | 351500.0 | 528444 | 730000 | 1160000 | ▇▅▇▃▁ |
| resale\_price  | SEMBAWANG       |          0 |              1 | 397067.9 |  58446.19 | 215000 | 355000.0 | 388000 | 435000 |  590000 | ▁▆▇▃▁ |
| resale\_price  | SENGKANG        |          0 |              1 | 434891.6 |  70541.64 | 196000 | 385000.0 | 428888 | 475000 |  690000 | ▁▅▇▃▁ |
| resale\_price  | SERANGOON       |          0 |              1 | 484239.8 | 153876.14 | 170000 | 365000.0 | 460000 | 585000 | 1000000 | ▃▇▅▂▁ |
| resale\_price  | TAMPINES        |          0 |              1 | 466150.2 | 118829.93 | 250000 | 380000.0 | 445000 | 530000 |  990000 | ▅▇▂▁▁ |
| resale\_price  | TOA PAYOH       |          0 |              1 | 480215.7 | 217834.09 | 182000 | 300000.0 | 390000 | 650000 | 1160888 | ▇▃▃▂▁ |
| resale\_price  | WOODLANDS       |          0 |              1 | 388467.0 |  96693.06 | 180000 | 328000.0 | 375000 | 423000 |  855000 | ▂▇▂▁▁ |
| resale\_price  | YISHUN          |          0 |              1 | 357607.9 |  89600.31 | 195000 | 295000.0 | 335444 | 400000 |  868000 | ▇▇▂▁▁ |

Similar to the flat sizes, resale prices across the towns have diverse
distributions, with most tending towards the left. This is probably
explained in parallel to the flat sizes sold, which naturally would
imply lower prices.

### Closer look into Resale price distribution

``` r
ggplot(sales, aes(x = resale_price)) + 
  geom_histogram(binwidth = 50000)
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

However, looking at the overall distribution of resale prices, it is
clearly seen that it is left skewed, with much of the transactions
valueing towards the lower end.

### Distribution Curve for Resale Prices

``` r
ggplot(sales, aes(x = resale_price)) + 
  geom_histogram(aes(y = ..density..), binwidth = 50000) +
  stat_function(fun = dnorm, args = list(mean = mean(sales$resale_price), sd = sd(sales$resale_price)))
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Summary of Remaining Lease

``` r
sales %>% 
  group_by(town) %>% 
skim(remaining_lease)
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 79100      |
| Number of columns                                | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | town       |

Data summary

**Variable type:
numeric**

| skim\_variable     | town              |   n\_missing |   complete\_rate |     mean |     sd |   p0 |    p25 |    p50 |    p75 |   p100 | hist                                                                                                                                      |
| :----------------- | :---------------- | -----------: | ---------------: | -------: | -----: | ---: | -----: | -----: | -----: | -----: | :---------------------------------------------------------------------------------------------------------------------------------------- |
| remaining\_lease   | ANG MO KIO        |            0 |                1 |    65.37 |   9.76 |   56 |     60 |     62 |     64 |     96 | ▇▁▁▁▁                                                                                                                                     |
| remaining\_lease   | BEDOK             |            0 |                1 |    65.44 |   8.29 |   52 |     60 |     62 |     68 |     94 | ▅▇▂▁▁                                                                                                                                     |
| remaining\_lease   | BISHAN            |            0 |                1 |    71.84 |   6.97 |   54 |     68 |     71 |     74 |     93 | ▁▆▇▁▁                                                                                                                                     |
| remaining\_lease   | BUKIT BATOK       |            0 |                1 |    70.87 |   6.41 |   63 |     67 |     68 |     71 |     89 | ▇▃▁▁▁                                                                                                                                     |
| remaining\_lease   | BUKIT MERAH       |            0 |                1 |    71.63 |  14.93 |   47 |     58 |     67 |     86 |     97 | ▆▇▁▇▅                                                                                                                                     |
| remaining\_lease   | BUKIT PANJANG     |            0 |                1 |    78.94 |   7.27 |   67 |     71 |     80 |     84 |     95 | ▇▁▇▅▂                                                                                                                                     |
| remaining\_lease   | BUKIT TIMAH       |            0 |                1 |    65.49 |   6.88 |   54 |     57 |     69 |     71 |     73 | ▅▁▁▃▇                                                                                                                                     |
| remaining\_lease   | CENTRAL AREA      |            0 |                1 |    75.34 |  15.83 |   51 |     61 |     67 |     93 |     95 | ▃▆▁▁▇                                                                                                                                     |
| remaining\_lease   | CHOA CHU KANG     |            0 |                1 |    77.61 |   6.50 |   58 |     74 |     78 |     82 |     95 | ▁▂▇▅▁                                                                                                                                     |
| remaining\_lease   | CLEMENTI          |            0 |                1 |    66.33 |  10.81 |   58 |     61 |     62 |     65 |     96 | ▇▁▁▁▁                                                                                                                                     |
| remaining\_lease   | GEYLANG           |            0 |                1 |    64.54 |  12.28 |   47 |     55 |     63 |     69 |     95 | ▆▇▃▂▂                                                                                                                                     |
| remaining\_lease   | HOUGANG           |            0 |                1 |    72.11 |   7.46 |   55 |     67 |     70 |     79 |     95 | ▁▇▃▃▁                                                                                                                                     |
| remaining\_lease   | JURONG EAST       |            0 |                1 |    70.00 |   9.67 |   47 |     64 |     66 |     79 |     96 | ▁▇▂▃▁                                                                                                                                     |
| remaining\_lease   | JURONG WEST       |            0 |                1 |    75.79 |   9.80 |   51 |     68 |     78 |     83 |     96 | ▂▅▅▇▂                                                                                                                                     |
| remaining\_lease   | KALLANG/WHAMPOA   |            0 |                1 |    68.38 |  13.90 |   49 |     56 |     64 |     81 |     96 | ▇▆▂▃▅                                                                                                                                     |
| remaining\_lease   | MARINE PARADE     |            0 |                1 |    57.02 |   1.36 |   55 |     56 |     57 |     58 |     61 | ▇▅▅▂▁                                                                                                                                     |
| remaining\_lease   | PASIR RIS         |            0 |                1 |    75.09 |   2.99 |   60 |     73 |     76 |     77 |     80 | ▁▁▃▇▆                                                                                                                                     |
| remaining\_lease   | PUNGGOL           |            0 |                1 |    90.21 |   4.33 |   82 |     86 |     93 |     94 |     95 | ▂▅▁▁▇                                                                                                                                     |
| remaining\_lease   | QUEENSTOWN        |            0 |                1 |    70.23 |  17.93 |   47 |     54 |     60 |     90 |     96 | ▇▅▁▃▇                                                                                                                                     |
| remaining\_lease   | SEMBAWANG         |            0 |                1 |    83.63 |   3.04 |   78 |     82 |     83 |     85 |     95 | ▃▇▃▁▁                                                                                                                                     |
| remaining\_lease   | SENGKANG          |            0 |                1 |    86.72 |   5.02 |   79 |     83 |     85 |     92 |     96 | ▆▇▃▂▆                                                                                                                                     |
| remaining\_lease   | SERANGOON         |            0 |                1 |    70.01 |   4.98 |   58 |     67 |     69 |     73 |     83 | ▁▇▅▂▂                                                                                                                                     |
| remaining\_lease   | TAMPINES          |            0 |                1 |    71.82 |   6.49 |   62 |     67 |     70 |     77 |     95 | ▇▆▅▁▁                                                                                                                                     |
| remaining\_lease   | TOA PAYOH         |            0 |                1 |    66.36 |  14.80 |   47 |     53 |     64 |     79 |     93 | ▇▃▃▃▃                                                                                                                                     |
| remaining\_lease   | WOODLANDS         |            0 |                1 |    77.60 |   7.80 |   54 |     76 |     79 |     81 |     95 | ▁▂▃▇▁                                                                                                                                     |
| remaining\_lease   | YISHUN            |            0 |                1 |    71.17 |   7.98 |   58 |     67 |     69 |     71 |     96 | ▁▇▁▁▁                                                                                                                                     |
| Looking at remaini | ng years of lease | for flats, i | t can be seen th | at there | is not | much | a tren | d acro | ss the | towns. | This could be due to the rapid national development in Singapore, which resulted in many towns having HDB’s produced during the same era. |

### Closer look into remaining lease distribution

``` r
ggplot(sales, aes(x = remaining_lease)) + 
  geom_histogram(binwidth = 10)
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

From the remaining lease distribution throughout the resale flats, a
normal distribution can clearly be seen. Most of the flats range between
60-80 years remaining, showing that most of the resale market has been
participated by newer estates and flats. This could be due to younger
families upgrading from smaller models to bigger ones as their family
size increases.

### Normal Distribution curve for remaining lease

``` r
ggplot(sales, aes(x = remaining_lease)) + 
  geom_histogram(aes(y = ..density..), binwidth = 10) +
  stat_function(fun = dnorm, args = list(mean = mean(sales$remaining_lease), sd = sd(sales$remaining_lease)))
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Summary of Flat-types

``` r
sales_ordered<-sales %>%
  mutate(flattype=factor(flat_type,levels = c("1 ROOM","2 ROOM","3 ROOM","4 ROOM","5 ROOM","EXECUTIVE","MULTI-GENERATION"),ordered = T))
  

skim(sales_ordered$flattype)
```

|                                                  |                         |
| :----------------------------------------------- | :---------------------- |
| Name                                             | sales\_ordered$flattype |
| Number of rows                                   | 79100                   |
| Number of columns                                | 1                       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                         |
| Column type frequency:                           |                         |
| factor                                           | 1                       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                         |
| Group variables                                  | None                    |

Data summary

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                   |
| :------------- | ---------: | -------------: | :------ | --------: | :-------------------------------------------- |
| data           |          0 |              1 | TRUE    |         7 | 4 R: 32476, 3 R: 20187, 5 R: 19189, EXE: 6337 |

``` r
sales_ordered %>% 
  ggplot(aes(x = flattype)) + 
  geom_bar()
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Most of the flats are made up of 4 room units, which is the middle
ground for most of the Singaporean populace, as affordable and easily
upgradeable especially among growing families.

### Summary of Township

``` r
skim(sales$town)
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | sales$town |
| Number of rows                                   | 79100      |
| Number of columns                                | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                |
| :------------- | ---------: | -------------: | :------ | --------: | :----------------------------------------- |
| data           |          0 |              1 | FALSE   |        26 | JUR: 6125, WOO: 5791, SEN: 5779, TAM: 5320 |

``` r
sales %>% 
  ggplot(aes(x = town)) + 
  theme(axis.text.x = element_text(angle = 90,size = 6,vjust = 0.5))+
  labs(y="No. of HDB units", x="Neighbourhoods")+
  geom_bar()
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-17-1.png)<!-- --> Resale
flats transacted between 2015 to 2018 have been seen to be popular among
peripheral locations such as Woodlands, Sengkang and Jurong West. These
3 towns have also experienced significant reimagination of their
neighbourhoods over these years, indicating that the influx of sales of
flats could be due to the effectiveness of development of these estates.

### Summary of flat models

``` r
skim(sales$flat_model)
```

|                                                  |                   |
| :----------------------------------------------- | :---------------- |
| Name                                             | sales$flat\_model |
| Number of rows                                   | 79100             |
| Number of columns                                | 1                 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                   |
| Column type frequency:                           |                   |
| factor                                           | 1                 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                   |
| Group variables                                  | None              |

Data summary

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                   |
| :------------- | ---------: | -------------: | :------ | --------: | :-------------------------------------------- |
| data           |          0 |              1 | FALSE   |        21 | Mod: 24184, Imp: 20017, New: 12325, Pre: 8250 |

``` r
sales %>% 
  ggplot(aes(x = flat_model,fill=flat_model)) + 
   theme(axis.text.x = element_text(angle = 90,size = 6,vjust = 0.5))+
  theme(legend.position = "none")+
  geom_bar()+
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2))
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-19-1.png)<!-- --> \#\#\#
Summary of Storey
Ranges

``` r
skim(sales$storey_range)
```

|                                                  |                     |
| :----------------------------------------------- | :------------------ |
| Name                                             | sales$storey\_range |
| Number of rows                                   | 79100               |
| Number of columns                                | 1                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                     |
| Column type frequency:                           |                     |
| factor                                           | 1                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                     |
| Group variables                                  | None                |

Data summary

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                    |
| :------------- | ---------: | -------------: | :------ | --------: | :--------------------------------------------- |
| data           |          0 |              1 | FALSE   |        17 | 04 : 18618, 07 : 17223, 10 : 15086, 01 : 14456 |

``` r
sales_ordered<-sales_ordered %>% 
  mutate(storey=factor(storey_range, levels = c("01 TO 03","04 TO 06","07 TO 09","10 TO 12","13 TO 15","16 TO 18","19 TO 21","22 TO 24","25 TO 27","28 TO 30","31 TO 33","34 TO 36","37 TO 39","40 TO 42","43 TO 45","46 TO 48","49 TO 51"), ordered = T))
  


sales_ordered %>% 
  ggplot(aes(x=storey,fill=storey)) +
  theme(axis.text.x = element_text(angle = 90,size = 6,vjust = 0.5))+
  theme(legend.position = "none")+
  geom_bar()
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Most of the resale market seems to involve the lower tier blocks, which
is appropriate as most of the high rise HDBs are relatively new in the
market. Also, the 3 prominent towns (Jurong West, Woodlands and
SengKang) do not have high rise HDBs, due to flight restrictions of
nearby airspace. This could probably explain the distribution observed
above.

## Part 2: Analysis of HDB Resale Variables

### Comparison of Flat-types transacted between Hougang and Bedok

``` r
sales_ordered %>% 
  filter(town==c("HOUGANG","BEDOK")) %>%
  ggplot(aes(x=flat_type,fill=flat_type)) +
  theme(axis.text.x = element_text(angle = 90,size = 6,vjust = 0.5))+
  geom_bar()+
  facet_wrap(vars(town),scales = "fixed")
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

As Hougang and Bedok were developed roughly during the same period, it
is interesting to study how the flats fare in such mature townships. It
can be seen that Bedok has experienced much more resale action as
compared to Hougang, especially in the 3-Room category. The data does
not provide a way to normalise the findings to the existing volume of
flats, however, if by going through these tables, it can be seen that
Bedok has a rise in popularity as compared to Hougang, assuming this is
from the rise of demand. Bedok has seen increased development of
amenities, especially Bedok Hub whereas Hougang has not seen much as
compared to them, probably due to political
differences.

### Analysis between Unit Size and Resale prices for different regional neighbourhoods (Ang Mo Kio, Jurong East, Tampines, Woodlands)

``` r
sales_ordered %>% 
  filter(town==c("ANG MO KIO","JURONG EAST","TAMPINES","WOODLANDS")) %>%
  ggplot(aes(x=floor_area_sqm,y=resale_price, color=town,fill=town))+
  geom_violin()+
  scale_y_continuous(labels=comma)+
  labs(y="Resale Price ($SGD)", x="Floor Area (Sq/M)")+
  facet_wrap(vars(town),scales = "fixed")
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

Ang Mo Kio, Tampines, Jurong East and Woodlands were chosen next as they
represent regional centres that the URA had set apart from the master
plans of the past. These towns also comprise of diverse land use, not
only residential, but commercial and industrial, allowing for less
commute among workers, giving rise to demand for their housing units.
Comparing across in reference to resale price, it can be seen that Ang
Mo Kio has been enjoying a much higher value in their residential units.
Eventhough Woodlands seems to offer large flats, Ang Mo Kio seems to
paying off due to its proximity to the traditional city centre (CBD).All
4 towns do show similar violin shapes, with heavy bottoms indicating the
transaction of relatively lower valued units that do not necessarily
mean smaller units.

### Resale value of HDB flats between the 4 regional centres

``` r
sales_ordered %>%
  filter(town==c("ANG MO KIO","JURONG EAST","TAMPINES","WOODLANDS")) %>%
  ggplot(aes(x=resale_price,fill=flat_type))+
  geom_histogram(aes(y=..density..),binwidth=10000)+
  scale_x_continuous(labels=comma)+
  stat_function(fun = dnorm, args = list(mean = mean(sales$resale_price), sd = sd(sales$resale_price)))+
  facet_wrap(vars(town),scales = "free_y")
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
Further looking into the 4 towns, with flat type in consideration with
the normal distribution curve, it can be seen that the curves has rather
low kurtosis, mostly skewing to the left. Follwing up from the previous
graphs, most of the resale movement seems to ve involving 2 room and 3
room flats, with Ang Mo Kio, Jurong East and Woodlands displaying near
outliers with high proportions of 2 room flats sold. This could be due
the rise of immigration and Permanent Residents, choosing to move out
from the city centre to these towns where the prices of the flats are
relatively
cheaper.

## Part 3: Analysis of distribution of towns and flat types with resale prices

### Comparing resale price of 4 Room flats across towns

``` r
sales_ordered %>% 
  group_by(town,flat_type) %>%
  filter(flat_type=="4 ROOM") %>% 
  skim(resale_price)
```

|                                                  |                  |
| :----------------------------------------------- | :--------------- |
| Name                                             | Piped data       |
| Number of rows                                   | 32476            |
| Number of columns                                | 13               |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                  |
| Column type frequency:                           |                  |
| numeric                                          | 1                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                  |
| Group variables                                  | town, flat\_type |

Data summary

**Variable type:
numeric**

| skim\_variable | town            | flat\_type | n\_missing | complete\_rate |     mean |        sd |     p0 |    p25 |    p50 |    p75 |    p100 | hist  |
| :------------- | :-------------- | :--------- | ---------: | -------------: | -------: | --------: | -----: | -----: | -----: | -----: | ------: | :---- |
| resale\_price  | ANG MO KIO      | 4 ROOM     |          0 |              1 | 486002.7 |  94047.97 | 300000 | 420000 | 458400 | 522250 |  782000 | ▂▇▂▂▁ |
| resale\_price  | BEDOK           | 4 ROOM     |          0 |              1 | 434288.4 |  79024.67 | 300000 | 380000 | 410000 | 460000 |  740000 | ▅▇▂▁▁ |
| resale\_price  | BISHAN          | 4 ROOM     |          0 |              1 | 562721.2 |  89309.65 | 358000 | 506888 | 558000 | 609000 |  835000 | ▂▇▇▂▁ |
| resale\_price  | BUKIT BATOK     | 4 ROOM     |          0 |              1 | 395450.1 |  48920.00 | 270888 | 360000 | 397500 | 428000 |  590000 | ▂▇▇▁▁ |
| resale\_price  | BUKIT MERAH     | 4 ROOM     |          0 |              1 | 643622.4 | 120701.41 | 360000 | 550000 | 650000 | 745000 | 1018000 | ▃▇▇▅▁ |
| resale\_price  | BUKIT PANJANG   | 4 ROOM     |          0 |              1 | 375084.9 |  55095.77 | 270000 | 335000 | 360000 | 407500 |  580000 | ▃▇▃▁▁ |
| resale\_price  | BUKIT TIMAH     | 4 ROOM     |          0 |              1 | 645495.9 |  62782.51 | 510000 | 600000 | 650000 | 699000 |  750000 | ▃▅▆▇▆ |
| resale\_price  | CENTRAL AREA    | 4 ROOM     |          0 |              1 | 765520.3 | 171298.41 | 385000 | 621250 | 839400 | 900000 | 1028000 | ▃▃▂▇▆ |
| resale\_price  | CHOA CHU KANG   | 4 ROOM     |          0 |              1 | 350718.2 |  32584.28 | 265000 | 328000 | 347000 | 368000 |  515000 | ▂▇▃▁▁ |
| resale\_price  | CLEMENTI        | 4 ROOM     |          0 |              1 | 553250.9 | 120641.11 | 310000 | 455000 | 530000 | 650000 |  915000 | ▃▇▃▃▁ |
| resale\_price  | GEYLANG         | 4 ROOM     |          0 |              1 | 507152.9 | 120457.37 | 300000 | 395000 | 485000 | 630000 |  778000 | ▇▇▅▆▂ |
| resale\_price  | HOUGANG         | 4 ROOM     |          0 |              1 | 389257.4 |  41490.56 | 289300 | 360000 | 385000 | 411500 |  600000 | ▂▇▃▁▁ |
| resale\_price  | JURONG EAST     | 4 ROOM     |          0 |              1 | 419387.0 |  53974.93 | 290000 | 381000 | 412000 | 455000 |  565000 | ▂▇▇▅▂ |
| resale\_price  | JURONG WEST     | 4 ROOM     |          0 |              1 | 376430.7 |  43272.14 | 275000 | 345000 | 370000 | 406250 |  565000 | ▂▇▅▁▁ |
| resale\_price  | KALLANG/WHAMPOA | 4 ROOM     |          0 |              1 | 559342.4 | 116815.58 | 322888 | 470000 | 543888 | 650000 |  880000 | ▃▇▆▃▁ |
| resale\_price  | MARINE PARADE   | 4 ROOM     |          0 |              1 | 529301.4 |  43173.18 | 419000 | 500000 | 530000 | 560000 |  670000 | ▂▆▇▃▁ |
| resale\_price  | PASIR RIS       | 4 ROOM     |          0 |              1 | 405522.7 |  25903.46 | 311000 | 388000 | 403000 | 420000 |  537000 | ▁▇▇▁▁ |
| resale\_price  | PUNGGOL         | 4 ROOM     |          0 |              1 | 444424.9 |  51412.31 | 320000 | 408000 | 440000 | 480000 |  590000 | ▂▆▇▅▁ |
| resale\_price  | QUEENSTOWN      | 4 ROOM     |          0 |              1 | 680063.6 |  97244.64 | 385000 | 625000 | 700000 | 750000 |  980000 | ▁▃▇▅▁ |
| resale\_price  | SEMBAWANG       | 4 ROOM     |          0 |              1 | 353928.8 |  28730.61 | 280000 | 335000 | 355000 | 372888 |  438000 | ▂▆▇▅▁ |
| resale\_price  | SENGKANG        | 4 ROOM     |          0 |              1 | 416147.7 |  55760.53 | 271000 | 375000 | 410000 | 445000 |  583888 | ▁▇▇▂▁ |
| resale\_price  | SERANGOON       | 4 ROOM     |          0 |              1 | 462195.1 |  88130.55 | 275500 | 395000 | 450000 | 520000 |  726000 | ▂▇▆▃▁ |
| resale\_price  | TAMPINES        | 4 ROOM     |          0 |              1 | 432823.8 |  43904.07 | 333000 | 405000 | 425000 | 455000 |  648000 | ▂▇▂▁▁ |
| resale\_price  | TOA PAYOH       | 4 ROOM     |          0 |              1 | 564179.9 | 138783.50 | 290000 | 440000 | 570000 | 680000 |  888000 | ▅▇▇▇▂ |
| resale\_price  | WOODLANDS       | 4 ROOM     |          0 |              1 | 347194.7 |  34905.49 | 225000 | 323000 | 348000 | 370000 |  470000 | ▁▃▇▃▁ |
| resale\_price  | YISHUN          | 4 ROOM     |          0 |              1 | 356161.4 |  42128.21 | 250000 | 320000 | 355000 | 385200 |  609888 | ▃▇▃▁▁ |

While the dataset includes other room types, due to constraints of the
table, 4 room flats and their prices were chosen for further analysis,
They make up the bulk of resale flats transacted during 2015 and 2018
and it makes sense to take a look further. While it is expected the
Central Area to have a high resale value for its flats, Ang Mo Kio
offered nearly similar value, even in comparison to other towns that are
nearer to the Central Area such as Bishan and Toa Payoh. Also, it can be
seen that peripheral towns do exhibit a negative relationship with
resale-prices, relatively, as towns such as Pasir Ris, Punggol, Jurong
West and Woodlands experience low resale values. Distance still plays an
important factor, regardless of urban mass transport systems.

## Part 4: Moving average for resale price per sq/m for 6-month interval

### Loading Additional libraries

``` r
library(zoo)
library(styler)
library(tidyquant)
library(cranlogs)
```

### Calculation of Price per sqm and Rolling Average for Price per sqm

``` r
sales_ordered_movingaverage<-sales_ordered %>%
  mutate(Psqm=resale_price/floor_area_sqm)

sales_ordered_movingaverage<-sales_ordered_movingaverage %>% 
  group_by(month,town) %>% 
  mutate(rollave=rollmean(x=Psqm,6,align ="right",fill = 0))
```

### Calculation of mean rolling average per month

``` r
sales_movingaverage_month<-sales_ordered_movingaverage %>% 
  group_by(month) %>% 
  summarise(meanrollave=mean(rollave)) 
```

### Calculation of mean Price per sqm per month

``` r
sales_average_month<-sales_ordered_movingaverage %>%
  group_by(month) %>% 
  summarise(meanPsqm=mean(Psqm))
```

### Plotting Moving Average and Average Price per sqm per month for comparison

``` r
sales_movingaverage_month %>%
  ggplot(aes(x=month,group=1))+ 
  geom_point(aes(y=meanrollave,color=meanrollave))+
  geom_line(aes(y=meanrollave,color=meanrollave))+
  geom_point(data=sales_average_month,aes(y=meanPsqm,color=meanPsqm))+
  geom_line(data=sales_average_month,aes(y=meanPsqm,color=meanPsqm))+
  labs(title = "Moving average vs Average Resale Prices per Sqm", subtitle = "6-month moving average in comparison to the average resale prices per sqm during 2015 to 2018", x= "Years", y= "Price (SGD)", color="Legend (SGD)")+
  theme(axis.text.x = element_text(angle = 45))
```

![](Assignment-1_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

The upper line is indicative of the mean price per sqm per month while
the lower line is the moving average for price per sqm per month. When
deploying smoothing techniques, it can be seen that there are seasons
for the resale market, especially during the beginning of most years,
there is a significant drop in the resale amrket. The celebration of the
Chinese New Year could be an important factor of this trend, as most
Chinese families would seek to avoid moving houses so close to a period
where family and friends would visit one another. The preparation needed
to get a new house up and running within a month could be a strong
deterrent for the resale market, since the Chinese make up a significant
portion of the populace. 2015 showed the most erratic prices, even for
smoothing, and this could be due to the stabilising of the property
market after the housing crunch.
