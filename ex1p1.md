Invest for Green Buildings
================

Introduction
------------

An Austin real-estate developer is wonder whether building a new 15-story mixed-use building as a green building would be worth it. The developer's on-staff data guru conducted an analysis using 7,894 commercial rental properties' data.

``` r
library(tidyverse)
library(knitr)
```

``` r
gbldgs <- read.csv("../data/greenbuildings.csv")
head(gbldgs)
```

    ##   CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories age
    ## 1        379105       1 260300    2.22 38.56        91.39      14  16
    ## 2        122151       1  67861    2.22 28.57        87.14       5  27
    ## 3        379839       1 164848    2.22 33.31        88.94      13  36
    ## 4         94614       1  93372    2.22 35.00        97.04      13  46
    ## 5        379285       1 174307    2.22 40.69        96.58      16   5
    ## 6         94765       1 231633    2.22 43.16        92.74      14  20
    ##   renovated class_a class_b LEED Energystar green_rating net amenities
    ## 1         0       1       0    0          1            1   0         1
    ## 2         0       0       1    0          0            0   0         1
    ## 3         1       0       1    0          0            0   0         1
    ## 4         1       0       1    0          0            0   0         0
    ## 5         0       1       0    0          0            0   0         1
    ## 6         0       1       0    0          0            0   0         1
    ##   cd_total_07 hd_total07 total_dd_07 Precipitation  Gas_Costs
    ## 1        4988         58        5046         42.57 0.01370000
    ## 2        4988         58        5046         42.57 0.01373149
    ## 3        4988         58        5046         42.57 0.01373149
    ## 4        4988         58        5046         42.57 0.01373149
    ## 5        4988         58        5046         42.57 0.01373149
    ## 6        4988         58        5046         42.57 0.01373149
    ##   Electricity_Costs cluster_rent
    ## 1        0.02900000        36.78
    ## 2        0.02904455        36.78
    ## 3        0.02904455        36.78
    ## 4        0.02904455        36.78
    ## 5        0.02904455        36.78
    ## 6        0.02904455        36.78

Review of the in-house analysis
-------------------------------

The on-staff data guru recommended that building a green buliding would be more profitable, because the analysis showed that a green building's rent tended to be $2.60 more per square foot than a non-green building's rent. The staff compared the median rent of green bulidngs and non-green building, which were respectively $27.60 and $25 per square foot. Moreover, the staff cleaned the data by deleting the data set with very low occupancy rates(less than 10%).

To reivew the analysis, I started by cleaning the data set as the analysis conducted.

``` r
gbldgs = gbldgs[gbldgs$leasing_rate>=10,]
```

Then I reviewed the rent distribution by calculating the median and visualizing the distribution.

``` r
gbldgs$green_rate = cut(gbldgs$green_rating, breaks=2, labels=c("NON-GREEN", "GREEN"))
```

``` r
gbldgs$green_rate = cut(gbldgs$green_rating, breaks=2, labels=c("NON-GREEN", "GREEN"))
gbldgs_a = gbldgs %>%
  group_by(green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_a, caption = "The median rent in the in-house analysis")
```

| green\_rate |    med|  number|
|:------------|------:|-------:|
| NON-GREEN   |  25.03|    6995|
| GREEN       |  27.60|     684|

``` r
ggplot(gbldgs) + geom_boxplot(aes(green_rate, Rent)) + ylim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=green_rate), alpha=0.6) +xlim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-5-2.png)

The number is the same as the in-house analysis, and the distribution seems to be good to support the result.

Deeper Analysis
---------------

There are a lot of properties we can use for deeper analysis in the data set.

The building is new and 15-story building. And its location is on East Cesar Chavez, just across I-35, thus there will be a lot of amenities near the site. Moreover, Austin is hot in summer, war in winer, thus the cooling costs might be high, while the heating costs might be low. The average rent in Austin might not be too high or too low.

### Look up the building age

I categorized the building age data by using quantile. This is new building. As follows, the rent difference of young buliding group tends to be less than other groups. The gap is only $0.3 per square foot, thus the green building advantage might be much smaller than expected.

``` r
gbldgs$Bldg_Age=cut(gbldgs$age, breaks=c(-1,quantile(gbldgs$age, p=0.25),quantile(gbldgs$age, p=0.75),max(gbldgs$age))) # To include 0-year building, apply breaks from -1
gbldgs_age = gbldgs %>%
  group_by(Bldg_Age, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_age, caption = "The median rent in the in-house analysis w.r.t. Building Age")
```

| Bldg\_Age | green\_rate |    med|  number|
|:----------|:------------|------:|-------:|
| (-1,23\]  | NON-GREEN   |  28.00|    1702|
| (-1,23\]  | GREEN       |  28.30|     414|
| (23,79\]  | NON-GREEN   |  24.60|    3474|
| (23,79\]  | GREEN       |  26.78|     253|
| (79,187\] | NON-GREEN   |  23.33|    1819|
| (79,187\] | GREEN       |  24.17|      17|

``` r
ggplot(gbldgs) + geom_boxplot(aes(Bldg_Age, Rent, color=green_rate)) + ylim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Bldg_Age, nrow=3)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-6-2.png)

### Look up the building class

This is a new mixed-use building located near downtown, so the building class might be A. As follows, the rent difference of good condition building group tends to be less than other groups. The gap is only $0.24 per square foot, thus the green building advantage might be much smaller than expected.

``` r
gbldgs=mutate(gbldgs, Building_Class=class_a*2 + class_b)
gbldgs_class=gbldgs %>%
  group_by(Building_Class, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_class, caption = "The median rent in the in-house analysis w.r.t. Building Class")
```

|  Building\_Class| green\_rate |    med|  number|
|----------------:|:------------|------:|-------:|
|                0| NON-GREEN   |  22.11|    1015|
|                0| GREEN       |  32.00|       7|
|                1| NON-GREEN   |  24.00|    3391|
|                1| GREEN       |  25.20|     131|
|                2| NON-GREEN   |  28.20|    2589|
|                2| GREEN       |  28.44|     546|

``` r
ggplot(gbldgs) + geom_boxplot(aes(factor(Building_Class), Rent, color=green_rate)) + ylim(0,70)
```

    ## Warning: Removed 151 rows containing non-finite values (stat_boxplot).

![](ex1p1__files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=factor(green_rate)), alpha=0.6) + xlim(0,70) +facet_wrap(~factor(Building_Class), nrow=3)
```

    ## Warning: Removed 151 rows containing non-finite values (stat_density).

![](ex1p1__files/figure-markdown_github/unnamed-chunk-7-2.png)

### Look up the building story

I categorized the building story data by using quantile. This is 15-story building. As follows, the rent difference of medium height(5-20 story) buliding group tends to be a little less than low-story building groups. The gap is $2.31 per square foot, thus the green building advantage might be little smaller than expected.

``` r
gbldgs$Bldg_Height=cut(gbldgs$stories, breaks=c(0,quantile(gbldgs$stories, p=0.25),quantile(gbldgs$stories, p=0.75),max(gbldgs$stories)))
gbldgs_story = gbldgs %>%
  group_by(Bldg_Height, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_story, caption = "The median rent in the in-house analysis w.r.t. Stories")
```

| Bldg\_Height | green\_rate |     med|  number|
|:-------------|:------------|-------:|-------:|
| (0,4\]       | NON-GREEN   |  23.880|    1821|
| (0,4\]       | GREEN       |  27.000|     149|
| (4,20\]      | NON-GREEN   |  26.690|    3595|
| (4,20\]      | GREEN       |  29.000|     351|
| (20,110\]    | NON-GREEN   |  24.220|    1579|
| (20,110\]    | GREEN       |  25.235|     184|

``` r
ggplot(gbldgs) + geom_boxplot(aes(Bldg_Height, Rent, color=green_rate)) + ylim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Bldg_Height, nrow=3)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-8-2.png)

### Look up the amenity condition

There will be a lot of amenities. As follows, the rent difference of amenity fluent buliding group tends to be a little bigger than the other. The gap is $2.77 per square foot, thus the green building advantage might be little bigger than expected.

``` r
gbldgs_ame = gbldgs %>%
  group_by(amenities, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_ame, caption = "The median rent in the in-house analysis w.r.t. Amenities")
```

|  amenities| green\_rate |     med|  number|
|----------:|:------------|-------:|-------:|
|          0| NON-GREEN   |  25.070|    3362|
|          0| GREEN       |  27.045|     186|
|          1| NON-GREEN   |  25.000|    3633|
|          1| GREEN       |  27.775|     498|

``` r
ggplot(gbldgs) + geom_boxplot(aes(factor(amenities), Rent, color=green_rate)) + ylim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=factor(green_rate)), alpha=0.6) + xlim(0,70) +facet_wrap(~factor(amenities), nrow=2)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-9-2.png)

### Look up the heating and cooling costs

I assumed that gas is used for heating and electricity is used for cooling. I calculated the heating costs level and the cooling costs level by multiplying the costs and heating or cooling days. Then, categorized the costs data by using quantile. We might use much money for cooling and little money for heating in Austin. As follows, the rent difference of low heating costs and high cooling costs buliding group tends to be less than other groups. The gap is only $0.82 per square foot, thus the green building advantage might be smaller than expected.

``` r
### heating
gbldgs=mutate(gbldgs, heat_costs=hd_total07*Gas_Costs)
gbldgs$Heating_Costs=cut(gbldgs$heat_costs, breaks=c(-1,quantile(gbldgs$heat_costs,0.25),quantile(gbldgs$heat_costs,0.75),max(gbldgs$heat_costs)), labels=c("low 25%","med","high 25%")) # To include 0-year building, apply breaks from -1
### cooling
gbldgs=mutate(gbldgs, cool_costs=cd_total_07*Electricity_Costs)
gbldgs$Cooling_Costs=cut(gbldgs$cool_costs, breaks=c(0,quantile(gbldgs$cool_costs,0.25),quantile(gbldgs$cool_costs,0.75),max(gbldgs$cool_costs)), labels=c("low 25%","med","high 25%"))
### heat + cool 
gbldgs_ucost = gbldgs %>%
  group_by(Heating_Costs, Cooling_Costs, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_ucost, caption = "The median rent in the in-house analysis w.r.t. Heating and Cooling Cost")
```

| Heating\_Costs | Cooling\_Costs | green\_rate |     med|  number|
|:---------------|:---------------|:------------|-------:|-------:|
| low 25%        | med            | NON-GREEN   |  29.650|    1541|
| low 25%        | med            | GREEN       |  33.950|      27|
| low 25%        | high 25%       | NON-GREEN   |  22.180|     329|
| low 25%        | high 25%       | GREEN       |  23.000|      51|
| med            | low 25%        | NON-GREEN   |  28.500|    1925|
| med            | low 25%        | GREEN       |  35.095|      72|
| med            | med            | NON-GREEN   |  22.330|     369|
| med            | med            | GREEN       |  33.000|     265|
| med            | high 25%       | NON-GREEN   |  28.750|    1304|
| med            | high 25%       | GREEN       |  23.290|     137|
| high 25%       | low 25%        | NON-GREEN   |  18.595|     182|
| high 25%       | low 25%        | GREEN       |  23.500|      13|
| high 25%       | med            | NON-GREEN   |  19.500|    1306|
| high 25%       | med            | GREEN       |  21.000|     109|
| high 25%       | high 25%       | NON-GREEN   |  20.000|      39|
| high 25%       | high 25%       | GREEN       |  21.765|      10|

``` r
ggplot(gbldgs) + geom_boxplot(aes(green_rate, Rent))+ ylim(0,70) +facet_grid(Heating_Costs~ Cooling_Costs)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=green_rate), alpha=0.6) + xlim(0,70) + facet_grid(Heating_Costs~ Cooling_Costs)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-10-2.png)

### Look up the average rent

I categorized the average rent data by using quantile. The rent level of Austin might be medium. As follows, the rent difference of medium rent area buliding group tends to be bigger than other groups. The gap is $3.26 per square foot, thus the green building advantage might be bigger than expected.

``` r
gbldgs$Average_Rent=cut(gbldgs$cluster_rent, breaks=c(0,quantile(gbldgs$cluster_rent, p=0.25),quantile(gbldgs$cluster_rent, p=0.75),max(gbldgs$cluster_rent)), labels=c("low 25%","med","high 25%"))
gbldgs_rent = gbldgs %>%
  group_by(Average_Rent, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_rent, caption = "The median rent in the in-house analysis w.r.t. Averave Rent")
```

| Average\_Rent | green\_rate |     med|  number|
|:--------------|:------------|-------:|-------:|
| low 25%       | NON-GREEN   |  16.920|    1735|
| low 25%       | GREEN       |  19.010|     188|
| med           | NON-GREEN   |  25.190|    3478|
| med           | GREEN       |  28.455|     360|
| high 25%      | NON-GREEN   |  39.430|    1782|
| high 25%      | GREEN       |  42.000|     136|

``` r
ggplot(gbldgs) + geom_boxplot(aes(Average_Rent, Rent, color=green_rate)) + ylim(0,70)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Average_Rent, nrow=3)
```

![](ex1p1__files/figure-markdown_github/unnamed-chunk-11-2.png)

Conclusion
----------

We can consider more properties to decide whether go green or not. Mostly, a green-building is more profitable. However, the advantage is not always high as expected. If we consider the climate, the building class, the building age or the building story, the advantage decreases to $0.24 per square foot. Though the advantage might be bigger when it comes to the average rent, the amenity condition, I would like to suggest that deeper analysis need to be conducted.
