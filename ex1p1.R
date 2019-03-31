# loading packages
library(tidyverse)
library(knitr)

# import dataset 
gbldgs = read.csv("../data/greenbuildings.csv")
head(gbldgs)

# data-cleansing : remove the bulidings with low occupancy rates(lees than 10%)
gbldgs = gbldgs[gbldgs$leasing_rate>=10,]

# change the attribute of green_rating 
gbldgs$green_rate = cut(gbldgs$green_rating, breaks=2, labels=c("NON-GREEN", "GREEN"))

# data guru's conclusion
gbldgs_a = gbldgs %>%
  group_by(green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_a, caption = "The median rent in the in-house analysis")
ggplot(gbldgs) + geom_boxplot(aes(green_rate, Rent)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=green_rate), alpha=0.6) +xlim(0,70)

# more analysis

## building class
gbldgs=mutate(gbldgs, Building_Class=class_a*2 + class_b)
gbldgs_class=gbldgs %>%
  group_by(Building_Class, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_class, caption = "The median rent in the in-house analysis w.r.t. Building Class")
ggplot(gbldgs) + geom_boxplot(aes(factor(Building_Class), Rent, color=green_rate)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=factor(green_rate)), alpha=0.6) + xlim(0,70) +facet_wrap(~factor(Building_Class), nrow=3)

## cluster_rent 
gbldgs$Average_Rent=cut(gbldgs$cluster_rent, breaks=c(0,quantile(gbldgs$cluster_rent, p=0.25),quantile(gbldgs$cluster_rent, p=0.75),max(gbldgs$cluster_rent)), labels=c("low 25%","med","high 25%"))
gbldgs_rent = gbldgs %>%
  group_by(Average_Rent, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_rent, caption = "The median rent in the in-house analysis w.r.t. Averave Rent")
ggplot(gbldgs) + geom_boxplot(aes(Average_Rent, Rent, color=green_rate)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Average_Rent, nrow=3)

## stories
gbldgs$Bldg_Height=cut(gbldgs$stories, breaks=c(0,quantile(gbldgs$stories, p=0.25),quantile(gbldgs$stories, p=0.75),max(gbldgs$stories)))
gbldgs_story = gbldgs %>%
  group_by(Bldg_Height, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_story, caption = "The median rent in the in-house analysis w.r.t. Stories")
ggplot(gbldgs) + geom_boxplot(aes(Bldg_Height, Rent, color=green_rate)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Bldg_Height, nrow=3)

## age
gbldgs$Bldg_Age=cut(gbldgs$age, breaks=c(-1,quantile(gbldgs$age, p=0.25),quantile(gbldgs$age, p=0.75),max(gbldgs$age)))  # To include 0-year building, apply breaks from -1
gbldgs_age = gbldgs %>% 
  group_by(Bldg_Age, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_age, caption = "The median rent in the in-house analysis w.r.t. Building Age")
ggplot(gbldgs) + geom_boxplot(aes(Bldg_Age, Rent, color=green_rate)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=log(Rent)), fill=factor(green_rate)), alpha=0.6) + facet_wrap(~Bldg_Age, nrow=3)

## heating and cooling costs : heating costs = heating days*gas prices, cooling costs = cooling days * electricity price
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
ggplot(gbldgs) + geom_boxplot(aes(green_rate, Rent))+ ylim(0,70) +facet_grid(Heating_Costs~ Cooling_Costs)
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=green_rate), alpha=0.6) + xlim(0,70) + facet_grid(Heating_Costs~ Cooling_Costs)

## Amenities
gbldgs_ame = gbldgs %>%
  group_by(amenities, green_rate) %>%
  summarize(med = median(Rent), number = n())
kable(gbldgs_ame, caption = "The median rent in the in-house analysis w.r.t. Amenities")
ggplot(gbldgs) + geom_boxplot(aes(factor(amenities), Rent, color=green_rate)) + ylim(0,70)
ggplot(gbldgs) + geom_density(aes((x=Rent), fill=factor(green_rate)), alpha=0.6) + xlim(0,70) +facet_wrap(~factor(amenities), nrow=2)

