library(mosaic)
library(tidyverse)
library(gamlr) 

gbldg = read.csv("../data/greenbuildings.csv")
nrow(gbldg)

# data cleansing
length(which(complete.cases(gbldg)=="FALSE")) 
gbldg[which(complete.cases(gbldg)=="FALSE"),"empl_gr"]
gbldg = na.omit(gbldg)
nrow(gbldg)

length(which(gbldg$LEED==1))
length(which(gbldg$Energystar==1))
length(which(gbldg$green_rating==1))

summary(gbldg)

# find the best model by stepwise selection 
## find lm0 by lasso

gb_x = sparse.model.matrix(Rent ~ .-CS_PropertyID - cluster - LEED - Energystar, data = gbldg)[,-1]
gb_y = gbldg$Rent
gblasso = gamlr(gb_x, gb_y)
coef(gblasso)
min(AICc(gblasso))


## stepwise selection  

lm0 = lm(Rent ~ size + empl_gr + leasing_rate + age +
           renovated + class_a + class_b +
           green_rating + net + amenities + 
           cd_total_07 + hd_total07 +
           Gas_Costs + Electricity_Costs + 
           cluster_rent, data = gbldg)

lm_st = step(lm0, scope = ~ (size + empl_gr + leasing_rate + stories + age + 
                             renovated + class_a + class_b + green_rating + net + amenities +
                             cd_total_07 + hd_total07 + total_dd_07 + Precipitation + 
                             Gas_Costs + Electricity_Costs + cluster_rent)^2)

summary(lm_st)
