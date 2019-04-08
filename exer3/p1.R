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

gbldg_cluster = gbldg %>%
  group_by(cluster) %>%
  select(cluster, green_rating, empl_gr, 
         cd_total_07, hd_total07, amenities, total_dd_07, Precipitation, 
         Gas_Costs, Electricity_Costs, cluster_rent) %>%
  unique()


gbldg_clst_ngb = gbldg %>%
  group_by(cluster) %>%
  summarize(n_gb = sum(green_rating)) 
nrow(gbldg_clst_ngb)
length(which(gbldg_clst_ngb$n_gb==1))


ggplot(gbldg) + geom_point(aes(x=Electricity_Costs, y=cluster_rent))

xtabs(~green_rating + renovated, data = gbldg)
xtabs(~green_rating + class_a, data = gbldg)
xtabs(~renovated + class_a, data = gbldg)
xtabs(~green_rating + net, data = gbldg)

# data descriptions
## 74 rows have omitted values in 'empl_gr' var => omit them
## green_rating : LEED or Energystar => all are dummies
## there is only 1 green_rating building in a cluster
## 8 clusters have 0 green_rating building
## The data clustered by 'cluster' have the same values of 
### : empl_gr, cd_total_07, hd_total07, total_dd_07, Precipitation, 
### : Gas_Costs, Electricity_Costs, cluster_rent
### 'amenities' var is not related to 'cluster' var
## total_dd_07 = cd_total_07 + hd_total07
## dummy var except g_ratings : renovated, class_a, class_b, net, amenities

summary(gbldg)

# find the best model by stepwise selection 
## find lm0 by lasso

gb_x = sparse.model.matrix(Rent ~ .-CS_PropertyID - cluster - LEED - Energystar, data = gbldg)[,-1]
gb_y = gbldg$Rent
gblasso = gamlr(gb_x, gb_y)
coef(gblasso)
min(AICc(gblasso))

####
gbcvl = cv.gamlr(gb_x, gb_y, nfold = 10,verb=TRUE)
gbb.min = coef(gbcvl, select="min")
log(gbcvl$lambda.min) # same as AIC ??


plot(gbcvl, bty="n")
# lines(log(gblasso$lambda),AICc(gblasso)/n, col="green", lwd=2)

## stepwise selection  

lm_0 = lm(Rent ~ 1, data = gbldg)
lm_fwd1 = step(lm_0, direction='forward',
               scope = ~ (size + empl_gr + leasing_rate + stories + age + 
                            renovated + class_a + class_b + green_rating + net + amenities +
                            cd_total_07 + hd_total07 + total_dd_07 + Precipitation + 
                            Gas_Costs + Electricity_Costs + cluster_rent)^2) #AIC = 34512


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
#AIC = 34510

summary(lm_st)


