##################################################################################
###                                                                            ###
###       El Niño-Southern Oscillation and Under-5 Diarrhea in Botswana        ###
###       Made by: Alexandra K. Heaney                                         ###
###       Nov. 11, 2019                                                        ###
###                                                                            ###
##################################################################################

###################################################
#####      Load Required Libraries             ####
###################################################

require(pander)
require(MASS)
require(stargazer)
require(ggplot2)
require(cowplot)
require(raster)
require(ncdf4)
require(tidyverse)
require(ncdf4.helpers)
require(PCICt)
require(raster)
require(maptools)
require(rgdal)
require(sp)
require(rasterVis)


###################################################
#####            Load Data                     ####
###################################################

all_lag <- read.csv("~/Data_empty.csv") ## This data is not publically available. Contact the authors and the Botswana Ministry of Health to obtain diarrhea and meteorlogical data

# Create Seasonal Datasets
DJF <- all_lag[all_lag$Month %in% c(12,1,2),]
MAM <- all_lag[all_lag$Month %in% c(3,4,5),]
JJA <- all_lag[all_lag$Month %in% c(6,7,8),]
SON <- all_lag[all_lag$Month %in% c(9,10,11),]

DJF.12 <- all_12[all_12$Month %in% c(12,1,2),]
MAM.12 <- all_12[all_12$Month %in% c(3,4,5),]
JJA.12 <- all_12[all_12$Month %in% c(6,7,8),]
SON.12 <- all_12[all_12$Month %in% c(9,10,11),]

DJF.17 <- all_17[all_17$Month %in% c(12,1,2),]
MAM.17 <- all_17[all_17$Month %in% c(3,4,5),]
JJA.17 <- all_17[all_17$Month %in% c(6,7,8),]
SON.17 <- all_17[all_17$Month %in% c(9,10,11),]


###################################################
#####                                          ####
#####   1. Monthly Correlation Analyses        ####
#####                                          ####
###################################################

############################
#### Diarrhea Anomalies ####
############################

lag0_NINO3.4 <- paste(round(cor(all_lag$NINO3.4, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.4, all_lag$diarrhea_anom)$p.value, 4), ")")
lag0_NINO3.4 <- paste(round(cor(all_lag$NINO3.41, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.41, all_lag$diarrhea_anom)$p.value, 4), ")")
lag2_NINO3.4 <- paste(round(cor(all_lag$NINO3.42, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.42, all_lag$diarrhea_anom)$p.value, 4), ")")
lag3_NINO3.4 <- paste(round(cor(all_lag$NINO3.43, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.43, all_lag$diarrhea_anom)$p.value, 4), ")")
lag4_NINO3.4 <- paste(round(cor(all_lag$NINO3.44, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.44, all_lag$diarrhea_anom)$p.value, 4), ")")
lag5_NINO3.4 <- paste(round(cor(all_lag$NINO3.45, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.45, all_lag$diarrhea_anom)$p.value, 4), ")")
lag6_NINO3.4 <- paste(round(cor(all_lag$NINO3.46, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.46, all_lag$diarrhea_anom)$p.value, 4), ")")
lag7_NINO3.4 <- paste(round(cor(all_lag$NINO3.47, all_lag$diarrhea_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.47, all_lag$diarrhea_anom)$p.value, 4), ")")

## Correlation Matrix, Entries represent correlation (p-value) ##
diarrhea_cor <- rbind(lag0_NINO3.4, lag0_NINO3.4, lag2_NINO3.4, lag3_NINO3.4)
pander(diarrhea_cor)

############################
### Rainfall Anomalies   ###
############################

lag0_NINO3.4 <- paste(round(cor(all_lag$NINO3.4, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.4, all_lag$rain_anom)$p.value, 4), ")")
lag1_NINO3.4 <- paste(round(cor(all_lag$NINO3.41, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.41, all_lag$rain_anom)$p.value, 4), ")")
lag2_NINO3.4 <- paste(round(cor(all_lag$NINO3.42, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.42, all_lag$rain_anom)$p.value, 4), ")")
lag3_NINO3.4 <- paste(round(cor(all_lag$NINO3.43, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.43, all_lag$rain_anom)$p.value, 4), ")")
lag4_NINO3.4 <- paste(round(cor(all_lag$NINO3.44, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.44, all_lag$rain_anom)$p.value, 4), ")")
lag5_NINO3.4 <- paste(round(cor(all_lag$NINO3.45, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.45, all_lag$rain_anom)$p.value, 4), ")")
lag6_NINO3.4 <- paste(round(cor(all_lag$NINO3.46, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.46, all_lag$rain_anom)$p.value, 4), ")")
lag7_NINO3.4 <- paste(round(cor(all_lag$NINO3.47, all_lag$rain_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.47, all_lag$rain_anom)$p.value, 4), ")")

## Correlation Matrix, Entries represent correlation (p-value) ##
rain_cor <- rbind(lag0_NINO3.4, lag1_NINO3.4, lag2_NINO3.4, lag3_NINO3.4)
pander(rain_cor)


############################
####   Tmin Anomalies   ####
############################

lag0_NINO3.4 <- paste(round(cor(all_lag$NINO3.4, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.4, all_lag$tmin_anom)$p.value, 4), ")")
lag1_NINO3.4 <- paste(round(cor(all_lag$NINO3.41, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.41, all_lag$tmin_anom)$p.value, 4), ")")
lag2_NINO3.4 <- paste(round(cor(all_lag$NINO3.42, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.42, all_lag$tmin_anom)$p.value, 4), ")")
lag3_NINO3.4 <- paste(round(cor(all_lag$NINO3.43, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.43, all_lag$tmin_anom)$p.value, 4), ")")
lag4_NINO3.4 <- paste(round(cor(all_lag$NINO3.44, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.44, all_lag$tmin_anom)$p.value, 4), ")")
lag5_NINO3.4 <- paste(round(cor(all_lag$NINO3.45, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.45, all_lag$tmin_anom)$p.value, 4), ")")
lag6_NINO3.4 <- paste(round(cor(all_lag$NINO3.46, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.46, all_lag$tmin_anom)$p.value, 4), ")")
lag7_NINO3.4 <- paste(round(cor(all_lag$NINO3.47, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.47, all_lag$tmin_anom)$p.value, 4), ")")
lag8_NINO3.4 <- paste(round(cor(all_lag$NINO3.48, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.48, all_lag$tmin_anom)$p.value, 4), ")")
lag9_NINO3.4 <- paste(round(cor(all_lag$NINO3.49, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.49, all_lag$tmin_anom)$p.value, 4), ")")
lag10_NINO3.4 <- paste(round(cor(all_lag$NINO3.410, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.410, all_lag$tmin_anom)$p.value, 4), ")")
lag11_NINO3.4 <- paste(round(cor(all_lag$NINO3.411, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.411, all_lag$tmin_anom)$p.value, 4), ")")
lag12_NINO3.4 <- paste(round(cor(all_lag$NINO3.412, all_lag$tmin_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.412, all_lag$tmin_anom)$p.value, 4), ")")

## Correlation Matrix, Entries represent correlation (p-value) ##
tmin_cor <- rbind(lag0_NINO3.4, lag1_NINO3.4, lag2_NINO3.4, lag3_NINO3.4)
pander(tmin_cor)


#############################
### River Height Anoms.   ###
#############################

lag0_NINO3.4 <- paste(round(cor(all_lag$NINO3.4, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.4, all_lag$height_anom)$p.value, 4), ")")
lag1_NINO3.4 <- paste(round(cor(all_lag$NINO3.41, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.41, all_lag$height_anom)$p.value, 4), ")")
lag2_NINO3.4 <- paste(round(cor(all_lag$NINO3.42, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.42, all_lag$height_anom)$p.value, 4), ")")
lag3_NINO3.4 <- paste(round(cor(all_lag$NINO3.43, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.43, all_lag$height_anom)$p.value, 4), ")")
lag4_NINO3.4 <- paste(round(cor(all_lag$NINO3.44, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.44, all_lag$height_anom)$p.value, 4), ")")
lag5_NINO3.4 <- paste(round(cor(all_lag$NINO3.45, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.45, all_lag$height_anom)$p.value, 4), ")")
lag6_NINO3.4 <- paste(round(cor(all_lag$NINO3.46, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.46, all_lag$height_anom)$p.value, 4), ")")
lag7_NINO3.4 <- paste(round(cor(all_lag$NINO3.47, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.47, all_lag$height_anom)$p.value, 4), ")")
lag8_NINO3.4 <- paste(round(cor(all_lag$NINO3.48, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.48, all_lag$height_anom)$p.value, 4), ")")
lag9_NINO3.4 <- paste(round(cor(all_lag$NINO3.49, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.49, all_lag$height_anom)$p.value, 4), ")")
lag10_NINO3.4 <- paste(round(cor(all_lag$NINO3.410, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.410, all_lag$height_anom)$p.value, 4), ")")
lag11_NINO3.4 <- paste(round(cor(all_lag$NINO3.411, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.411, all_lag$height_anom)$p.value, 4), ")")
lag12_NINO3.4 <- paste(round(cor(all_lag$NINO3.412, all_lag$height_anom, use="complete.obs"), 3), "(", round(cor.test(all_lag$NINO3.412, all_lag$height_anom)$p.value, 4), ")")

## Correlation Matrix, Entries represent correlation (p-value) ##
height_cor <- rbind(lag0_NINO3.4, lag1_NINO3.4, lag2_NINO3.4, lag3_NINO3.4)
pander(height_cor)


###################################################
#####                                          ####
#####   2. Seasonal Regression Analyses       ####
#####                                          ####
###################################################

############################
#### Diarrhea Anomalies ####
############################
#### DJF Season ####

# Sum all under-5 diarrhea cases in DJF each year 
djf <- c(sum(DJF$diarrhea[3:5]), sum(DJF$diarrhea[6:8]), sum(DJF$diarrhea[9:11]), sum(DJF$diarrhea[12:14]), sum(DJF$diarrhea[15:17]),
         sum(DJF$diarrhea[18:20]), sum(DJF$diarrhea[21:23]), sum(DJF$diarrhea[24:26]), sum(DJF$diarrhea[27:29]), sum(DJF$diarrhea[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases
djf_feb <- glm.nb(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- glm.nb(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- glm.nb(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- glm.nb(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- glm.nb(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- glm.nb(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- glm.nb(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- glm.nb(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- glm.nb(djf ~ NINO3.48, data=DJF.ar)
djf_may <- glm.nb(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- glm.nb(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- glm.nb(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- glm.nb(djf ~ NINO3.412, data=DJF.ar)

stargazer(djf_feb, djf_jan, djf_dec, djf_nov, djf_oct, djf_sep, djf_aug, djf_jul, djf_jun, djf_may, djf_apr,type="text") # Summarize regression results

# Sum all under-5 diarrhea cases in DJF each year before July 2012 (RV intro)
djf.12 <- c(sum(DJF.12$diarrhea[3:5]), sum(DJF.12$diarrhea[6:8]), sum(DJF.12$diarrhea[9:11]), sum(DJF.12$diarrhea[12:14]), sum(DJF.12$diarrhea[15:17]))
djf.12.pred <- DJF.12[DJF.12$Month == 2,]
DJF.12.ar <- cbind(djf.12.pred[-1,], djf.12)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data before July, 2012
djf_feb.12 <- glm.nb(djf.12 ~ NINO3.4, data=DJF.12.ar)
djf_jan.12 <- glm.nb(djf.12 ~ NINO3.41, data=DJF.12.ar)
djf_dec.12 <- glm.nb(djf.12 ~ NINO3.42, data=DJF.12.ar)
djf_nov.12 <- glm.nb(djf.12 ~ NINO3.43, data=DJF.12.ar)
djf_oct.12 <- glm.nb(djf.12 ~ NINO3.44, data=DJF.12.ar)
djf_sep.12 <- glm.nb(djf.12 ~ NINO3.45, data=DJF.12.ar)
djf_aug.12 <- glm.nb(djf.12 ~ NINO3.46, data=DJF.12.ar)
djf_jul.12 <- glm.nb(djf.12 ~ NINO3.47, data=DJF.12.ar)
djf_jun.12 <- glm.nb(djf.12 ~ NINO3.48, data=DJF.12.ar)
djf_may.12 <- glm.nb(djf.12 ~ NINO3.49, data=DJF.12.ar)
djf_apr.12 <- glm.nb(djf.12 ~ NINO3.410, data=DJF.12.ar)
djf_mar.12 <- glm.nb(djf.12 ~ NINO3.411, data=DJF.12.ar)
djf_feb2.12 <- glm.nb(djf.12 ~ NINO3.412, data=DJF.12.ar)

stargazer(djf_feb.12, djf_jan.12, djf_dec.12, djf_nov.12, djf_oct.12, djf_sep.12, type="text") # Summarize regression results

# Sum all under-5 diarrhea cases in DJF each year after July 2012 (RV intro)
djf.17 <- c(sum(DJF.17$diarrhea[1:3]), sum(DJF.17$diarrhea[4:6]), sum(DJF.17$diarrhea[7:9]), sum(DJF.17$diarrhea[10:12]), sum(DJF.17$diarrhea[13:15]))
djf.17.pred <- DJF.17[DJF.17$Month == 2,]
DJF.17.ar <- cbind(djf.17.pred, djf.17)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data before July, 2017
djf_feb.17 <- glm.nb(djf.17 ~ NINO3.4, data=DJF.17.ar)
djf_jan.17 <- glm.nb(djf.17 ~ NINO3.41, data=DJF.17.ar)
djf_dec.17 <- glm.nb(djf.17 ~ NINO3.42, data=DJF.17.ar)
djf_nov.17 <- glm.nb(djf.17 ~ NINO3.43, data=DJF.17.ar)
djf_oct.17 <- glm.nb(djf.17 ~ NINO3.44, data=DJF.17.ar)
djf_sep.17 <- glm.nb(djf.17 ~ NINO3.45, data=DJF.17.ar)
djf_aug.17 <- glm.nb(djf.17 ~ NINO3.46, data=DJF.17.ar)
djf_jul.17 <- glm.nb(djf.17 ~ NINO3.47, data=DJF.17.ar)
djf_jun.17 <- glm.nb(djf.17 ~ NINO3.48, data=DJF.17.ar)
djf_may.17 <- glm.nb(djf.17 ~ NINO3.49, data=DJF.17.ar)
djf_apr.17 <- glm.nb(djf.17 ~ NINO3.410, data=DJF.17.ar)
djf_mar.17 <- glm.nb(djf.17 ~ NINO3.411, data=DJF.17.ar)
djf_feb2.17 <- glm.nb(djf.17 ~ NINO3.412, data=DJF.17.ar)

stargazer(djf_feb.17, djf_jan.17, djf_dec.17, djf_nov.17, djf_oct.17, djf_sep.17, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data (columns 1-2), data before July 2012 (columns 3-4), and data after July 2012 (columns 5-6))
djf_ma <- matrix(0, nrow=13, ncol=6)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se", "AV_estimate", "AV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))

djf_ma[1,3:4] <- c(round(summary(djf_feb.12)$coefficients[2,1],3), round(summary(djf_feb.12)$coefficients[2,2],3))
djf_ma[2,3:4] <- c(round(summary(djf_jan.12)$coefficients[2,1],3), round(summary(djf_jan.12)$coefficients[2,2],3))
djf_ma[3,3:4] <- c(round(summary(djf_dec.12)$coefficients[2,1],3), round(summary(djf_dec.12)$coefficients[2,2],3))
djf_ma[4,3:4] <- c(round(summary(djf_nov.12)$coefficients[2,1],3), round(summary(djf_nov.12)$coefficients[2,2],3))
djf_ma[5,3:4] <- c(round(summary(djf_oct.12)$coefficients[2,1],3), round(summary(djf_oct.12)$coefficients[2,2],3))
djf_ma[6,3:4] <- c(round(summary(djf_sep.12)$coefficients[2,1],3), round(summary(djf_sep.12)$coefficients[2,2],3))
djf_ma[7,3:4] <- c(round(summary(djf_aug.12)$coefficients[2,1],3), round(summary(djf_aug.12)$coefficients[2,2],3))
djf_ma[8,3:4] <- c(round(summary(djf_jul.12)$coefficients[2,1],3), round(summary(djf_jul.12)$coefficients[2,2],3))
djf_ma[9,3:4] <- c(round(summary(djf_jun.12)$coefficients[2,1],3), round(summary(djf_jun.12)$coefficients[2,2],3))
djf_ma[10,3:4] <- c(round(summary(djf_may.12)$coefficients[2,1],3), round(summary(djf_may.12)$coefficients[2,2],3))
djf_ma[11,3:4] <- c(round(summary(djf_apr.12)$coefficients[2,1],3), round(summary(djf_apr.12)$coefficients[2,2],3))
djf_ma[12,3:4] <- c(round(summary(djf_mar.12)$coefficients[2,1],3), round(summary(djf_mar.12)$coefficients[2,2],3))
djf_ma[13,3:4] <- c(round(summary(djf_feb2.12)$coefficients[2,1],3), round(summary(djf_feb.12)$coefficients[2,2],3))

djf_ma[1,5:6] <- c(round(summary(djf_feb.17)$coefficients[2,1],3), round(summary(djf_feb.17)$coefficients[2,2],3))
djf_ma[2,5:6] <- c(round(summary(djf_jan.17)$coefficients[2,1],3), round(summary(djf_jan.17)$coefficients[2,2],3))
djf_ma[3,5:6] <- c(round(summary(djf_dec.17)$coefficients[2,1],3), round(summary(djf_dec.17)$coefficients[2,2],3))
djf_ma[4,5:6] <- c(round(summary(djf_nov.17)$coefficients[2,1],3), round(summary(djf_nov.17)$coefficients[2,2],3))
djf_ma[5,5:6] <- c(round(summary(djf_oct.17)$coefficients[2,1],3), round(summary(djf_oct.17)$coefficients[2,2],3))
djf_ma[6,5:6] <- c(round(summary(djf_sep.17)$coefficients[2,1],3), round(summary(djf_sep.17)$coefficients[2,2],3))
djf_ma[7,5:6] <- c(round(summary(djf_aug.17)$coefficients[2,1],3), round(summary(djf_aug.17)$coefficients[2,2],3))
djf_ma[8,5:6] <- c(round(summary(djf_jul.17)$coefficients[2,1],3), round(summary(djf_jul.17)$coefficients[2,2],3))
djf_ma[9,5:6] <- c(round(summary(djf_jun.17)$coefficients[2,1],3), round(summary(djf_jun.17)$coefficients[2,2],3))
djf_ma[10,5:6] <- c(round(summary(djf_may.17)$coefficients[2,1],3), round(summary(djf_may.17)$coefficients[2,2],3))
djf_ma[11,5:6] <- c(round(summary(djf_apr.17)$coefficients[2,1],3), round(summary(djf_apr.17)$coefficients[2,2],3))
djf_ma[12,5:6] <- c(round(summary(djf_mar.17)$coefficients[2,1],3), round(summary(djf_mar.17)$coefficients[2,2],3))
djf_ma[13,5:6] <- c(round(summary(djf_feb2.17)$coefficients[2,1],3), round(summary(djf_feb.17)$coefficients[2,2],3))

## All data plot ##
# Extract coefficients and 90% confidence intervals for each NINO3.4 monthly lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$perc <- (1-exp(AY_djf$est))*-100
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

# Create labels for the x-axis of plots
labs <- c("Feb(lag12)", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb(lag0)")
labs <- rev(labs)

#Plot the coefficients and 95% confidence intervals
djfplot <- ggplot(AY_djf, aes(month, perc)) + geom_ribbon(aes(ymin=-100*(1-exp(est-1.96*se)), ymax=-100*(1-exp(est+1.96*se))), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid")  + ylab("Relative Percent Change") + ggtitle("DJF, 2007-2017") +
  scale_x_discrete(name="", limits=labs) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data before July, 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 monthly lag 
AY_djf.12 <- data.frame(cbind(djf_ma[,3:4], row.names(djf_ma)))
colnames(AY_djf.12) <- c("est", "se", "Month")
AY_djf.12$est <- as.numeric(as.character(AY_djf.12$est))
AY_djf.12$perc <- (1-exp(AY_djf.12$est))*-100
AY_djf.12$se <- as.numeric(as.character(AY_djf.12$se))
AY_djf.12$Month <- as.character(AY_djf.12$Month)
AY_djf.12$month <- c(1:13)

#Plot the coefficients and 95% confidence intervals
djfplot12 <- ggplot(AY_djf.12, aes(month, perc)) + geom_ribbon(aes(ymin=-100*(1-exp(est-1.96*se)), ymax=-100*(1-exp(est+1.96*se))), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Relative Percent Change") + ggtitle("DJF, Before RV1, 2007-2012") +
  scale_x_discrete(name="", limits=labs) + 
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + xlab("Relative Percent Change") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data after July, 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 monthly lag 
AY_djf.17 <- data.frame(cbind(djf_ma[,5:6], row.names(djf_ma)))
colnames(AY_djf.17) <- c("est", "se", "Month")
AY_djf.17$est <- as.numeric(as.character(AY_djf.17$est))
AY_djf.17$perc <- (1-exp(AY_djf.17$est))*-100
AY_djf.17$se <- as.numeric(as.character(AY_djf.17$se))
AY_djf.17$Month <- as.character(AY_djf.17$Month)
AY_djf.17$month <- c(1:13)

#Plot the coefficients and 95% confidence intervals
djfplot17 <- ggplot(AY_djf.17, aes(month, perc)) + geom_ribbon(aes(ymin=-100*(1-exp(est-1.96*se)), ymax=-100*(1-exp(est+1.96*se))), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("") + ggtitle("DJF, After RV1, 2012-2017") +
  scale_x_discrete(name="", limits=labs) + 
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

#### Produce Figure 4 ####
plot_grid(djfplot, djfplot12, djfplot17, nrow=3, ncol=1, labels=c("(A)", "(B)", "(C)"), label_size = 18) ## Put all DJF plots together

#### MAM Season ####
# Sum all under-5 diarrhea cases in MAM each year 
mam <- c(sum(MAM$diarrhea[1:3]), sum(MAM$diarrhea[4:6]), sum(MAM$diarrhea[7:9]), sum(MAM$diarrhea[10:12]), sum(MAM$diarrhea[13:15]),
         sum(MAM$diarrhea[16:18]), sum(MAM$diarrhea[19:21]), sum(MAM$diarrhea[22:24]), sum(MAM$diarrhea[25:27]), sum(MAM$diarrhea[28:30]),
         sum(MAM$diarrhea[31:33]))
mam.pred <- MAM[MAM$Month == 5,]
MAM.ar <- cbind(mam.pred, mam)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total MAM cases
mam_may <- glm.nb(mam ~ NINO3.4, data=MAM.ar)
mam_apr <- glm.nb(mam ~ NINO3.41, data=MAM.ar)
mam_mar <- glm.nb(mam ~ NINO3.42, data=MAM.ar)
mam_feb <- glm.nb(mam ~ NINO3.43, data=MAM.ar)
mam_jan <- glm.nb(mam ~ NINO3.44, data=MAM.ar)
mam_dec <- glm.nb(mam ~ NINO3.45, data=MAM.ar)
mam_nov <- glm.nb(mam ~ NINO3.46, data=MAM.ar)
mam_oct <- glm.nb(mam ~ NINO3.47, data=MAM.ar)
mam_sep <- glm.nb(mam ~ NINO3.48, data=MAM.ar)
mam_aug <- glm.nb(mam ~ NINO3.49, data=MAM.ar)
mam_jul <- glm.nb(mam ~ NINO3.410, data=MAM.ar)
mam_jun <- glm.nb(mam ~ NINO3.411, data=MAM.ar)
mam_may2 <- glm.nb(mam ~ NINO3.412, data=MAM.ar)

stargazer(mam_may, mam_apr, mam_mar, mam_feb, mam_jan, mam_dec, type="text") # Summarize regression results

# Sum all under-5 diarrhea cases in MAM each year before July 2012 (RV intro)
mam.12 <- c(sum(MAM$diarrhea[1:3]), sum(MAM$diarrhea[4:6]), sum(MAM$diarrhea[7:9]), sum(MAM$diarrhea[10:12]), sum(MAM$diarrhea[13:15]),
            sum(MAM$diarrhea[16:18]))
mam.12.pred <- MAM.12[MAM.12$Month == 5,]
MAM.12.ar <- cbind(mam.12.pred, mam.12)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data before July, 2012
mam_may.12 <- glm.nb(mam.12 ~ NINO3.4, data=MAM.12.ar)
mam_apr.12 <- glm.nb(mam.12 ~ NINO3.41, data=MAM.12.ar)
mam_mar.12 <- glm.nb(mam.12 ~ NINO3.42, data=MAM.12.ar)
mam_feb.12 <- glm.nb(mam.12 ~ NINO3.43, data=MAM.12.ar)
mam_jan.12 <- glm.nb(mam.12 ~ NINO3.44, data=MAM.12.ar)
mam_dec.12 <- glm.nb(mam.12 ~ NINO3.45, data=MAM.12.ar)
mam_nov.12 <- glm.nb(mam.12 ~ NINO3.46, data=MAM.12.ar)
mam_oct.12 <- glm.nb(mam.12 ~ NINO3.47, data=MAM.12.ar)
mam_sep.12 <- glm.nb(mam.12 ~ NINO3.48, data=MAM.12.ar)
mam_aug.12 <- glm.nb(mam.12 ~ NINO3.49, data=MAM.12.ar)
mam_jul.12 <- glm.nb(mam.12 ~ NINO3.410, data=MAM.12.ar)
mam_jun.12 <- glm.nb(mam.12 ~ NINO3.411, data=MAM.12.ar)
mam_may2.12 <- glm.nb(mam.12 ~ NINO3.412, data=MAM.12.ar)

stargazer(mam_may.12, mam_apr.12, mam_mar.12, mam_feb.12, mam_jan.12, mam_dec.12, type="text") # Summarize regression results

# Sum all under-5 diarrhea cases in MAM each year after July 2012 (RV intro)
mam.17 <- c(sum(MAM$diarrhea[19:21]), sum(MAM$diarrhea[22:24]), sum(MAM$diarrhea[25:27]), sum(MAM$diarrhea[28:30]), sum(MAM$diarrhea[31:33]))
mam.17.pred <- MAM.17[MAM.17$Month == 5,]
MAM.17.ar <- cbind(mam.17.pred, mam.17)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data after July, 2017
mam_may.17 <- glm.nb(mam.17 ~ NINO3.4, data=MAM.17.ar)
mam_apr.17 <- glm.nb(mam.17 ~ NINO3.41, data=MAM.17.ar)
mam_mar.17 <- glm.nb(mam.17 ~ NINO3.42, data=MAM.17.ar)
mam_feb.17 <- glm.nb(mam.17 ~ NINO3.43, data=MAM.17.ar)
mam_jan.17 <- glm.nb(mam.17 ~ NINO3.44, data=MAM.17.ar)
mam_dec.17 <- glm.nb(mam.17 ~ NINO3.45, data=MAM.17.ar)
mam_nov.17 <- glm.nb(mam.17 ~ NINO3.46, data=MAM.17.ar)
mam_oct.17 <- glm.nb(mam.17 ~ NINO3.47, data=MAM.17.ar)
mam_sep.17 <- glm.nb(mam.17 ~ NINO3.48, data=MAM.17.ar)
mam_aug.17 <- glm.nb(mam.17 ~ NINO3.49, data=MAM.17.ar)
mam_jul.17 <- glm.nb(mam.17 ~ NINO3.410, data=MAM.17.ar)
mam_jun.17 <- glm.nb(mam.17 ~ NINO3.411, data=MAM.17.ar)
mam_may2.17 <- glm.nb(mam.17 ~ NINO3.412, data=MAM.17.ar)


stargazer(mam_may.17, mam_apr.17, mam_mar.17, mam_feb.17, mam_jan.17, mam_dec.17, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data (columns 1-2), data before July 2012 (columns 3-4), and data after July 2012 (columns 5-6)
mam_ma <- matrix(0, nrow=13, ncol=6)
colnames(mam_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se", "AV_estimate", "AV_se")
rownames(mam_ma) <- c("May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May2")
mam_ma[1,1:2] <- c(round(summary(mam_may)$coefficients[2,1],3), round(summary(mam_may)$coefficients[2,2],3))
mam_ma[2,1:2] <- c(round(summary(mam_apr)$coefficients[2,1],3), round(summary(mam_apr)$coefficients[2,2],3))
mam_ma[3,1:2] <- c(round(summary(mam_mar)$coefficients[2,1],3), round(summary(mam_mar)$coefficients[2,2],3))
mam_ma[4,1:2] <- c(round(summary(mam_feb)$coefficients[2,1],3), round(summary(mam_feb)$coefficients[2,2],3))
mam_ma[5,1:2] <- c(round(summary(mam_jan)$coefficients[2,1],3), round(summary(mam_jan)$coefficients[2,2],3))
mam_ma[6,1:2] <- c(round(summary(mam_dec)$coefficients[2,1],3), round(summary(mam_dec)$coefficients[2,2],3))
mam_ma[7,1:2] <- c(round(summary(mam_nov)$coefficients[2,1],3), round(summary(mam_nov)$coefficients[2,2],3))
mam_ma[8,1:2] <- c(round(summary(mam_oct)$coefficients[2,1],3), round(summary(mam_oct)$coefficients[2,2],3))
mam_ma[9,1:2] <- c(round(summary(mam_sep)$coefficients[2,1],3), round(summary(mam_sep)$coefficients[2,2],3))
mam_ma[10,1:2] <- c(round(summary(mam_aug)$coefficients[2,1],3), round(summary(mam_aug)$coefficients[2,2],3))
mam_ma[11,1:2] <- c(round(summary(mam_jul)$coefficients[2,1],3), round(summary(mam_jul)$coefficients[2,2],3))
mam_ma[12,1:2] <- c(round(summary(mam_jun)$coefficients[2,1],3), round(summary(mam_jun)$coefficients[2,2],3))
mam_ma[13,1:2] <- c(round(summary(mam_may2)$coefficients[2,1],3), round(summary(mam_may2)$coefficients[2,2],3))

mam_ma[1,3:4] <- c(round(summary(mam_may.12)$coefficients[2,1],3), round(summary(mam_may.12)$coefficients[2,2],3))
mam_ma[2,3:4] <- c(round(summary(mam_apr.12)$coefficients[2,1],3), round(summary(mam_apr.12)$coefficients[2,2],3))
mam_ma[3,3:4] <- c(round(summary(mam_mar.12)$coefficients[2,1],3), round(summary(mam_mar.12)$coefficients[2,2],3))
mam_ma[4,3:4] <- c(round(summary(mam_feb.12)$coefficients[2,1],3), round(summary(mam_feb.12)$coefficients[2,2],3))
mam_ma[5,3:4] <- c(round(summary(mam_jan.12)$coefficients[2,1],3), round(summary(mam_jan.12)$coefficients[2,2],3))
mam_ma[6,3:4] <- c(round(summary(mam_dec.12)$coefficients[2,1],3), round(summary(mam_dec.12)$coefficients[2,2],3))
mam_ma[7,3:4] <- c(round(summary(mam_nov.12)$coefficients[2,1],3), round(summary(mam_nov.12)$coefficients[2,2],3))
mam_ma[8,3:4] <- c(round(summary(mam_oct.12)$coefficients[2,1],3), round(summary(mam_oct.12)$coefficients[2,2],3))
mam_ma[9,3:4] <- c(round(summary(mam_sep.12)$coefficients[2,1],3), round(summary(mam_sep.12)$coefficients[2,2],3))
mam_ma[10,3:4] <- c(round(summary(mam_aug.12)$coefficients[2,1],3), round(summary(mam_aug.12)$coefficients[2,2],3))
mam_ma[11,3:4] <- c(round(summary(mam_jul.12)$coefficients[2,1],3), round(summary(mam_jul.12)$coefficients[2,2],3))
mam_ma[12,3:4] <- c(round(summary(mam_jun.12)$coefficients[2,1],3), round(summary(mam_jun.12)$coefficients[2,2],3))
mam_ma[13,3:4] <- c(round(summary(mam_may2.12)$coefficients[2,1],3), round(summary(mam_may2.12)$coefficients[2,2],3))

mam_ma[1,5:6] <- c(round(summary(mam_may.17)$coefficients[2,1],3), round(summary(mam_may.17)$coefficients[2,2],3))
mam_ma[2,5:6] <- c(round(summary(mam_apr.17)$coefficients[2,1],3), round(summary(mam_apr.17)$coefficients[2,2],3))
mam_ma[3,5:6] <- c(round(summary(mam_mar.17)$coefficients[2,1],3), round(summary(mam_mar.17)$coefficients[2,2],3))
mam_ma[4,5:6] <- c(round(summary(mam_feb.17)$coefficients[2,1],3), round(summary(mam_feb.17)$coefficients[2,2],3))
mam_ma[5,5:6] <- c(round(summary(mam_jan.17)$coefficients[2,1],3), round(summary(mam_jan.17)$coefficients[2,2],3))
mam_ma[6,5:6] <- c(round(summary(mam_dec.17)$coefficients[2,1],3), round(summary(mam_dec.17)$coefficients[2,2],3))
mam_ma[7,5:6] <- c(round(summary(mam_nov.17)$coefficients[2,1],3), round(summary(mam_nov.17)$coefficients[2,2],3))
mam_ma[8,5:6] <- c(round(summary(mam_oct.17)$coefficients[2,1],3), round(summary(mam_oct.17)$coefficients[2,2],3))
mam_ma[9,5:6] <- c(round(summary(mam_sep.17)$coefficients[2,1],3), round(summary(mam_sep.17)$coefficients[2,2],3))
mam_ma[10,5:6] <- c(round(summary(mam_aug.17)$coefficients[2,1],3), round(summary(mam_aug.17)$coefficients[2,2],3))
mam_ma[11,5:6] <- c(round(summary(mam_jul.17)$coefficients[2,1],3), round(summary(mam_jul.17)$coefficients[2,2],3))
mam_ma[12,5:6] <- c(round(summary(mam_jun.17)$coefficients[2,1],3), round(summary(mam_jun.17)$coefficients[2,2],3))
mam_ma[13,5:6] <- c(round(summary(mam_may2.17)$coefficients[2,1],3), round(summary(mam_may2.17)$coefficients[2,2],3))

## All data plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam <- data.frame(cbind(mam_ma[,1:2], row.names(mam_ma)))
colnames(AY_mam) <- c("est", "se", "Month")
AY_mam$est <- as.numeric(as.character(AY_mam$est))
AY_mam$se <- as.numeric(as.character(AY_mam$se))
AY_mam$Month <- as.character(AY_mam$Month)
AY_mam$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
mamplot <- ggplot(AY_mam, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid")  + ylab("Relative Percent Change") + ggtitle("MAM, 2007-2017") +
  scale_x_discrete(name="", limits=rev(c("May(lag12)", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May(lag0)"))) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data before July, 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam.12 <- data.frame(cbind(mam_ma[,3:4], row.names(mam_ma)))
colnames(AY_mam.12) <- c("est", "se", "Month")
AY_mam.12$est <- as.numeric(as.character(AY_mam.12$est))
AY_mam.12$se <- as.numeric(as.character(AY_mam.12$se))
AY_mam.12$Month <- as.character(AY_mam.12$Month)
AY_mam.12$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
mamplot12 <- ggplot(AY_mam.12, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Relative Percent Change") + ggtitle("MAM, Before RV1, 2007-2012") +
  scale_x_discrete(name="", limits=rev(c("May(lag12)", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May(lag0)")))  + 
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data after July, 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam.17 <- data.frame(cbind(mam_ma[,5:6], row.names(mam_ma)))
colnames(AY_mam.17) <- c("est", "se", "Month")
AY_mam.17$est <- as.numeric(as.character(AY_mam.17$est))
AY_mam.17$se <- as.numeric(as.character(AY_mam.17$se))
AY_mam.17$Month <- as.character(AY_mam.17$Month)
AY_mam.17$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
mamplot17 <- ggplot(AY_mam.17, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Beta Coefficient") + ggtitle("MAM, After RV1, 2012-2017") +
  scale_x_discrete(name="", limits=rev(c("May(lag12)", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May(lag0)")))  + 
  theme_bw() + ylab("") + geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Produce MAM Figure ##
plot_grid(mamplot, mamplot12, mamplot17, nrow=3, ncol=1, labels=c("(A)", "(B)", "(C)"), label_size = 16)

#### JJA Season #####

# Sum all under-5 diarrhea cases in JJA each year 
jja <- c(sum(JJA$diarrhea[1:3]), sum(JJA$diarrhea[4:6]), sum(JJA$diarrhea[7:9]), sum(JJA$diarrhea[10:12]), sum(JJA$diarrhea[13:15]),
         sum(JJA$diarrhea[16:18]), sum(JJA$diarrhea[19:21]), sum(JJA$diarrhea[22:24]), sum(JJA$diarrhea[25:27]), sum(JJA$diarrhea[28:30]))
jja.pred <- JJA[JJA$Month == 8,]
JJA.ar <- cbind(jja.pred, jja)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total JJA cases
jja_aug <- glm.nb(jja ~ NINO3.4, data=JJA.ar)
jja_jul <- glm.nb(jja ~ NINO3.41, data=JJA.ar)
jja_jun <- glm.nb(jja ~ NINO3.42, data=JJA.ar)
jja_may <- glm.nb(jja ~ NINO3.43, data=JJA.ar)
jja_apr <- glm.nb(jja ~ NINO3.44, data=JJA.ar)
jja_mar <- glm.nb(jja ~ NINO3.45, data=JJA.ar)
jja_feb <- glm.nb(jja ~ NINO3.46, data=JJA.ar)
jja_jan <- glm.nb(jja ~ NINO3.47, data=JJA.ar)
jja_dec <- glm.nb(jja ~ NINO3.48, data=JJA.ar)
jja_nov <- glm.nb(jja ~ NINO3.49, data=JJA.ar)
jja_oct <- glm.nb(jja ~ NINO3.410, data=JJA.ar)
jja_sep <- glm.nb(jja ~ NINO3.411, data=JJA.ar)
jja_aug2 <- glm.nb(jja ~ NINO3.412, data=JJA.ar)

stargazer(jja_aug, jja_jul, jja_jun, jja_may, jja_apr, jja_mar, type="text") # Summarize Regression Results

# Sum all under-5 diarrhea cases in MAM each year before July 2012 (RV intro)
jja.12 <- c(sum(JJA$diarrhea[1:3]), sum(JJA$diarrhea[4:6]), sum(JJA$diarrhea[7:9]), sum(JJA$diarrhea[10:12]), sum(JJA$diarrhea[13:15]))
jja.12.pred <- JJA.12[JJA.12$Month == 8,]
JJA.12.ar <- cbind(jja.12.pred, jja.12)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data before July, 2012
jja_aug.12 <- glm.nb(jja.12 ~ NINO3.4, data=JJA.12.ar)
jja_jul.12 <- glm.nb(jja.12 ~ NINO3.41, data=JJA.12.ar)
jja_jun.12 <- glm.nb(jja.12 ~ NINO3.42, data=JJA.12.ar)
jja_may.12 <- glm.nb(jja.12 ~ NINO3.43, data=JJA.12.ar)
jja_apr.12 <- glm.nb(jja.12 ~ NINO3.44, data=JJA.12.ar)
jja_mar.12 <- glm.nb(jja.12 ~ NINO3.45, data=JJA.12.ar)
jja_feb.12 <- glm.nb(jja.12 ~ NINO3.46, data=JJA.12.ar)
jja_jan.12 <- glm.nb(jja.12 ~ NINO3.47, data=JJA.12.ar)
jja_dec.12 <- glm.nb(jja.12 ~ NINO3.48, data=JJA.12.ar)
jja_nov.12 <- glm.nb(jja.12 ~ NINO3.49, data=JJA.12.ar)
jja_oct.12 <- glm.nb(jja.12 ~ NINO3.410, data=JJA.12.ar)
jja_sep.12 <- glm.nb(jja.12 ~ NINO3.411, data=JJA.12.ar)
jja_aug2.12 <- glm.nb(jja.12 ~ NINO3.412, data=JJA.12.ar)

stargazer(jja_aug.12, jja_jul.12, jja_jun.12, jja_may.12, jja_apr.12, jja_mar.12, type="text") # Summarize regression results

# Sum all under-5 diarrhea cases in MAM each year after July 2012 (RV intro)
jja.17 <- c(sum(JJA$diarrhea[16:18]), sum(JJA$diarrhea[19:21]), sum(JJA$diarrhea[22:24]), sum(JJA$diarrhea[25:27]), sum(JJA$diarrhea[28:30]))
jja.17.pred <- JJA.17[JJA.17$Month == 8,]
JJA.17.ar <- cbind(jja.17.pred, jja.17)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data after July, 2017
jja_aug.17 <- glm.nb(jja.17 ~ NINO3.4, data=JJA.17.ar)
jja_jul.17 <- glm.nb(jja.17 ~ NINO3.41, data=JJA.17.ar)
jja_jun.17 <- glm.nb(jja.17 ~ NINO3.42, data=JJA.17.ar)
jja_may.17 <- glm.nb(jja.17 ~ NINO3.43, data=JJA.17.ar)
jja_apr.17 <- glm.nb(jja.17 ~ NINO3.44, data=JJA.17.ar)
jja_mar.17 <- glm.nb(jja.17 ~ NINO3.45, data=JJA.17.ar)
jja_feb.17 <- glm.nb(jja.17 ~ NINO3.46, data=JJA.17.ar)
jja_jan.17 <- glm.nb(jja.17 ~ NINO3.47, data=JJA.17.ar)
jja_dec.17 <- glm.nb(jja.17 ~ NINO3.48, data=JJA.17.ar)
jja_nov.17 <- glm.nb(jja.17 ~ NINO3.49, data=JJA.17.ar)
jja_oct.17 <- glm.nb(jja.17 ~ NINO3.410, data=JJA.17.ar)
jja_sep.17 <- glm.nb(jja.17 ~ NINO3.411, data=JJA.17.ar)
jja_aug2.17 <- glm.nb(jja.17 ~ NINO3.412, data=JJA.17.ar)

stargazer(jja_aug.17, jja_jul.17, jja_jun.17, jja_may.17, jja_apr.17, jja_mar.17, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data (columns 1-2), data before July 2012 (columns 3-4), and data after July 2012 (columns 5-6)
jja_ma <- matrix(0, nrow=13, ncol=6)
colnames(jja_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se", "AV_estimate", "AV_se")
rownames(jja_ma) <- c("Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug2")
jja_ma[1,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))
jja_ma[2,1:2] <- c(round(summary(jja_jul)$coefficients[2,1],3), round(summary(jja_jul)$coefficients[2,2],3))
jja_ma[3,1:2] <- c(round(summary(jja_jun)$coefficients[2,1],3), round(summary(jja_jun)$coefficients[2,2],3))
jja_ma[4,1:2] <- c(round(summary(jja_may)$coefficients[2,1],3), round(summary(jja_may)$coefficients[2,2],3))
jja_ma[5,1:2] <- c(round(summary(jja_apr)$coefficients[2,1],3), round(summary(jja_apr)$coefficients[2,2],3))
jja_ma[6,1:2] <- c(round(summary(jja_mar)$coefficients[2,1],3), round(summary(jja_mar)$coefficients[2,2],3))
jja_ma[7,1:2] <- c(round(summary(jja_feb)$coefficients[2,1],3), round(summary(jja_feb)$coefficients[2,2],3))
jja_ma[8,1:2] <- c(round(summary(jja_jan)$coefficients[2,1],3), round(summary(jja_jan)$coefficients[2,2],3))
jja_ma[9,1:2] <- c(round(summary(jja_dec)$coefficients[2,1],3), round(summary(jja_dec)$coefficients[2,2],3))
jja_ma[10,1:2] <- c(round(summary(jja_nov)$coefficients[2,1],3), round(summary(jja_nov)$coefficients[2,2],3))
jja_ma[11,1:2] <- c(round(summary(jja_oct)$coefficients[2,1],3), round(summary(jja_oct)$coefficients[2,2],3))
jja_ma[12,1:2] <- c(round(summary(jja_sep)$coefficients[2,1],3), round(summary(jja_sep)$coefficients[2,2],3))
jja_ma[13,1:2] <- c(round(summary(jja_aug2)$coefficients[2,1],3), round(summary(jja_aug2)$coefficients[2,2],3))

jja_ma[1,3:4] <- c(round(summary(jja_aug.12)$coefficients[2,1],3), round(summary(jja_aug.12)$coefficients[2,2],3))
jja_ma[2,3:4] <- c(round(summary(jja_jul.12)$coefficients[2,1],3), round(summary(jja_jul.12)$coefficients[2,2],3))
jja_ma[3,3:4] <- c(round(summary(jja_jun.12)$coefficients[2,1],3), round(summary(jja_jun.12)$coefficients[2,2],3))
jja_ma[4,3:4] <- c(round(summary(jja_may.12)$coefficients[2,1],3), round(summary(jja_may.12)$coefficients[2,2],3))
jja_ma[5,3:4] <- c(round(summary(jja_apr.12)$coefficients[2,1],3), round(summary(jja_apr.12)$coefficients[2,2],3))
jja_ma[6,3:4] <- c(round(summary(jja_mar.12)$coefficients[2,1],3), round(summary(jja_mar.12)$coefficients[2,2],3))
jja_ma[7,3:4] <- c(round(summary(jja_feb.12)$coefficients[2,1],3), round(summary(jja_feb.12)$coefficients[2,2],3))
jja_ma[8,3:4] <- c(round(summary(jja_jan.12)$coefficients[2,1],3), round(summary(jja_jan.12)$coefficients[2,2],3))
jja_ma[9,3:4] <- c(round(summary(jja_dec.12)$coefficients[2,1],3), round(summary(jja_dec.12)$coefficients[2,2],3))
jja_ma[10,3:4] <- c(round(summary(jja_nov.12)$coefficients[2,1],3), round(summary(jja_nov.12)$coefficients[2,2],3))
jja_ma[11,3:4] <- c(round(summary(jja_oct.12)$coefficients[2,1],3), round(summary(jja_oct.12)$coefficients[2,2],3))
jja_ma[12,3:4] <- c(round(summary(jja_sep.12)$coefficients[2,1],3), round(summary(jja_sep.12)$coefficients[2,2],3))
jja_ma[13,3:4] <- c(round(summary(jja_aug2.12)$coefficients[2,1],3), round(summary(jja_aug2.12)$coefficients[2,2],3))

jja_ma[1,5:6] <- c(round(summary(jja_aug.17)$coefficients[2,1],3), round(summary(jja_aug.17)$coefficients[2,2],3))
jja_ma[2,5:6] <- c(round(summary(jja_jul.17)$coefficients[2,1],3), round(summary(jja_jul.17)$coefficients[2,2],3))
jja_ma[3,5:6] <- c(round(summary(jja_jun.17)$coefficients[2,1],3), round(summary(jja_jun.17)$coefficients[2,2],3))
jja_ma[4,5:6] <- c(round(summary(jja_may.17)$coefficients[2,1],3), round(summary(jja_may.17)$coefficients[2,2],3))
jja_ma[5,5:6] <- c(round(summary(jja_apr.17)$coefficients[2,1],3), round(summary(jja_apr.17)$coefficients[2,2],3))
jja_ma[6,5:6] <- c(round(summary(jja_mar.17)$coefficients[2,1],3), round(summary(jja_mar.17)$coefficients[2,2],3))
jja_ma[7,5:6] <- c(round(summary(jja_feb.17)$coefficients[2,1],3), round(summary(jja_feb.17)$coefficients[2,2],3))
jja_ma[8,5:6] <- c(round(summary(jja_jan.17)$coefficients[2,1],3), round(summary(jja_jan.17)$coefficients[2,2],3))
jja_ma[9,5:6] <- c(round(summary(jja_dec.17)$coefficients[2,1],3), round(summary(jja_dec.17)$coefficients[2,2],3))
jja_ma[10,5:6] <- c(round(summary(jja_nov.17)$coefficients[2,1],3), round(summary(jja_nov.17)$coefficients[2,2],3))
jja_ma[11,5:6] <- c(round(summary(jja_oct.17)$coefficients[2,1],3), round(summary(jja_oct.17)$coefficients[2,2],3))
jja_ma[12,5:6] <- c(round(summary(jja_sep.17)$coefficients[2,1],3), round(summary(jja_sep.17)$coefficients[2,2],3))
jja_ma[13,5:6] <- c(round(summary(jja_aug2.17)$coefficients[2,1],3), round(summary(jja_aug2.17)$coefficients[2,2],3))

## All data plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja <- data.frame(cbind(jja_ma[,1:2], row.names(jja_ma)))
colnames(AY_jja) <- c("est", "se", "Month")
AY_jja$est <- as.numeric(as.character(AY_jja$est))
AY_jja$se <- as.numeric(as.character(AY_jja$se))
AY_jja$Month <- as.character(AY_jja$Month)
AY_jja$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
jjaplot <- ggplot(AY_jja, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Relative Percent Change") + ggtitle("JJA, 2007-2017") +
  scale_x_discrete(name="", limits=rev(c("Aug(lag12)", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug(lag0)"))) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data before July 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja.12 <- data.frame(cbind(jja_ma[,3:4], row.names(jja_ma)))
colnames(AY_jja.12) <- c("est", "se", "Month")
AY_jja.12$est <- as.numeric(as.character(AY_jja.12$est))
AY_jja.12$se <- as.numeric(as.character(AY_jja.12$se))
AY_jja.12$Month <- as.character(AY_jja.12$Month)
AY_jja.12$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
jjaplot12 <- ggplot(AY_jja.12, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Relative Percent Change") + ggtitle("JJA, Before RV1, 2007-2012") +
  scale_x_discrete(name="", limits=rev(c("Aug(lag12)", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug(lag0)")))  + 
  theme_bw() +  geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data after July 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja.17 <- data.frame(cbind(jja_ma[,5:6], row.names(jja_ma)))
colnames(AY_jja.17) <- c("est", "se", "Month")
AY_jja.17$est <- as.numeric(as.character(AY_jja.17$est))
AY_jja.17$se <- as.numeric(as.character(AY_jja.17$se))
AY_jja.17$Month <- as.character(AY_jja.17$Month)
AY_jja.17$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
jjaplot17 <- ggplot(AY_jja.17, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Beta Coefficient") + ggtitle("JJA, After RV1, 2012-2017") +
  scale_x_discrete(name="", limits=rev(c("Aug(lag12)", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug(lag0)")))  + 
  theme_bw() + ylab("") + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Produce JJA Figure ##
plot_grid(jjaplot, jjaplot12, jjaplot17, nrow=3, ncol=1, labels=c("(A)", "(B)", "(C)"), label_size = 16)


#### SON Season ######

# Sum all under-5 diarrhea cases in SON each year 
son <- c(sum(SON$diarrhea[1:3]), sum(SON$diarrhea[4:6]), sum(SON$diarrhea[7:9]), sum(SON$diarrhea[10:12]), sum(SON$diarrhea[13:15]),
         sum(SON$diarrhea[16:18]), sum(SON$diarrhea[19:21]), sum(SON$diarrhea[22:24]), sum(SON$diarrhea[25:27]), sum(SON$diarrhea[28:30]))
son.pred <- SON[SON$Month == 11,]
SON.ar <- cbind(son.pred, son)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total SON cases
son_nov <- glm.nb(son ~ NINO3.4, data=SON.ar)
son_oct <- glm.nb(son ~ NINO3.41, data=SON.ar)
son_sep <- glm.nb(son ~ NINO3.42, data=SON.ar)
son_aug <- glm.nb(son ~ NINO3.43, data=SON.ar)
son_jul <- glm.nb(son ~ NINO3.44, data=SON.ar)
son_jun <- glm.nb(son ~ NINO3.45, data=SON.ar)
son_may <- glm.nb(son ~ NINO3.46, data=SON.ar)
son_apr <- glm.nb(son ~ NINO3.47, data=SON.ar)
son_mar <- glm.nb(son ~ NINO3.48, data=SON.ar)
son_feb <- glm.nb(son ~ NINO3.49, data=SON.ar)
son_jan <- glm.nb(son ~ NINO3.410, data=SON.ar)
son_dec <- glm.nb(son ~ NINO3.411, data=SON.ar)
son_nov2 <- glm.nb(son ~ NINO3.412, data=SON.ar)

stargazer(son_nov, son_oct, son_sep, son_aug, son_jul, son_jun, type="text") #Summarize regression results

# Sum all under-5 diarrhea cases in MAM each year before July 2012 (RV intro)
son.12 <- c(sum(SON$diarrhea[1:3]), sum(SON$diarrhea[4:6]), sum(SON$diarrhea[7:9]), sum(SON$diarrhea[10:12]), sum(SON$diarrhea[13:15]))
son.12.pred <- SON.12[SON.12$Month == 11,]
SON.12.ar <- cbind(son.12.pred, son.12)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data before July, 2012
son_nov.12 <- glm.nb(son.12 ~ NINO3.4, data=SON.12.ar)
son_oct.12 <- glm.nb(son.12 ~ NINO3.41, data=SON.12.ar)
son_sep.12 <- glm.nb(son.12 ~ NINO3.42, data=SON.12.ar)
son_aug.12 <- glm.nb(son.12 ~ NINO3.43, data=SON.12.ar)
son_jul.12 <- glm.nb(son.12 ~ NINO3.44, data=SON.12.ar)
son_jun.12 <- glm.nb(son.12 ~ NINO3.45, data=SON.12.ar)
son_may.12 <- glm.nb(son.12 ~ NINO3.46, data=SON.12.ar)
son_apr.12 <- glm.nb(son.12 ~ NINO3.47, data=SON.12.ar)
son_mar.12 <- glm.nb(son.12 ~ NINO3.48, data=SON.12.ar)
son_feb.12 <- glm.nb(son.12 ~ NINO3.49, data=SON.12.ar)
son_jan.12 <- glm.nb(son.12 ~ NINO3.410, data=SON.12.ar)
son_dec.12 <- glm.nb(son.12 ~ NINO3.411, data=SON.12.ar)
son_nov2.12 <- glm.nb(son.12 ~ NINO3.412, data=SON.12.ar)

stargazer(son_nov.12, son_oct.12, son_sep.12, son_aug.12, son_jul.12, son_jun.12, type="text") #Summarize regression reults

# Sum all under-5 diarrhea cases in MAM each year after July 2017 (RV intro)
son.17 <- c(sum(SON$diarrhea[16:18]), sum(SON$diarrhea[19:21]), sum(SON$diarrhea[22:24]), sum(SON$diarrhea[25:27]), sum(SON$diarrhea[28:30]))
son.17.pred <- SON.17[SON.17$Month == 11,]
SON.17.ar <- cbind(son.17.pred, son.17)

# Run negative binomial regressions using NINO3.4 lagged 0-12 months to predict total DJF cases using data after July, 2017
son_nov.17 <- glm.nb(son.17 ~ NINO3.4, data=SON.17.ar)
son_oct.17 <- glm.nb(son.17 ~ NINO3.41, data=SON.17.ar)
son_sep.17 <- glm.nb(son.17 ~ NINO3.42, data=SON.17.ar)
son_aug.17 <- glm.nb(son.17 ~ NINO3.43, data=SON.17.ar)
son_jul.17 <- glm.nb(son.17 ~ NINO3.44, data=SON.17.ar)
son_jun.17 <- glm.nb(son.17 ~ NINO3.45, data=SON.17.ar)
son_may.17 <- glm.nb(son.17 ~ NINO3.46, data=SON.17.ar)
son_apr.17 <- glm.nb(son.17 ~ NINO3.47, data=SON.17.ar)
son_mar.17 <- glm.nb(son.17 ~ NINO3.48, data=SON.17.ar)
son_feb.17 <- glm.nb(son.17 ~ NINO3.49, data=SON.17.ar)
son_jan.17 <- glm.nb(son.17 ~ NINO3.410, data=SON.17.ar)
son_dec.17 <- glm.nb(son.17 ~ NINO3.411, data=SON.17.ar)
son_nov2.17 <- glm.nb(son.17 ~ NINO3.412, data=SON.17.ar)

stargazer(son_nov.17, son_oct.17, son_sep.17, son_aug.17, son_jul.17, son_jun.17, type="text") #Summarize regression reults

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data (columns 1-2), data before July 2012 (columns 3-4), and data after July 2012 (columns 5-6)
son_ma <- matrix(0, nrow=13, ncol=6)
colnames(son_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se", "AV_estimate", "AV_se")
rownames(son_ma) <- c("Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov2")
son_ma[1,1:2] <- c(round(summary(son_nov)$coefficients[2,1],3), round(summary(son_nov)$coefficients[2,2],3))
son_ma[2,1:2] <- c(round(summary(son_oct)$coefficients[2,1],3), round(summary(son_oct)$coefficients[2,2],3))
son_ma[3,1:2] <- c(round(summary(son_sep)$coefficients[2,1],3), round(summary(son_sep)$coefficients[2,2],3))
son_ma[4,1:2] <- c(round(summary(son_aug)$coefficients[2,1],3), round(summary(son_aug)$coefficients[2,2],3))
son_ma[5,1:2] <- c(round(summary(son_jul)$coefficients[2,1],3), round(summary(son_jul)$coefficients[2,2],3))
son_ma[6,1:2] <- c(round(summary(son_jun)$coefficients[2,1],3), round(summary(son_jun)$coefficients[2,2],3))
son_ma[7,1:2] <- c(round(summary(son_may)$coefficients[2,1],3), round(summary(son_may)$coefficients[2,2],3))
son_ma[8,1:2] <- c(round(summary(son_apr)$coefficients[2,1],3), round(summary(son_apr)$coefficients[2,2],3))
son_ma[9,1:2] <- c(round(summary(son_mar)$coefficients[2,1],3), round(summary(son_mar)$coefficients[2,2],3))
son_ma[10,1:2] <- c(round(summary(son_feb)$coefficients[2,1],3), round(summary(son_feb)$coefficients[2,2],3))
son_ma[11,1:2] <- c(round(summary(son_jan)$coefficients[2,1],3), round(summary(son_jan)$coefficients[2,2],3))
son_ma[12,1:2] <- c(round(summary(son_dec)$coefficients[2,1],3), round(summary(son_dec)$coefficients[2,2],3))
son_ma[13,1:2] <- c(round(summary(son_nov2)$coefficients[2,1],3), round(summary(son_nov2)$coefficients[2,2],3))

son_ma[1,3:4] <- c(round(summary(son_nov.12)$coefficients[2,1],3), round(summary(son_nov.12)$coefficients[2,2],3))
son_ma[2,3:4] <- c(round(summary(son_oct.12)$coefficients[2,1],3), round(summary(son_oct.12)$coefficients[2,2],3))
son_ma[3,3:4] <- c(round(summary(son_sep.12)$coefficients[2,1],3), round(summary(son_sep.12)$coefficients[2,2],3))
son_ma[4,3:4] <- c(round(summary(son_aug.12)$coefficients[2,1],3), round(summary(son_aug.12)$coefficients[2,2],3))
son_ma[5,3:4] <- c(round(summary(son_jul.12)$coefficients[2,1],3), round(summary(son_jul.12)$coefficients[2,2],3))
son_ma[6,3:4] <- c(round(summary(son_jun.12)$coefficients[2,1],3), round(summary(son_jun.12)$coefficients[2,2],3))
son_ma[7,3:4] <- c(round(summary(son_may.12)$coefficients[2,1],3), round(summary(son_may.12)$coefficients[2,2],3))
son_ma[8,3:4] <- c(round(summary(son_apr.12)$coefficients[2,1],3), round(summary(son_apr.12)$coefficients[2,2],3))
son_ma[9,3:4] <- c(round(summary(son_mar.12)$coefficients[2,1],3), round(summary(son_mar.12)$coefficients[2,2],3))
son_ma[10,3:4] <- c(round(summary(son_feb.12)$coefficients[2,1],3), round(summary(son_feb.12)$coefficients[2,2],3))
son_ma[11,3:4] <- c(round(summary(son_jan.12)$coefficients[2,1],3), round(summary(son_jan.12)$coefficients[2,2],3))
son_ma[12,3:4] <- c(round(summary(son_dec.12)$coefficients[2,1],3), round(summary(son_dec.12)$coefficients[2,2],3))
son_ma[13,3:4] <- c(round(summary(son_nov2.12)$coefficients[2,1],3), round(summary(son_nov2.12)$coefficients[2,2],3))

son_ma[1,5:6] <- c(round(summary(son_nov.17)$coefficients[2,1],3), round(summary(son_nov.17)$coefficients[2,2],3))
son_ma[2,5:6] <- c(round(summary(son_oct.17)$coefficients[2,1],3), round(summary(son_oct.17)$coefficients[2,2],3))
son_ma[3,5:6] <- c(round(summary(son_sep.17)$coefficients[2,1],3), round(summary(son_sep.17)$coefficients[2,2],3))
son_ma[4,5:6] <- c(round(summary(son_aug.17)$coefficients[2,1],3), round(summary(son_aug.17)$coefficients[2,2],3))
son_ma[5,5:6] <- c(round(summary(son_jul.17)$coefficients[2,1],3), round(summary(son_jul.17)$coefficients[2,2],3))
son_ma[6,5:6] <- c(round(summary(son_jun.17)$coefficients[2,1],3), round(summary(son_jun.17)$coefficients[2,2],3))
son_ma[7,5:6] <- c(round(summary(son_may.17)$coefficients[2,1],3), round(summary(son_may.17)$coefficients[2,2],3))
son_ma[8,5:6] <- c(round(summary(son_apr.17)$coefficients[2,1],3), round(summary(son_apr.17)$coefficients[2,2],3))
son_ma[9,5:6] <- c(round(summary(son_mar.17)$coefficients[2,1],3), round(summary(son_mar.17)$coefficients[2,2],3))
son_ma[10,5:6] <- c(round(summary(son_feb.17)$coefficients[2,1],3), round(summary(son_feb.17)$coefficients[2,2],3))
son_ma[11,5:6] <- c(round(summary(son_jan.17)$coefficients[2,1],3), round(summary(son_jan.17)$coefficients[2,2],3))
son_ma[12,5:6] <- c(round(summary(son_dec.17)$coefficients[2,1],3), round(summary(son_dec.17)$coefficients[2,2],3))
son_ma[13,5:6] <- c(round(summary(son_nov2.17)$coefficients[2,1],3), round(summary(son_nov2.17)$coefficients[2,2],3))


## All data plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son <- data.frame(cbind(son_ma[,1:2], row.names(son_ma)))
colnames(AY_son) <- c("est", "se", "Month")
AY_son$est <- as.numeric(as.character(AY_son$est))
AY_son$se <- as.numeric(as.character(AY_son$se))
AY_son$Month <- as.character(AY_son$Month)
AY_son$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
sonplot <- ggplot(AY_son, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid")  + ylab("Relative Percent Change") + ggtitle("SON, 2007-2017") +
  scale_x_discrete(name="Month", limits=rev(c("Nov(lag12)", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov(lag0)"))) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") +  
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data before July 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son.12 <- data.frame(cbind(son_ma[,3:4], row.names(son_ma)))
colnames(AY_son.12) <- c("est", "se", "Month")
AY_son.12$est <- as.numeric(as.character(AY_son.12$est))
AY_son.12$se <- as.numeric(as.character(AY_son.12$se))
AY_son.12$Month <- as.character(AY_son.12$Month)
AY_son.12$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
sonplot12 <- ggplot(AY_son.12, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Relative Percent Change") + ggtitle("SON, Before RV1, 2007-2012") + xlab("Relative Percent Change") +
  scale_x_discrete(name="Month", limits=rev(c("Nov(lag12)", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov(lag0)")))  + 
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

## Data after July 2012 plot ##
# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son.17 <- data.frame(cbind(son_ma[,5:6], row.names(son_ma)))
colnames(AY_son.17) <- c("est", "se", "Month")
AY_son.17$est <- as.numeric(as.character(AY_son.17$est))
AY_son.17$se <- as.numeric(as.character(AY_son.17$se))
AY_son.17$Month <- as.character(AY_son.17$Month)
AY_son.17$month <- c(1:13)

#Plot coefficients and 95% confidence intervals
sonplot17 <- ggplot(AY_son.17, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="darkorchid", alpha=0.2) + 
  geom_point(color="darkorchid") + geom_line(color="darkorchid") + ylab("Beta Coefficient") + ggtitle("SON, After RV1, 2012-2017") +
  scale_x_discrete(name="Month", limits=rev(c("Nov(lag12)", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov(lag0)")))  + 
  theme_bw() + ylab("") + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=14)) 

# Make SON Figure #
plot_grid(sonplot, sonplot12, sonplot17, nrow=3, ncol=1, labels=c("(A)", "(B)", "(C)"), label_size = 16)

##### Produce diarrhea supplementary figures #####

plot_grid(djfplot12, djfplot17, mamplot12, mamplot17, jjaplot12, jjaplot17,
          sonplot12, sonplot17,
          nrow=4, ncol=2, labels=c("(A)", "", "(B)", "", "(C)", "", "(D)",  ""), label_size = 16)

plot_grid(djfplot, mamplot, jjaplot, sonplot, 
          nrow=4, ncol=1, labels=c("(A)", "(B)", "(C)", "(D)"), label_size = 16)

############################
####   Rainfall (mm)    ####
############################
#### DJF Season ######
# Sum all rainfall in DJF each year 
djf <- c(sum(DJF$rain_sum[3:5]), sum(DJF$rain_sum[6:8]), sum(DJF$rain_sum[9:11]), sum(DJF$rain_sum[12:14]), sum(DJF$rain_sum[15:17]),
         sum(DJF$rain_sum[18:20]), sum(DJF$rain_sum[21:23]), sum(DJF$rain_sum[24:26]), sum(DJF$rain_sum[27:29]), sum(DJF$rain_sum[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict total DJF rainfall
djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

stargazer(djf_feb, djf_jan, djf_dec, djf_nov, djf_oct, djf_sep, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

## Make DJF rainfall Figure ##
djfplot <- ggplot(AY_djf, aes(month, est)) + geom_ribbon(aes(ymin=(est-(1.96*se)), ymax=(est+(1.96*se))), fill="grey85") + 
  geom_point() + geom_line() + ylab("Beta Coefficient") + ggtitle("DJF, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Feb (lag0) ", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb (lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
djfplot

#### MAM Season ####
# Sum all rainfall in MAM each year 
mam <- c(sum(MAM$rain_sum[1:3]), sum(MAM$rain_sum[4:6]), sum(MAM$rain_sum[7:9]), sum(MAM$rain_sum[10:12]), sum(MAM$rain_sum[13:15]),
         sum(MAM$rain_sum[16:18]), sum(MAM$rain_sum[19:21]), sum(MAM$rain_sum[22:24]), sum(MAM$rain_sum[25:27]), sum(MAM$rain_sum[28:30]),
         sum(MAM$rain_sum[31:33]))
mam.pred <- MAM[MAM$Month == 5,]
MAM.ar <- cbind(mam.pred, mam)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict total MAM rainfall
mam_may <- lm(mam ~ NINO3.4, data=MAM.ar)
mam_apr <- lm(mam ~ NINO3.41, data=MAM.ar)
mam_mar <- lm(mam ~ NINO3.42, data=MAM.ar)
mam_feb <- lm(mam ~ NINO3.43, data=MAM.ar)
mam_jan <- lm(mam ~ NINO3.44, data=MAM.ar)
mam_dec <- lm(mam ~ NINO3.45, data=MAM.ar)
mam_nov <- lm(mam ~ NINO3.46, data=MAM.ar)
mam_oct <- lm(mam ~ NINO3.47, data=MAM.ar)
mam_sep <- lm(mam ~ NINO3.48, data=MAM.ar)
mam_aug <- lm(mam ~ NINO3.49, data=MAM.ar)
mam_jul <- lm(mam ~ NINO3.410, data=MAM.ar)
mam_jun <- lm(mam ~ NINO3.411, data=MAM.ar)
mam_may2 <- lm(mam ~ NINO3.412, data=MAM.ar)


stargazer(mam_may, mam_apr, mam_mar, mam_feb, mam_jan, mam_dec, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
mam_ma <- matrix(0, nrow=13, ncol=2)
colnames(mam_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(mam_ma) <- c("May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May2")
mam_ma[1,1:2] <- c(round(summary(mam_may)$coefficients[2,1],3), round(summary(mam_may)$coefficients[2,2],3))
mam_ma[2,1:2] <- c(round(summary(mam_apr)$coefficients[2,1],3), round(summary(mam_apr)$coefficients[2,2],3))
mam_ma[3,1:2] <- c(round(summary(mam_mar)$coefficients[2,1],3), round(summary(mam_mar)$coefficients[2,2],3))
mam_ma[4,1:2] <- c(round(summary(mam_feb)$coefficients[2,1],3), round(summary(mam_feb)$coefficients[2,2],3))
mam_ma[5,1:2] <- c(round(summary(mam_jan)$coefficients[2,1],3), round(summary(mam_jan)$coefficients[2,2],3))
mam_ma[6,1:2] <- c(round(summary(mam_dec)$coefficients[2,1],3), round(summary(mam_dec)$coefficients[2,2],3))
mam_ma[7,1:2] <- c(round(summary(mam_nov)$coefficients[2,1],3), round(summary(mam_nov)$coefficients[2,2],3))
mam_ma[8,1:2] <- c(round(summary(mam_oct)$coefficients[2,1],3), round(summary(mam_oct)$coefficients[2,2],3))
mam_ma[9,1:2] <- c(round(summary(mam_sep)$coefficients[2,1],3), round(summary(mam_sep)$coefficients[2,2],3))
mam_ma[10,1:2] <- c(round(summary(mam_aug)$coefficients[2,1],3), round(summary(mam_aug)$coefficients[2,2],3))
mam_ma[11,1:2] <- c(round(summary(mam_jul)$coefficients[2,1],3), round(summary(mam_jul)$coefficients[2,2],3))
mam_ma[12,1:2] <- c(round(summary(mam_jun)$coefficients[2,1],3), round(summary(mam_jun)$coefficients[2,2],3))
mam_ma[13,1:2] <- c(round(summary(mam_may2)$coefficients[2,1],3), round(summary(mam_may2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam <- data.frame(cbind(mam_ma[,1:2], row.names(mam_ma)))
colnames(AY_mam) <- c("est", "se", "Month")
AY_mam$est <- as.numeric(as.character(AY_mam$est))
AY_mam$se <- as.numeric(as.character(AY_mam$se))
AY_mam$Month <- as.character(AY_mam$Month)
AY_mam$month <- c(1:13)

# Make rainfall MAM figure #
mamplot <- ggplot(AY_mam, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("MAM, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("May(lag0)", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
mamplot

#### JJA Season ####
# Sum all rainfall in JJA each year 
jja <- c(sum(JJA$rain_sum[1:3]), sum(JJA$rain_sum[4:6]), sum(JJA$rain_sum[7:9]), sum(JJA$rain_sum[10:12]), sum(JJA$rain_sum[13:15]),
         sum(JJA$rain_sum[16:18]), sum(JJA$rain_sum[19:21]), sum(JJA$rain_sum[22:24]), sum(JJA$rain_sum[25:27]), sum(JJA$rain_sum[28:30]))
jja.pred <- JJA[JJA$Month == 8,]
JJA.ar <- cbind(jja.pred, jja)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict total JJA rainfall
jja_aug <- lm(jja ~ NINO3.4, data=JJA.ar)
jja_jul <- lm(jja ~ NINO3.41, data=JJA.ar)
jja_jun <- lm(jja ~ NINO3.42, data=JJA.ar)
jja_may <- lm(jja ~ NINO3.43, data=JJA.ar)
jja_apr <- lm(jja ~ NINO3.44, data=JJA.ar)
jja_mar <- lm(jja ~ NINO3.45, data=JJA.ar)
jja_feb <- lm(jja ~ NINO3.46, data=JJA.ar)
jja_jan <- lm(jja ~ NINO3.47, data=JJA.ar)
jja_dec <- lm(jja ~ NINO3.48, data=JJA.ar)
jja_nov <- lm(jja ~ NINO3.49, data=JJA.ar)
jja_oct <- lm(jja ~ NINO3.410, data=JJA.ar)
jja_sep <- lm(jja ~ NINO3.411, data=JJA.ar)
jja_aug2 <- lm(jja ~ NINO3.412, data=JJA.ar)

stargazer(jja_aug, jja_jul, jja_jun, jja_may, jja_apr, jja_mar, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
jja_ma <- matrix(0, nrow=13, ncol=4)
colnames(jja_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(jja_ma) <- c("Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug2")
jja_ma[1,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))
jja_ma[2,1:2] <- c(round(summary(jja_jul)$coefficients[2,1],3), round(summary(jja_jul)$coefficients[2,2],3))
jja_ma[3,1:2] <- c(round(summary(jja_jun)$coefficients[2,1],3), round(summary(jja_jun)$coefficients[2,2],3))
jja_ma[4,1:2] <- c(round(summary(jja_may)$coefficients[2,1],3), round(summary(jja_may)$coefficients[2,2],3))
jja_ma[5,1:2] <- c(round(summary(jja_apr)$coefficients[2,1],3), round(summary(jja_apr)$coefficients[2,2],3))
jja_ma[6,1:2] <- c(round(summary(jja_mar)$coefficients[2,1],3), round(summary(jja_mar)$coefficients[2,2],3))
jja_ma[7,1:2] <- c(round(summary(jja_feb)$coefficients[2,1],3), round(summary(jja_feb)$coefficients[2,2],3))
jja_ma[8,1:2] <- c(round(summary(jja_jan)$coefficients[2,1],3), round(summary(jja_jan)$coefficients[2,2],3))
jja_ma[9,1:2] <- c(round(summary(jja_dec)$coefficients[2,1],3), round(summary(jja_dec)$coefficients[2,2],3))
jja_ma[10,1:2] <- c(round(summary(jja_nov)$coefficients[2,1],3), round(summary(jja_nov)$coefficients[2,2],3))
jja_ma[11,1:2] <- c(round(summary(jja_oct)$coefficients[2,1],3), round(summary(jja_oct)$coefficients[2,2],3))
jja_ma[12,1:2] <- c(round(summary(jja_sep)$coefficients[2,1],3), round(summary(jja_sep)$coefficients[2,2],3))
jja_ma[13,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja <- data.frame(cbind(jja_ma[,1:2], row.names(jja_ma)))
colnames(AY_jja) <- c("est", "se", "Month")
AY_jja$est <- as.numeric(as.character(AY_jja$est))
AY_jja$se <- as.numeric(as.character(AY_jja$se))
AY_jja$Month <- as.character(AY_jja$Month)
AY_jja$month <- c(1:13)

# Make JJA rainfall plot #
jjaplot <- ggplot(AY_jja, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("JJA, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Aug(lag0)", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
jjaplot

#### SON Season ######
# Sum all rainfall in SON each year 
son <- c(sum(SON$rain_sum[1:3]), sum(SON$rain_sum[4:6]), sum(SON$rain_sum[7:9]), sum(SON$rain_sum[10:12]), sum(SON$rain_sum[13:15]),
         sum(SON$rain_sum[16:18]), sum(SON$rain_sum[19:21]), sum(SON$rain_sum[22:24]), sum(SON$rain_sum[25:27]), sum(SON$rain_sum[28:30]))
son.pred <- SON[SON$Month == 11,]
SON.ar <- cbind(son.pred, son)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict total SON rainfall
son_nov <- lm(son ~ NINO3.4, data=SON.ar)
son_oct <- lm(son ~ NINO3.41, data=SON.ar)
son_sep <- lm(son ~ NINO3.42, data=SON.ar)
son_aug <- lm(son ~ NINO3.43, data=SON.ar)
son_jul <- lm(son ~ NINO3.44, data=SON.ar)
son_jun <- lm(son ~ NINO3.45, data=SON.ar)
son_may <- lm(son ~ NINO3.46, data=SON.ar)
son_apr <- lm(son ~ NINO3.47, data=SON.ar)
son_mar <- lm(son ~ NINO3.48, data=SON.ar)
son_feb <- lm(son ~ NINO3.49, data=SON.ar)
son_jan <- lm(son ~ NINO3.410, data=SON.ar)
son_dec <- lm(son ~ NINO3.411, data=SON.ar)
son_nov2 <- lm(son ~ NINO3.412, data=SON.ar)

stargazer(son_nov, son_oct, son_sep, son_aug, son_jul, son_jun, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
son_ma <- matrix(0, nrow=13, ncol=4)
colnames(son_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(son_ma) <- c("Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov2")
son_ma[1,1:2] <- c(round(summary(son_nov)$coefficients[2,1],3), round(summary(son_nov)$coefficients[2,2],3))
son_ma[2,1:2] <- c(round(summary(son_oct)$coefficients[2,1],3), round(summary(son_oct)$coefficients[2,2],3))
son_ma[3,1:2] <- c(round(summary(son_sep)$coefficients[2,1],3), round(summary(son_sep)$coefficients[2,2],3))
son_ma[4,1:2] <- c(round(summary(son_aug)$coefficients[2,1],3), round(summary(son_aug)$coefficients[2,2],3))
son_ma[5,1:2] <- c(round(summary(son_jul)$coefficients[2,1],3), round(summary(son_jul)$coefficients[2,2],3))
son_ma[6,1:2] <- c(round(summary(son_jun)$coefficients[2,1],3), round(summary(son_jun)$coefficients[2,2],3))
son_ma[7,1:2] <- c(round(summary(son_may)$coefficients[2,1],3), round(summary(son_may)$coefficients[2,2],3))
son_ma[8,1:2] <- c(round(summary(son_apr)$coefficients[2,1],3), round(summary(son_apr)$coefficients[2,2],3))
son_ma[9,1:2] <- c(round(summary(son_mar)$coefficients[2,1],3), round(summary(son_mar)$coefficients[2,2],3))
son_ma[10,1:2] <- c(round(summary(son_feb)$coefficients[2,1],3), round(summary(son_feb)$coefficients[2,2],3))
son_ma[11,1:2] <- c(round(summary(son_jan)$coefficients[2,1],3), round(summary(son_jan)$coefficients[2,2],3))
son_ma[12,1:2] <- c(round(summary(son_dec)$coefficients[2,1],3), round(summary(son_dec)$coefficients[2,2],3))
son_ma[13,1:2] <- c(round(summary(son_nov2)$coefficients[2,1],3), round(summary(son_nov2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son <- data.frame(cbind(son_ma[,1:2], row.names(son_ma)))
colnames(AY_son) <- c("est", "se", "Month")
AY_son$est <- as.numeric(as.character(AY_son$est))
AY_son$se <- as.numeric(as.character(AY_son$se))
AY_son$Month <- as.character(AY_son$Month)
AY_son$month <- c(1:13)

# Make SON rainfall figure 
sonplot <- ggplot(AY_son, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("SON, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Nov(lag0)", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + xlab("Month") +
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
sonplot


#### Rainfall Supplemental Figure ####

plot_grid(djfplot, mamplot, jjaplot, sonplot, 
          nrow=4, ncol=1, labels=c("(A)", "(B)", "(C)", "(D)"), label_size = 24)

############################
#### River Height (m)   ####
############################
##### DJF Season ######
# Average Chobe river height in DJF each year 
djf <- c(mean(DJF$height[3:5]), mean(DJF$height[6:8]), mean(DJF$height[9:11]), mean(DJF$height[12:14]), mean(DJF$height[15:17]),
         mean(DJF$height[18:20]), mean(DJF$height[21:23]), mean(DJF$height[24:26]), mean(DJF$height[27:29]), mean(DJF$height[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict average DJF river height
djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

stargazer(djf_feb, djf_jan, djf_dec, djf_nov, djf_oct, djf_sep, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

# Make DJF River Height Figure #
djfplot <- ggplot(AY_djf, aes(month, est)) + geom_ribbon(aes(ymin=(est-(1.96*se)), ymax=(est+(1.96*se))), fill="grey85") + 
  geom_point() + geom_line() + ylab("Beta Coefficient") + ggtitle("DJF, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Feb (lag0) ", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb (lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
djfplot

##### MAM Season ######
# Average Chobe river height in MAM each year 
mam <- c(mean(MAM$height[1:3]), mean(MAM$height[4:6]), mean(MAM$height[7:9]), mean(MAM$height[10:12]), mean(MAM$height[13:15]),
         mean(MAM$height[16:18]), mean(MAM$height[19:21]), mean(MAM$height[22:24]), mean(MAM$height[25:27]), mean(MAM$height[28:30]),
         mean(MAM$height[31:33]))
mam.pred <- MAM[MAM$Month == 5,]
MAM.ar <- cbind(mam.pred, mam)

# Run Gaussian regressions using NINO3.4 lagged 0-12 months to predict average MAM River Height
mam_may <- lm(mam ~ NINO3.4, data=MAM.ar)
mam_apr <- lm(mam ~ NINO3.41, data=MAM.ar)
mam_mar <- lm(mam ~ NINO3.42, data=MAM.ar)
mam_feb <- lm(mam ~ NINO3.43, data=MAM.ar)
mam_jan <- lm(mam ~ NINO3.44, data=MAM.ar)
mam_dec <- lm(mam ~ NINO3.45, data=MAM.ar)
mam_nov <- lm(mam ~ NINO3.46, data=MAM.ar)
mam_oct <- lm(mam ~ NINO3.47, data=MAM.ar)
mam_sep <- lm(mam ~ NINO3.48, data=MAM.ar)
mam_aug <- lm(mam ~ NINO3.49, data=MAM.ar)
mam_jul <- lm(mam ~ NINO3.410, data=MAM.ar)
mam_jun <- lm(mam ~ NINO3.411, data=MAM.ar)
mam_may2 <- lm(mam ~ NINO3.412, data=MAM.ar)


stargazer(mam_may, mam_apr, mam_mar, mam_feb, mam_jan, mam_dec, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
mam_ma <- matrix(0, nrow=13, ncol=2)
colnames(mam_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(mam_ma) <- c("May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May2")
mam_ma[1,1:2] <- c(round(summary(mam_may)$coefficients[2,1],3), round(summary(mam_may)$coefficients[2,2],3))
mam_ma[2,1:2] <- c(round(summary(mam_apr)$coefficients[2,1],3), round(summary(mam_apr)$coefficients[2,2],3))
mam_ma[3,1:2] <- c(round(summary(mam_mar)$coefficients[2,1],3), round(summary(mam_mar)$coefficients[2,2],3))
mam_ma[4,1:2] <- c(round(summary(mam_feb)$coefficients[2,1],3), round(summary(mam_feb)$coefficients[2,2],3))
mam_ma[5,1:2] <- c(round(summary(mam_jan)$coefficients[2,1],3), round(summary(mam_jan)$coefficients[2,2],3))
mam_ma[6,1:2] <- c(round(summary(mam_dec)$coefficients[2,1],3), round(summary(mam_dec)$coefficients[2,2],3))
mam_ma[7,1:2] <- c(round(summary(mam_nov)$coefficients[2,1],3), round(summary(mam_nov)$coefficients[2,2],3))
mam_ma[8,1:2] <- c(round(summary(mam_oct)$coefficients[2,1],3), round(summary(mam_oct)$coefficients[2,2],3))
mam_ma[9,1:2] <- c(round(summary(mam_sep)$coefficients[2,1],3), round(summary(mam_sep)$coefficients[2,2],3))
mam_ma[10,1:2] <- c(round(summary(mam_aug)$coefficients[2,1],3), round(summary(mam_aug)$coefficients[2,2],3))
mam_ma[11,1:2] <- c(round(summary(mam_jul)$coefficients[2,1],3), round(summary(mam_jul)$coefficients[2,2],3))
mam_ma[12,1:2] <- c(round(summary(mam_jun)$coefficients[2,1],3), round(summary(mam_jun)$coefficients[2,2],3))
mam_ma[13,1:2] <- c(round(summary(mam_may2)$coefficients[2,1],3), round(summary(mam_may2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam <- data.frame(cbind(mam_ma[,1:2], row.names(mam_ma)))
colnames(AY_mam) <- c("est", "se", "Month")
AY_mam$est <- as.numeric(as.character(AY_mam$est))
AY_mam$se <- as.numeric(as.character(AY_mam$se))
AY_mam$Month <- as.character(AY_mam$Month)
AY_mam$month <- c(1:13)

# Make River Height MAM figure #
mamplot <- ggplot(AY_mam, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("MAM, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("May(lag0)", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 


##### JJA Season ######
# Average Chobe river height in JJA each year 
jja <- c(mean(JJA$height[1:3]), mean(JJA$height[4:6]), mean(JJA$height[7:9]), mean(JJA$height[10:12]), mean(JJA$height[13:15]),
         mean(JJA$height[16:18]), mean(JJA$height[19:21]), mean(JJA$height[22:24]), mean(JJA$height[25:27]), mean(JJA$height[28:30], na.rm=TRUE))
jja.pred <- JJA[JJA$Month == 8,]
JJA.ar <- cbind(jja.pred, jja)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF river height
jja_aug <- lm(jja ~ NINO3.4, data=JJA.ar)
jja_jul <- lm(jja ~ NINO3.41, data=JJA.ar)
jja_jun <- lm(jja ~ NINO3.42, data=JJA.ar)
jja_may <- lm(jja ~ NINO3.43, data=JJA.ar)
jja_apr <- lm(jja ~ NINO3.44, data=JJA.ar)
jja_mar <- lm(jja ~ NINO3.45, data=JJA.ar)
jja_feb <- lm(jja ~ NINO3.46, data=JJA.ar)
jja_jan <- lm(jja ~ NINO3.47, data=JJA.ar)
jja_dec <- lm(jja ~ NINO3.48, data=JJA.ar)
jja_nov <- lm(jja ~ NINO3.49, data=JJA.ar)
jja_oct <- lm(jja ~ NINO3.410, data=JJA.ar)
jja_sep <- lm(jja ~ NINO3.411, data=JJA.ar)
jja_aug2 <- lm(jja ~ NINO3.412, data=JJA.ar)

stargazer(jja_aug, jja_jul, jja_jun, jja_may, jja_apr, jja_mar, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
jja_ma <- matrix(0, nrow=13, ncol=2)
colnames(jja_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(jja_ma) <- c("Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug2")
jja_ma[1,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))
jja_ma[2,1:2] <- c(round(summary(jja_jul)$coefficients[2,1],3), round(summary(jja_jul)$coefficients[2,2],3))
jja_ma[3,1:2] <- c(round(summary(jja_jun)$coefficients[2,1],3), round(summary(jja_jun)$coefficients[2,2],3))
jja_ma[4,1:2] <- c(round(summary(jja_may)$coefficients[2,1],3), round(summary(jja_may)$coefficients[2,2],3))
jja_ma[5,1:2] <- c(round(summary(jja_apr)$coefficients[2,1],3), round(summary(jja_apr)$coefficients[2,2],3))
jja_ma[6,1:2] <- c(round(summary(jja_mar)$coefficients[2,1],3), round(summary(jja_mar)$coefficients[2,2],3))
jja_ma[7,1:2] <- c(round(summary(jja_feb)$coefficients[2,1],3), round(summary(jja_feb)$coefficients[2,2],3))
jja_ma[8,1:2] <- c(round(summary(jja_jan)$coefficients[2,1],3), round(summary(jja_jan)$coefficients[2,2],3))
jja_ma[9,1:2] <- c(round(summary(jja_dec)$coefficients[2,1],3), round(summary(jja_dec)$coefficients[2,2],3))
jja_ma[10,1:2] <- c(round(summary(jja_nov)$coefficients[2,1],3), round(summary(jja_nov)$coefficients[2,2],3))
jja_ma[11,1:2] <- c(round(summary(jja_oct)$coefficients[2,1],3), round(summary(jja_oct)$coefficients[2,2],3))
jja_ma[12,1:2] <- c(round(summary(jja_sep)$coefficients[2,1],3), round(summary(jja_sep)$coefficients[2,2],3))
jja_ma[13,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja <- data.frame(cbind(jja_ma[,1:2], row.names(jja_ma)))
colnames(AY_jja) <- c("est", "se", "Month")
AY_jja$est <- as.numeric(as.character(AY_jja$est))
AY_jja$se <- as.numeric(as.character(AY_jja$se))
AY_jja$Month <- as.character(AY_jja$Month)
AY_jja$month <- c(1:13)

# Make JJA River Height plot #
jjaplot <- ggplot(AY_jja, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("JJA, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Aug(lag0)", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
jjaplot

##### SON Season ######
# Average Chobe river height in SON each year 
son <- c(mean(SON$height[1:3]), mean(SON$height[4:6]), mean(SON$height[7:9]), mean(SON$height[10:12]), mean(SON$height[13:15]),
         mean(SON$height[16:18]), mean(SON$height[19:21]), mean(SON$height[22:24]), mean(SON$height[25:27]), mean(SON$height[28:30]))
son.pred <- SON[SON$Month == 11,]
SON.ar <- cbind(son.pred, son)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF river height
son_nov <- lm(son ~ NINO3.4, data=SON.ar)
son_oct <- lm(son ~ NINO3.41, data=SON.ar)
son_sep <- lm(son ~ NINO3.42, data=SON.ar)
son_aug <- lm(son ~ NINO3.43, data=SON.ar)
son_jul <- lm(son ~ NINO3.44, data=SON.ar)
son_jun <- lm(son ~ NINO3.45, data=SON.ar)
son_may <- lm(son ~ NINO3.46, data=SON.ar)
son_apr <- lm(son ~ NINO3.47, data=SON.ar)
son_mar <- lm(son ~ NINO3.48, data=SON.ar)
son_feb <- lm(son ~ NINO3.49, data=SON.ar)
son_jan <- lm(son ~ NINO3.410, data=SON.ar)
son_dec <- lm(son ~ NINO3.411, data=SON.ar)
son_nov2 <- lm(son ~ NINO3.412, data=SON.ar)

stargazer(son_nov, son_oct, son_sep, son_aug, son_jul, son_jun, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
son_ma <- matrix(0, nrow=13, ncol=2)
colnames(son_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(son_ma) <- c("Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov2")
son_ma[1,1:2] <- c(round(summary(son_nov)$coefficients[2,1],3), round(summary(son_nov)$coefficients[2,2],3))
son_ma[2,1:2] <- c(round(summary(son_oct)$coefficients[2,1],3), round(summary(son_oct)$coefficients[2,2],3))
son_ma[3,1:2] <- c(round(summary(son_sep)$coefficients[2,1],3), round(summary(son_sep)$coefficients[2,2],3))
son_ma[4,1:2] <- c(round(summary(son_aug)$coefficients[2,1],3), round(summary(son_aug)$coefficients[2,2],3))
son_ma[5,1:2] <- c(round(summary(son_jul)$coefficients[2,1],3), round(summary(son_jul)$coefficients[2,2],3))
son_ma[6,1:2] <- c(round(summary(son_jun)$coefficients[2,1],3), round(summary(son_jun)$coefficients[2,2],3))
son_ma[7,1:2] <- c(round(summary(son_may)$coefficients[2,1],3), round(summary(son_may)$coefficients[2,2],3))
son_ma[8,1:2] <- c(round(summary(son_apr)$coefficients[2,1],3), round(summary(son_apr)$coefficients[2,2],3))
son_ma[9,1:2] <- c(round(summary(son_mar)$coefficients[2,1],3), round(summary(son_mar)$coefficients[2,2],3))
son_ma[10,1:2] <- c(round(summary(son_feb)$coefficients[2,1],3), round(summary(son_feb)$coefficients[2,2],3))
son_ma[11,1:2] <- c(round(summary(son_jan)$coefficients[2,1],3), round(summary(son_jan)$coefficients[2,2],3))
son_ma[12,1:2] <- c(round(summary(son_dec)$coefficients[2,1],3), round(summary(son_dec)$coefficients[2,2],3))
son_ma[13,1:2] <- c(round(summary(son_nov2)$coefficients[2,1],3), round(summary(son_nov2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son <- data.frame(cbind(son_ma[,1:2], row.names(son_ma)))
colnames(AY_son) <- c("est", "se", "Month")
AY_son$est <- as.numeric(as.character(AY_son$est))
AY_son$se <- as.numeric(as.character(AY_son$se))
AY_son$Month <- as.character(AY_son$Month)
AY_son$month <- c(1:13)

# Make SON River Height figure 
sonplot <- ggplot(AY_son, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("SON, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Nov(lag0)", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + xlab("Month") +
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
sonplot


##### River Height Supplemental Figure #############
plot_grid(djfplot, mamplot, jjaplot, sonplot, 
          nrow=4, ncol=1, labels=c("(A)", "(B)", "(C)", "(D)"), label_size = 24)

############################
#### Minimum Temp (C)   ####
############################
#### DJF Season ######
# Average minimum temperature in DJF each year 
djf <- c(mean(DJF$tmin[3:5]), mean(DJF$tmin[6:8]), mean(DJF$tmin[9:11]), mean(DJF$tmin[12:14]), mean(DJF$tmin[15:17]),
         mean(DJF$tmin[18:20]), mean(DJF$tmin[21:23]), mean(DJF$tmin[24:26]), mean(DJF$tmin[27:29]), mean(DJF$tmin[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF minimum temperature
djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

stargazer(djf_feb, djf_jan, djf_dec, djf_nov, djf_oct, djf_sep, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

# Make DJF Min. Temp. Figure #
djfplot <- ggplot(AY_djf, aes(month, est)) + geom_ribbon(aes(ymin=(est-(1.96*se)), ymax=(est+(1.96*se))), fill="grey85") + 
  geom_point() + geom_line() + ylab("Beta Coefficient") + ggtitle("DJF, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Feb (lag0) ", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb (lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 

#### MAM Season ######
# Average minimum temperature in MAM each year 
mam <- c(mean(MAM$tmin[1:3]), mean(MAM$tmin[4:6]), mean(MAM$tmin[7:9]), mean(MAM$tmin[10:12]), mean(MAM$tmin[13:15]),
         mean(MAM$tmin[16:18]), mean(MAM$tmin[19:21]), mean(MAM$tmin[22:24]), mean(MAM$tmin[25:27]), mean(MAM$tmin[28:30]),
         mean(MAM$tmin[31:33]))
mam.pred <- MAM[MAM$Month == 5,]
MAM.ar <- cbind(mam.pred, mam)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF minimum temperature
mam_may <- lm(mam ~ NINO3.4, data=MAM.ar)
mam_apr <- lm(mam ~ NINO3.41, data=MAM.ar)
mam_mar <- lm(mam ~ NINO3.42, data=MAM.ar)
mam_feb <- lm(mam ~ NINO3.43, data=MAM.ar)
mam_jan <- lm(mam ~ NINO3.44, data=MAM.ar)
mam_dec <- lm(mam ~ NINO3.45, data=MAM.ar)
mam_nov <- lm(mam ~ NINO3.46, data=MAM.ar)
mam_oct <- lm(mam ~ NINO3.47, data=MAM.ar)
mam_sep <- lm(mam ~ NINO3.48, data=MAM.ar)
mam_aug <- lm(mam ~ NINO3.49, data=MAM.ar)
mam_jul <- lm(mam ~ NINO3.410, data=MAM.ar)
mam_jun <- lm(mam ~ NINO3.411, data=MAM.ar)
mam_may2 <- lm(mam ~ NINO3.412, data=MAM.ar)


stargazer(mam_may, mam_apr, mam_mar, mam_feb, mam_jan, mam_dec, type="text") # Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
mam_ma <- matrix(0, nrow=13, ncol=2)
colnames(mam_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(mam_ma) <- c("May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May2")
mam_ma[1,1:2] <- c(round(summary(mam_may)$coefficients[2,1],3), round(summary(mam_may)$coefficients[2,2],3))
mam_ma[2,1:2] <- c(round(summary(mam_apr)$coefficients[2,1],3), round(summary(mam_apr)$coefficients[2,2],3))
mam_ma[3,1:2] <- c(round(summary(mam_mar)$coefficients[2,1],3), round(summary(mam_mar)$coefficients[2,2],3))
mam_ma[4,1:2] <- c(round(summary(mam_feb)$coefficients[2,1],3), round(summary(mam_feb)$coefficients[2,2],3))
mam_ma[5,1:2] <- c(round(summary(mam_jan)$coefficients[2,1],3), round(summary(mam_jan)$coefficients[2,2],3))
mam_ma[6,1:2] <- c(round(summary(mam_dec)$coefficients[2,1],3), round(summary(mam_dec)$coefficients[2,2],3))
mam_ma[7,1:2] <- c(round(summary(mam_nov)$coefficients[2,1],3), round(summary(mam_nov)$coefficients[2,2],3))
mam_ma[8,1:2] <- c(round(summary(mam_oct)$coefficients[2,1],3), round(summary(mam_oct)$coefficients[2,2],3))
mam_ma[9,1:2] <- c(round(summary(mam_sep)$coefficients[2,1],3), round(summary(mam_sep)$coefficients[2,2],3))
mam_ma[10,1:2] <- c(round(summary(mam_aug)$coefficients[2,1],3), round(summary(mam_aug)$coefficients[2,2],3))
mam_ma[11,1:2] <- c(round(summary(mam_jul)$coefficients[2,1],3), round(summary(mam_jul)$coefficients[2,2],3))
mam_ma[12,1:2] <- c(round(summary(mam_jun)$coefficients[2,1],3), round(summary(mam_jun)$coefficients[2,2],3))
mam_ma[13,1:2] <- c(round(summary(mam_may2)$coefficients[2,1],3), round(summary(mam_may2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_mam <- data.frame(cbind(mam_ma[,1:2], row.names(mam_ma)))
colnames(AY_mam) <- c("est", "se", "Month")
AY_mam$est <- as.numeric(as.character(AY_mam$est))
AY_mam$se <- as.numeric(as.character(AY_mam$se))
AY_mam$Month <- as.character(AY_mam$Month)
AY_mam$month <- c(1:13)

# Make Min. Temp. MAM figure #
mamplot <- ggplot(AY_mam, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("MAM, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("May(lag0)", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 


#### JJA Season ######
# Average minimum temperature in JJA each year 
jja <- c(mean(JJA$tmin[1:3]), mean(JJA$tmin[4:6]), mean(JJA$tmin[7:9]), mean(JJA$tmin[10:12]), mean(JJA$tmin[13:15]),
         mean(JJA$tmin[16:18]), mean(JJA$tmin[19:21]), mean(JJA$tmin[22:24]), mean(JJA$tmin[25:27]), mean(JJA$tmin[28:30]))
jja.pred <- JJA[JJA$Month == 8,]
JJA.ar <- cbind(jja.pred, jja)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF minimum temperature
jja_aug <- lm(jja ~ NINO3.4, data=JJA.ar)
jja_jul <- lm(jja ~ NINO3.41, data=JJA.ar)
jja_jun <- lm(jja ~ NINO3.42, data=JJA.ar)
jja_may <- lm(jja ~ NINO3.43, data=JJA.ar)
jja_apr <- lm(jja ~ NINO3.44, data=JJA.ar)
jja_mar <- lm(jja ~ NINO3.45, data=JJA.ar)
jja_feb <- lm(jja ~ NINO3.46, data=JJA.ar)
jja_jan <- lm(jja ~ NINO3.47, data=JJA.ar)
jja_dec <- lm(jja ~ NINO3.48, data=JJA.ar)
jja_nov <- lm(jja ~ NINO3.49, data=JJA.ar)
jja_oct <- lm(jja ~ NINO3.410, data=JJA.ar)
jja_sep <- lm(jja ~ NINO3.411, data=JJA.ar)
jja_aug2 <- lm(jja ~ NINO3.412, data=JJA.ar)

stargazer(jja_aug, jja_jul, jja_jun, jja_may, jja_apr, jja_mar, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
jja_ma <- matrix(0, nrow=13, ncol=4)
colnames(jja_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(jja_ma) <- c("Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug2")
jja_ma[1,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))
jja_ma[2,1:2] <- c(round(summary(jja_jul)$coefficients[2,1],3), round(summary(jja_jul)$coefficients[2,2],3))
jja_ma[3,1:2] <- c(round(summary(jja_jun)$coefficients[2,1],3), round(summary(jja_jun)$coefficients[2,2],3))
jja_ma[4,1:2] <- c(round(summary(jja_may)$coefficients[2,1],3), round(summary(jja_may)$coefficients[2,2],3))
jja_ma[5,1:2] <- c(round(summary(jja_apr)$coefficients[2,1],3), round(summary(jja_apr)$coefficients[2,2],3))
jja_ma[6,1:2] <- c(round(summary(jja_mar)$coefficients[2,1],3), round(summary(jja_mar)$coefficients[2,2],3))
jja_ma[7,1:2] <- c(round(summary(jja_feb)$coefficients[2,1],3), round(summary(jja_feb)$coefficients[2,2],3))
jja_ma[8,1:2] <- c(round(summary(jja_jan)$coefficients[2,1],3), round(summary(jja_jan)$coefficients[2,2],3))
jja_ma[9,1:2] <- c(round(summary(jja_dec)$coefficients[2,1],3), round(summary(jja_dec)$coefficients[2,2],3))
jja_ma[10,1:2] <- c(round(summary(jja_nov)$coefficients[2,1],3), round(summary(jja_nov)$coefficients[2,2],3))
jja_ma[11,1:2] <- c(round(summary(jja_oct)$coefficients[2,1],3), round(summary(jja_oct)$coefficients[2,2],3))
jja_ma[12,1:2] <- c(round(summary(jja_sep)$coefficients[2,1],3), round(summary(jja_sep)$coefficients[2,2],3))
jja_ma[13,1:2] <- c(round(summary(jja_aug)$coefficients[2,1],3), round(summary(jja_aug)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_jja <- data.frame(cbind(jja_ma[,1:2], row.names(jja_ma)))
colnames(AY_jja) <- c("est", "se", "Month")
AY_jja$est <- as.numeric(as.character(AY_jja$est))
AY_jja$se <- as.numeric(as.character(AY_jja$se))
AY_jja$Month <- as.character(AY_jja$Month)
AY_jja$month <- c(1:13)

# Make JJA Min. Temp. plot #
jjaplot <- ggplot(AY_jja, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("JJA, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Aug(lag0)", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 


#### SON Season ######
# Average minimum temperature in SON each year 
son <- c(mean(SON$tmin[1:3]), mean(SON$tmin[4:6]), mean(SON$tmin[7:9]), mean(SON$tmin[10:12]), mean(SON$tmin[13:15]),
         mean(SON$tmin[16:18]), mean(SON$tmin[19:21]), mean(SON$tmin[22:24]), mean(SON$tmin[25:27]), mean(SON$tmin[28:30]))
son.pred <- SON[SON$Month == 11,]
SON.ar <- cbind(son.pred, son)

# Run Gaussian regressions using NINO3.4 lagged 0-5 months to predict average DJF minimum temperature
son_nov <- lm(son ~ NINO3.4, data=SON.ar)
son_oct <- lm(son ~ NINO3.41, data=SON.ar)
son_sep <- lm(son ~ NINO3.42, data=SON.ar)
son_aug <- lm(son ~ NINO3.43, data=SON.ar)
son_jul <- lm(son ~ NINO3.44, data=SON.ar)
son_jun <- lm(son ~ NINO3.45, data=SON.ar)
son_may <- lm(son ~ NINO3.46, data=SON.ar)
son_apr <- lm(son ~ NINO3.47, data=SON.ar)
son_mar <- lm(son ~ NINO3.48, data=SON.ar)
son_feb <- lm(son ~ NINO3.49, data=SON.ar)
son_jan <- lm(son ~ NINO3.410, data=SON.ar)
son_dec <- lm(son ~ NINO3.411, data=SON.ar)
son_nov2 <- lm(son ~ NINO3.412, data=SON.ar)

stargazer(son_nov, son_oct, son_sep, son_aug, son_jul, son_jun, type="text") #Summarize regression results

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag using all data 
son_ma <- matrix(0, nrow=13, ncol=4)
colnames(son_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(son_ma) <- c("Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov2")
son_ma[1,1:2] <- c(round(summary(son_nov)$coefficients[2,1],3), round(summary(son_nov)$coefficients[2,2],3))
son_ma[2,1:2] <- c(round(summary(son_oct)$coefficients[2,1],3), round(summary(son_oct)$coefficients[2,2],3))
son_ma[3,1:2] <- c(round(summary(son_sep)$coefficients[2,1],3), round(summary(son_sep)$coefficients[2,2],3))
son_ma[4,1:2] <- c(round(summary(son_aug)$coefficients[2,1],3), round(summary(son_aug)$coefficients[2,2],3))
son_ma[5,1:2] <- c(round(summary(son_jul)$coefficients[2,1],3), round(summary(son_jul)$coefficients[2,2],3))
son_ma[6,1:2] <- c(round(summary(son_jun)$coefficients[2,1],3), round(summary(son_jun)$coefficients[2,2],3))
son_ma[7,1:2] <- c(round(summary(son_may)$coefficients[2,1],3), round(summary(son_may)$coefficients[2,2],3))
son_ma[8,1:2] <- c(round(summary(son_apr)$coefficients[2,1],3), round(summary(son_apr)$coefficients[2,2],3))
son_ma[9,1:2] <- c(round(summary(son_mar)$coefficients[2,1],3), round(summary(son_mar)$coefficients[2,2],3))
son_ma[10,1:2] <- c(round(summary(son_feb)$coefficients[2,1],3), round(summary(son_feb)$coefficients[2,2],3))
son_ma[11,1:2] <- c(round(summary(son_jan)$coefficients[2,1],3), round(summary(son_jan)$coefficients[2,2],3))
son_ma[12,1:2] <- c(round(summary(son_dec)$coefficients[2,1],3), round(summary(son_dec)$coefficients[2,2],3))
son_ma[13,1:2] <- c(round(summary(son_nov2)$coefficients[2,1],3), round(summary(son_nov2)$coefficients[2,2],3))

# Plot coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_son <- data.frame(cbind(son_ma[,1:2], row.names(son_ma)))
colnames(AY_son) <- c("est", "se", "Month")
AY_son$est <- as.numeric(as.character(AY_son$est))
AY_son$se <- as.numeric(as.character(AY_son$se))
AY_son$Month <- as.character(AY_son$Month)
AY_son$month <- c(1:13)

# Make SON Min. Temp. figure 
sonplot <- ggplot(AY_son, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="grey85") + 
  geom_point() + geom_line()  + ylab("Beta Coefficient") + ggtitle("SON, 2007-2017") +
  scale_x_discrete(name="Month", limits=c("Nov(lag0)", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") + xlab("Month") +
  theme(axis.title = element_text(size=16), title = element_text(size=24), axis.text=element_text(size=16)) 
sonplot

#### Min. Temp. Supplemental Figure ####
plot_grid(djfplot, mamplot, jjaplot, sonplot, 
          nrow=4, ncol=1, labels=c("(A)", "(B)", "(C)", "(D)"), label_size = 20)

############################
##    Create Figure 2    ###
############################

#### DJF River Height Regressions (same as above) ####
djf <- c(mean(DJF$height[3:5]), mean(DJF$height[6:8]), mean(DJF$height[9:11]), mean(DJF$height[12:14]), mean(DJF$height[15:17]),
         mean(DJF$height[18:20]), mean(DJF$height[21:23]), mean(DJF$height[24:26]), mean(DJF$height[27:29]), mean(DJF$height[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

AY_djf_RH <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf_RH) <- c("est", "se", "Month")
AY_djf_RH$est <- as.numeric(as.character(AY_djf_RH$est))
AY_djf_RH$se <- as.numeric(as.character(AY_djf_RH$se))
AY_djf_RH$Month <- as.character(AY_djf_RH$Month)
AY_djf_RH$month <- c(1:13)

#### DJF Rainfall Regressions (same as above) ####
djf <- c(sum(DJF$rain_sum[3:5]), sum(DJF$rain_sum[6:8]), sum(DJF$rain_sum[9:11]), sum(DJF$rain_sum[12:14]), sum(DJF$rain_sum[15:17]),
         sum(DJF$rain_sum[18:20]), sum(DJF$rain_sum[21:23]), sum(DJF$rain_sum[24:26]), sum(DJF$rain_sum[27:29]), sum(DJF$rain_sum[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

djf_ma <- matrix(0, nrow=13, ncol=4)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

AY_djf_rain <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf_rain) <- c("est", "se", "Month")
AY_djf_rain$est <- as.numeric(as.character(AY_djf_rain$est))
AY_djf_rain$se <- as.numeric(as.character(AY_djf_rain$se))
AY_djf_rain$Month <- as.character(AY_djf_rain$Month)
AY_djf_rain$month <- c(1:13)


#### DJF Tmin Regressions (same as above) ####
djf <- c(mean(DJF$tmin[3:5]), mean(DJF$tmin[6:8]), mean(DJF$tmin[9:11]), mean(DJF$tmin[12:14]), mean(DJF$tmin[15:17]),
         mean(DJF$tmin[18:20]), mean(DJF$tmin[21:23]), mean(DJF$tmin[24:26]), mean(DJF$tmin[27:29]), mean(DJF$tmin[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], djf)

djf_feb <- lm(djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(djf ~ NINO3.412, data=DJF.ar)

djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se", "BV_estimate", "BV_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

AY_djf_tmin <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf_tmin) <- c("est", "se", "Month")
AY_djf_tmin$est <- as.numeric(as.character(AY_djf_tmin$est))
AY_djf_tmin$se <- as.numeric(as.character(AY_djf_tmin$se))
AY_djf_tmin$Month <- as.character(AY_djf_tmin$Month)
AY_djf_tmin$month <- c(1:13)

#### Generate Figure 2 ####
AY_djf_RH$var <- "River Height"
AY_djf_rain$var <- "Rainfall"
AY_djf_tmin$var <- "Tmin"
AY_djf_rain$est <- AY_djf_rain$est/100 ## Scale rainfall to be in 100s of millimeters
AY_djf_rain$se <- AY_djf_rain$se/100 ## Scale rainfall to be in 100s of millimeters
AY_djf <- rbind(AY_djf_RH, AY_djf_rain, AY_djf_tmin)


djfplot <- ggplot(AY_djf, aes(month, est, group=var)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se), alpha=0.5, fill=var)) + 
  geom_point(aes(color=var)) + geom_line(aes(color=var)) + ylab("Beta Coefficient") + ggtitle("DJF, 2007-2017") +
  scale_x_discrete(name="", limits=c("Feb(lag0)", "Jan(lag1)", "Dec(lag2)", "Nov(lag3)", "Oct(lag4)", "Sep(lag5)",
                                     "Aug(lag6)", "Jul(lag7)", "Jun(lag8)", "May(lag9)", "Apr(lag10)", "Mar(lag11)", 
                                     "Feb(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  scale_alpha_continuous(guide=FALSE) + 
  scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
  theme(axis.title = element_text(size=14), title = element_text(size=10), axis.text=element_text(size=8), 
        legend.title=element_blank(), legend.text=element_text(size=12)) 


############################
#### Water Quality      ####
############################

#### E.coli DJF ######
# Average e.coli measures across DJF season
ecoli.djf <- c(mean(DJF$ecoli.mo[3:5]), mean(DJF$ecoli.mo[6:8]), mean(DJF$ecoli.mo[9:11]), mean(DJF$ecoli.mo[12:14]), mean(DJF$ecoli.mo[15:17]),
               mean(DJF$ecoli.mo[18:20]), mean(DJF$ecoli.mo[21:23]), mean(DJF$ecoli.mo[24:26]), mean(DJF$ecoli.mo[27:29]), mean(DJF$ecoli.mo[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], ecoli.djf)

# Use Gaussian regressions to predict average DJF e.coli using NINO3.4 lagged 0-12 months
djf_feb <- lm(ecoli.djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(ecoli.djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(ecoli.djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(ecoli.djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(ecoli.djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(ecoli.djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(ecoli.djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(ecoli.djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(ecoli.djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(ecoli.djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(ecoli.djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(ecoli.djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(ecoli.djf ~ NINO3.412, data=DJF.ar)

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data  
djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

# Make e.coli DJF plot #
djfplot.ecoli <- ggplot(AY_djf, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="brown", alpha=0.2) + 
  geom_point(color="brown") + geom_line(color="brown") +  ylab("Beta Coefficient") + ggtitle("E.coli - DJF (n=5)") +
  scale_x_discrete(name="", limits = c("Feb(lag0)", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=16)) 

#### TSS DJF ######
# Average TSS measures across DJF season
TSS.djf <- c(mean(DJF$TSS.mo[3:5]), mean(DJF$TSS.mo[6:8]), mean(DJF$TSS.mo[9:11]), mean(DJF$TSS.mo[12:14]), mean(DJF$TSS.mo[15:17]),
             mean(DJF$TSS.mo[18:20]), mean(DJF$TSS.mo[21:23]), mean(DJF$TSS.mo[24:26]), mean(DJF$TSS.mo[27:29]), mean(DJF$TSS.mo[30:32]))
djf.pred <- DJF[DJF$Month == 2,]
DJF.ar <- cbind(djf.pred[-1,], TSS.djf)

# Use Gaussian regressions to predict average DJF TSS using NINO3.4 lagged 0-12 months
djf_feb <- lm(TSS.djf ~ NINO3.4, data=DJF.ar)
djf_jan <- lm(TSS.djf ~ NINO3.41, data=DJF.ar)
djf_dec <- lm(TSS.djf ~ NINO3.42, data=DJF.ar)
djf_nov <- lm(TSS.djf ~ NINO3.43, data=DJF.ar)
djf_oct <- lm(TSS.djf ~ NINO3.44, data=DJF.ar)
djf_sep <- lm(TSS.djf ~ NINO3.45, data=DJF.ar)
djf_aug <- lm(TSS.djf ~ NINO3.46, data=DJF.ar)
djf_jul <- lm(TSS.djf ~ NINO3.47, data=DJF.ar)
djf_jun <- lm(TSS.djf ~ NINO3.48, data=DJF.ar)
djf_may <- lm(TSS.djf ~ NINO3.49, data=DJF.ar)
djf_apr <- lm(TSS.djf ~ NINO3.410, data=DJF.ar)
djf_mar <- lm(TSS.djf ~ NINO3.411, data=DJF.ar)
djf_feb2 <- lm(TSS.djf ~ NINO3.412, data=DJF.ar)

# Create matrix of regression results, coefficients and p-values for each NINO3.4 lag, using all data (columns 1-2) 
djf_ma <- matrix(0, nrow=13, ncol=2)
colnames(djf_ma) <- c("AY_estimate", "AY_se")
rownames(djf_ma) <- c("Feb", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb2")
djf_ma[1,1:2] <- c(round(summary(djf_feb)$coefficients[2,1],3), round(summary(djf_feb)$coefficients[2,2],3))
djf_ma[2,1:2] <- c(round(summary(djf_jan)$coefficients[2,1],3), round(summary(djf_jan)$coefficients[2,2],3))
djf_ma[3,1:2] <- c(round(summary(djf_dec)$coefficients[2,1],3), round(summary(djf_dec)$coefficients[2,2],3))
djf_ma[4,1:2] <- c(round(summary(djf_nov)$coefficients[2,1],3), round(summary(djf_nov)$coefficients[2,2],3))
djf_ma[5,1:2] <- c(round(summary(djf_oct)$coefficients[2,1],3), round(summary(djf_oct)$coefficients[2,2],3))
djf_ma[6,1:2] <- c(round(summary(djf_sep)$coefficients[2,1],3), round(summary(djf_sep)$coefficients[2,2],3))
djf_ma[7,1:2] <- c(round(summary(djf_aug)$coefficients[2,1],3), round(summary(djf_aug)$coefficients[2,2],3))
djf_ma[8,1:2] <- c(round(summary(djf_jul)$coefficients[2,1],3), round(summary(djf_jul)$coefficients[2,2],3))
djf_ma[9,1:2] <- c(round(summary(djf_jun)$coefficients[2,1],3), round(summary(djf_jun)$coefficients[2,2],3))
djf_ma[10,1:2] <- c(round(summary(djf_may)$coefficients[2,1],3), round(summary(djf_may)$coefficients[2,2],3))
djf_ma[11,1:2] <- c(round(summary(djf_apr)$coefficients[2,1],3), round(summary(djf_apr)$coefficients[2,2],3))
djf_ma[12,1:2] <- c(round(summary(djf_mar)$coefficients[2,1],3), round(summary(djf_mar)$coefficients[2,2],3))
djf_ma[13,1:2] <- c(round(summary(djf_feb2)$coefficients[2,1],3), round(summary(djf_feb2)$coefficients[2,2],3))

# Extract coefficients and 95% confidence intervals for each NINO3.4 lag 
AY_djf <- data.frame(cbind(djf_ma[,1:2], row.names(djf_ma)))
colnames(AY_djf) <- c("est", "se", "Month")
AY_djf$est <- as.numeric(as.character(AY_djf$est))
AY_djf$se <- as.numeric(as.character(AY_djf$se))
AY_djf$Month <- as.character(AY_djf$Month)
AY_djf$month <- c(1:13)

# Make TSS DJF plot #
djfplot.TSS <- ggplot(AY_djf, aes(month, est)) + geom_ribbon(aes(ymin=(est-1.96*se), ymax=(est+1.96*se)), fill="brown", alpha=0.2) + 
  geom_point(color="brown") + geom_line(color="brown") +  ylab("Beta Coefficient") + ggtitle("TSS - DJF (n=5)") +
  scale_x_discrete(name="", limits=c("Feb(lag0)", "Jan", "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb(lag12)")) +  
  theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.title = element_text(size=16), title = element_text(size=16), axis.text=element_text(size=16)) 

#### Generate Supplementary Water Quality Figure ####
plot_grid(djfplot.ecoli, djfplot.TSS, nrow=2, ncol=1, labels=c("(A)", "(B)"), label_size = 16)


###################################################
#####                                          ####
#####  3. Regional TRMM Rainfall Analyses      ####
#####                                          ####
###################################################

# In addition, we investigated seasonal teleconnections 
# of ENSO with regional rainfall using correlations between 
# Niño 3.4 and TRMM gridded rainfall estimates from 1998-2015
# in each season. Correlations were calculated within each 
# grid cell for each season and year. 


######################################
###     Load Precipitation data    ###
######################################
rain <- nc_open("~/TRMM.nc")
lon <- ncvar_get(rain, varid = "X") #extract longitudes
lat <- ncvar_get(rain, varid = "Y") #extract latitudes
day <- ncvar_get(rain, varid = "T") #extract temperature values
day <- as.Date(day, origin="1998-01-01") #extract dates

prec <- ncvar_get(rain, "precipitation") ## This is the array where x axis is lon, y is lat, and z is time
dimnames(prec)[[1]] <- lon
dimnames(prec)[[2]] <- lat
dimnames(prec)[[3]] <- as.Date(day, origin="1998-01-01")

## Convert daily to total monthly precip in each grid cell ##
month <- data.frame(day=day)
month$mo <- format(day, format="%m")
month$yr <- format(day, format="%Y")
month$cnt <- 1
month <- month %>%
  group_by(mo,yr) %>%
  summarise(cnt=sum(cnt))
month <- month[with(month, order(yr, mo)),]

prec.mo <- array(NA, dim=c(dim(prec)[1], dim(prec)[2], nrow(month))) 
for(x in 1:dim(prec)[1]){
  for(y in 1:dim(prec)[2]){
    temp <- prec[x,y,]
    for(i in 1:nrow(month)){
      if(i == 1){
        prec.mo[x,y,1] <- sum(temp[1:month$cnt[1]])
      }
      if(i != 1) {
        prev <- sum(month$cnt[1:i-1]) 
        net <- prev + month$cnt[i]
        prec.mo[x,y,i] <- sum(temp[(prev+1):net])
      }
    }
  }
} #This calculates the total rainfall each month in each grid cell

## Calculate Monthy Anomalies ##
tm.anom <- array(0, dim=dim(prec.mo))
for(i in 1:dim(prec.mo)[1]){
  for(j in 1:dim(prec.mo)[2]){
    tm <- prec.mo[i,j,]
    tm_df <- data.frame(tm = tm, mo = as.numeric(month$mo))
    tm_ave <- tm_df %>%
      group_by(mo) %>%
      summarise(ave = mean(tm), sd=sd(tm))
    tm_df_ave <- merge(tm_df, tm_ave, by="mo", all=TRUE)
    tm.anom[i,j,] <- (tm_df_ave$tm - tm_df_ave$ave)/tm_df_ave$sd
  }
} #This calculates the rainfall anomalies each month in each grid cell



######################################
###     Load ENSO data             ###
######################################

SST <- read.table("~/SST.txt", header=TRUE, quote="\"")
SST <- SST[SST$YR >= 1998,]
SST <- SST[1:209,]
keep <- c("YR", "MON", "ANOM.2", "ANOM.3")
detach("package:plyr", unload=TRUE)
library(plyr)
SST <- SST[keep]
SST <- rename(SST, c("ANOM.2"="NINO4", "ANOM.3"="NINO3.4"))


#######################################
###     Correlations for DJF        ###
#######################################

# Calculate correlations between monthly NINO3.4 and TRMM rainfall in each grid cell during DJF
index <- which(month$mo %in% c("12", "01", "02"))
prec.djf <- prec.mo[,,index]
SST.djf <- SST$NINO3.4[index]
yr <- month$yr[index]

cor.DJF <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.DJF)){
  for(y in 1:ncol(cor.DJF)){
    cor.DJF[x,y] <- cor(prec.djf[x,y,], SST.djf)
  }
}

## Create matrix of p-values from correlation results above. Each entry corresponds to a grid cell ##
cor.djf.p <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.DJF)){
  for(y in 1:ncol(cor.DJF)){
    cor.tmp <- cor.test(prec.djf[x,y,], SST.djf)
    cor.djf.p[x,y] <- cor.tmp$p.value
  }
}

## Remove any correlation results if they are not statistically significant at p <=0.05 ##
cor.djf.plog <- cor.djf.p <= 0.05
cor.djf.plog[is.na(cor.djf.plog)] <- "FALSE"
cor.djf.sig <- matrix(0, nrow=128, ncol=120)
for (i in 1:nrow(cor.DJF)){
  for (j in 1:ncol(cor.DJF)){
    if(cor.djf.plog[i,j] == "TRUE"){
      cor.djf.sig[i,j] <- cor.DJF[i,j]
    } 
    else {
      cor.djf.sig[i,j] <- NA
    }
  }
}


#######################################
###     Correlations for MAM        ###
#######################################

# Calculate correlations between monthly NINO3.4 and TRMM rainfall in each grid cell during MAM
index <- which(month$mo %in% c("03", "04", "05"))
prec.mam <- prec.mo[,,index]
SST.mam <- SST$NINO3.4[index]
yr <- month$yr[index]

cor.MAM <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.MAM)){
  for(y in 1:ncol(cor.MAM)){
    cor.MAM[x,y] <- cor(prec.mam[x,y,], SST.mam)
  }
}

## Create matrix of p-values from correlation results above. Each entry corresponds to a grid cell ##
cor.mam.p <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.MAM)){
  for(y in 1:ncol(cor.MAM)){
    cor.tmp <- cor.test(prec.mam[x,y,], SST.mam)
    cor.mam.p[x,y] <- cor.tmp$p.value
  }
}

## Remove any correlation results if they are not statistically significant at p <=0.05 ##
cor.mam.plog <- cor.mam.p <= 0.05
cor.mam.plog[is.na(cor.mam.plog)] <- "FALSE"
cor.mam.sig <- matrix(0, nrow=128, ncol=120)
for (i in 1:nrow(cor.MAM)){
  for (j in 1:ncol(cor.MAM)){
    if(cor.mam.plog[i,j] == "TRUE"){
      cor.mam.sig[i,j] <- cor.MAM[i,j]
    } 
    else {
      cor.mam.sig[i,j] <- NA
    }
  }
}



#######################################
###     Correlations for JJA        ###
#######################################

# Calculate correlations between monthly NINO3.4 and TRMM rainfall in each grid cell during JJA
index <- which(month$mo %in% c("06", "07", "08"))
prec.jja <- prec.mo[,,index]
SST.jja <- SST$NINO3.4[index]
yr <- month$yr[index]

cor.JJA <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.JJA)){
  for(y in 1:ncol(cor.JJA)){
    cor.JJA[x,y] <- cor(prec.jja[x,y,], SST.jja)
  }
}

## Create matrix of p-values from correlation results above. Each entry corresponds to a grid cell ##
cor.jja.p <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.JJA)){
  for(y in 1:ncol(cor.JJA)){
    cor.tmp <- cor.test(prec.jja[x,y,], SST.jja)
    cor.jja.p[x,y] <- cor.tmp$p.value
  }
}

## Remove any correlation results if they are not statistically significant at p <=0.05 ##
cor.jja.plog <- cor.jja.p <= 0.05
cor.jja.plog[is.na(cor.jja.plog)] <- "FALSE"
cor.jja.sig <- matrix(0, nrow=128, ncol=120)
for (i in 1:nrow(cor.JJA)){
  for (j in 1:ncol(cor.JJA)){
    if(cor.jja.plog[i,j] == "TRUE"){
      cor.jja.sig[i,j] <- cor.JJA[i,j]
    } 
    else {
      cor.jja.sig[i,j] <- NA
    }
  }
}



#######################################
###     Correlations for SON        ###
#######################################

# Calculate correlations between monthly NINO3.4 and TRMM rainfall in each grid cell during SON
index <- which(month$mo %in% c("09", "10", "11"))
prec.son <- prec.mo[,,index]
SST.son <- SST$NINO3.4[index]
yr <- month$yr[index]

cor.SON <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.SON)){
  for(y in 1:ncol(cor.SON)){
    cor.SON[x,y] <- cor(prec.son[x,y,], SST.son)
  }
}

## Create matrix of p-values from correlation results above. Each entry corresponds to a grid cell ##
cor.son.p <- matrix(NA, nrow=128, ncol=120)
for(x in 1:nrow(cor.SON)){
  for(y in 1:ncol(cor.SON)){
    cor.tmp <- cor.test(prec.son[x,y,], SST.son)
    cor.son.p[x,y] <- cor.tmp$p.value
  }
}

## Remove any correlation results if they are not statistically significant at p <=0.05 ##
cor.son.plog <- cor.son.p <= 0.05
cor.son.plog[is.na(cor.son.plog)] <- "FALSE"
cor.son.sig <- matrix(0, nrow=128, ncol=120)
for (i in 1:nrow(cor.SON)){
  for (j in 1:ncol(cor.SON)){
    if(cor.son.plog[i,j] == "TRUE"){
      cor.son.sig[i,j] <- cor.SON[i,j]
    } 
    else {
      cor.son.sig[i,j] <- NA
    }
  }
}



#######################################
###     Create Raster Files         ###
#######################################

#### Load in Africa Borders 
border <- readOGR("~/Africa.shp")
proj4string(border) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#### Create Raster Layer - DJF 
cor.djf <- data.frame(cor.djf.sig)
names(cor.djf) <- lat
cor.djf$lon <- lon
cor.djf <- melt(cor.djf, id.vars ="lon")
names(cor.djf) <- c("x", "y", "z")
cor.djf$y <- as.numeric(as.character(cor.djf$y))

rast.cor.djf <- rasterFromXYZ(cor.djf)
proj4string(rast.cor.djf) <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
writeRaster(rast.cor.djf, file="~/cor_djf.asc", overwrite=TRUE)

#### Create Raster Layer - MAM 
cor.mam <- data.frame(cor.mam.sig)
names(cor.mam) <- lat
cor.mam$lon <- lon
cor.mam <- melt(cor.mam, id.vars ="lon")
names(cor.mam) <- c("x", "y", "z")
cor.mam$y <- as.numeric(as.character(cor.mam$y))

rast.cor.mam <- rasterFromXYZ(cor.mam)
proj4string(rast.cor.mam) <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
writeRaster(rast.cor.mam, file="~/cor_mam.asc", overwrite=TRUE)

#### Create Raster Layer - JJA 
cor.jja <- data.frame(cor.jja.sig)
names(cor.jja) <- lat
cor.jja$lon <- lon
cor.jja <- melt(cor.jja, id.vars ="lon")
names(cor.jja) <- c("x", "y", "z")
cor.jja$y <- as.numeric(as.character(cor.jja$y))

rast.cor.jja <- rasterFromXYZ(cor.jja)
proj4string(rast.cor.jja) <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
writeRaster(rast.cor.jja, file="~/cor_jja.asc", overwrite=TRUE)

#### Create Raster Layer - SON 
cor.son <- data.frame(cor.son.sig)
names(cor.son) <- lat
cor.son$lon <- lon
cor.son <- melt(cor.son, id.vars ="lon")
names(cor.son) <- c("x", "y", "z")
cor.son$y <- as.numeric(as.character(cor.son$y))

rast.cor.son <- rasterFromXYZ(cor.son)
proj4string(rast.cor.son) <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
writeRaster(rast.cor.son, file="~/cor_son.asc", overwrite=TRUE)


#######################################
###     Plot Correlations           ###
#######################################
breaks <- seq(-0.3, 0.2, by=0.01)
cols <- colorRampPalette(c("blue", "white", "red"))(length(breaks)-1)

par(mfrow=c(2,2))
plot(rast.cor.djf, main="Correlation of NINO3.4 with Rainfall, 1998-2015, DJF", col=cols, asp=1)
plot(border, add=TRUE)

plot(rast.cor.mam, main="Correlation of NINO3.4 with Rainfall, 1998-2015, MAM", col=cols, asp=1)
plot(border, add=TRUE)

plot(rast.cor.jja, main="Correlation of NINO3.4 with Rainfall, 1998-2015, JJA", col=cols, asp=1)
plot(border, add=TRUE)

plot(rast.cor.son, main="Correlation of NINO3.4 with Rainfall, 1998-2015, SON", col=cols, asp=1)
plot(border, add=TRUE)


###################################################
###     Correlation Manuscript Figures          ###
###################################################

# Final correlation figures between Nino3.4 and TRMM rainfallwere made using these raster 
# files in QGIS to show the study area in more detail and Zambezi river basin 

