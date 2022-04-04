library(plm)
library(foreign)
data <- read.dta("/Volumes/GoogleDrive/Mon Drive/Guns/Guns.dta")
estim_pooled_mcg <- pggls(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, 
                          data= data, model="pooling",  index=c("stateid"))
summary(estim_pooled_mcg)

summary(lm(log(vio)~shall, 
           data= data))

summary(lm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, 
           data= data))

estim_pooled <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, 
                          data= data, model="pooling",  index=c("stateid"))
summary(estim_pooled)

estim_within <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029 + factor(year), 
                    data= data, model="within",  index=c("stateid"))
summary(estim_within)

