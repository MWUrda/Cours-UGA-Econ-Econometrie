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
# "pooled" MCO: random sans MCG(identique à lm)
estim_pooled <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, 
                          data= data, model="pooling",  index=c("stateid"))
summary(estim_pooled)

# MCG

estim_mcg <- pggls(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029, 
                    data= data, model="pooling",  index=c("stateid"))
summary(estim_mcg)

# Effets fixes(avec effets temporels sous forme de dummies)

estim_within <- plm(log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029 + factor(year), 
                    data= data, model="within",  index=c("stateid"))
summary(estim_within)

