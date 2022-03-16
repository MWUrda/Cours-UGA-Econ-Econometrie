

####################
# Part II: Main program
install.packages("sandwich","foreign","AER","systemfit","gmm","lmtest")
install.packages("systemfit")
install.packages("gmm")
library(lmtest)
library(sandwich);
library(foreign);
library(AER);
#library(ivpack);
library(systemfit);
library(gmm);
library(systemfit)
# Reading the data;

na
attach(data);
alpha <- .05; # Significance level

# In this example it is more convenient not to partial-out the controls because the sample size is small. If you partial out, 
# you would need to manually correct the small sample adjustments of the  White standard errors

# ols results;

# Demand:
formula    <- qty ~ price + day1 + day2 + day3 + day4 + rainy + cold;
ols.fit    <- lm(formula, data = data );
summary(ols.fit)
ols.coef.d <- ols.fit$coef[2];
ols.se.d   <- coeftest(ols.fit, vcov = hc)[2,2];
ols.lci.d  <- ols.coef.d + qnorm(alpha/2)*ols.se.d;
ols.uci.d  <- ols.coef.d + qnorm(1-alpha/2)*ols.se.d;

# Supply:
formula    <- qty ~ price + stormy + mixed;
ols.fit    <- lm(formula);
summary(ols.fit)
ols.coef.s <- ols.fit$coef[2];
ols.se.s   <- coeftest(ols.fit, vcov = hc)[2,2];
ols.lci.s  <- ols.coef.s + qnorm(alpha/2)*ols.se.s;
ols.uci.s  <- ols.coef.s + qnorm(1-alpha/2)*ols.se.s;


# First stage;

# Demand;
formula   <- price ~ stormy + mixed + day1 + day2 + day3 + day4 + rainy + cold;
fs.fit   <- lm(formula);
fs.Fstat <- waldtest(fs.fit, . ~ . - stormy - mixed, vcov = hc,  test = "F")$F[2];
print(paste('F-stat: ', fs.Fstat));

# Supply;
fs.Fstat <- waldtest(fs.fit, . ~ .  - day1 - day2 - day3 - day4 - rainy - cold, vcov = hc,  test = "F")$F[2];
print(paste('F-stat: ', fs.Fstat));

# tsls results

# Demand;
formula     <- qty ~ price + day1 + day2 + day3 + day4  + rainy + cold| 
     stormy + mixed + day1 + day2 + day3 + day4  + rainy + cold;
tsls.fit    <- ivreg(formula);
summary(tsls.fit)
tsls.coef.d <- tsls.fit$coef[2];
tsls.se.d   <- coeftest(tsls.fit, vcov = hc)[2,2]; 
tsls.lci.d  <- tsls.coef.d + qnorm(alpha/2)*tsls.se.d;
tsls.uci.d  <- tsls.coef.d + qnorm(1-alpha/2)*tsls.se.d;

# Supply;
formula     <- qty ~ price + stormy + mixed| day1 + day2 + day3 + day4  + rainy + cold + stormy + mixed;
tsls.fit    <- ivreg(formula);
summary(tsls.fit)
tsls.coef.s <- tsls.fit$coef[2];
tsls.se.s   <- coeftest(tsls.fit, vcov = hc)[2,2]; 
tsls.lci.s  <- tsls.coef.s + qnorm(alpha/2)*tsls.se.s;
tsls.uci.s  <- tsls.coef.s + qnorm(1-alpha/2)*tsls.se.s;

# 2-stage gmm results: equation by equation;

# Demand;
instr   <- cbind(stormy, mixed,day1,day2,day3,day4,rainy,cold);
formula <- qty ~ price + day1 + day2 + day3 + day4  + rainy + cold;
gmm.fit <- gmm(formula, x = instr, vcoc = "iid");
gmm.coef.d.all<-gmm.fit$coefficients
gmm.coef.d <- summary(gmm.fit)$coefficients[2,1];
gmm.se.d   <- summary(gmm.fit)$coefficients[2,2];
gmm.lci.d  <- gmm.coef.d + qnorm(alpha/2)*gmm.se.d;
gmm.uci.d  <- gmm.coef.d + qnorm(1-alpha/2)*gmm.se.d;
# print(summary(gmm.fit)$stest);



# Supply;
instr   <- cbind(stormy, mixed,day1,day2,day3,day4,rainy,cold);
formula <- qty ~ price + stormy + mixed;
gmm.fit <- gmm(formula, x = instr, vcoc = "iid");
gmm.coef.s.all<-gmm.fit$coefficients
gmm.coef.s <- summary(gmm.fit)$coefficients[2,1];
gmm.se.s   <- summary(gmm.fit)$coefficients[2,2];
gmm.lci.s  <- gmm.coef.s + qnorm(alpha/2)*gmm.se.s;
gmm.uci.s  <- gmm.coef.s + qnorm(1-alpha/2)*gmm.se.s;
# print(summary(gmm.fit)$stest);

## 3SLS: systemfit


formula.d    <- qty ~ price + day1 + day2 + day3 + day4 + rainy + cold;
formula.s    <- qty ~ price + stormy + mixed;
formula      <- list(demand = formula.d, supply = formula.s);
sols.fit     <- systemfit(formula);
s3sls.fit     <- systemfit(formula, method = "3SLS", inst = ~ + day1 + day2 + day3 + day4 + rainy + cold + stormy + mixed);

# Demand;
s3sls.coef.d <- s3sls.fit$coefficients[2];
s3sls.se.d   <- summary(s3sls.fit)$coefficients[2,2]; 
s3sls.lci.d  <- s3sls.coef.d + qnorm(alpha/2)*s3sls.se.d;
s3sls.uci.d  <- s3sls.coef.d + qnorm(1-alpha/2)*s3sls.se.d;

# Supply;
s3sls.coef.s <- s3sls.fit$coefficients[10];
s3sls.se.s   <- summary(s3sls.fit)$coefficients[10,2]; 
s3sls.lci.s  <- s3sls.coef.s + qnorm(alpha/2)*s3sls.se.s;
s3sls.uci.s  <- s3sls.coef.s + qnorm(1-alpha/2)*s3sls.se.s;

### 07/23
###  3SLS implementation with gmm package (instead of systemfit)


gf<-function(theta,x) {
  n  <- length(x[,'qty']);
  intercept <- x[,'qty']/x[,'qty'];
  Z  <- cbind(intercept, x[,'day1'], x[,'day2'], x[,'day3'], x[,'day4'], x[,'rainy'], x[,'cold'], x[,'stormy'], x[,'mixed']);
  D1 <- cbind(intercept, x[,'price'], x[,'day1'], x[,'day2'], x[,'day3'], x[,'day4'], x[,'rainy'], x[,'cold']);
  D2 <- cbind(intercept, x[,'price'], x[,'stormy'], x[,'mixed']);
  
  Formula1<-matrix(rep(x[,'qty']-D1%*%theta[1:dim(D1)[2]],each=dim(Z)[2]),ncol=dim(Z)[2],byrow=TRUE)*Z;
  
  Formula2<-matrix(rep(x[,'qty']-D2%*%theta[(1+dim(D1)[2]):(dim(D2)[2]+dim(D1)[2])],each=dim(Z)[2]),ncol=dim(Z)[2],byrow=TRUE)*Z;
  
  return(cbind(Formula1,Formula2))
  
}

data<-data[,c('price','qty','stormy','mixed','day1','day2','day3','day4','rainy','cold')]

gradient<-function(theta,x) {
  n  <- length(x[,'qty']);
  intercept <- x[,'qty']/x[,'qty'];
  Z  <- cbind(intercept, x[,'day1'], x[,'day2'], x[,'day3'], x[,'day4'], x[,'rainy'], x[,'cold'], x[,'stormy'], x[,'mixed']);
  D1 <- cbind(intercept, x[,'price'], x[,'day1'], x[,'day2'], x[,'day3'], x[,'day4'], x[,'rainy'], x[,'cold']);
  D2 <- cbind(intercept, x[,'price'], x[,'stormy'], x[,'mixed']);
  G  <- rbind(cbind(- 1/n*t(Z) %*% D1, matrix(0, nrow = ncol(Z), ncol = ncol(D2))), cbind(matrix(0, nrow = ncol(Z), ncol = ncol(D1)), - 1/n*t(Z) %*% D2) );
  return(as.matrix(G))
  
}

# The issue of starting value has been fixed ! Thanks Mert!

gmm3sls.fit<-gmm(gf,x=data,t0=rep(0,12),gradient,type="twoStep",vcov="HAC",method="BFGS",control=list(fnscale=1e-8))
#gmm3sls.fit<-gmm(gf,x=data,t0=s3sls.fit$coefficients+rnorm(12)/10,gradient,type="twoStep",vcov="HAC",method="BFGS",control=list(fnscale=1e-8))


#gmm3sls.fit<-gmm(gf,x=data,t0=s3sls.fit$coefficients+rnorm(12)/10,gradient,type="twoStep",vcov="HAC")




# 3sls system estimation: "manual" estimation;



# Estimation of optimal weighting matrix by 2sls;

formula     <- qty ~ price + day1 + day2 + day3 + day4  + rainy + cold| stormy + mixed + day1 + day2 + day3 + day4  + rainy + cold;
tsls.fit    <- ivreg(formula);
res1        <- tsls.fit$res;


# Supply;
formula     <- qty ~ price + stormy + mixed| day1 + day2 + day3 + day4  + rainy + cold + stormy + mixed;
tsls.fit    <- ivreg(formula);
res2        <- tsls.fit$res;


# Construction of terms;

n  <- length(qty);
intercept <- qty/qty;
Z  <- cbind(intercept, day1, day2, day3, day4, rainy, cold, stormy, mixed);
D1 <- cbind(intercept, price, day1, day2, day3, day4, rainy, cold);
D2 <- cbind(intercept, price, stormy, mixed);
G  <- rbind(cbind(- t(Z) %*% D1, matrix(0, nrow = ncol(Z), ncol = ncol(D2))), cbind(matrix(0, nrow = ncol(Z), ncol = ncol(D1)), - t(Z) %*% D2) );
g0 <- rbind(t(Z) %*% qty, t(Z) %*% qty);

gtilde      <- cbind(Z * kronecker(res1, matrix(1, ncol = ncol(Z))), Z * kronecker(res2, matrix(1, ncol = ncol(Z))));
A           <- solve(t(gtilde) %*% gtilde);                                   

s3sls.coef  <- - solve(t(G) %*% A %*% G)  %*% t(G) %*% A %*% g0;
s3sls.se    <- sqrt(diag(solve(t(G) %*% A %*% G)));


# Demand;
sm3sls.coef.d <- s3sls.coef[2];
sm3sls.se.d   <- s3sls.se[2]; 
sm3sls.lci.d  <- sm3sls.coef.d + qnorm(alpha/2)*sm3sls.se.d;
sm3sls.uci.d  <- sm3sls.coef.d + qnorm(1-alpha/2)*sm3sls.se.d;

# Supply;
sm3sls.coef.s <- s3sls.coef[10];
sm3sls.se.s   <- s3sls.se[10]; 
sm3sls.lci.s  <- sm3sls.coef.s + qnorm(alpha/2)*sm3sls.se.s;
sm3sls.uci.s  <- sm3sls.coef.s + qnorm(1-alpha/2)*sm3sls.se.s;


# GMM equation by equation estimation: "manual" estimation;

# Estimation of optimal weighting matrix by 2sls;

formula     <- qty ~ price + day1 + day2 + day3 + day4  + rainy + cold| stormy + mixed + day1 + day2 + day3 + day4  + rainy + cold;
tsls.fit    <- ivreg(formula);
res1        <- tsls.fit$res;


# Supply;
formula     <- qty ~ price + stormy + mixed| day1 + day2 + day3 + day4  + rainy + cold + stormy + mixed;
tsls.fit    <- ivreg(formula);
res2        <- tsls.fit$res;


# Construction of terms;

n  <- length(qty);
intercept <- qty/qty;
Z  <- cbind(intercept, day1, day2, day3, day4, rainy, cold, stormy, mixed);
D1 <- cbind(intercept, price, day1, day2, day3, day4, rainy, cold);
D2 <- cbind(intercept, price, stormy, mixed);
G  <- rbind(cbind(- t(Z) %*% D1, matrix(0, nrow = ncol(Z), ncol = ncol(D2))), cbind(matrix(0, nrow = ncol(Z), ncol = ncol(D1)), - t(Z) %*% D2) );
g0 <- rbind(t(Z) %*% qty, t(Z) %*% qty);

gtilde1    <- Z * kronecker(res1, matrix(1, ncol = ncol(Z)));
gtilde2    <- Z * kronecker(res2, matrix(1, ncol = ncol(Z)));
A11        <- t(gtilde1) %*% gtilde1;
A22        <- t(gtilde2) %*% gtilde2;
A          <- solve(rbind(cbind(A11, matrix(0, nrow = nrow(A11), ncol = ncol(A22))), cbind(matrix(0, nrow = nrow(A22), ncol = ncol(A11)), A22)));


mgmm.coef  <- - solve(t(G) %*% A %*% G)  %*% t(G) %*% A %*% g0;
mgmm.se    <- sqrt(diag(solve(t(G) %*% A %*% G)));


# Demand;
mgmm.coef.d <- mgmm.coef[2];
mgmm.se.d   <- mgmm.se[2]; 
mgmm.lci.d  <- mgmm.coef.d + qnorm(alpha/2)*mgmm.se.d;
mgmm.uci.d  <- mgmm.coef.d + qnorm(1-alpha/2)*mgmm.se.d;

# Supply;
mgmm.coef.s <- mgmm.coef[10];
mgmm.se.s   <- mgmm.se[10]; 
mgmm.lci.s  <- mgmm.coef.s + qnorm(alpha/2)*mgmm.se.s;
mgmm.uci.s  <- mgmm.coef.s + qnorm(1-alpha/2)*mgmm.se.s;

options(digits=2);

table <- matrix(0, ncol = 4, nrow = 6, dimnames = list(c('LI-OLS', 'LI-TSLS', 'LI-3SLS', 'FI-3SLS', 'FI2-3SLS', 'LI2-3SLS'), c('Est.', 'Std. Error', '95% LCI','95% UCI')));

table[1,1] <- ols.coef.d;
table[1,2] <- ols.se.d;
table[1,3] <- ols.lci.d;
table[1,4] <- ols.uci.d;

table[2,1] <- tsls.coef.d;
table[2,2] <- tsls.se.d;
table[2,3] <- tsls.lci.d;
table[2,4] <- tsls.uci.d;

table[3,1] <- gmm.coef.d;
table[3,2] <- gmm.se.d;
table[3,3] <- gmm.lci.d;
table[3,4] <- gmm.uci.d;

table[4,1] <- s3sls.coef.d;
table[4,2] <- s3sls.se.d;
table[4,3] <- s3sls.lci.d;
table[4,4] <- s3sls.uci.d;

table[5,1] <- sm3sls.coef.d;
table[5,2] <- sm3sls.se.d;
table[5,3] <- sm3sls.lci.d;
table[5,4] <- sm3sls.uci.d;

table[6,1] <- mgmm.coef.d;
table[6,2] <- mgmm.se.d;
table[6,3] <- mgmm.lci.d;
table[6,4] <- mgmm.uci.d;



print(table);


table <- matrix(0, ncol = 4, nrow = 6, dimnames = list(c('LI-OLS', 'LI-TSLS', 'LI-3SLS', 'FI-3SLS', 'FI2-3SLS', 'LI2-2SLS'), c('Est.', 'Std. Error', '95% LCI','95% UCI')));

table[1,1] <- ols.coef.s;
table[1,2] <- ols.se.s;
table[1,3] <- ols.lci.s;
table[1,4] <- ols.uci.s;

table[2,1] <- tsls.coef.s;
table[2,2] <- tsls.se.s;
table[2,3] <- tsls.lci.s;
table[2,4] <- tsls.uci.s;

table[3,1] <- gmm.coef.s;
table[3,2] <- gmm.se.s;
table[3,3] <- gmm.lci.s;
table[3,4] <- gmm.uci.s;

table[4,1] <- s3sls.coef.s;
table[4,2] <- s3sls.se.s;
table[4,3] <- s3sls.lci.s;
table[4,4] <- s3sls.uci.s;

table[5,1] <- sm3sls.coef.s;
table[5,2] <- sm3sls.se.s;
table[5,3] <- sm3sls.lci.s;
table[5,4] <- sm3sls.uci.s;

table[6,1] <- mgmm.coef.s;
table[6,2] <- mgmm.se.s;
table[6,3] <- mgmm.lci.s;
table[6,4] <- mgmm.uci.s;




print(table);
