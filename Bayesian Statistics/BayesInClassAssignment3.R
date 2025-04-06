library(rstan)
library(ggplot2)
library(AmesHousing)
library(dplyr)
library(titanic)
library(rstanarm)
library(bayesplot)

set.seed(123)
ames <- make_ordinal_ames() 
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

train$Age=train$Year_Sold-train$Year_Built
test$Age=test$Year_Sold-test$Year_Built

train_reg <- train %>% 
  dplyr::select(Sale_Price, 
                Lot_Area,
                Age,
                Total_Bsmt_SF,
                Garage_Area,
                Gr_Liv_Area,
                Central_Air) 

test_reg <- test %>%
  dplyr::select(Sale_Price, 
                Lot_Area,
                Age,
                Total_Bsmt_SF,
                Garage_Area,
                Gr_Liv_Area,
                Central_Air) 


#Bayes Method
model1<-stan_glm(Sale_Price~.+I(Age^2),data=train_reg, seed=1097,refresh=0)
summary(model1)

#Frequentist
model2<-lm(Sale_Price~.+I(Age^2),data=train_reg)
summary(model2)

#Posterior Distributions - Bayes
sims <- as.array(model1)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.95) + plot_title

#Posterior Distributions - Frequentist
sims2<-sims[,,-c(1,3,7,9)] ## Lot Area, Total Basement SF, Garage Area, Ground Living Area, Age^2
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims2, prob = 0.95) + plot_title


#For one variable
mcmc_areas(sims,
           pars = c("Lot_Area"),
           prob = 0.95) + plot_title

#quantiles for coefficients
quantile(sims[,,3], probs = c(.025,.975))  


#mcmc plots
color_scheme_set("mix-blue-red")
mcmc_trace(sims, pars = c("Age", "sigma"), 
           facet_args = list(ncol = 1, strip.position = "left"))

stan_ac(model1)

#how well model predicts on data - really underpredicts large values
pred.y<- posterior_predict(model1)
dim(pred.y)


pred.y2<-data.frame(x=c(rep(1:20,each=nrow(pred.y))),y=as.numeric(pred.y[,1:20]))
actual.y=data.frame(x2=1:20,y2=train_reg$Sale_Price[1:20])
ggplot(pred.y2,aes(x=as.factor(x),y=y))+geom_boxplot()+geom_point(data=actual.y,aes(x=as.factor(x2),y=y2),color="red",shape=9)+
  labs(x="Observations")

#plot pred vs actual
post.mean=apply(pred.y,2,mean)
plot.dat=data.frame(actual=train_reg$Sale_Price,predicted=post.mean)
ggplot(plot.dat,aes(x=actual,y=predicted))+ geom_point()+geom_abline(slope=1,intercept = coef(model1)[1])


###############
## Logistic Regression
############

##Get rid of NA's
new.dat=titanic_train[complete.cases(titanic_train),]

titanic.model<-stan_glm(Survived~ Sex+Age + Fare+ Sex:Fare,data=new.dat,family = binomial(link="logit"),prior = normal(0,100),prior_intercept = normal(0,100),seed=03786,refresh=0,QR=T)

summary(titanic.model)

#posterior plot
sims <- as.array(titanic.model)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.95) + plot_title


sims2<-sims[,,3:5]
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims2, prob = 0.95) + plot_title

summary(titanic.model) # not helpful because numbers are small

#
quant.fun1 <- function(x){
  temp=quantile(x,probs = 0.025)
  return(temp)}
quant.fun2 <- function(x){
  temp=quantile(x,probs = 0.975)
  return(temp)}
correct.output<-matrix(c(apply(sims,3,mean),apply(sims,3,median),
                         apply(sims,3,sd),apply(sims,3,quant.fun1),apply(sims,3,quant.fun2)),ncol=5)
colnames(correct.output)<-c("mean","median","standard error","2.5%","97.5%")
rownames(correct.output)<-c("Intercept","Male","Age","Fare","Sex:Fare")
correct.output


mcmc_trace(titanic.model)


#model
model1<-stan_glm(Sale_Price~.,data=train_reg, seed=1097,refresh=0,
                 chains = 4,      # number of Markov chains
                 warmup = 3000,   # number of warmup iterations per chain
                 iter = 6000,    # total number of iterations per chain
                 refresh = 0    # no progress shown
)
summary(model1)


pred.y<- posterior_predict(titanic.model)



######################
### Complex Model
######################


data {
  int<lower=0> N;    // Number of rats
  int<lower=0> Npts; // Number of data points
  int<lower=0> rat[Npts]; // Lookup index for rat
  real x[Npts];
  real y[Npts];
  real xbar;
}

parameters {
  real alpha[N];
  real beta[N];
  real mu_alpha;
  real mu_beta; 
  real <lower=0> sigmasq_y;
  real <lower=0> sigmasq_alpha;
  real <lower=0> sigmasq_beta;          
}
transformed parameters {
  real<lower=0> sigma_y;       
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  sigma_y = sqrt(sigmasq_y);
  sigma_alpha = sqrt(sigmasq_alpha);
  sigma_beta = sqrt(sigmasq_beta);
}
model {
  mu_alpha ~ normal(0, 100);
  mu_beta ~ normal(0, 100);
  sigmasq_y ~ inv_gamma(0.001, 0.001);
  sigmasq_alpha ~ inv_gamma(0.001, 0.001);
  sigmasq_beta ~ inv_gamma(0.001, 0.001);
  alpha ~ normal(mu_alpha, sigma_alpha); // vectorized
  beta ~ normal(mu_beta, sigma_beta);  // vectorized
  for (n in 1:Npts){
    int irat;
    irat = rat[n];
    y[n] ~ normal(alpha[irat] + beta[irat] * (x[n] - xbar), sigma_y);
  }
}
generated quantities {
  real alpha0;
  alpha0 = mu_alpha - xbar * mu_beta;
}



#################
N = 30
y =
  structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134, 
              160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146, 
              157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210, 
              189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212, 
              203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249, 
              263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242, 
              248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248, 
              219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 
              273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 
              286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
              338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321, 
              334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30, 
                                                                          5))
x = c(8.0, 15.0, 22.0, 29.0, 36.0)
xbar = 22.0
rats.dat = list(
  N = N,
  Npts = length(y),
  rat = rep(1:nrow(y), ncol(y)),
  x = rep(x, each = nrow(y)),
  y = as.numeric(y),
  xbar = xbar)
rats.stan = stan( "rats.stan", data = rats.dat)

#############


rats.output=extract(rats.stan)
print(rats.stan,pars = c("mu_alpha","mu_beta","sigma_y","sigma_alpha","sigma_beta","alpha0"))



################################################################################
##############################################################################
################################################################################
library(rstan)
library(ggplot2)
library(AmesHousing)
library(dplyr)
library(titanic)
library(rstanarm)
library(bayesplot)
library(mlbench)

data("BreastCancer")

#manually changed these to factor
BreastCancer$Cell.shape <- as.numeric(BreastCancer$Cell.shape)


##Get rid of NA's
new.dat=BreastCancer[complete.cases(BreastCancer),]

bc.model<-stan_glm(Class~ .-Id-Cell.size - Cell.shape - Epith.c.size,data=new.dat,family = binomial(link="logit"),prior = normal(0,100),prior_intercept = normal(0,100),seed=85208,refresh=0,QR=T)

summary(bc.model)

#posterior plot
sims <- as.array(bc.model)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.95) + plot_title


sims2<-sims[,,3:5]
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims2, prob = 0.95) + plot_title

bc.model<-stan_glm(Class~ Cl.thickness + Bare.nuclei+Bl.cromatin,data=new.dat,family = binomial(link="logit"),prior = normal(0,100),prior_intercept = normal(0,100),seed=85208,refresh=0,QR=T)


summary(bc.model) # not helpful because numbers are small

#
quant.fun1 <- function(x){
  temp=quantile(x,probs = 0.025)
  return(temp)}
quant.fun2 <- function(x){
  temp=quantile(x,probs = 0.975)
  return(temp)}
correct.output<-matrix(c(apply(sims,3,mean),apply(sims,3,median),
                         apply(sims,3,sd),apply(sims,3,quant.fun1),apply(sims,3,quant.fun2)),ncol=5)
colnames(correct.output)<-c("mean","median","standard error","2.5%","97.5%")
rownames(correct.output)<-c("Intercept","Cl.thickness","Marg.adhesion", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoili", "Mitoses")
correct.output


mcmc_trace(bc.model)
summary(bc.model)


bc.model<-stan_glm(Class~ Cl.thickness + Bare.nuclei,data=new.dat,family = binomial(link="logit"),prior = normal(0,100),prior_intercept = normal(0,100),seed=85208,refresh=0,QR=T)

summary(bc.model)

#posterior plot
sims <- as.array(bc.model)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.95) + plot_title
