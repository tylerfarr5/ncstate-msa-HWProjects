library(rstan)
library(ggplot2)
library(AmesHousing)
library(dplyr)
library(titanic)


set.seed(12976)

ex1 <- "
data {
    int <lower=0> y;
    int <lower=0> n;
    }
parameters {
    real <lower=0, upper=1> p;
            }
model {
    p ~ beta(2,9);
    y ~ binomial(n, p);
       }
"


## Data needs to be in a list:
binom.data=list(n=1200, y=800)

### This is how you run Stan
binom.stan= rstan::stan(model_code = ex1, data=binom.data, seed=12976)



### Pull off the posterior samples
post.samp.binom=extract(binom.stan)
new.p=post.samp.binom$p

mean(new.p)
sd(new.p)



####Graph the posterior
p.post=data.frame(new.p)
ggplot(p.post,aes(new.p)) +
  geom_histogram() +
  labs(title="Posterior distribution of p",y="Frequency",x="P")


###Probability p is lower than 0.30
sum(new.p<=0.3)/length(new.p)


###95% Probability Interval
quantile(new.p,p=c(0.025,0.975))


####################################################################
########## Quiz 1
###################################################################

library(lmtest)
unemployment <- data.frame(unemployment)

#To analyze this data set, you should model the data as a lognormal 
#(the command is lognormal(mu, sigma)).  For prior distributions, 
#assume that mu is normal(0,1000) and sigma is inverse chi-square with
#89 degrees of freedom (inv_chi_square(89)). Use the default for everything else.




set.seed(10678)
unemp <- "
data {
    int <lower = 0> n;
    vector[n] y;
    }
parameters {
    real mu;
    real <lower=0> sigma;
            }
model {
    mu ~ normal(0,1000);
    sigma ~ inv_chi_square(89);
    y ~ lognormal(mu, sigma);
       }
"


## Data needs to be in a list:
unemp.data=list(n = nrow(unemployment), y=as.vector(unemployment[, "UN"]))

### This is how you run Stan
binom.stan= rstan::stan(model_code = unemp, data=unemp.data, seed=10678)



### Pull off the posterior samples
new.mu= extract(binom.stan)$mu

new.sigma = extract(binom.stan)$sigma

mean(new.mu)
sd(new.mu)

mean(new.sigma)

####Graph the posterior
p.post=data.frame(new.p)
ggplot(p.post,aes(new.p)) +
  geom_histogram() +
  labs(title="Posterior distribution of p",y="Frequency",x="P")


###Probability p is lower than 0.30
sum(new.mu>3.4)/length(new.mu)


###95% Probability Interval
quantile(new.p,p=c(0.025,0.975))

