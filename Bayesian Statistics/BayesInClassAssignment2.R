library(quantmod)
library(rstan)
library(tidyverse)
library(ggplot2)

tickers = c("AAPL")
getSymbols(tickers)

AAPL$aapl_r <- ROC(AAPL$AAPL.Close)
stocks <- cbind(tail(AAPL$AAPL.Close, 500)*1000, tail(AAPL$aapl_r, 500))

ex2 <- "
data {
    int <lower=0> n;
    real y[n];
    }
parameters {
    real <lower=0> sigma2;
    real mu;
            }
model {
    mu ~ normal(0,100);
    y ~ normal(mu, sigma2);
    sigma2 ~ inv_gamma(0.001, 0.001);
       }
"

stock.data <- list(n = nrow(stocks), y = as.vector(stocks$aapl_r))
stock.stan <- stan(model_code = ex2, data = stock.data, seed = 15893, chains = 4, 
                   warmup = 3000, iter = 6000, refresh = 0, thin = 3)

traceplot(stock.stan,inc_warmup=F)
print(stock.stan, pars=c("mu", "sigma2"), probs=c(.1,.5,.9))

#Interesting plots
plot(stock.stan)
stan_ac(stock.stan)

post.params=extract(stock.stan)
new.mu=post.params$mu
new.sigma2=post.params$sigma2

one_per=qnorm(0.01,new.mu,sqrt(new.sigma2))

hist(one_per)
mean(one_per)

quantile(one_per,c(0.025,0.975))

#use Pt+1 to simulate tomorrow's price for #2. then multiply by 1000


###############################################################################################


# Convert the 'precip' column to a vector
y <- precip

# Define the Stan model
ex2 <- "
data {
    int <lower=0> n;
    real y[n];
}
parameters {
    real <lower=0> sigma2;
    real mu;
}
model {
    mu ~ normal(0,1000);
    y ~ normal(mu,sigma2);
    sigma2 ~ inv_gamma(69/2,69*0.5);  // Inverse Chi-square with 69 degrees of freedom
}
"

# Define data list
stock.data <- list(n = length(y), y = y)

# Run the Stan model
stock.stan <- rstan::stan(model_code = ex2, data = stock.data, seed = 69321,
                   warmup = 1000,
                   iter = 3000)

# Print the summary of the posterior distribution for mu
summary(stock.stan, pars = "mu")
summary(stock.stan, pars = "sigma2")

# Extract posterior samples of sigma and mu
posterior_samples <- extract(stock.stan)
posterior_sigma <- posterior_samples$sigma2
posterior_mu <- posterior_samples$mu

# Calculate coefficient of variation (CV)
posterior_cv <- posterior_sigma / posterior_mu

# Find the probability that CV is larger than 0.4
prob_cv_gt_0.4 <- sum(posterior_cv > 0.4) / length(posterior_cv)

# Print the probability
print(prob_cv_gt_0.4)
hist(posterior_cv)
traceplot(stock.stan,inc_warmup=T)

