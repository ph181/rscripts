

#https://rpubs.com/mingo1226/hmm



install.packages(c( "depmixS4", "nnetm", "MASS", "Rsolnp", "nlme", "TTR", "ggplot2", "reshape2","xts", "markovchain "))

install.packages("markovchain")


library(depmixS4)
library(nnet)
library(MASS)
library(Rsolnp)
library(nlme)
library(TTR)
library(ggplot2)
library(reshape2)
library(xts)
library(markovchain)


prozessdaten <- as.data.frame  (subset(swe, select=c(G, L, DScS, Fehlerc, Dauerc, Fortschrittc, Seitenc, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))



#find DS
PD.klasse$DSneu <- ifelse(PD.klasse$DS > 0, 1, 0)



#classify each observation as a specific state
PD.klasse$markov_states <- ifelse(PD.klasse$DSneu == 1 
                               #& PD.num$tmax < 53
                               , "zu", 
                               ifelse(PD.num$DSneu == 0 
                                      #& PD.num$tmax < 53
                                      , "ab", "kein"))


#define state: 
S <- c('zu','ab', 'kein')


PD.klasse$markov_states[1:10]
simple_A <- markovchainFit(PD.klasse$markov_states[1:10])
simple_A$estimate

transition <- markovchainFit(data=PD.klasse$markov_states)
transition$estimate


A <- rbind(c(transition$estimate[1]), (transition$estimate[2])
           , (transition$estimate[3])
           )

apply(A, 1, sum)

V_labels <- c('H', 'L','M') 

#Emissions probabilities
#contingency table
e_table <- table(PD.num$markov_states, PD.num$DS)
e_table


#probabilities
clear <- e_table[1,]/sum(e_table[1,])
rain <- e_table[2,]/sum(e_table[2,])
#warm <- e_table[3,]/sum(e_table[3,])

#make sure order is same as S vector above
B <- rbind(clear, rain
           #, warm
           )
B
table(spring$markov_states)/length(spring$markov_states)
pi <- rbind(c(.26, .14, .6))
#pi <- rbind(c(1,0,0))

# Initialise HMM
hmm = initHMM(S, V_labels, startProbs = pi, transProbs = A, emissionProbs = B)
print(hmm)


install.packages('quantmod')
library('depmixS4')
library('quantmod')

# Create the parameters for the bull and
# bear market returns distributions
Nk_lower <- 50
Nk_upper <- 150
bull_mean <- 0.1
bull_var <- 0.1
bear_mean <- -0.05
bear_var <- 0.2




# Create the list of durations (in days) for each regime
days <- replicate(5, sample(Nk_lower:Nk_upper, 1))

# Create the various bull and bear markets returns
market_bull_1 <- rnorm( days[1], bull_mean, bull_var ) 
market_bear_2 <- rnorm( days[2], bear_mean, bear_var ) 
market_bull_3 <- rnorm( days[3], bull_mean, bull_var ) 
market_bear_4 <- rnorm( days[4], bear_mean, bear_var ) 
market_bull_5 <- rnorm( days[5], bull_mean, bull_var )


# Create the list of true regime states and full returns list
true_regimes <- c( rep(1,days[1]), rep(2,days[2]), rep(1,days[3]), rep(2,days[4]), rep(1,days[5]))
returns <- c( market_bull_1, market_bear_2, market_bull_3, market_bear_4, market_bull_5)





plot(returns, type="l", xlab='', ylab="Returns") 


# Create and fit the Hidden Markov Model
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)



# Output both the true regimes and the 
# posterior probabilities of the regimes
post_probs <- posterior(hmmfit)
layout(1:2)
plot(post_probs$state, type='s', main='True Regimes', xlab='', ylab='Regime')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Bull','Bear'), fill=1:2, bty='n')



# Obtain S&P500 data from 2004 onwards and
# create the returns stream from this
getSymbols( "^GSPC", from="2004-01-01" )
gspcRets = diff( log( Cl( GSPC ) ) )
returns = as.numeric(gspcRets)


plot(gspcRets)


# Fit a Hidden Markov Model with two states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')



# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')







https://www.datatechnotes.com/2017/12/hidden-markov-model-example-in-r.html

install.packages('HMM')   



library(HMM) 



states <- c("Target","Outlier")
targetProb <- c(0.4, 0.6)
outlierProb <- c(0.6, 0.4)


transProb <- matrix(c(targetProb, outlierProb), 2)
print(transProb)

elements <- c("short","normal","long")
targetStateProb <- c(0.1, 0.3, 0.6)
outlierStateProb <- c(0.6, 0.3, 0.1)


emissProb <- matrix(c(targetStateProb,outlierStateProb), 2, byrow = T) 
print(emissProb) 

hmm <- initHMM(States = states, 
               Symbols = elements,
               transProbs=transProb,
               emissionProbs = emissProb)
print(hmm) 



simhmm <- simHMM(hmm, 10)
simulated <- data.frame(state=simhmm$states, element=simhmm$observation)

print(simulated) 



testElements <- c("long","normal","normal","short",
                  "normal","normal","short","long")
stateViterbi <- viterbi(hmm, testElements) 



predState <- data.frame(Element=testElements, State=stateViterbi)
print(predState)   


