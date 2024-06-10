library(tidyverse)
library(knitr)
library(lavaan)
library(psych)

mydata <- Erhebung
mydata <- Erhebung[which(Erhebung$C ==0),]


mod1 <- "# a path
         FL ~ a * DFW

         # b path
         SP ~ b * FL

         # c prime path 
         SP ~ cp * DFW

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)

fsem1 <- sem(mod1, data = Erhebung
             ,group = "C"
             , se = "bootstrap", bootstrap = 1000)


summary(fsem1, standardized = TRUE)

parameterestimates(fsem1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()



###########
set.seed(1234)

model <- ' # direct effect
             DFW ~ c*SP
           # mediator
             DFW ~ a*FL
             SP ~ b*FL
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = mydata)
summary(fit)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

myModel <- '
DSW ~ b1 * FL + c1 * SP + c2 * SC + c3 * C
FL ~ a1 * SP + a2 * SC + a3 * C


#indirect effects
indirect1 := a1 * b1
indirect3 := a2 * b1

# contrasts
con1 := a1 * b1 - a2 * b1
con3 := (a1-a2) * b1

# total effect
total1 := c1 + (a1 * b1) 
total2 := c2 + (a2 * b1) 
total3 := c3 + (a3 * b1) 
# covariates

'
require("lavaan")
fit <- sem(myModel, 
           data=mydata, 
           se = "bootstrap", 
           bootstrap = 5000)



# parameters
hayes4 <- ' # direct effect
              FN ~ c*FV
              direct := c

            # regressions
              FL ~ a*FV
              FN ~ b*FL

            # indirect effect (a*b)
              indirect := a*b

            # total effect
              total := c + (a*b)

            # Prop
              prop := indirect/total'

# fit model
sem4 <- sem(model = hayes4,
            data = mydata,
            se = "bootstrap",
            bootstrap = 200)
# fit measures
summary(sem4,
        fit.measures = FALSE,
        standardize = TRUE,
        rsquare = TRUE)
