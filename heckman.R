#heckman selection model
#corrects for selection bias (e.g. survey response by systematic factor)
#eg. income survey, only people who work respond
#aka incidental truncation
#Step 1: estimate probit model for the selection mechanism
#Compute the inverse mills ratio
#Compute regression model for selected sample (includes the inverse Mills ratio)

#1 yi = B1 + e
#2 di = gz + v
#3 di = 1 or 0 (whether person is in the sample or not)
#4 yi = yi*di

#1 Regression of interest
#2 reduced form for the latent variable capturing sample selection

# x z predicted by vars; g predicted by selection var used
# z contains one var which does not appear in x (see above) for semi-par procedures
# Primary aim is to consistently estimate B

#straightforward with the exception of the correction (mills ratio) the second step had adjusted SEs to account for first step estimation

#load packages
library(sampleSelection)

#example using Mroz87 data ffrom Toomet & henningsen R package write up

#2 step method
data ("Mroz87")
#female labour supply example (Greene, 2002)

  #Step 1: model labor force participation (selection outcome) ('lfp')
  #Step 2: model wage (outcome of interest)

#create kids variable
Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0)

#regular OLS model
ols1 = lm(wage ~ educ + exper + I( exper^2 ) + city, data=subset(Mroz87, lfp==1))
summary(ols1)

#estimate the selection, followed by outcome models
greeneTS <- selection(lfp~ age + I(age^2) + faminc + kids + educ, +
                        wage ~ exper + I(exper^2) + educ + city,
                        data = Mroz87, method = "2step")

#exclusion restriction (including var(s) in selection modeling not in outcome modeling; satisfied by age, faminc, kids)
summary(greeneTS)

#sigma = 3.2; if sigma > 0 inidcates the observed outcomes are better than average
#our OLS = upward bias here

#same example using the ML estimation (simulatneous estimation) this assumes stricter distributional assumptsions
greeneML <- selection (lfp ~ age + I(age^2) + faminc + kids + educ,
                        + wage ~ exper + I(exper^2) + educ + city, data = Mroz87,
                        maxMethod = "BHHH")
summary(greeneML)
#includes the Berndt-Hall-Hall_hausman method to obtain SEs published by Greene

#Implementation of this method works well IF:
#A) misspecification is unlikely
#B) exclusion restriction is fufilled
#in some cases 2 step method is preferred bc more robust to misspecification and issues with exclusion restriction fufillment

#No method is available in this package to estimate multilevel selection, or selection issues in panel data (dropout, etc); this is only avail via GLLAMM in Stata (Rabe-Hesketh)
#Information for GLLAMM: Rabe-Hesketh - http://fmwww.bc.edu/RePEc/dsug2002/select
#Proposed work around for multilevel selection is to specify a second sample selection rule 
#(per https://stats.stackexchange.com/questions/4989/multi-stage-selection-model-with-panel-data-in-r)


