library(glmnet)
library(useful)


state_finance_data$ln_general_expenditure_capita<-log(state_finance_data$general_expenditure_capita)

# Original econometric model
valueFormula<-ln_general_expenditure_capita~bbr1+bbr2+bbr3+bbr4+bbr5+bbr6+bbr7+bbr8+bbr9+
  gov_rep_lag+house_rep_lag+senate_rep_lag+divided_gov_lag+revenue_limit+expenditure_limit+
  as.factor(year)-1


model<-lm(valueFormula, state_finance_data)
sfbbrX_train<-build.x(valueFormula, data=state_finance_data,
                      contrasts=FALSE, sparse=TRUE)
sfbbrY_train<-build.y(valueFormula, data=state_finance_data)
result<-cv.glmnet(x=sfbbrX_train, y=sfbbrY_train, family="gaussian",nfolds=5)

coefplot(result, lambda='lambda.1se')
