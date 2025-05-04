library(urca)
library(forecast)

vars_names <- names(data_montly_d)

for(name in vars_names){
  if(name != 'date'){
    print(paste('Minnesota AR(1) coef para ',name,' : ', 
                as.character(round(
                  as.numeric(
                    arima(data_montly_d[name], order = c(1,0,0))$coef[1]), 3))
                
                
))
  }
}

round(as.numeric(arima(data_montly_d$brlx, order = c(1,0,0))$coef[1]), 3)

coef_prior <- sapply(vars_names[vars_names != "date"], function(name) {
  as.numeric(arima(data_montly_d[[name]], order = c(1,0,0))$coef[1])
})


