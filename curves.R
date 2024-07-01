library(nlme)
library(tidyverse)
# data ----

dis <- read.table("dis_soil_24.txt", 
                  sep ="\t", 
                  dec = ".", 
                  header = TRUE)
# 

model_exp <- nlme(
  percentage ~ 100 * exp(beta * day)
  ,data = dis
  ,fixed = list(beta ~ 1)
  ,random = list(beta ~ 1)
  ,groups =  ~ drug_soil
  ,start = c(-0.05)
  ,na.action = na.omit
  ,control = list(maxIter = 500000)
  , method = "REML"
)

summary(model_exp)

plot(model_exp)

blups <- model_exp$coefficients$random$drug_soil

# average formula

model_exp$coefficients$fixed

#f_ave <- "100*exp(-0.04935424*day)"


aliast <- list()

for(i in 1:7)  {
  d <- eval(parse(text = paste(
    'function(x) {100*exp(' ,
    (model_exp$coefficients$fixed['beta'] + blups[i, 'beta']),
    '*x)',
    '}',
    sep = ''
  )))
  
  aliast[i] = list(d)
  
}


dis |> 
  ggplot(aes(x=day,y=percentage, col=drug_1, shape = formulation))+ 
  geom_point(size=2, alpha=0.6) +
  geom_smooth()+
  #geom_errorbar(aes(ymin=`13C`- se, ymax=`13C`+se))+
  #ylab("13C") + 
  scale_x_continuous(limits=c(0,150), "time (days)")+
  ggthemes::theme_hc()+
  ggthemes::scale_color_hc()+
  #facet_grid(soil~.)+
  geom_function(fun=as.function(aliast[[1]]), colour = "#5A9BD4")+
  geom_function(fun=as.function(aliast[[2]]), colour = "#5A9BD4")+
  geom_function(fun=as.function(aliast[[3]]), colour = "#5A9BD4")+
  geom_function(fun=as.function(aliast[[4]]), colour = "grey30")+
  geom_function(fun=as.function(aliast[[5]]), colour = "grey30")+
  geom_function(fun=as.function(aliast[[6]]), colour = "#7AC36A")+
  geom_function(fun=as.function(aliast[[7]]), colour = "#7AC36A")
