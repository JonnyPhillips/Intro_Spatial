## Functions to calculate Moran's I and LISA

library(tidyverse)
library(sf)
library(sp)
library(spdep)

lisa_classify <- function(x,neighbours){
  
  x <- x %>% mutate(Indicator=(Indicator-mean(Indicator))/sd(Indicator))
  
  lisa <- localmoran(x$Indicator,
                     nb2listw(neighbours),
                     zero.policy=T)
  
  d <- x %>% bind_cols(lisa %>% as.data.frame())
  
  d <- d %>% mutate(Indicator_Lagged=lag.listw(nb2listw(neighbours),
                                               d$Indicator,NAOK=TRUE))
  
  d <- d %>% rename("LISA_p"=`Pr(z > 0)`)
  
  d <- d %>% mutate(LISA_Sig=case_when(LISA_p<0.05~TRUE,
                                       TRUE~NA),
                    LISA_Cluster=case_when(LISA_p<0.05 & Indicator>=0 & Indicator_Lagged>=0~"High-High",
                                           LISA_p<0.05 & Indicator>=0 & Indicator_Lagged<=0~"High-Low",
                                           LISA_p<0.05 & Indicator<=0 & Indicator_Lagged>=0~"Low-High",
                                           LISA_p<0.05 & Indicator<=0 & Indicator_Lagged<=0~"Low-Low",
                                           TRUE~"Insignificant"))
  return(d)
}
