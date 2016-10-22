library(tibble)
library(ggplot2)
library(dplyr)


data <-read.csv('/Users/vpberges/Documents/Stanford/Quarter7/MSE226/Project/CollegeScorecard_Raw_Data/MERGED2010_PP.csv')

data[data=='PrivacySuppressed']<-NA
data[data=='NULL']<-NA

#d = data %>% filter((mn_earn_wne_p7!='PrivacySuppressed')&!is.na(mn_earn_wne_p7)&(mn_earn_wne_p7!='NULL') )
d = data %>% filter(!is.na(mn_earn_wne_p7) )


for(i in names(d)){
  if (sum(is.na(d[[i]])) / 5967 > 0.05 ){
    print(i)
    print(sum(is.na(d[[i]])))
    print(sum(is.na(d[[i]])) / 5967)
  }
}










