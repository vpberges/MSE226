library(tibble)
library(ggplot2)
library(dplyr)

rmse <- function(rr)
{
  return (sqrt( (t(rr)%*%rr)/length(rr) ))
  #return (1/length(rr)*(t(rr)%*%rr))^0.5
}


data <-read.csv('/Users/vpberges/Documents/Stanford/Quarter7/MSE226/MSE226/CollegeScorecard_Raw_Data/MERGED2010_PP.csv')

data[data=='PrivacySuppressed']<-NA
data[data=='NULL']<-NA

d = data %>% filter(!is.na(mn_earn_wne_p7) )
d[["mn_earn_wne_p7"]] <- as.numeric(as.character(d[["mn_earn_wne_p7"]]))

###### Filtering 

for (i in names(d)) {
  if (i != "INSTNM"){
    d[[i]] <- as.numeric(as.character(d[[i]]))
  }
}

values_to_keep = c()
values_to_delete = c()
for(i in names(d)){
  if (sum(is.na(d[[i]])) / 5967 > 0.10 ){
    print(i)
    print(sum(is.na(d[[i]])))
    print(sum(is.na(d[[i]])) / 5967)
    values_to_delete = c(values_to_delete,i)
  }
  else {
    values_to_keep = c(values_to_keep,i)
  }
}

print(values_to_keep)

d = d[values_to_keep]



####Split
train_ind <- sample(seq_len(nrow(d)), size = 5000)
train <- d[train_ind, ]
test <- d[-train_ind, ]



### Processing
reg = lm(mn_earn_wne_p7 ~ UNITID,data = train)

predict(object = reg,newdata = test)







