library(tibble)
library(ggplot2)
library(dplyr)

setwd('/Users/vpberges/Documents/Stanford/Quarter7/MSE226/MSE226')

rmse <- function(rr)
{
  return (sqrt( (t(rr)%*%rr)/length(rr) ))
  #return (1/length(rr)*(t(rr)%*%rr)s)^0.5
}


dict <- data <-read.csv('CollegeScorecard_Raw_Data/CollegeScorecardDataDictionary-09-12-2015.csv')
dict <- as.data.frame(sapply(dict, tolower))
lookup <-function(name){
  print( dict$NAME.OF.DATA.ELEMENT[which(tolower(dict$VARIABLE.NAME) == tolower(name))])
}
data <-read.csv('CollegeScorecard_Raw_Data/MERGED2010_PP.csv')
na_fraction <- function(name){
  print(sum(is.na(data[[name]])) / length(data[[name]])) 
}



data[data=='PrivacySuppressed']<-NA
data[data=='NULL']<-NA

d = data %>% filter(!is.na(mn_earn_wne_p7) )
d[["mn_earn_wne_p7"]] <- as.numeric(as.character(d[["mn_earn_wne_p7"]]))

d = d[ , -which(names(d) %in% 
                c("mn_earn_wne_p9",
                  "sd_earn_wne_p9",
                  "gt_25k_p9",
                  "count_wne_p9",
                  "sd_earn_wne_p7",
                  "gt_25k_p7",
                  "count_wne_p7",
                  "mn_earn_wne_p8",
                  "sd_earn_wne_p8",
                  "gt_25k_p8",
                  "count_wne_p8",
                  "mn_earn_wne_p9",
                  "sd_earn_wne_p9",
                  "gt_25k_p9",
                  "count_wne_p9"))]


###### Filtering 

for (i in names(d)) {
  if (i != "INSTNM"){
    d[[i]] <- as.numeric(as.character(d[[i]]))
  }
}

values_to_keep = c()
values_to_delete = c()
for(i in names(d)){
  if (sum(is.na(d[[i]])) / 5967 > 0.20 ){ #0.1 ?
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

d[is.na(d)]<-0

####Split
train_ind <- sample(seq_len(nrow(d)), size = 5000)
train <- d[train_ind, ]
test <- d[-train_ind, ]



### Processing
reg = lm(mn_earn_wne_p7 ~ .,data = train)

predict(object = reg,newdata = test)


for (i in names(train)){
  if (i!= "INSTNM"){
    print(i)
    print(cor(d$mn_earn_wne_p7,d[[i]],use = "na.or.complete"))
  }
}



ggpairs(train[c("mn_earn_wne_p7","C150_4")])


