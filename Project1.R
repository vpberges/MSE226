library(tibble)
library(ggplot2)
library(reshape2)
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
  return (toString( dict$NAME.OF.DATA.ELEMENT[which(tolower(dict$VARIABLE.NAME) == tolower(name))]))
}
data <-read.csv('CollegeScorecard_Raw_Data/MERGED2010_PP.csv')
na_fraction <- function(name){
  return(sum(is.na(data[[name]])) / length(data[[name]])) 
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

values_to_keep = c('mn_earn_wne_p7')
values_to_delete = c()
vince = read.csv('Vince_var.txt')$VARIABLE.NAME
for(i in names(d)){
  #if (sum(is.na(d[[i]])) / 5967 > 0.20 ){ #0.1 ?
  if (!(i %in% vince )){ 
    #print(i)
    #print(sum(is.na(d[[i]])))
    #print(sum(is.na(d[[i]])) / 5967)
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


# Correlation
for (i in names(train)){
  if ((i!= "INSTNM") &(i!= "CITY")&(i!= "STABBR") ){
    print(paste(i,"       ",cor(d$mn_earn_wne_p7, d[[i]], use = "na.or.complete")))
  }
}

cormap = matrix(nrow = length(values_to_keep), ncol = length(values_to_keep))
for (i in 1:length(values_to_keep)){
  for (j in 1:length(values_to_keep)){
          cormap[i,j] = tryCatch( 
  cor(train[[values_to_keep[i]]], train[[values_to_keep[j]]], use = "na.or.complete"),
            warning = function(w){return(0)},error = function(e){return(0)} ) 
          #print(cor(train[[i]], train[[j]], use = "na.or.complete"))
    }
}



q <- qplot(x=Var1, y=Var2, 
  data=melt(cor(train[ , sapply(train, is.numeric)], use = "na.or.complete")),
  fill=value, geom="tile")

q + theme(axis.text.x = element_text(angle = 90))



### Processing
reg = lm(mn_earn_wne_p7 ~ . , data = train)

predict(object = reg, newdata = test)






