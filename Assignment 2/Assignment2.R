##ESERCIZIO 1

##Retrieving the content of the S&P 500 component stocks table from wikipedia
library(XML)
library(httr)

ubase = "https://en.wikipedia.org/"
url = paste(ubase, "wiki/List_of_S%26P_500_companies", sep = "")
h <- handle(url)
res <- GET(handle = h)
doc = htmlParse(res)

preNode = getNodeSet(doc, "//td")

txt = xmlValue(preNode)

txt=matrix(txt,ncol=8,byrow=T)
txt=data.frame(txt)
colnames(txt)=c('Symbol','Security','GICS Sector','GICS Sub-Industry',
                'Headquarters Location','Date Added','CIK','Founded')

##S&P 500 contains 503 stocks because it includes two share classes of stock from 3 of its 
##component companies
txt=txt[1:502,]
head(txt)

txt$Symbol <- ifelse(grepl('\n', txt$Symbol),
                     substr(txt$Symbol, 1, nchar(txt$Symbol) - 1),
                     txt$Symbol)
txt$CIK <- ifelse(grepl('\n', txt$CIK),
                  substr(txt$CIK, 1, nchar(txt$CIK) - 1),
                  txt$CIK)
txt$Founded <- ifelse(grepl('\n', txt$Founded),
                      substr(txt$Founded, 1, nchar(txt$Founded) - 1),
                      txt$Founded)
txt$'GICS Sector' <- ifelse(grepl('\n', txt$'GICS Sector'),
                      substr(txt$'GICS Sector', 1, nchar(txt$'GICS Sector') - 1),
                      txt$'GICS Sector')
head(txt)
tail(txt)

## exploratory data analysis and summary statistics
txt$`GICS Sector` <-as.factor(txt$`GICS Sector`)
txt$`GICS Sub-Industry`<-as.factor(txt$`GICS Sub-Industry`)
txt$`Date Added`<-as.Date(txt$`Date Added`)
txt$Founded <-substr(txt$Founded, 1, 4)
txt$Founded <-as.numeric(txt$Founded)

summary(txt)
levels(txt$`GICS Sector`)

data_split <- strsplit(txt$`Headquarters Location`, ',', fixed = TRUE)

txt$`Headquarters City` <- sapply(data_split, function(x) x[1])
txt$`Headquarters State` <-sapply(data_split, function(x) x[2])

txt$`Headquarters City`<-as.factor(txt$`Headquarters City`)
txt$`Headquarters State`<-as.factor(txt$`Headquarters State`)
summary(txt)

library(ggplot2)
library(doBy)
ggplot(txt,aes(x=txt$'GICS Sector'))+
  geom_bar()+labs(title = "Bar Plot of sector")
ggplot(txt,aes(x=Founded, fill=txt$`GICS Sector`))+
  geom_histogram()+labs(title = "Histogram Date Founded",subtitle="segment: GICS Sector")
ggplot(txt,aes(x=txt$`Date Added`, fill=txt$`GICS Sector`))+
  geom_histogram()+labs(title = "Histogram Date Added",subtitle="segment: GICS Sector")

x<-data.frame(table(txt$'Headquarters State', txt$'GICS Sector'))
x<-x[order(x$Freq),]
colnames(x)<-c('State','Sector','Frequency')
tail(x,10)[10:1,]

##ESERCIZIO 2

## Importing data
setwd('C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Foundations of Financial Data Science/Assignment/Assignment2/HW2_data')
df1<-read.csv(file='securities.csv')
df2<-read.csv(file='fundamentals.csv')

## subset of 100 tickers, year 2013
df2$Period.Ending <- ifelse(grepl('2013', df2$Period.Ending), df2$Period.Ending, NA)
df2<-na.omit(df2)
df2<-df2[1:100,]

library(dplyr)
df1 <- df1 %>%
  filter(df1$Ticker.symbol %in% df2$Ticker.Symbol)

colfilter<-c('Ticker.Symbol', 'Period.Ending', 'After.Tax.ROE', 'Cash.Ratio', 'Current.Ratio', 'Pre.Tax.Margin', 
             'Pre.Tax.ROE', 'Profit.Margin', 'Quick.Ratio', 'Total.Assets', 'Total.Liabilities', 'Earnings.Before.Tax')
df2<-subset(df2,select = colfilter)

df2 <- df2[order(df2$Ticker.Symbol), ]
df1 <- df1[order(df1$Ticker.symbol), ]
df2[,3:12]<-sapply(df2[,3:12],FUN= function(x) (x-mean(x))/sd(x) )
rownames(df2)<-df2[,1]
df2<-df2[,3:12]

head(df2)
head(df1)

# Lp Norm
Lp_norm<-function(p){
  Lp_norm<-c()
  for (i in 1:100){
    for (j in 1:100){
    Lp_norm<- c(Lp_norm, sum(abs(df2[i,]-df2[j,])^p)^(1/p))
    }
  }
  Lp_norm<-matrix(Lp_norm,nrow=100,byrow=T)  
  Lp_norm<-data.frame(Lp_norm)
  colnames(Lp_norm)<-rownames(df2)
  rownames(Lp_norm)<-rownames(df2)
  return(Lp_norm)
}

Lp_norm_1<-Lp_norm(p=1)
Lp_norm_2<-Lp_norm(p=2)
Lp_norm_3<-Lp_norm(p=3)
Lp_norm_10<-Lp_norm(p=10)

subset(head(Lp_norm_1), select=1:10)
subset(head(Lp_norm_2), select=1:10)
subset(head(Lp_norm_3), select=1:10)
subset(head(Lp_norm_10), select=1:10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  y<-tail(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  a<-matrix(a,ncol=2,byrow=T)
  a<-data.frame(a)
  for (i in 1:10){
    a$X3[i]<-quantity[a$X1[i],a$X2[i]]
  }
  return(a)
}

top_10<-function(quantity){
  x<-sort(unlist(quantity))
  x<-x[x!=0]
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  a<-matrix(a,ncol=2,byrow=T)
  a<-data.frame(a)
  for (i in 1:10){
    a$X3[i]<-quantity[a$X1[i],a$X2[i]]
  }
  return(a)
}

bottom_10(Lp_norm_1)
top_10(Lp_norm_1)

bottom_10(Lp_norm_2)
top_10(Lp_norm_2)

bottom_10(Lp_norm_3)
top_10(Lp_norm_3)

bottom_10(Lp_norm_10)
top_10(Lp_norm_10)

# Minkowski
weights<-c(0.5,0.7,0.8,0.9,1,0.7,0.3,0.2,0.2,0.8)
Minkowski<-function(p){
  Minkowski<-c()
  for (i in 1:100){
    for (j in 1:100){
      Minkowski<- c(Minkowski, sum(weights*abs(df2[i,]-df2[j,])^p)^(1/p))
    }
  }
  Minkowski<-matrix(Minkowski,nrow=100,byrow=T)  
  Minkowski<-data.frame(Minkowski)
  colnames(Minkowski)<-rownames(df2)
  rownames(Minkowski)<-rownames(df2)
  return(Minkowski)
}

Minkowski_1<-Minkowski(p=1)
Minkowski_2<-Minkowski(p=2)
Minkowski_3<-Minkowski(p=3)
Minkowski_10<-Minkowski(p=10)

subset(head(Minkowski_1), select=1:10)
subset(head(Minkowski_2), select=1:10)
subset(head(Minkowski_3), select=1:10)
subset(head(Minkowski_10), select=1:10)

bottom_10(Minkowski_1)
top_10(Minkowski_1)

bottom_10(Minkowski_2)
top_10(Minkowski_2)

bottom_10(Minkowski_3)
top_10(Minkowski_3)

bottom_10(Minkowski_10)
top_10(Minkowski_10)

#Match-Based Similarity Computation (4 equi-depth bucket)
df3<-sapply(df2,FUN=function(x) cut(x,c(quantile(x,0), quantile(x,0.25),quantile(x,0.50),
                                    quantile(x,0.75),quantile(x,1)),include.lowest=T))
df3<-data.frame(df3)
for (i in 1:10){
  df3[,i]<-factor(df3[,i])
  levels<-levels(df3[,i])
  start_interv <- as.numeric(sapply(levels, function(x) {
    if (substring(x, 1, 1) == "(") {
      return(as.numeric(sub("^\\((.*?),.*\\]", "\\1", x)))
    } else if (substring(x, 1, 1) == "[") {
      return(as.numeric(sub("^\\[(.*?),.*\\]", "\\1", x)))
    }
  }))
  levels_ordered <- levels[order(start_interv)]
  df3[,i]<-factor(df3[,i],levels = levels_ordered)
}

similarity<-function(p){
  similarity<-matrix(NA,nrow=100,ncol=100)
  rownames(similarity)<-rownames(df2)
  colnames(similarity)<-rownames(df2)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:10){
        if (as.numeric(df3[i,z])==1){
            quartile<-0.25}
        else if (as.numeric(df3[i,z])==2){
            quartile<-0.5}
        else if (as.numeric(df3[i,z])==3){
            quartile<-0.75}
        else {
            quartile<-1}
        if (df3[i,z]==df3[j,z]){
          diff<-quantile(df2[,z],quartile)- quantile(df2[,z],quartile-0.25)
          match<-c(match, (1 - (abs(df2[i,z]-df2[j,z])/diff))^p)}}
      similarity[i,j]<-(sum(match))^1/p
    }
  }
  return(data.frame(similarity))
}

similarity<-similarity(1)
subset(head(similarity), select=1:10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  x<-x[x!=10]
  y<-tail(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  a<-matrix(a,ncol=2,byrow=T)
  a<-data.frame(a)
  for (i in 1:10){
    a$X3[i]<-quantity[a$X1[i],a$X2[i]]
  }
  return(a)
}

top_10<-function(quantity){
  x<-sort(unlist(quantity))
  y<-x[x==0]
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  res<-data.frame(a)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

bottom_10(similarity)
top<-top_10(similarity)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)


# Mahalanobis distance

df4<-as.matrix(df2)
matrice_var_cov <- cov(df4)
Maha<-function(){
  Maha<-c()
  for (i in 1:100){
    for (j in 1:100){
      Maha<- c(Maha, sqrt((df4[i,]-df4[j,]) %*%matrice_var_cov%*%(df4[i,]-df4[j,])))
    }
  }
  Maha<-matrix(Maha,nrow=100,byrow=T)  
  Maha<-data.frame(Maha)
  colnames(Maha)<-rownames(df2)
  rownames(Maha)<-rownames(df2)
  return(Maha)
}
Maha<-Maha()
subset(head(Maha), select=1:10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  y<-tail(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  a<-matrix(a,ncol=2,byrow=T)
  a<-data.frame(a)
  for (i in 1:10){
    a$X3[i]<-quantity[a$X1[i],a$X2[i]]
  }
  return(a)
}

top_10<-function(quantity){
  x<-sort(unlist(quantity))
  x<-x[x!=0]
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  a<-matrix(a,ncol=2,byrow=T)
  a<-data.frame(a)
  for (i in 1:10){
    a$X3[i]<-quantity[a$X1[i],a$X2[i]]
  }
  return(a)
}

bottom_10(Maha)
top_10(Maha)

###Categorical data

#Similarity: overlap measure
rownames(df1)<-df1[,1]
df1<-df1[,4:6]

overlap<-function(){
  overlap<-matrix(NA,nrow=100,ncol=100)
  rownames(overlap)<-rownames(df1)
  colnames(overlap)<-rownames(df1)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df1[i,z]==df1[j,z]){
          match<-c(match, 1)}
      overlap[i,j]<-sum(match)}
      }
  }
  return(data.frame(overlap))
}

overlap<-overlap()
subset(head(overlap), select=1:10)

top_10<-function(quantity){
  x<-sort(unlist(quantity))
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  res<-data.frame(a)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  a<-gsub("[0-9]", "", names(x))
  b<-as.numeric(gsub("[A-Z]", "", names(x)))
  b<-rownames(quantity)[b]
  p<-cbind(a,b)
  p<-p[a!=b,]
  y<-tail(p,20)
  res<-data.frame(y)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

bottom<-bottom_10(overlap)
bottom$pair <- apply(bottom, 1, function(x) paste(sort(x), collapse="-"))
bottom <-bottom[!duplicated(bottom$pair),]
bottom<-subset(bottom,select=c(1,2,3))
tail(bottom,10)

top<-top_10(overlap)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)

#Similarity: inverse frequency
inverse<-function(){
  inverse<-matrix(NA,nrow=100,ncol=100)
  rownames(inverse)<-rownames(df1)
  colnames(inverse)<-rownames(df1)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df1[i,z]==df1[j,z]){
          n<-(sum(df1[,z]==df1[i,z])/nrow(df1))^2
          match<-c(match, 1/n)}
        inverse[i,j]<-sum(match)}
    }
  }
  return(data.frame(inverse))
}

inverse<-inverse()
subset(head(inverse), select=1:10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  x<-x[1:9900]
  y<-tail(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  a<-data.frame(a)
  for (i in 1:nrow(a)){
    a$zeros[i]<-quantity[a$a[i],a$b[i]]}
  return(a)
}


bottom<-bottom_10(inverse)
bottom$pair <- apply(bottom, 1, function(x) paste(sort(x), collapse="-"))
bottom <-bottom[!duplicated(bottom$pair),]
bottom<-subset(bottom,select=c(1,2,3))
bottom

top_n<-function(quantity){
  x<-sort(unlist(quantity))
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  res<-data.frame(a)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

top<-top_n(inverse)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)

#Similarity: Goodall
goodall<-function(){
  goodall<-matrix(NA,nrow=100,ncol=100)
  rownames(goodall)<-rownames(df1)
  colnames(goodall)<-rownames(df1)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df1[i,z]==df1[j,z]){
          n<-(sum(df1[,z]==df1[i,z])/nrow(df1))^2
          match<-c(match, 1-n)}
        goodall[i,j]<-sum(match)}
    }
  }
  return(data.frame(goodall))
}

goodall<-goodall()
subset(head(goodall), select=1:10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  a<-gsub("[0-9]", "", names(x))
  b<-as.numeric(gsub("[A-Z]", "", names(x)))
  b<-rownames(quantity)[b]
  p<-cbind(a,b)
  p<-p[a!=b,]
  y<-tail(p,20)
  res<-data.frame(y)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}


bottom<-bottom_10(goodall)
bottom$pair <- apply(bottom, 1, function(x) paste(sort(x), collapse="-"))
bottom <-bottom[!duplicated(bottom$pair),]
bottom<-subset(bottom,select=c(1,2,3))
tail(bottom,10)

top_n<-function(quantity){
  x<-sort(unlist(quantity))
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  res<-data.frame(a)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

top<-top_n(goodall)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)


##Mixed data: categorical and quantitative
#Overall similarity between tickers by using mixed type data
df.all<-cbind(df1,df2)

overall<-function(){
  overall<-matrix(NA,nrow=100,ncol=100)
  lambda<-0.6
  rownames(overall)<-rownames(df.all)
  colnames(overall)<-rownames(df.all)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(1-lambda))}}
      for (z in 4:13){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(lambda))}}
      overall[i,j]<-sum(match)
      }
    }
  return(data.frame(overall))
}

overall<-overall()
subset(head(overall), select=1:10)

top_10<-function(quantity){
  x<-sort(unlist(quantity))
  y<-head(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  res<-data.frame(a)
  for (i in 1:nrow(res)){
    res$zeros[i]<-quantity[res$a[i],res$b[i]]}
  return(res)
}

top<-top_10(overall)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)

bottom_10<-function(quantity){
  x<-sort(unlist(quantity))
  x<-x[1:9900]
  y<-tail(x,20)
  z<-names(y)
  a<-gsub("[0-9]", "", names(y))
  b<-as.numeric(gsub("[A-Z]", "", names(y)))
  b<-rownames(quantity)[b]
  a<-cbind(a,b)
  a<-data.frame(a)
  for (i in 1:nrow(a)){
    a$zeros[i]<-quantity[a$a[i],a$b[i]]}
  return(a)
}

bottom<-bottom_10(overall)
bottom$pair <- apply(bottom, 1, function(x) paste(sort(x), collapse="-"))
bottom <-bottom[!duplicated(bottom$pair),]
bottom<-subset(bottom,select=c(1,2,3))
tail(bottom,10)

#Overall normalized similarity between tickers by using mixed type data
overall_cat<-function(){
  overall_norm<-matrix(NA,nrow=100,ncol=100)
  lambda<-0.6
  rownames(overall_norm)<-rownames(df.all)
  colnames(overall_norm)<-rownames(df.all)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(1-lambda))}}
      overall[i,j]<-sum(match)
    }
  }
  return(data.frame(overall))
}


overall_num<-function(){
  overall_norm<-matrix(NA,nrow=100,ncol=100)
  lambda<-0.6
  rownames(overall_norm)<-rownames(df.all)
  colnames(overall_norm)<-rownames(df.all)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 4:13){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(lambda))}}
      overall[i,j]<-sum(match)
    }
  }
  return(data.frame(overall))
}

overall_num<-overall_num()
overall_cat<-overall_cat()

a<-gsub("[0-9]", "", names(sort(unlist(overall_num))))
b<-as.numeric(gsub("[A-Z]", "", names(unlist(overall_num))))
b<-rownames(df1)[b]
res<-cbind(a,b)
res<-data.frame(res)
res<-res[1:9900,]
for (i in 1:nrow(res)){
  res$zeros[i]<-overall_num[res$a[i],res$b[i]]}

num_std<-sd(res$zeros)

a<-gsub("[0-9]", "", names(sort(unlist(overall_cat))))
b<-as.numeric(gsub("[A-Z]", "", names(unlist(overall_cat))))
b<-rownames(df1)[b]
res<-cbind(a,b)
res<-data.frame(res)
res<-res[1:9900,]

cat_std<-sd(unlist(overall_cat))

overall_norm<-function(){
  overall_norm<-matrix(NA,nrow=100,ncol=100)
  lambda<-0.6
  rownames(overall_norm)<-rownames(df.all)
  colnames(overall_norm)<-rownames(df.all)
  for (i in 1:100){
    for (j in 1:100){
      match<-c()
      for (z in 1:3){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(1-lambda)/cat_std)}}
      for (z in 4:13){
        if (df.all[i,z]==df.all[j,z]){
          match<-c(match, 1*(lambda)/num_std)}}
      overall_norm[i,j]<-sum(match)
    }
  }
  return(data.frame(overall_norm))
}

overall_norm<-overall_norm()

bottom<-bottom_10(overall_norm)
bottom$pair <- apply(bottom, 1, function(x) paste(sort(x), collapse="-"))
bottom <-bottom[!duplicated(bottom$pair),]
bottom<-subset(bottom,select=c(1,2,3))
tail(bottom,10)

top<-top_10(overall_norm)
top$pair <- apply(top, 1, function(x) paste(sort(x), collapse="-"))
top <-top[!duplicated(top$pair),]
top<-subset(top,select=c(1,2,3))
head(top,10)
