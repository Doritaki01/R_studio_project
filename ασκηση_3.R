library(RISmed)
library(ggplot2)
library(UsingR)
library(plyr)

#Μέρος Α'

search_topic ='e-prescription'
search_query<- EUtilsSummary(search_topic, retmax=50, mindate=2016, maxdate=2016)
summary(search_query) #βάσει ποιών λέξεων και περιορισμών έγινε η αναζήτηση
QueryId(search_query) #τον αναγνωριστικό αριθμό που αντιστοιχεί σε καθένα από τα 50 άρθρα.

records<-EUtilsGet(search_query) #περιέχονται διάφορες πληροφορίες σχετικά με τα άρθρα
data<- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records)) #τοποθετούμε τους τίτλους και τις περιλήψεις των 50 άρθρων που επιλέχθηκαν στον πίνακα data
View(data)
head(data$Title,10) #τιτλος των 10 πρωτων
tail(data$Abstract,2) #περιληψη 2 τελευταιων

#Μέρος Β     

library(readr)

icd10 <- read_delim("C:/Users/geo20/Downloads/icd10.csv",";", escape_double = FALSE, trim_ws = TRUE) #διαβασμα csv
View(icd10)

first_disease=icd10[(icd10$code =='G710'),]    #George Charalambous el19706 δεν υπάρχει επιλέγω G710
print(first_disease$name)

second_disease=icd10[(icd10$code =='C060'),]  #George Charalambous el19706  δεν υπάρχει eπιλέγω C060

third_disease=icd10[(icd10$code =='D720'),]    ##τυχαια
print(third_disease$name)

#τα τελευταία 5 χρόνια 
search_query1 <-EUtilsSummary(first_disease$name,mindate=2018,maxdate=2022)
summary(search_query1)

search_query2 <-EUtilsSummary(second_disease$name,mindate=2018,maxdate=2022)
summary(search_query2)

search_query3 <-EUtilsSummary(third_disease$name,mindate=2018,maxdate=2022)
summary(search_query3)


#περιέχονται διάφορες πληροφορίες σχετικά με τα αρθρα

records1<-EUtilsGet(search_query1)
data1<- data.frame('Abstract'=AbstractText(records1))
View(data1)
newdata1 <- na.omit(data1) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata1)
nrow(newdata1)

records2<-EUtilsGet(search_query2)
data2<- data.frame('Abstract'=AbstractText(records2))
View(data2)
newdata2 <- na.omit(data2) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata2)
nrow(newdata2)

records3<-EUtilsGet(search_query3)
data3<- data.frame('Abstract'=AbstractText(records3))
View(data3)
newdata3 <- na.omit(data3) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata3)
nrow(newdata3)

#θα αποθηκεύσουμε σε 3 πίνακες για τα άρθρα που βρήκαμε προηγουμένως την χρονιά που δημοσιεύτηκαν

data_5_last_years_disease1<-data.frame('Year'=YearPubmed(records1),'Title'=ArticleTitle(records1))
data_5_last_years_disease1[is.na(data_5_last_years_disease1)] <- 0
View(data_5_last_years_disease1)


data_5_last_years_disease2<- data.frame('Year'=YearPubmed(records2),'Title'=ArticleTitle(records2))
data_5_last_years_disease2[is.na(data_5_last_years_disease2)] <- 0
View(data_5_last_years_disease2)

data_5_last_years_disease3<- data.frame('Year'=YearPubmed(records3),'Title'=ArticleTitle(records3))
data_5_last_years_disease3[is.na(data_5_last_years_disease3)] <- 0
View(data_5_last_years_disease3)

#τα 3 τελευταία χρόνια
search_query1 <-EUtilsSummary(first_disease$name,mindate=2020,maxdate=2022)
summary(search_query1)

search_query2 <-EUtilsSummary(second_disease$name,mindate=2020,maxdate=2022)
summary(search_query2)

search_query3 <-EUtilsSummary(third_disease$name,mindate=2020,maxdate=2022)
summary(search_query3)

records1<-EUtilsGet(search_query1)
data1<- data.frame('Abstract'=AbstractText(records1))
View(data1)
newdata1 <- na.omit(data1) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata1)
nrow(newdata1)

records2<-EUtilsGet(search_query2)
data2<- data.frame('Abstract'=AbstractText(records2))
View(data2)
newdata2 <- na.omit(data2) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata2)
nrow(newdata2)

records3<-EUtilsGet(search_query3)
data3<- data.frame('Abstract'=AbstractText(records3))
View(data3)
newdata3 <- na.omit(data3) #δημιουργια μια νεας βασης χωρις τα ΝΑ
View(newdata3)
nrow(newdata3)

data_3_last_years_disease1<-data.frame('Year'=YearPubmed(records1),'Title'=ArticleTitle(records1))
data_3_last_years_disease1[is.na(data_3_last_years_disease1)] <- 0
View(data_3_last_years_disease1)


data_3_last_years_disease2<- data.frame('Year'=YearPubmed(records2),'Title'=ArticleTitle(records2))
data_3_last_years_disease2[is.na(data_3_last_years_disease2)] <- 0
View(data_5_last_years_disease2)

data_3_last_years_disease3<- data.frame('Year'=YearPubmed(records3),'Title'=ArticleTitle(records3))
data_3_last_years_disease3[is.na(data_3_last_years_disease3)] <- 0
View(data_3_last_years_disease3)

#φτιαχουμε ενα κοινο πινακα

data_3_last_years_disease1$id <- first_disease$name
data_3_last_years_disease2$id <- second_disease$name
data_3_last_years_disease3$id <- third_disease$name

data_3_last_years<-rbind(data_3_last_years_disease1,data_3_last_years_disease2,data_3_last_years_disease3)
data_3_last_years<-data_3_last_years[!(data_3_last_years$Year==0),]
View(data_3_last_years)

#ιστογραμμα

f=first_disease$name
s=second_disease$name
t=third_disease$name

ggplot(data_3_last_years,aes(x=Year)) +
  geom_histogram(data=subset(data_3_last_years,id==f),binwidth =1,aes(fill=id) )+
  geom_histogram(data=subset(data_3_last_years,id==s),binwidth =1,aes(fill=id)) +
  geom_histogram(data=subset(data_3_last_years,id==t),binwidth =1,aes(fill=id)) +
  scale_fill_manual(name="Disease", values=c("lightgreen","pink","lightblue"),labels=c(f,s,t))+
  labs(y='Number of articles per year')


library(treemapify)

#ολα σε ενα πινακα

data_3_last_years_disease1<-data_3_last_years_disease1[!(data_3_last_years_disease1$Year==0),]
data_3_last_years_disease1<-data_3_last_years_disease2[!(data_3_last_years_disease2$Year==0),]
data_3_last_years_disease1<-data_3_last_years_disease3[!(data_3_last_years_disease3$Year==0),]
v1<-sapply(unique(data_3_last_years_disease1$Year), function (x) x = c(sum(data_3_last_years_disease1$Year == x)))
v2<-sapply(unique(data_3_last_years_disease2$Year), function (x) x = c(sum(data_3_last_years_disease2$Year == x)))
v3<-sapply(unique(data_3_last_years_disease3$Year), function (x) x = c(sum(data_3_last_years_disease3$Year == x)))
v<-c(v1,v2,v3)
v[is.na(v)] <- 0
v
year<-c('2022','2021','2020','2022','2021','2020','2022','2021','2020')
disease<-c(f,f,f,s,s,s,t,t,t)
sum_3_last_years<-data.frame('Number'=v,'Year'=year,'Disease'=disease)
View(sum_3_last_years)

#treemap

ggplot(sum_3_last_years, aes(area = Number,fill=Number,subgroup=Disease,label=year)) +
  geom_treemap()+geom_treemap_subgroup_border(colour="red",size=3) +
  geom_treemap_subgroup_text(place = "bottom", grow = T, alpha = 1, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)