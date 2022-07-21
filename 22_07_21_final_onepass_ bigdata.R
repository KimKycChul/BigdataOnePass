# part2 

# 4장 R 데이터 연산과 기본함수, 123page
data(iris)
head(iris) 
head(iris,3)
head(iris,3)
tail(iris,3)
tail(iris,3)
dim(iris)
length(iris)
length(iris$Petal.Length)
class(iris)
class(iris$Petal.Length)
class(iris$Species)
ls(iris)
mean(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg)
sum(mtcars$mpg)
range(mtcars$mpg)
max(mtcars$mpg)
min(mtcars$mpg)
quantile(mtcars$mpg)
IQR(mtcars$mpg)
summary(iris) 

# 04 빈도분석, 128page
setwd("c:/data")
df<-read.csv("Data1.csv")
df$Gender<-factor(df$Gender)
df$EDU<-factor(df$EDU)
levels(df$Gender)
levels(df$EDU)
install.packages("plyr")
library(plyr)
df$Gender<-revalue(df$Gender,replace = c("0"="female","1"= "male"))
df$EDU<-revalue(df$EDU,replace=c('1'='high','2'='university',
                                   '3'='graduate','4'="phd"))
table(df$Gender)
table(df$EDU)
table(df$Gender,df$EDU)
a<-table(df$Gender)
b<-table(df$EDU)
prop.table(a)
prop.table(b)
e<-table(df$Gender,df$EDU)
prop.table(e)

prop.table(e,1) 
prop.table(e,2)
round(0.7811159,2)
round(prop.table(e),2)

# 5장 데이터 전처리,133page
#
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
data("diamonds")
#01 파이프(Pipes) 연산자
diamonds %>% head %>% dim
#02 rename 
daimonds1<-diamonds %>% rename(c=clarity,p=price)
head(daimonds1,3)
#03 count
count(diamonds,cut)
table(diamonds$cut)
class(count(diamonds,cut))
class(table(diamonds$cut))
#04 select
df1<-diamonds %>% select(carat,price)
head(df1,3)
df2<-diamonds %>% select(-carat,-price)
head(df2,3)
#05 slice
diamonds %>% slice(1:5)
diamonds %>% slice(-1)
#06 filter
diamonds %>% filter(cut=="Good") %>% head(3)
max(diamonds$price)
diamonds %>% filter(price==max(price))
diamonds %>% filter(price==18823)
diamonds %>% filter(cut!="Premium") %>% head(3)
diamonds %>% filter(price>=1000) %>% head(3)
diamonds %>% filter(price!=1000) %>% head(3)
diamonds %>% filter(price==1000) %>% head(3)
diamonds %>% filter(price!=1000&cut=="Ideal") %>% head(3)
diamonds %>% filter(price!=1000&cut=="Ideal"&color=="E") %>% head(3)
diamonds %>% filter(carat<1|carat>5) %>% head(3)
diamonds %>% filter(cut%in%c("ldeal","Good")) %>% head(3)
diamonds %>% select(carat,depth,price) %>% 
  filter(depth==max(depth)|price==min(price))

#07 mutate
diamonds %>% mutate(Ratio=price/carat,Double=Ratio*2) %>% head(3)
diamonds %>% summarize(mean(price))
diamonds %>% summarize(AvgPrice=mean(price),
                       MedianPrice=median(price),
                       AvgCarat=mean(carat))
#08 group_by
diamonds %>% group_by(cut) %>%
  summarize(AvgPrice=mean(price),SumCarat=sum(carat))

diamonds %>% group_by(cut) %>% 
  summarize(n=n()) %>% 
  mutate(total=sum(n), pct=n/total*100)
#09 mutate, ifelse
quantile(diamonds$price) 
diamonds1<-diamonds %>% mutate(price_class=ifelse(price>= 5324.25,"best",
                                                  ifelse(price>=2401,"good",
                                      ifelse(price>=950,"normal","bad"))))
table(diamonds1$price_class) 
#10 arrange
diamonds %>% group_by(cut) %>% 
  summarize(AvgPrice=mean(price)) %>% 
  arrange(desc(AvgPrice))

#11 데이터 결합하기
ott1<-data.frame(id=c(1,2,3),car=c("bmw","bmw","bmw"),fe=c(20,22,24))
ott2<-data.frame(id=c(1,4,5),fe1=c(30,34,35))
ott1
ott2
left_join(ott1,ott2,by="id")
inner_join(ott1,ott2,by="id")
full_join(ott1,ott2,by="id")
ott3<-data.frame(nation_code=c(1,2,3,4),nation=c("korea","japan",
                                                 "china","germany"))
ott4<-data.frame(car=c("bmw","toyota","kia"),nation_code=c(3,3,2))

ott3
ott4
left_join(ott4,ott3,by="nation_code")

ott5<-data.frame(car=c("bmw","bmw","bmw"),fe=c(20,22,24))
ott6<-data.frame(car=c("audi","audi","audi"),fe1=c(20,22,24))
bind_rows(ott5,ott6)

# 12 실기 시험에 알아두면 유용한 함수
q1<-c("nike","polo","adidas","wilson","yonex")
q1_factor<-as.factor(q1)
q1_factor
as.numeric(q1_factor)
factor(x=c("high school","colleage","masters"),
       levels=c("high school","colleage","masters"),
       ordered = TRUE)

economics<-ggplot2::economics
head(economics)

economics<-economics %>% mutate(year=substr(economics$date,1,4))
economics%>%group_by(year)%>%summarize(m=mean(psavert))%>% 
arrange(desc(m)) %>% head(5)

as.Date("2021-05-01")
as.Date("20210501")

install.packages("lubridate")
library(lubridate)
library(dplyr)
data(lakers)
lakers<-lakers %>% as_tibble
lakers %>% select(date,time)

lakers<-lakers %>% 
  mutate(date=paste(date,time) %>% ymd_hm) %>% 
  rename(time_index=date) %>% 
  select(-time) 
head(lakers)

lakers %>% group_by(month(time_index)) %>% 
  summarize(mean_x=mean(x,na.rm=TRUE),mean_y=mean(y,na.rm=TRUE))

lakers %>% group_by(year(time_index)) %>% 
  summarize(mean_x=mean(x,na.rm=TRUE),mean_y=mean(y,na.rm=TRUE))

lakers %>% filter(time_index<=ymd_hms("2008-10-28 12:00:00")) %>% 
  head(3)

lakers%>%filter(time_index<=ymd_hms("2008-10-28 12:00:00"),
                 time_index<=ymd_hms("2009-03-09 00:33:00")) %>% head(3)


airquality$grade_Wind1<-cut(airquality$Wind,breaks=3)
airquality$grade_Wind2<-cut(airquality$Wind,breaks=3,
                            labels=c("s","m","w"))

head(airquality,3)
mean(airquality$Wind)
range(airquality$Wind)

airquality$grade_Wind3<-cut(airquality$Wind,
                            breaks=c(1.7,9.96,20.7),
                            labels=c("low","high"),
                            include.lowest = TRUE)
head(airquality,3)


# 6장 결측치와 이상치 처리, 164page

data("airquality")
library(dplyr)
summary(airquality)
names(airquality)<-tolower(names(airquality))
is.na(airquality$ozone)
table(is.na(airquality))
table(is.na(airquality$ozone))
summary(is.na(airquality))

sum(airquality$ozone)
mean(airquality$ozone)
sum(airquality$ozone,na.rm=TRUE)
mean(airquality$ozone,na.rm=TRUE)
airquality<-na.omit(airquality)
table(is.na(airquality))

library(dplyr)
names(airquality)<-tolower(names(airquality))
airquality %>% filter(!is.na(ozone)) %>% head(3)

airquality %>% filter(!is.na(ozone)&!is.na(solar.r)) %>%
  head(3)

mean(airquality$ozone,na.rm=TRUE)
airquality$ozone<-ifelse(is.na(airquality$ozone),42.0991,airquality$ozone)
table(is.na(airquality$ozone))

# 02 이상치, 170page
ott7<-data.frame(gender=c("1","1","2","2","2","3"),
                 income=c(200,250,200,300,200,150))
ott7
table(ott7$gender)
ott7$gender<-ifelse(ott7$gender==3,NA,ott7$gender)
table(is.na(ott7$gender))
ott7 %>% filter(!is.na(gender))
boxplot(iris$Sepal.Width)$stats
iris %>% filter(Sepal.Width>4.0|Sepal.Width<2.2)
iris$Sepal.Width<-ifelse(iris$Sepal.Width>4.0|iris$Sepal.Width<2.2,
                         NA,iris$Sepal.Width)

table(is.na(iris$Sepal.Width))
iris %>% filter(!is.na(Sepal.Width)) %>% dim



# 7장 연습문제,174page
#01
library(dplyr)
install.packages("hflights")
library(hflights)
count<-dplyr::count
head(hflights)
hflights %>% count(Dest)%>% filter(n==max(n)|n==min(n))

#02
table(is.na(hflights))

#03
table(hflights$Cancelled)

#04
data("airquality")
table(is.na(airquality$Ozone))
median(airquality$Ozone,na.rm=TRUE)
airquality$Ozone<-ifelse(is.na(airquality$Ozone),31.5,airquality$ Ozone)
mean(airquality$Ozone)


#05
library(dplyr)
library(ggplot2)
diamonds %>% select(price)%>% arrange(desc(price))%>% head(3)

#06
rm(list=ls())
install.packages("dplyr")
library(dplyr)
data("diamonds")
diamonds %>% group_by(cut,color) %>% summarize(m=mean(price)) %>% 
  arrange(desc(m)) %>% head(3)


#07
diamonds %>% group_by(cut) %>% summarize(n=n()) %>% 
  mutate(total=sum(n),pct=n/total*100)

#08
install.packages("gapminder")
library(gapminder) 
str(gapminder)
gapminder %>% filter(continent=="Africa"|continent=="Europe") %>%
  arrange(desc(pop))



# 8장 실전문제
getwd()
setwd('c:/data')
library(dplyr)
install.packages("readx1")
library(readxl)
airseoul<-read_excel("period1.xlsx")
str(airseoul)
names(airseoul)
airseoul1<-airseoul %>% 
  rename(date="날짜",
           region="측정소명",
           pm10="미세먼지 PM10\r\n(㎍/m3)",
           pm2.5="초미세먼지\r\nPM2.5 (㎍/m3)") %>% 
  select(date,region,pm10,pm2.5)

table(airseoul1$date)
table(airseoul1$region)
airseoul1<-airseoul1 %>% filter(date!="전체"&region!="평균")
table(airseoul1$date)
table(airseoul1$region)
summary(airseoul1$pm10)
summary(airseoul1$pm2.5)
airseoul1<-airseoul1 %>% filter(!is.na(pm10)&!is.na(pm2.5))
# 1
#01)
airseoul1 %>% filter(pm10==max(pm10)) %>%select(date,region,pm10)

#02)
airseoul1 %>% group_by(region) %>% summarize(m=mean(pm10)) %>%
  arrange(desc(m)) %>% head(5)

#03)
airseoul1 %>% mutate(pm_grade=ifelse(pm10<=30,"good",
                                     ifelse(pm10<=81,"normal",
                                            ifelse(pm10<=150,"bad","worse")))) %>% 
  group_by(pm_grade) %>% 
  summarize(n=n()) %>% 
  mutate(total=sum(n),pct=n/total*100)

#04)
airseoul1 %>% filter(pm2.5==min(pm2.5)) %>% 
  arrange(desc(pm10))

#2
library(dplyr)
subway_202203<-read.csv("CARD_SUBWAY_MONTH_202203.csv")
str(subway_202203)
subway_202203<-subway_202203 %>% 
  rename(date="사용일자",
        line="노선명",
        station="역명",
        on_pass="승차총승객수",
        off_pass="하차총승객수") %>% 
  select(-"등록일자")
summary(subway_202203)

#01)
subway_202203 %>% summarise(on_p=mean(on_pass),off_p=mean(off_pass))

#02)
subway_202203 %>% filter(on_pass==max(on_pass))

#03)
subway_202203 %>% group_by(station) %>% 
  mutate(total_pass=on_pass+off_pass)%>% 
  summarize(m=mean(total_pass)) %>% arrange(desc(m)) %>% head(3)

#04)
subway_202203 %>%mutate(total_pass=on_pass+off_pass)%>%
  filter(line=="1호선") %>% filter( total_pass==max(total_pass)) 

#05)
table(subway_202203$date)
subway_202203$day<-substr(subway_202203$date,7,8)
table(subway_202203$day)
subway_202203$day<-as.numeric(subway_202203$day)
subway_202203$week<-ifelse(subway_202203$day%in%c(5,6,12,13,19,20,26,27),
                           "weekend","weekday")
table(subway_202203$week)
options(scipen = 999)
subway_202203<-subway_202203 %>% mutate(total_pass=on_pass+off_pass)
t.test(data=subway_202203,total_pass~week)


# 3
install.packages("foreign")
library(foreign)
koweps<-read.spss("koweps_h16_2021_beta1.sav")
class(koweps)
korwps_21<-as.data.frame(koweps)
house<-korwps_21 %>% select(h1601_4,h1601_5,h1601_6,
                            h16_reg5,h1608_114,h1608_122)

str(house)
house1<-house %>% 
  rename(gender=h1601_4,
        birth=h1601_5,
        edu=h1601_6,
        region=h16_reg5,
        r_salary=h1608_114,
        t_salary=h1608_122)

summary(house1)

house1$r_salary<-ifelse(house1$r_salary==0,NA,house1$r_salary)
house1$t_salary<-ifelse(house1$t_salary==0,NA,house1$t_salary)
house1$age<-2021-house1$birth+1
range(house1$age)
table(house1$edu)
house1$edu_grade<-ifelse(house1$edu%in%c(2,3,4),"중학이하",
                         ifelse(house1$edu==5,"고교",
                         ifelse(house1$edu==6,"전문대","대학이상")))
table(house1$edu_grade)
table(house1$region)
region_name<-data.frame(region=c(1,2,3,4,5),
                        region1=c("서울","광역시","시","구","도농복합구"))
house1<-left_join(house1,region_name,by="region")
str(house1)

#01)
house1 %>% 
  filter(!is.na(r_salary)) %>% 
  group_by(gender) %>% 
  filter(r_salary==max(r_salary))

#02)
house1 %>% filter(!is.na(r_salary)) %>% 
  group_by(age) %>% 
  summarize(m=mean(r_salary)) %>% 
  arrange(desc(m)) %>% head(3)

#03)
house1 %>% filter(!is.na(r_salary)) %>% 
  group_by(gender,edu_grade) %>% 
  summarize(m=mean(r_salary)) %>% 
  arrange(desc(m))

#04)
house1 %>%filter(!is.na(t_salary)) %>% 
  group_by(region1) %>% 
  summarize(m=mean(t_salary)) %>% 
  arrange(desc(m))


#4)
library(dplyr)
food<-read.csv("6110000_서울특별시_07_24_04_P_일반음식점.csv",na="")

food1<-food %>% 
  rename(open_date=인허가일자,
           status=상세영업상태명,
           close_date=폐업일자,
           name=사업장명,
           type=업태구분명,
           address=소재지전체주소) %>% 
  select("open_date","status","name","close_date","type","address")
str(food1)
summary(is.na(food1))
table(food1$type)

food1$type<-ifelse(food1$type%in%c("까페","다방","라이브카페","커피숍","카페"),
                    "카페",food1$type)
food1$type<-ifelse(food1$type%in%c("통닭(치킨)","호프/통닭"),"치킨",food1$type)
food1$type<-ifelse(food1$type%in%c("일식","회집","횟집"),"회집",food1$type)
food1$type<-ifelse(food1$type%in%c("경양식","패밀리레스토랑"),
                    "레스토랑",food1$type)
food1$type<-ifelse(food1$type%in%c("정종/대포집/소주방"),"소주방",food1$type)
food1$type<-ifelse(food1$type=="외국음식전문점(인도,태국등)",
                    "외국음식전문점",food1$type)
food1$type<-ifelse(food1$type%in%c("기타","193959.1505"),NA,food1$type)

range(food1$open_date)
food1$open_date<-ifelse(food1$open_date<19700301,NA,food1$open_date)
table(is.na(food1$open_date))
food1$open_year<-substr(food1$open_date,1,4)
range(food1$close_date,na.rm=TRUE)
food1$close_year<-substr(food1$close_date,1,4)


#01)
food1 %>% filter(!is.na(open_date)&status=="영업") %>% 
  filter(open_date==min(open_date)) %>% 
  select(type,open_year,name)

#02)
food1 %>% filter(!is.na(open_date)&!is.na(type)) %>% 
  group_by(type) %>% 
  summarize(n=n()) %>% 
  mutate(total=sum(n),
           pct=n/total*100) %>% 
  arrange(desc(n)) %>% head(3)

#03)
food1 %>% filter(!is.na(open_date)) %>% 
  group_by(open_year) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% head(3)
food1 %>% filter(!is.na(close_date)) %>% 
  group_by(close_year) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% head(3)


# part3 작업형 제1유형
rm(list=ls())
#01
library(dplyr)
house<-read.csv("housing.csv")
nrow(house)
rownum<-nrow(house)*0.8
house1<-house[1:rownum,]
house1 %>% glimpse
colSums(is.na(house1))
df1<-sd(house1$total_bedrooms,na.rm=TRUE)
df1
df2<-median(house1$total_bedrooms,na.rm=TRUE)
df2
house1$total_bedrooms<-ifelse(is.na(house1$total_bedrooms),df2,
                              house1$total_bedrooms)
colSums(is.na(house1))
df3<-sd(house1$total_bedrooms)
df3
df4<-df1-df3
print(df4)

#02
library(dplyr)
house<-read.csv("housing.csv")
nrow(house)
colSums(is.na(house))
house<-house%>% filter(!is.na(total_bedrooms))
colSums(is.na(house))

rownum<-nrow(house)*0.7
rownum
house1<-house[1:rownum,]
quantile(house1$housing_median_age)

df<-quantile(house1$housing_median_age)[[2]]
print(df)


#03
library(dplyr)
titanic<-read.csv("train100.csv")
titanic %>% glimpse
colSums(is.na(titanic))
titanic$Embarked<-as.factor(titanic$Embarked)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Pclass<-as.factor(titanic$Pclass)
summary(titanic)
table(titanic$Embarked)
df<-nrow(titanic)
titanic %>% filter(is.na(Age)|Age=='') %>% summarize(n=n()) %>% 
  mutate(pct=n/df*100)->df1
titanic %>% filter(is.na(Embarked)|Embarked=='') %>% summarize(n=n()) %>% 
  mutate(pct=n/df*100)->df2
df1;df2
names(titanic)[6]->df3
print(df3)

#04
library(dplyr)
library(MASS)
data("Boston")
Boston %>% glimpse

boston1<-Boston %>% arrange(desc(crim))
boston1 %>% head
select<-dplyr::select
boston1$crim[10]
boston1$crim[1:10]<-25.9406
boston1 %>% head(10)
boston1 %>% filter(age>=80) %>% select(crim) %>% summarize(m=mean(crim))->df
df
print(df[[1]])



#05
library(dplyr)
insurance<-read.csv("insurance.csv")
colSums(is.na(insurance))
avg=mean(insurance$charges)
avg
sd=sd(insurance$charges)
sd
insurance1<-insurance %>% filter(charges>=avg+1.5*sd|charges<=avg-1.5*sd)
sum(insurance1$charges) 
print(sum(insurance1$charges))

#06
library(dplyr)
data(mtcars)
mtcars %>% glimpse
select<-dplyr::select
result<-mtcars %>% select(wt) %>%
  mutate(min_max=((wt)-min(wt))/(max(wt)-min(wt))) %>% 
  filter(min_max>0.5) %>% NROW
print(result) 

#07
library(dplyr)
library(mlbench)
data("PimaIndiansDiabetes")
pima<-PimaIndiansDiabetes
pima %>% glimpse
colSums(is.na(pima))
pima %>% mutate(age_class=ifelse(age>=60,"3",
                                 ifelse(age>=41,"2","1")))->pima1

table(pima1$age_class)
pima1 %>% group_by(age_class) %>% 
  summarize(n=n(),fre=sum(diabetes=='pos')) %>% 
  mutate(ill_rate=fre/n*100)->df
df
round(print(df$ill_rate[2]),1)


# 08
library(dplyr)
library(gapminder)
gapminder %>% glimpse
gapminder %>% filter(year==2002) %>% summarize(m=mean(lifeExp))
gapminder %>% filter(year==2002) %>% group_by(country) %>%
  summarize(m=mean(lifeExp)) %>% 
  filter(m>=67.5) %>% NROW->result
print(result)

#09
library(dplyr)
df<-read.csv("disease.csv")
df %>% glimpse
library(reshape)
df1<-melt(df,id="year")
df1 %>% glimpse
colSums(is.na(df1))
names(df1)[2:3]<-c("country","disease")
names(df1)
df1 %>% filter(year==2000) %>% summarize(m=mean(disease))
df1 %>% filter(year==2000) %>% filter(disease>81.01036) %>% NROW->result
print(result)


#10
x<-sample(1:20,10)
y<-c(1,2,3,4,5,37,41,42,44,10)
a<-data.frame(x,y)
quantile(a$y)
df<-quantile(a$y)
df1<-abs(df[[2]]-df[[4]])
df1<-floor(df1)
cat(df1)


#11
df<-read.csv("facebook.csv")
df %>% glimpse
colSums(is.na(df))
df %>% mutate(ratio=(num_loves+num_wows)/(num_reactions)) %>% 
  filter(ratio<0.5&ratio>0.4) %>% filter(status_type=="video") %>% 
  NROW


#12
df<-read.csv("netflix.csv")
df %>% glimpse
colSums(is.na(df))
library(lubridate)
df %>% filter(country=="United Kingdom") %>% 
  mutate(ymd=dmy(date_added)) %>%select(ymd) %>% 
  filter(ymd>='2018-01-01'&ymd<='2018-01-30')

df %>% filter(country=="United Kingdom") %>% count(date_added)



# part 4 작업형 제2유형
rm(list=ls())
library(dplyr)
library(caret)
library(recipes)
library(pROC)

setwd('c:/data')
x_test<-read.csv('X_test.csv')
x_train<-read.csv('X_train.csv')
y_train<-read.csv('y_train.csv')
x_train %>% glimpse
y_train %>% glimpse
left_join(x_train,y_train,by='cust_id') %>% mutate(index='train')->train
train %>% glimpse

x_test %>% mutate(index='test')->test
test %>% glimpse
bind_rows(train,test)->full

full$gender<-ifelse(full$gender==0,"남성","여성")
full$gender<-as.factor(full$gender)
full$index<-as.factor(full$index)
names(full)

select<-dplyr::select
rename<-dplyr::rename
data<-full %>% rename(total="총구매액",
                      max="최대구매액",
                      refund="환불금액",
                      product="주구매상품",
                      store="주구매지점",
                      day="내점일수",
                      count= "내점당구매건수",
                      week="주말방문비율",
                      cycle="구매주기") %>% 
  select(cust_id,index,gender,total,max,refund,product,store,day,count,
         week,cycle)
colSums(is.na(data))


data$refund<-ifelse(is.na(data$refund),0,data$refund)
colSums(is.na(data))

library(recipes)
recipe(gender~.,data=data) %>%
  step_YeoJohnson(total,max,refund,day,count,week,cycle) %>% 
  step_scale(total,max,refund,day,count,week,cycle) %>% 
  step_center(total,max,refund,day,count,week,cycle) %>% 
  prep() %>% juice()->data1
data1%>%glimpse

data1 %>% filter(index=="train") %>% select(-index)->train
data1 %>% filter(index=="test") %>% select(-index)->test


library(caret)
ctrl<-trainControl(method='cv',number=10,
                   summaryFunction = twoClassSummary,
                   classProbs =TRUE)
set.seed(1357)
train(gender~.,data=train,
      method='rpart',
      metric="ROC",
      trControl=ctrl)->rffit

train(gender~.,data=train,
      method='glm',family=binomial,
      metric="ROC",
      trControl=ctrl)->rffit1

rffit
predict(rffit,test,type="prob")->pred_fit1
head(pred_fit1)

pred_fit1
predict(rffit,test,type="raw")->pred_fit2
head(pred_fit2)

names(pred_fit1)[1]<-"gender"
head(pred_fit1)
bind_cols(x_test,pred_fit1) %>% select(cust_id,gender)->df
head(df)


# 02
library(dplyr)
library(caret)
library(recipes)
library(pROC)
df<-read.csv("travel_data.csv")
set.seed(1357)
train_list<-createDataPartition(y=df$TravelInsurance,p=0.75,list=FALSE)
df_train<-df[train_list,]
df_test<-df[-train_list,]
NROW(df_train)

NROW(df_test)
df_train %>% glimpse

df_train %>% mutate(index="train")->df_train
df_test %>% mutate(index='test')->df_test
bind_rows(df_train,df_test)->full

full$TravelInsurance<-ifelse(full$TravelInsurance==0,"미가입","가입")
full$TravelInsurance<-as.factor(full$TravelInsurance)
full$GraduateOrNot<-as.factor(full$GraduateOrNot)
full$FrequentFlyer<-as.factor(full$FrequentFlyer)
full$EverTravelledAbroad<-as.factor(full$EverTravelledAbroad)
colSums(is.na(full))

recipe(TravelInsurance~.,data=full) %>%
  step_YeoJohnson(Age,AnnualIncome,FamilyMembers) %>% 
  step_center(Age,AnnualIncome,FamilyMembers) %>% 
  step_scale(Age,AnnualIncome,FamilyMembers) %>% prep() %>% juice()->data
data %>%filter(index=="train") %>% select(-index)->train 
data %>%filter(index=='test') %>% select(-index)->test

ctrl<-trainControl(method="cv",summaryFunction = twoClassSummary,
                   classProbs = TRUE)
train(TravelInsurance~.,data=train,
      method='rpart',metric="ROC",
      trControl=ctrl)->rpfit

rpfit
confusionMatrix(rpfit)

predict(rpfit,test,type='prob')->rffit1
head(rffit1)
predict(rpfit,test,type="raw")->rffit2
head(rffit2)

confusionMatrix(rffit2,test$TravelInsurance)
library(pROC)
rffit2_num<-as.numeric(rffit2)
head(rffit2_num)
result<-roc(test$TravelInsurance,rffit2_num)
result$auc

names(rffit1)[1]<-"y_pred"
bind_cols(df_test,rffit1) %>% select(INDEX,y_pred)->df
head(df)


#03
rm(list=ls())
train<-read.csv("Insurance_train_10.csv")
test<-read.csv("Insurance_test_10.csv")
train %>% glimpse
test %>% glimpse

colSums(is.na(train))
library(caret)
train$Segmentation<-as.factor(train$Segmentation)

ctrl<-trainControl(method="cv",number=10)
train(Segmentation~.,data=train,
      method='knn',trControl=ctrl,
      preProcess=c("center","scale"))->knn_fit

knn_fit
confusionMatrix(knn_fit)

predict(knn_fit,test)->pred_fit
head(pred_fit)
NROW(pred_fit)
test %>% glimpse
bind_cols(test,pred_fit)->df
names(df)[9]<-"Segmentaton_pred"
select<-dplyr::select
df %>% select(9)->df1
head(df1)



set.seed(12345)
IDX<-createDataPartition(train$Segmentation,p=0.7,list=FALSE)
train_t<-train[IDX,]
test_v<-train[-IDX,]
train_t$Segmentation<-as.factor(train_t$Segmentation)
test_v$Segmentation<-as.factor(test_v$Segmentation)

ctrl<-trainControl(method="cv",number=10)
train(Segmentation~.,data=train_t,
      method='knn',trControl=ctrl,
      preProcess=c("center","scale"))->knn_fit1
predict(knn_fit1,newdata=test_v)->test_pred
confusionMatrix(test_pred,test_v$Segmentation,mode="prec_recall")



#02 예상문제
#01
library(dplyr)
library(recipes)
library(caret)
read.delim("titanic3.txt",header=TRUE,sep=",")->full
full %>% glimpse


set.seed(1357)
train_list<-createDataPartition(full$survived,p=0.7,list=FALSE)
full_train<-full[train_list,]
full_test<-full[-train_list,]
NROW(full_train)
NROW(full_test)
train<-full_train
test<-full_test

train %>% mutate(index='train')->train
test %>% mutate(index='test')->test
bind_rows(train,test)->full
full %>% select(-boat,-body,-home.dest)->full
full$survived<-ifelse(full$survived==0,"생존","사망")
full$survived<-as.factor(full$survived)
full$pclass<-as.factor(full$pclass)
full$sex<-as.factor(full$sex)
full$embarked<-as.factor(full$embarked) 
colSums(is.na(full))
table(full$embarked)
levels(full$embarked)[1]<-NA
table(full$embarked,useNA="always")

full %>% filter(!is.na(age)&!is.na(fare)&!is.na(embarked))->full
colSums(is.na(full))
recipe(survived~.,data=full) %>% step_YeoJohnson(age,sibsp,parch,fare) %>% 
  step_center(age,sibsp,parch,fare) %>% 
  step_scale(age,sibsp,parch,fare) %>% 
  prep() %>% juice()->data
data %>% filter(index=="train") %>% select(-index,-name,-ticket,-cabin)->train
data %>% filter(index=='test') %>% select(-index,-name,-ticket,-cabin)->test


ctrl<-trainControl(method="cv",summaryFunction = twoClassSummary,
                   classProbs = TRUE)
train(survived~.,data=train,
      method="rpart",metric='ROC',
      trControl=ctrl)->rffit
rffit
confusionMatrix(rffit)
predict(rffit,test,type="raw")->rffit2
library(pROC)
rffit2_num<-as.numeric(rffit2)
rffit2_num
result<-roc(test$survived,rffit2_num)
result$auc


#02
rm(list=ls())
library(dplyr)
library(recipes)
library(caret)
select<-dplyr::select
df<-read.csv("nyc.csv")
df %>% glimpse
nyc<-df %>% select(3:7)
summary(nyc)
set.seed(1357)
nyc %>% glimpse
train_list<-createDataPartition(nyc$Price,p=0.7,list=FALSE)
train<-nyc[train_list,]
test<-nyc[-train_list,]
NROW(train)
NROW(test)
train %>% mutate(index='train')->train
test %>% mutate(index='test')->test
bind_rows(train,test)->full
full %>% glimpse
recipe(Price~.,data=full) %>% step_YeoJohnson(Food,Decor, Service) %>% 
  step_center(Food,Decor, Service) %>% 
  step_scale(Food,Decor, Service) %>% 
  prep() %>% juice()->data
data %>% glimpse
data %>% filter(index=="train") %>% select(-index)->train
data %>% filter(index=='test') %>% select(-index)->test
tc<-trainControl(method="cv",number=10)
model_2<-train(Price ~.,train, method="lm", trControl=tc)
model_2

predict(model_2,newdata=test)->lmfit


# 2.1 RMSE 
library(ggplot2)
library(dplyr)
library(recipes)
library(caret)
data("diamonds")
diamonds %>% glimpse

set.seed(123)
train_list<-createDataPartition(diamonds$price,p=0.7,list=FALSE)
train<-diamonds[train_list,]
test<-diamonds[-train_list,]
NROW(train)
NROW(test)
model <- lm(price ~ ., train)
p <- predict(model, test)
error <- p-test[["price"]]
sqrt(mean(error^2))

# 2.2 KNN
library(ggplot2)
library(dplyr)
library(recipes)
library(caret)
df<-read.csv("nyc.csv")
tc <- trainControl(method="cv", number=10)
param_grid <- expand.grid(k = c(1:20))
model_3 <- train(Price ~ ., df, method="knn", 
                 preProcess=c("center", "scale"), 
                 tuneGrid=param_grid, trainControl=tc, 
                 metric="RMSE") 
model_3

model_4 <- train(Price ~ ., nyc, method="knn", 
                 preProcess=c("center", "scale"), 
                 tuneGrid=param_grid, trainControl=tc, 
                 metric="Rsquared")
model_4

#2.3 KNN
library(dplyr)
library(recipes)
library(caret)
wbc<-read.csv("wbc.csv") 
wbc$id <- NULL 

wbc$diagnosis<-factor(wbc$diagnosis,levels=c("B","M"),
                      labels=c("Benign","Malignant"))
round(prop.table(table(wbc$diagnosis)),2) 
wbc %>% select(-X)->wbc
colSums(is.na(wbc))
idx<-createDataPartition(wbc$diagnosis,p=0.7,list=F)
train<-wbc[idx,]
test<-wbc[-idx,]
train %>% glimpse
test %>% glimpse


tc <- trainControl(method="cv", number=10)
train(diagnosis ~ ., wbc, method="knn",
      preProcess=c("center", "scale"), trControl=tc,metric="Accuracy")->knnfit

knnfit

confusionMatrix(knnfit)
predict(knnfit,test,type='prob')->prefit1
head(prefit1)
predict(knnfit,test,type='raw')->prefit2
head(prefit2)
library(pROC)
rffit2_num<-as.numeric(prefit2)
rffit2_num
result<-roc(test$diagnosis,rffit2_num)
result$auc


