# MySQL과 R을 연동해 주는 패키지 설치
install.packages('RMySQL') #1
# 패키지 실행
library(RMySQL) #2 (MySQL 접속 패키지)
library(dplyr) # 데이터 전처리 패키지
# 접속 정보 입력
mydb = dbConnect(MySQL(),
                 user='edu', # MySQL 접속 계정
                 password='edu', # MYSQL 접속 계정 패스워드
                 dbname='edudb', # 데이터베이스명
                 host='multi-bigdata.cljkqcsbb9ok.ap-northeast-2.rds.amazonaws.com') # DB IP (local에 접속하는 경우: localhost) #3
# 데이터 data
data = dbGetQuery(mydb,'select * from dataset4') #4

############### 2) 문자열 숫자로 변경하기 ###############
data$Sex = ifelse(data$Sex == 'female',1,0)

############### 3) 분석할 변수 가져오기 ###############
M = data %>% select(Survived,Age,SibSp,Parch,Fare,Sex)

############### 4) 상관 계수 구하기 ###############
cor(M)

############### 5) 상관 관계 시각화 하기 ###############
install.packages('corrplot') # 상관관계 분석 패키지 설치
library(corrplot) # 상관관계 분석 패키지 불러오기
corr = cor(M) # 상관 계수 계산하기
corrplot(corr, method='circle') # 상관 계수 시각화





############### 272 페이지. 생존율 예측모델 생성 ###############

############### a) 데이터 임포트(SQL) ###############
# 패키지 실행
library(RMySQL) # MySQL 접속 패키지
library(dplyr) # 데이터 전처리 패키지
library(ggplot2)
mydb = dbConnect(MySQL(),
                 user='edu', # MySQL 접속 계정
                 password='edu', # MYSQL 접속 계정 패스워드
                 dbname='edudb', # 데이터베이스명
                 host='multi-bigdata.cljkqcsbb9ok.ap-northeast-2.rds.amazonaws.com') # DB IP (local에 접속하는 경우: localhost) #3
# 데이터 data
data = dbGetQuery(mydb,'select * from dataset4')

############### b) 생존율과 각 변수의 관계 파악 1(성별에 따른 생존율) ###############
# 성별에 따른 생존율
stat = data %>% group_by(Sex) %>%
  summarise(n_passengers = n(),
            n_survived = sum(Survived == 1)) %>% mutate(survived_rate = n_survived/n_passengers)
ggplot(stat,aes(x=Sex,y=survived_rate)) +
  geom_bar(stat='identity') + theme_bw() + ylab('Survived Rate (%)')

############### c) 생존율과 각 변수의 관계 파악 2(연령에 따른 생존율) ###############
# Histogram (Survived)
ggplot(data,aes(x=Age)) + geom_density() +
  facet_grid(~Survived) + theme_classic()

############### d) 생존율과 각 변수의 관계 파악 3(연령, 성별에 따른 생존율) ###############
# Histogram (Sex~Survived)
ggplot(data,aes(x=Age)) + geom_density() +
  facet_grid(Sex~Survived) + theme_classic()

