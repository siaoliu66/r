
  #載入套件
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(MASS)
library(sqldf)

# Q1 
data(survey)
# 1.1 移除na值，建立學生練習頻率分佈的 barchart
survey = na.omit(survey) 
ggplot(survey , aes(x= survey$Exer)) + geom_bar()

# 1.2-1 將身高分類 找出身高跟練習頻率的關係
survey$Height = cut(x= survey$Height,breaks=c(150,160,170,180,190,Inf),labels=c('150','160','170','180','>190'))
### costable_survey 這個變數可以自行改名，其他用取代方式一次取代
costable_survey = table(survey$Exer,survey$Height)
costable_survey

# 1.2-2 將身高分類 找出身高跟練習頻率的關係
recoding_Height <- function(x) {
  if (x >= 150 && x < 160) {
    Ht_group <- "150"
  } else if(x >= 160 && x < 170){
    Ht_group <- "160"
  }else if(x >= 170 && x < 180){
    Ht_group <- "170"
  }else if(x >= 180 && x < 190){
    Ht_group <- "180"
  }else{
    Ht_group <- "190"
  }
  return(Ht_group)
}
survey$Height_New <- sapply(FUN = recoding_Height, X = survey$Height)
library(gmodels) #加載gmodels包
CrossTable(survey$Exer,survey$Height_New,expected = T,format = "SPSS",fisher = T,prop.c = F,prop.t = F,prop.chisq = F)


# 1.3 假設檢定
chisq.test(costable_survey)
# p-value = 0.102，p值 > 0.05
#因觀測值有>5，會報警告標示，所以改用費雪精確檢定
fisher.test(costable_survey,simulate.p.value = T)
# p-value = 0.07796，p值 > 0.05，兩者沒有絕對關係

# Q2
my_StudentsPerformance <- read_csv("my_StudentsPerformance.csv")
# 2.1 寫一個my_summary 的function 計算平均、最大值、最小值和中值
my_summary = function(x) {
  return(data.frame(mean = mean(x),sd = sd(x),max = max(x),min = min(x),med = median(x)))
}
my_summary(my_StudentsPerformance$math_score) # math_score
my_summary(my_StudentsPerformance$reading_score) # reading_score
my_summary(my_StudentsPerformance$writing_score) # writing_score

# 2.2 計算數學、閱讀和寫作的平均分數並加入新欄avg_score
my_StudentsPerformance$avg_score = rowMeans(my_StudentsPerformance[,c("math_score","reading_score","writing_score")])

# 2.3-1 性別跟成績是否相關 anova
ggplot(aes(gender,avg_score),data = my_StudentsPerformance)+geom_boxplot()
ggplot(aes(avg_score,color = gender),data = my_StudentsPerformance)+geom_density(alpha = 0.1)+xlim(0,100)
aov.my_StudentsPerformance = aov(avg_score~gender,data=my_StudentsPerformance)
summary(aov.my_StudentsPerformance)
# 2.3-2 性別跟成績是否相關 常態檢定
ggplot(aes(gender,avg_score),data = my_StudentsPerformance)+geom_boxplot()
ggplot(aes(avg_score,color = gender),data = my_StudentsPerformance)+geom_density(alpha = 0.1)+xlim(0,100)
# 檢查是否趨近於常態 shapiro.test(x)
shapiro.test(avg_score)  # p<0.05 不是常態
t.test(avg_score~gender,data=my_StudentsPerformance) # T檢定 p<0.05 不是常態 或 分布不一樣
willcox.tes(avg_score~gender,data=my_StudentsPerformance) # 雙樣本中位數檢定 p<0.05 不是常態

# 2.4 雙親教育程度跟成績是否相關 anova
### tb_parental 這個變數可以自行改名，其他用取代方式一次取代
tb_parental = table(my_StudentsPerformance$parental_level_of_education);
prop.table(tb_parental)

lm_score = lm(avg_score~parental_level_of_education,data=my_StudentsPerformance) #一般線性迴歸

aov.parental_score = aov(avg_score~parental_level_of_education,data=my_StudentsPerformance)
summary(aov.parental_score)
### anova不準確所以多做一個檢定
willcox.tes(avg_score~parental_level_of_education,data=my_StudentsPerformance) #  p<0.05 


# Q3
diamonds = fread("diamonds.csv",data.table = F)
# 3.1 bivariate analysis 克拉數、切工使否跟價格有關
# 將chr轉factor
invisible(
  lapply( colnames(diamonds)[sapply(diamonds, is.character)],
    function(x) diamonds[,x] <<- factor(diamonds[,x]) )
)
# 個別對price做bivariate analysis
Map(function(x){
  lm_var = lm(eval(parse(text = paste("price ~",x))),data = diamonds)
  
  if(is.factor(diamonds[,x])){
    print(x)
    print(table(diamonds[,x],useNA = "always"))
    print(qplot(diamonds[,x],diamonds[,"price"],geom = "boxplot") + xlab(x) + ylab("price"))
    print(anova(lm_var))
  }else{
    print(x)
    print(qplot(diamonds[,x],diamonds[,"price"],geom = "point") + xlab(x) + ylab("price"))
    print(cor.test(diamonds[,x],diamonds[,"price"],method = "kendall"))
    print(summary(lm_var))
  }
}, setdiff(colnames(diamonds),"price"))

# 3.2 將資料切成70 30
# install.packages("caret")
library(caret)
#資料前處理
set.seed(1)
trainIndex <- sample(seq(nrow(diamonds)),0.7 * nrow(diamonds),replace = F)
train <- diamonds[trainIndex,]
test <- diamonds[-trainIndex,]
#鑽石價格的分佈
qplot(price , geom = "density" , data = train)
qplot(log(price) , geom = "density" , data = test)
# 轉換0~1
preprocess <- preProcess(train[, -7], method = c("range"))
preprocess
train_trans <- predict(preprocess,train)
test_trans <- predict(preprocess,test)

# 3.3 MAE
MAE <- function(predicted ,actual) return(mean(abs(predicted - actual)))

# 3.4 建立線性模式
lm_diamonds <- lm(price ~ carat + cut + color + clarity + depth + table + x + y + z, data = train_trans)
summary(lm_diamonds)
# 預測值
train_pred <- lm_diamonds$fitted.values
test_pred <- predict(lm_diamonds , newdata = test_trans)
# 跑MAE
round(MAE(train_pred , train_trans$price) , 4) # 746.4272
round(MAE(test_pred , test_trans$price) , 4)   # 719.8409

# 3.5 移除p值>0.05的變數(y,z)
lm_diamonds_rm <- lm(price ~ carat + cut + color + clarity + depth + table + x, data = train_trans)
summary(lm_diamonds_rm)
train_pred <- lm_diamonds_rm$fitted.values
test_pred <- predict(lm_diamonds_rm , newdata = test_trans)

# 3.6 考慮所有two wat interactions
lm_diamonds_twoway <- lm(price ~ (.)^2 , data = train_trans)
summary(lm_diamonds_twoway)
#預測
train_pred <- lm_diamonds_twoway$fitted.values
test_pred <- predict(lm_diamonds_twoway , newdata = test_trans)
round(MAE(train_pred , train_trans$price) , 4) # 391.9795
round(MAE(test_pred , test_trans$price) , 4)   # 391.3182
