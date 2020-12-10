#2020/11/27(五), 109學年第一學期 資料科學應用 exam
#學號: a107260086       姓名: 張允銓
library(readxl)
# 1(a)
Data <- read.csv("Calculus-score-A.csv", header = TRUE, skip = 2)
xlsx <- "Calculus-score-B.xls"
excel_sheets(xlsx)
Data1 <- read_excel(xlsx, sheet = "工作表1", na = "NA", skip = 2)
Data[c(1:5, 36:40), ]
as.data.frame(head(Data1, 5))
as.data.frame(tail(Data1, 5)) 

# 1(b)
Data2 <- as.data.frame(Data1)
names(Data)[1:12] <- c("座號", "學號", "姓名", "性別", "quiz.1.", "quiz.2.", "quiz.3.", "quiz.4.", "TA", "MidtermExam", "FinalExam", "Attendance") #change variable name
names(Data2)[1:12] <- c("座號", "學號", "姓名", "性別", "quiz.1.", "quiz.2.", "quiz.3.", "quiz.4.", "TA", "MidtermExam", "FinalExam", "Attendance") #change variable name
DataA <- transform(Data,class = "A") # 增加列
DataB <- transform(Data2,class = "B") # 增加列
names(Data2) == names(Data) #ensure names are the same
score <- rbind(DataA, DataB) #rbind two data frames.
score[38:43,]

# 1(c)
score[is.na(score)] <- 0 # 使用is.na（）將NA替換為0
Q <- score[5]*0.07 + score[6]*0.07 + score[7]*0.08 + score[8]*0.08 + score[9]*0.15 + score[10]*0.25 + score[11]*0.30 + score[12]
E <- c(Q[1:95,])
F <- ifelse(E >= 100, 100, E)
F1 <- as.data.frame(F)
names(F1)[1] <- c("學期成績")
F1

# 1(d)
w <- ifelse(60 > F &　F >= 50, E, (sep="0"))
w1 <- as.data.frame(w)
P <- which(w1 > 0) #找某元素在向量中的下標，可以用函數which實現
score[P,]

# 1(e)
A <- which(score[,13] == "A")
B <- which(score[,13] == "B")
# A班總成績平均各為多少
sum(F1[A,]) / length(A)
# B班總成績平均各為多少
sum(F1[B,]) / length(B)
A1 <- which(score[,4] == "女")
B1 <- which(score[,4] == "男")
# 女生總成績平均各為多少
sum(F1[A1,]) / length(A1)
# 男生總成績平均各為多少
sum(F1[B1,]) / length(B1)

# 1(f)
A2 <- ifelse(60 > F &　score[,13] == "A", E, (sep="0"))
A3 <- as.data.frame(A2)
A4 <- which(A3 > 0)
# A 班學期成績不及格比例為多少? 
length(A4) / length(A)
B2 <- ifelse(60 > F & score[,13] == "B" & score[,4] == "男", E, (sep="0"))
B3 <- as.data.frame(B2)
B4 <- which(B3 > 0)
# B 班男同學學期成績不及格比例為多少?
length(B4) / length(B)

# 1(g)
score1 <- transform(score,score = F1)
names(score1)[14] <- c("score")
SK <- score1[A1,]
ST <- score1[B1,]
SK1 <- order(SK$score, decreasing = TRUE)
ST1 <- order(ST$score, decreasing = TRUE)
SK2 <- SK[SK1,]
ST2 <- ST[ST1,]
head(SK2, 5)
head(ST2, 5)

# 2(a)
set.seed <- c(123456)
y <- c(sample(LETTERS[1:5], 20, replace=T))
x <-c()
for(i in 1:20){
  if(y[i] == "A")
    x[i] <- 1
  else if(y[i] == "E")
    x[i] <- 1
  else if(y[i] == "C")
    x[i] <- 2
  else
    x[i] <- 3
}
cat(x)

# 2(b)
LN <- data.frame(Letters.code = y, Numbers.code = x)
LN
