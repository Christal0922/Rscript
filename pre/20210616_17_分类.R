#数据准备
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="")
breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion","singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")
df <- breast[-1]
df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))
set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df)) #选取70%的样本进行训练
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

#逻辑回归
fit.logit <- glm(class~.,data = df.train,family = binomial()) #拟合逻辑回归，以类别为响应变量，其余变量为预测变量
summary(fit.logit) #检查模型
prob <- predict(fit.logit,df.validate,type = "response") #predict()函数默认输出肿瘤为恶性的对数概率，指定参数type="response”即可得到预测肿瘤为恶性的概率
logit.pred <- factor(prob>0.5,levels = c(FALSE,TRUE),
                     labels = c("benign","malignant"))
#对训练集集外样本单元分类
logit.perf <- table(df.validate$class,logit.pred,dnn=c("Actual","Predicted"))
logit.perf #评估预测准确性
#移除无统计学差异的变量，精简模型
logit.fit.reduced <- step(fit.logit)





