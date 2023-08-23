
rm(list = ls())  ## 魔幻操作，一键清空~
options(stringsAsFactors = F)#在调用as.data.frame的时，将stringsAsFactors设置为FALSE可以避免character类型自动转化为factor类型

load(file = 'step1-output.Rdata')
phe$event = phe$genotype
phe$event=ifelse(grepl('Healthy',phe$event),'0','1')
dat_deg2 =  read.csv(file = '123.csv',row.names = 1)
class(dat_deg2)
str(dat_deg2)

library(mlbench)
library(caret)
#BiocManager::install("mlbench")
# 加载数据集
PimaIndiansDiabetes = dat_deg2 
#PimaIndiansDiabetes = as.vector(PimaIndiansDiabetes)
PimaIndiansDiabetes$event = phe$event
class(PimaIndiansDiabetes)
control <- rfeControl(functions = caretFuncs, method = "cv", number = 5)

# 执行SVM-RFE算法
PimaIndiansDiabetes <- data.frame(PimaIndiansDiabetes)
PimaIndiansDiabetes = as.numeric(PimaIndiansDiabetes)
y = as.vector(PimaIndiansDiabetes$event)
str(PimaIndiansDiabetes)
PimaIndiansDiabetes <- as.data.frame(PimaIndiansDiabetes, stringsAsFactors = TRUE)
tmp = as.vector(y) 
a = as.factor(PimaIndiansDiabetes[,117])
a = as.numeric(PimaIndiansDiabetes[,117])
a = as.vector(a)  
PimaIndiansDiabetes  = dat_deg2
results <- rfe(PimaIndiansDiabetes[,1:116], 
               a, 
               sizes = c(1:116), 
               rfeControl = control,
               method = "svmRadial")
save(results,file = 'results')
load(file = 'results')
class(PimaIndiansDiabetes[,1:116])
# 结果分析
print(results)
# 列出选择的变量集
predictors(results)
# 绘制结果
plot(results, type=c("g", "o"))

dev.off()

