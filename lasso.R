rm(list=ls())
options(stringsAsFactors = F)
load(file = 'dat_updown_wgcna.Rdata')
load(file = 'step1-output.Rdata')

phe$event = phe$genotype
phe$event=ifelse(grepl('Healthy',phe$event),'0','1')
dat_deg = read.csv(file = '123.csv',row.names = 1)
#exprSet = as.data.frame(t(dat_updown_wgcna)) 
exprSet = as.data.frame(t( dat_deg ))        
phe$ID = rownames(phe)
## 必须保证生存资料和表达矩阵，两者一致
all(substring(colnames(exprSet),1,12)==phe$ID)


library(lars) 
library(glmnet) 
x=t(exprSet)
head(x)
y=phe$event
y = as.numeric(y)


# 111111111111111111 ------------------------------------------------------



model_lasso <- glmnet(x, y, family="binomial", nlambda=50, alpha=1)
print(model_lasso)
# 列%Dev代表了由模型解释的残差的比例，对于线性模型来说就是模型拟合的R^2(R-squred)。
# 它在0和1之间，越接近1说明模型的表现越好，
# 如果是0，说明模型的预测结果还不如直接把因变量的均值作为预测值来的有效。
head(coef(model_lasso, s=c(model_lasso$lambda[29],0.009)))
plot(model_lasso, xvar = "norm", label = TRUE)
plot(model_lasso, xvar="lambda", label=TRUE)
dev.off()
cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000)
plot(cv_fit)
# 两条虚线分别指示了两个特殊的λ值:
c(cv_fit$lambda.min,cv_fit$lambda.1se) 

model_lasso <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
lasso.prob <- predict(cv_fit, newx=x , s=c(cv_fit$lambda.min,cv_fit$lambda.1se) )
re=cbind(y ,lasso.prob)
dat=as.data.frame(re[,1:2])
colnames(dat)=c('event','prob')
dat$event=as.factor(dat$event)
library(ggpubr) 
p <- ggboxplot(dat, x = "event", y = "prob",
               color = "event", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means()

library(ROCR)
library(glmnet)
library(caret)
# calculate probabilities for TPR/FPR for predictions
pred <- prediction(re[,2], re[,1])
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
head(fit$beta)
#一倍SE内的更简洁的模型,是22个miRNA
#fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
#head(fit$beta)# 这里是40个miRNA
choose_gene=rownames(fit$beta)[as.numeric(fit$beta)!=0]

write.csv(choose_gene,file = "lasso_choose_datdeg.csv ")
read.csv(file = 'lasso_choose_datdeg.csv',row.names = 1)


write.csv(choose_gene,file = "lasso_choose_gene.csv ")
read.csv(file = 'lasso_choose_gene.csv',row.names = 1)



length(choose_gene)
myexpr=x[,choose_gene]

venn_list <- list(group1 = rownames(a), choose_gene)
venn.diagram(venn_list, filename = 'venn4.png', imagetype = 'png', 
             fill = c('red', 'blue'), alpha = 0.50, 
             cat.col = c('red', 'blue'), cat.cex = 1.5, cat.fontfamily = 'serif',
             col = c('red', 'blue'), cex = 1.5, fontfamily = 'serif')

inter <- get.venn.partitions(venn_list)
for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
write.table(inter[-c(5, 6)], 'venn4_inter.txt', row.names = FALSE, sep = '\t', quote = FALSE)


mysurv=phe[,c("days","event")]
mysurv$days[mysurv$days< 1] = 1 
# 详细代码参见这个网站https://github.com/jeffwong/glmnet/blob/master/R/coxnet.R#
fit <- glmnet( myexpr, Surv(mysurv$days,mysurv$event), 
               family = "cox") 
#用包自带的函数画图
plot(fit, xvar="lambda", label = TRUE)
plot(fit, label = TRUE)
## 如果需要打印基因名，需要修改函数，这里不展开。

library(pheatmap) 
choose_matrix=expr[choose_gene,]
choose_matrix[1:4,1:4]
n=t(scale(t(log2(choose_matrix+1))))  #scale()函数去中心化和标准化
#对每个探针的表达量进行去中心化和标准化
n[n>2]=2 #矩阵n中归一化后，大于2的项，赋值使之等于2（相当于设置了一个上限）
n[n< -2]= -2 #小于-2的项，赋值使之等于-2（相当于设置了一个下限）
n[1:4,1:4]

## http://www.bio-info-trainee.com/1980.html
annotation_col = data.frame( group_list=group_list  )
rownames(annotation_col)=colnames(expr)

pheatmap(n,show_colnames = F,annotation_col = annotation_col,
         filename = 'lasso_genes.heatmap.png')

library(ggfortify)
df=as.data.frame(t(choose_matrix))
df$group=group_list
png('lasso_genes.pca.png',res=120)
autoplot(prcomp( df[,1:(ncol(df)-1)] ), data=df,colour = 'group')+theme_bw()
dev.off()

## 也可以尝试其它主成分分析的R包，视频就不继续没完没了的讲解了。


library("FactoMineR")
library("factoextra")  
## 这里的PCA分析，被该R包包装成一个简单的函数，复杂的原理后面讲解。
dat.pca <- PCA(t(choose_matrix), graph = FALSE) #'-'表示“非”
fviz_pca_ind(dat.pca,repel =T,
             geom.ind = "point", # show points only (nbut not "text")只显示点不显示文本
             col.ind =  group_list, # color by groups 颜色组
             # palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses 集中成椭圆
             legend.title = "Groups"
)
