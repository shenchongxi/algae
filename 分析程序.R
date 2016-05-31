#初始化
library(DMwR)
library(car)

#读取数据文件
algae<-read.table("C:/Users/chongxi/Desktop/Analysis.txt",,col.name=c('season','riverSize','  waterSpeed','maxPH','minO2','avrCL','avrNO3','avrNH4','avrPO43','avrPO4','avrYLS','a1','a2','a3','a4','a5','a6','a7'),na.strings=c('XXXXXXX'))
#摘要
#summary(algae)

#直方图
algaeHistogram<-function()
{
	par(mfrow=c(2,4))
	hist(algae$maxPH,probability=T) 
	hist(algae$minO2,probability=T) 
	hist(algae$avrCL,probability=T) 
	hist(algae$avrNO3,probability=T) 
	hist(algae$avrNH4,probability=T) 
	hist(algae$avrPO43,probability=T) 
	hist(algae$avrPO4,probability=T) 
	hist(algae$avrYLS,probability=T)  
}

#qq图验证正态分布
algaeQQplot<-function()
{
	par(mfrow=c(2,4))
	qqPlot(algae$maxPH, main='QQplot of maxPH') 
	qqPlot(algae$minO2, main='QQplot of minO2') 
	qqPlot(algae$avrCL, main='QQplot of avrCL') 
	qqPlot(algae$avrNO3, main='QQplot of avrNO3') 
	qqPlot(algae$avrNH4, main='QQplot of avrNH4') 
	qqPlot(algae$avrPO43, main='QQplot of avrPO43') 
	qqPlot(algae$avrPO4, main='QQplot of avrPO4') 
	qqPlot(algae$avrYLS, main='QQplot of avrYLS') 
}

#单独的盒图
algaeBoxplot<-function()
{
	par(mfrow=c(2,4))
	boxplot(algae$maxPH,ylab="maxPH")
	boxplot(algae$minO2,ylab="avrPO4")
	boxplot(algae$avrCL,ylab="avrCL")
	boxplot(algae$avrNO3,ylab="avrNO3")
	boxplot(algae$avrNH4,ylab="avrNH4")
	boxplot(algae$avrPO43,ylab="avrPO43")
	boxplot(algae$avrPO4,ylab="avrPO4")
	boxplot(algae$avrYLS,ylab="avrYLS")
}

#条件盒图
algaeBwplot<-function(arg1,arg2)
{
	bwplot(arg1~arg2,data=algae,ylab = 'Y',xlab='X'	)
}

#将缺失部分剔除
#algae[!complete.cases(algae),]
#nrow(algae[!complete.cases(algae),])
#algae <- na.omit(algae) 

#用最高频率值来填补缺失值
#table(a)
#max(table(a))
#algae[is.na(algae$minO2),'minO2'] <- 9.8

#通过属性的相关关系来填补缺失
#options(digits=1)
#cor(algae[,4:18],use="complete.obs")
#symnum(cor(algae[,4:18],use="complete.obs"))
#data(algae)
#algae<-algae[-manyNAs(algae),]
#lm(avrPO43~avrPO4,data=algae)

#通过数据对象之间的相似性来填补缺失值
#algae<-algae[-manyNAs(algae),]
#algae<-knnImputation(algae,k=10)
#algae<-knnImputation(algae,k=10,meth="median")