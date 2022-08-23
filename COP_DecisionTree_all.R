
# clean data in memory
rm(list=ls())
gc()

# load data
#rawdata=read.csv("Two_all.csv",header=T)
#rawdata=read.csv("Two_raw.csv",header=T)
#rawdata=read.csv("Two_imf1.csv",header=T)
#rawdata=read.csv("Two_imf2.csv",header=T)
#rawdata=read.csv("Two_imf3.csv",header=T)
#rawdata=read.csv("Two_imf4.csv",header=T)
#rawdata=read.csv("Two_imf5.csv",header=T)
rawdata=read.csv("R_L_abs_all.csv",header=T)


#F_index=c(2,12);  #CF#for single,replace 3¡A3 to...72)
F_index=c(2:72); #C4#for single,replace 3¡A3 to...72)
dim(rawdata)
# data separation
np=52 # 20% testing data#The np value can be changed according to requirements #C4's np value is 177

test.index=sample(1:nrow(rawdata),np)
raw.testdata=rawdata[test.index,] # testing data
raw.traindata=rawdata[-test.index,] # traing data
dim(raw.testdata)
dim(raw.traindata)

# DT by cart
	DTc<-function(rawdata,np,test.index,raw.testdata,raw.traindata)
	# Decision Tree by cart, code example 6-2 #
	{
	require(rpart);
	rawdata.model<- rpart(Type ~.,data=raw.traindata,control=rpart.control(minsplit=5,cp=0.0001,maxdepth=30)); # Decision Tree by cart
	Type.traindata=rawdata$Type[-test.index];
	train.predict=factor(predict(rawdata.model,raw.traindata,type='class',levels=levels(Type.traindata)));
	table.traindata=table(Type.traindata,train.predict);
	TP=table.traindata[1,1];
	FN=table.traindata[1,2];
	FP=table.traindata[2,1];
	TN=table.traindata[2,2];
	acc.traindata=(TP+TN)/(TP+FN+TN+FP)*100; # accuracy
	sen.traindata=TP/(TP+FN)*100; # sensitivity
	spe.traindata=TN/(TN+FP)*100; # specificity

	Type.testdata=rawdata$Type[test.index];
	test.predict=factor(predict(rawdata.model,raw.testdata,type='class',levels=levels(Type.testdata)));
	table.testdata=table(Type.testdata,test.predict);
	TP=table.testdata[1,1];
	FN=table.testdata[1,2];
	FP=table.testdata[2,1];
	TN=table.testdata[2,2];
	acc.testdata=(TP+TN)/(TP+FN+TN+FP)*100; # accuracy
	sen.testdata=TP/(TP+FN)*100; # sensitivity
	spe.testdata=TN/(TN+FP)*100; # specificity
	
	result=c(acc.traindata,sen.traindata,spe.traindata,acc.testdata,sen.testdata,spe.testdata);
	return(result)
	}

# SVM
	SVMc<-function(rawdata,np,test.index,raw.testdata,raw.traindata)
	# SVM #
	{
	require(e1071)

tune.model = tune(svm,Type~.,data=raw.traindata,kernel="radial", # RBF kernel function
                  range=list(cost=10^(-1:2), gamma=c(.5,1,2))# The most important line of the tuning parameter
			)
	#summary(tune.model)
	tune.model$best.model

 	rawdata.model<- svm(formula=Type ~.,data=raw.traindata, kernel = "radial", gamma = tune.model$best.model$gamma, cost = tune.model$best.model$cost); # SVM
	
#     rawdata.model<- svm(formula=Type ~.,data=raw.traindata); # SVM
train.pred = predict(rawdata.model, raw.traindata);
	test.pred = predict(rawdata.model, raw.testdata);
	table.traindata=table(real=raw.traindata$Type, predict=train.pred)
	TP=table.traindata[1,1];
	FN=table.traindata[1,2];
	FP=table.traindata[2,1];
	TN=table.traindata[2,2];
	acc.traindata=(TP+TN)/(TP+FN+TN+FP)*100; # accuracy
	sen.traindata=TP/(TP+FN)*100; # sensitivity
	spe.traindata=TN/(TN+FP)*100; # specificity

	table.testdata=table(real=raw.testdata$Type, predict=test.pred)
	TP=table.testdata[1,1];
	FN=table.testdata[1,2];
	FP=table.testdata[2,1];
	TN=table.testdata[2,2];
	acc.testdata=(TP+TN)/(TP+FN+TN+FP)*100; # accuracy
	sen.testdata=TP/(TP+FN)*100; # sensitivity
	spe.testdata=TN/(TN+FP)*100; # specificity
	
	result=c(acc.traindata,sen.traindata,spe.traindata,acc.testdata,sen.testdata,spe.testdata);
	return(result)
	}

##

np=105 # 20%

	Result_all=matrix(0,100,6)
	for (i in 1:100)#See how many times it needs to run, this time it¡¦s 20 runs

	{
	print(i)
	test.index=sample(1:nrow(rawdata),np)
	raw.testdata=rawdata[test.index,] # testing data
	raw.traindata=rawdata[-test.index,] # traing data
	result=DTc(rawdata,np,test.index,raw.testdata,raw.traindata)
	#result=SVMc(rawdata,np,test.index,raw.testdata,raw.traindata)
	for (k in 1:6)
	{
	Result_all[i,k]=result[k]
	}
	}
	acc.train.all=mean(Result_all[,1]);
	sen.train.all=mean(Result_all[,2]);
	spe.train.all=mean(Result_all[,3]);
	acc.test.all=mean(Result_all[,4]);
	sen.test.all=mean(Result_all[,5]);
	spe.test.all=mean(Result_all[,6]);

      acc.train.sd=sd(Result_all[,1]);
	sen.train.sd=sd(Result_all[,2]);
	spe.train.sd=sd(Result_all[,3]);
	acc.test.sd=sd(Result_all[,4]);
	sen.test.sd=sd(Result_all[,5]);
	spe.test.sd=sd(Result_all[,6]);  


    np52=c(acc.train.all,sen.train.all,spe.train.all,acc.test.all,sen.test.all,spe.test.all);
print(np52)
    np52.sd=c(acc.train.sd,sen.train.sd,spe.train.sd,acc.test.sd,sen.test.sd,spe.test.sd);
print(np52.sd) 



 write.table(Result_all,file="R-L_abs_all_result.csv",sep=",",col.names=FALSE,row.names=FALSE,quote=TRUE) # edit files names#Just change cf_6
