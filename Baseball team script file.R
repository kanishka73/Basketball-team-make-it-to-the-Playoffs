getwd()
NBA=read.csv("NBA_train.csv")
str(NBA)
table(NBA$W,NBA$Playoffs)
NBA$PTSdiff=NBA$PTS-NBA$oppPTS
plot(NBA$PTSdiff,NBA$W)
WinsReg=lm(W~PTSdiff,data=NBA)
summary(WinsReg)
PointsReg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data=NBA)
summary(PointsReg)
SSE=sum(PointsReg$residual^2)
RMSE=sqrt(SSE/nrow(NBA))
RMSE
PointsReg2=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL,data=NBA)
summary(PointsReg2)
SSE2=sum(PointsReg2$residual^2)
RMSE2=sqrt(SSE2/nrow(NBA))
RMSE2
getwd()
NBA_test=read.csv("NBA_test.csv")
PointsPredictions=predict(PointsReg2,newdata=NBA_test)
SSE=sum((PointsPredictions-NBA_test$PTS)^2)
SSE
SST=sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2=1-SSE/SST
R2
RMSE=sqrt(SSE/nrow(NBA_test))
RMSE



