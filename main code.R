# multiple linear regression

#canopy or understory

#1 read csv
Melt_gather <- read.csv('~\\Soil_Miosture_mean_sd_Canopy_Understory_GCC_2017-2021_Year_Plot.csv',sep=",",header = T)#####
x=Melt_gather$Soil_Moisture
y=Melt_gather$Soil_Moisture_SD
z=Melt_gather$Canopy.GCC


#2 fit relationship
nl = lm(z~x+I(x^2)+y+I(y^2))
summary(nl) 
n1summary = rbind(summary(nl)[[4]])
nl_1<-function (x,y){ #GPP
  z=(n1summary[1,1]+n1summary[2,1]*x+n1summary[3,1]*I(x ^ 2)+n1summary[4,1]*y+n1summary[5,1]*I(y^ 2))
  return(z)
}

#3 simulation and Interpolation
N = 100
min(x);max(x)
min(y);max(y)
xmin = 5; xmax =45 # SM
ymin = -2; ymax =12 # SUMSM
xMAR2<-seq(xmin,xmax,(xmax-xmin)/N)
yMAR2<-seq(ymin,ymax,(ymax-ymin)/N)
for(i in 1:length(xMAR2)){
  simXY<-data.frame(xmar=xMAR2,ymar=rep(yMAR2[i],length(xMAR2)))
  write.table(simXY,'Cache XY.csv',sep=',',col.names=FALSE,row.names=FALSE,append=TRUE)
}
XY<-read.table('Cache XY.csv',sep=',',header=FALSE)
file.remove('Cache XY.csv')
xmar<-XY[,1]
ymar<-XY[,2]
zmar<-rep(NA,length(xmar))
simXYZ<-data.frame(x=xmar,y=ymar,z=zmar)
for (i in 1:length(xmar)){
  simXYZ[i,3]<-nl_1(xmar[i],ymar[i])
}
write.csv(simXYZ,'Cache XYZ.csv')
simXYZ_optimum=subset(simXYZ,simXYZ$z==max(simXYZ$z))


# structural equation model
library(lavaan)

#1 read csv
Melt_gather<- read.csv('~\\Soil_Miosture_mean_sd_Canopy_Understory_GCC.csv',sep=",",header = T)

Melt_gather$SM=scale(Melt_gather$SM,center = T,scale = T)[,1]
Melt_gather$Pre_season_SM=scale(Melt_gather$Pre_season_SM,center = T,scale = T)[,1]
Melt_gather$SMSD=scale(Melt_gather$SMSD,center = T,scale = T)[,1]
Melt_gather$Pre_season_SMSD=scale(Melt_gather$Pre_season_SMSD,center = T,scale = T)[,1]
Melt_gather$Overstory_Gcc=scale(Melt_gather$Overstory_Gcc,center = T,scale = T)[,1]
Melt_gather$Understory_Gcc=scale(Melt_gather$Understory_Gcc,center = T,scale = T)[,1]

#2 Build relationships
Full.model="SM~Treatment
            SMSD~Treatment
            Pre_season_SM~Treatment
            Pre_season_SMSD~Treatment
            Overstory_GCC~SM+SMSD+Pre_season_SM+Pre_season_SMSD
            Understory_GCC~SMSD+SM+Pre_season_SM+Pre_season_SMSD"
sem.t=sem(Full.model,data=Melt_gather,check.gradient = FALSE)

# output result
sem.p<-function(Sem,layout="tree3")
  {
  require(semPlot)
  semPaths(Sem,what="std",layout=layout,residual=F,nCharNodes = 0,posCol="black",negCol="red",edge.label.position=0.5,sizeMan = 5,sizeLat = 10,sizeMan2 = 5,sizeLat2 = 10,colFactor=0.5,edge.label.cex = 1.5,edge.label.bg=T,label.cex=2)
  print(fitMeasures(Sem,c("chisq","df","pvalue","cfi","nfi","ifi","rmsea","EVCI")))
}
sem.p(sem.t,"tree2")
summary(sem.t)

