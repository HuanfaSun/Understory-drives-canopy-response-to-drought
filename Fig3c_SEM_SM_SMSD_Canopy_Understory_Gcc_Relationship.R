library(lavaan)
Melt_gather<- read.csv('G:\\My File\\Manuscript\\2022_07_understory_drives_canopy_response_to_drought\\2_Excel\\Soil_Miosture_mean_sd_Canopy_Understory_Gcc_Two_Season_test_2022.07.03.csv',sep=",",header = T)#####
str(Melt_gather)
Melt_gather$SM=scale(Melt_gather$SM,center = T,scale = T)[,1]
Melt_gather$Pre_season_SM=scale(Melt_gather$Pre_season_SM,center = T,scale = T)[,1]
Melt_gather$SMSD=scale(Melt_gather$SMSD,center = T,scale = T)[,1]
Melt_gather$Pre_season_SMSD=scale(Melt_gather$Pre_season_SMSD,center = T,scale = T)[,1]
Melt_gather$Overstory_Gcc=scale(Melt_gather$Overstory_Gcc,center = T,scale = T)[,1]
Melt_gather$Understory_Gcc=scale(Melt_gather$Understory_Gcc,center = T,scale = T)[,1]

#ALL
Full.model="SM~Treatment
            SMSD~Treatment
            Pre_season_SM~Treatment
            Pre_season_SMSD~Treatment
            Overstory_Gcc~SM+SMSD+Pre_season_SM+Pre_season_SMSD
            Understory_Gcc~SMSD+SM+Pre_season_SM+Pre_season_SMSD"
sem.t=sem(Full.model,data=Melt_gather,check.gradient = FALSE)
sem.p<-function(Sem,layout="tree3"){
  require(semPlot)
  semPaths(Sem,what="std",layout=layout,residual=F,nCharNodes = 0,posCol="black",negCol="red",edge.label.position=0.5,
           sizeMan = 5,sizeLat = 10,sizeMan2 = 5,sizeLat2 = 10,colFactor=0.5,edge.label.cex = 1.5,edge.label.bg=T,label.cex=2)
  print(fitMeasures(Sem,c("chisq","df","pvalue","cfi","nfi","ifi","rmsea","EVCI")))
}
sem.p(sem.t,"tree2")
summary(sem.t)

#Dry season
Melt_gather_D=subset(Melt_gather,Melt_gather$Two_Season1=='Dry')
Full.model="SM~Treatment
            SMSD~Treatment
            PSM~Treatment
            PSMSD~Treatment
            Overstory_Gcc~SM+SMSD+PSM+PSMSD
            Understory_Gcc~SMSD+SM+PSM+PSMSD"
sem.t=sem(Full.model,data=Melt_gather_D,check.gradient = FALSE)
sem.p(sem.t,"tree2")
summary(sem.t)

#Wet season
Melt_gather_W=subset(Melt_gather,Melt_gather$Two_Season1=='Wet')
Full.model="SM~Treatment
            SMSD~Treatment
            PSM~Treatment
            PSMSD~Treatment
            Overstory_Gcc~SM+SMSD+PSM+PSMSD
            Understory_Gcc~SMSD+SM+PSM+PSMSD"
sem.t=sem(Full.model,data=Melt_gather_W,check.gradient = FALSE)
sem.p(sem.t,"tree2")
summary(sem.t)




###SEM Plot
sem.p<-function(Sem,layout="tree"){
  require(semPlot)
  semPaths(Sem,
           what="std",
           whatLabels='std',
           style='lisrel',
           layout=layout,residual=F,nCharNodes = 0,posCol="black",negCol="red",edge.label.position=0.5,
           sizeMan = 5,sizeLat = 10,sizeMan2 = 5,sizeLat2 = 10,colFactor=0.5,edge.label.cex = 1.5,edge.label.bg=T,label.cex=2)
  print(fitMeasures(Sem,c("chisq","df","pvalue","cfi","nfi","ifi","rmsea","EVCI")))
}
sem.p(sem.t)
