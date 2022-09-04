Sys.setlocale("LC_TIME", "English")
require("RColorBrewer")
display.brewer.all()
brewer.pal(9, "Set1")
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(ggpubr)
library(png)
library(reshape2)
library(car)
library(corrplot)

#######################################Canopy#################################
Melt_gather<- read.csv('G:\\My File\\Manuscript\\2022_07_understory_drives_canopy_response_to_drought\\2_Excel\\Soil_Moisture_5cm_Mean_SD_Canopy_Understory_Gcc_2017-2021_Year_Plot.csv',sep=",",header = T)#####
str(Melt_gather)


#dry season overstory
#Melt_gather=subset(Melt_gather,Melt_gather$Two_Season1=='Dry season')
x=Melt_gather$Soil_Moisture
y=Melt_gather$Soil_Moisture_SD
z=Melt_gather$Canopytory.Gcc


#拟合关系
nl = lm(z~x+I(x^2)+y+I(y^2))
summary(nl) 
n1summary = rbind(summary(nl)[[4]])

nl_1<-function (x,y){ #GPP
  z=(n1summary[1,1]+n1summary[2,1]*x+n1summary[3,1]*I(x ^ 2)+n1summary[4,1]*y+n1summary[5,1]*I(y^ 2))
  return(z)
}

#插值加模拟
N = 100
# xmax = ceiling(max(x)); xmin = floor(min(x))
# ymax = ceiling(max(y)); ymin = floor(min(y))
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

#画图
colormap<-colorRampPalette(brewer.pal(11,'RdYlGn'))(100)
colormap<-colorRampPalette(c("#A50026","#D73027","#FDAE61","#A6D96A","#66BD63","#1A9850","#006837"))(100)
max_z<-max(simXYZ$z,na.rm=TRUE);# max_z = ceiling(max_z)
min_z<-min(simXYZ$z,na.rm=TRUE);# min_z = floor(min_z)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/10)

#曲面图
P5<-ggplot()+
  geom_raster(data=simXYZ,aes(y=y,x=x,fill=z),interpolate=TRUE)+#根据高度填充
  scale_fill_gradientn(colours=colormap,limits=c(0.349, 0.451),n.breaks=3,labels = scales::comma_format(0.01))+
  geom_contour(data=simXYZ,aes(y=y,x=x,z=z),breaks=breaks_lines,color = 'dark grey')+ #contour
  geom_point(data = Melt_gather,aes(Soil_Moisture,Soil_Moisture_SD,size=Canopytory.Gcc,color=Treatment),shape=16,alpha=1)+#,group=Treatment,fill=Treatment
  #scale_size_area() + # set GPP as the area instead of radius
  scale_color_manual(values=c("#377EB8","#FF7F00"))+
  scale_size_continuous(range=c(3,8))+
  labs(x="Soil water availability (vol %)",y="Soil water variability",fill= 'Canopytory GCC')+
  scale_y_continuous(breaks=seq(0, 10, 2),limits=c(0, 10),expand = c(0,0))+#,labels = scales::comma_format(0.001)
  scale_x_continuous(breaks=seq(10, 40, 5),limits=c(10, 40),expand = c(0,0))+
  #ggtitle("A Canopy")+
  theme_bw()+
  theme(plot.margin = margin(t = 0.5, r = 0.5,b = 0.5,l = 0.5, unit = "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = 'transparent',color="black",linetype="solid"),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.line = element_line(linetype = "solid",size = 0.5),
        axis.ticks = element_line(colour = "black",size = 0.5),
        axis.ticks.length=unit(-0.5,"lines"),
        axis.title = element_text(size = 20, vjust = 0),
        axis.text = element_text(size = 18,color = "black", hjust = 1),
        axis.text.y = element_text(margin = unit(c(0.6, 0.6, 0.6, 0), 'cm')),
        axis.text.x = element_text(margin = unit(c(0.6, 0.6, 0, 0.6), 'cm'),hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.box.background = element_rect(fill = "#FFFFFF",colour = NA),
        legend.position ="right",
        legend.direction ="vertical",
        plot.title = element_text(hjust = 0,size = 20))#+#legend.position =c(0.85,0.2))
#annotate("text", x = 10+0.05*(40-10), y =10-0.05*(10-0), label = "bold(A)",size = 8,parse = TRUE)
P5=P5 + guides(color = guide_legend(order = 1, override.aes = list(size=6)),
               shape = guide_legend(order = 2),
               size = 'none',
               fill=guide_colorbar(barheight = unit(2.5,'cm')))
P5
#####################################Canopy###################################

########################################Understory#################################
Melt_gather<- read.csv('G:\\My File\\Manuscript\\2022_07_understory_drives_canopy_response_to_drought\\2_Excel\\Soil_Moisture_5cm_Mean_SD_Canopy_Understory_Gcc_2017-2021_Year_Plot.csv',sep=",",header = T)#####
str(Melt_gather)

#dry season overstory
#Melt_gather=subset(Melt_gather,Melt_gather$Two_Season1=='Dry season')
x=Melt_gather$Soil_Moisture
y=Melt_gather$Soil_Moisture_SD
z=Melt_gather$Understory.Gcc


#拟合关系
nl = lm(z~x+I(x^2)+y+I(y^2))
summary(nl) 
n1summary = rbind(summary(nl)[[4]])

nl_1<-function (x,y){ #GPP
  z=(n1summary[1,1]+n1summary[2,1]*x+n1summary[3,1]*I(x ^ 2)+n1summary[4,1]*y+n1summary[5,1]*I(y^ 2))
  return(z)
}

#插值加模拟
N = 100
# xmax = ceiling(max(x)); xmin = floor(min(x))
# ymax = ceiling(max(y)); ymin = floor(min(y))
min(x);max(x)
min(y);max(y)
xmin = 5; xmax =45 # SM
ymin = -2; ymax =12# SUMSM

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

#画图
colormap<-colorRampPalette(brewer.pal(11,'RdYlGn'))(100)
colormap<-colorRampPalette(c("#A50026","#D73027","#FDAE61","#A6D96A","#66BD63","#1A9850","#006837"))(100)
max_z<-max(simXYZ$z,na.rm=TRUE);# max_z = ceiling(max_z)
min_z<-min(simXYZ$z,na.rm=TRUE);# min_z = floor(min_z)
breaks_lines<-seq(min_z,max_z,by=(max_z-min_z)/10)

#曲面图
P6<-ggplot()+
  geom_raster(data=simXYZ,aes(y=y,x=x,fill=z),interpolate=TRUE)+#根据高度填充
  scale_fill_gradientn(colours=colormap,limits=c(0.329, 0.351),n.breaks=3)+
  geom_contour(data=simXYZ,aes(y=y,x=x,z=z),breaks=breaks_lines,color = 'dark grey')+ 
  geom_point(data = Melt_gather,aes(Soil_Moisture,Soil_Moisture_SD,size=Understory.Gcc,color=Treatment),shape=16,alpha=0.9)+#,group=Treatment,fill=Treatment
  #scale_size_area() + # set GPP as the area instead of radius
  scale_size_continuous(range=c(3,8))+
  scale_color_manual(values=c("#377EB8","#FF7F00"))+#contour
  labs(x="Soil water availability (vol %)",y="Soil water variability",fill= 'Understory GCC')+
  scale_y_continuous(breaks=seq(0, 10, 2),limits=c(0, 10),expand = c(0,0))+#,labels = scales::comma_format(0.001)
  scale_x_continuous(breaks=seq(10, 40, 5),limits=c(10, 40),expand = c(0,0))+
  #ggtitle("A Understory")+
  theme_bw()+
  theme(plot.margin = margin(t = 0.5, r = 0.5,b = 0.5,l = 0.5, unit = "cm"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = 'transparent',color="black",linetype="solid"),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.line = element_line(linetype = "solid",size = 0.5),
        axis.ticks = element_line(colour = "black",size = 0.5),
        axis.ticks.length=unit(-0.5,"lines"),
        axis.title = element_text(size = 20, vjust = 0),
        axis.text = element_text(size = 18,color = "black", hjust = 1),
        axis.text.y = element_text(margin = unit(c(0.6, 0.6, 0.6, 0), 'cm')),
        axis.text.x = element_text(margin = unit(c(0.6, 0.6, 0, 0.6), 'cm'),hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.box.background = element_rect(fill = "#FFFFFF",colour = NA),
        legend.position ="right",
        legend.direction ="vertical",
        plot.title = element_text(hjust = 0,size = 20))#+#legend.position =c(0.85,0.2))
#annotate("text", x = 10+0.05*(40-10), y =10-0.05*(10-0), label = "bold(B)",size = 8,parse = TRUE)
P6=P6+ guides(color = guide_legend(order = 1, override.aes = list(size=6)),
              shape = guide_legend(order = 2),
              size = 'none',
              fill=guide_colorbar(barheight = unit(2.5,'cm')))
P6
######################################Understory###################################

ggarrange(P5,P6,ncol = 1, nrow = 2)
ggsave(paste('G:\\My File\\Manuscript\\2022_07_understory_drives_canopy_response_to_drought\\3_Figure\\Fig3_surface_Gcc_SM_SMSD_2022.07.31.pdf',sep=''),width=14*0.75,height=7*0.75*2)#####,bg ="transparent"

