library(openxlsx)
library(ggplot2)
library(ggmap)
library(grid)
library(maptools)
library(maps)
library(dplyr)
library(sf)##read shp data type
library(cowplot)
register_google('AIzaSyCSQ3pL-qCRGEM4IMZcQZTn85cjL0n5IL4')
d<-read.xlsx("C:/Users/hp/Desktop/SDS/session/enteric_virus_data.xlsx",sheet = 1)
attach(d)


###2.1
##cut age into several stages
d$Age_cut[which(d$Age<=5)]<-'Baby'
d$Age_cut[which(5<d$Age&d$Age<=14)]<-'Children'
d$Age_cut[which(14<d$Age&d$Age<=26)]<-'Youth'
d$Age_cut[which(26<d$Age&d$Age<=50)]<-'Adults'
d$Age_cut[which(50<d$Age)]<-'Olds'
d$Age_cut<-factor(d$Age_cut,levels = c('Baby','Children','Youth','Adults','Olds'))
d$Gender<-as.factor(d$Gender)


##plot gender pie chart
comb00<-d%>%count(Gender)
label00<-paste(comb00$Gender,paste('(',round(comb00$n/sum(comb00$n)*100,1),'%)',sep=''),sep=' ')
ggplot(comb00,aes(x="",y=n,fill=Gender))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.7)+
  coord_polar(theta = "y") +    ##Coordinate transformation
  labs(x = "", y = "", title = "") +    ## set lables
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.3,.98),legend.background = element_blank(),legend.text = element_text(size = 13),legend.key.size = unit(16, "pt"),legend.title = element_text(size=16)) +  
  scale_fill_discrete('The percentage of patients under gender',breaks = comb00$Gender, labels = label00) +## 将原来的图例标签换成现在的myLabel
  theme(axis.text.x = element_blank()) 


##plot age pie chart
comb0<-d%>%count(Age_cut)
label0<-paste(comb0$Age_cut,paste('(',round(comb0$n/sum(comb0$n)*100,1),'%)',sep=''),sep=' ')
ggplot(comb0,aes(x="",y=n,fill=Age_cut))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.7)+
  coord_polar(theta = "y") +       ##Coordinate transformation
  labs(x = "", y = "", title = "") +      ## set lables
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.35,.93),legend.background = element_blank(),legend.text = element_text(size = 13),legend.key.size = unit(16, "pt"),legend.title = element_text(size=16)) +  
   scale_fill_discrete('The percentage of patients on different age',breaks = comb0$Age_cut, labels = label0) +## 将原来的图例标签换成现在的myLabel
  theme(axis.text.x = element_blank()) 


###2.2
##read boundry data
country<-st_read('C:/Users/hp/Desktop/SDS/session/scratch/Find_Locations_in_Vietnam_Country_Boundary.shp')
country<-fortify(country)
province<-st_read('C:/Users/hp/Desktop/SDS/session/dongt/dongt.shp')
province<-fortify(province)

##change the format of data
geometry<-unlist(country$geometry)
country_lon<-data.frame(long=numeric())
country_lat<-data.frame(lat=numeric())
for(i in 1:length(geometry)) {
  if(geometry[i]>100){
    country_lon=append(country_lon,geometry[i],after = length(country_lon))
  }else{
    country_lat=append(country_lat,geometry[i],after = length(country_lat))
  }
}
country_boundary<-cbind(data.frame(long=matrix(unlist(country_lon),byrow=T)),data.frame(lat=matrix(unlist(country_lat),byrow=T)))

geometry2<-unlist(province$geometry)
province_lon<-data.frame(long=numeric())
province_lat<-data.frame(lat=numeric())
for(i in 1:length(geometry2)) {
  if(geometry2[i]>100){
    province_lon=append(province_lon,geometry2[i],after = length(province_lon))
  }else{
    province_lat=append(province_lat,geometry2[i],after = length(province_lat))
  }
}
province_boundary<-cbind(data.frame(long=matrix(unlist(province_lon),byrow=T)),data.frame(lat=matrix(unlist(province_lat),byrow=T)))

###map the country boundry and the virus
lon_lat<-data.frame(lon=c(100,115),lat=c(8,35))
map1<-ggmap(get_map(location = 'vietnam',zoom=6,maptype = 'watercolor',source = 'stamen'), 
            base_layer = ggplot(aes(x = lon, y = lat),data = lon_lat))+
  labs(x='Longitude',y='Latitude')+
  geom_polygon(data=country_boundary[-c(1:17500),],aes(x=long,y=lat),colour='coral1',fill=NA,size=1.5)+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1)+ 
  geom_point(data=d,aes(x=LONGITUDE,y=LATITUDE,colour=if_common),alpha=0.5,size=3)+
  theme(legend.position = c(0.87,0.92),legend.background = element_blank(),legend.title = element_text(size = 18,color = 'white'),
        legend.text = element_text(size = 15,color = 'white'),legend.key.size = unit(18, "pt"))+
  geom_segment(arrow=arrow(length=unit(4,"mm"), type="closed", angle=40), 
               aes(x=109.7,xend=109.7,y=19,yend=19.5), colour='coral1') +
  geom_label(aes(x=109.7, y=19.1, label="N"),
             size=4, label.padding=unit(1,"mm"), label.r=unit(0.4,"lines")) 

##common virus distribution in the Dong Thap
map2<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Coinfection')+
  geom_point(data=d,aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(is_coinf)),alpha=1,size=5)+
  scale_color_manual(values = c('pink','lightpink2','palevioletred3','indianred4','grey31','grey11'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = c(0.838,0.78),legend.background = element_blank(),legend.title = element_text(size = 12,color = 'grey31'),
        legend.text = element_text(size = 9,color = 'grey31'),legend.key.size = unit(2, "pt"))+
  theme(axis.line = element_line(colour = NA),axis.text = element_text(color = NA),
        axis.ticks = element_line(color = NA),axis.title = element_text(color = NA))

grid.draw(ggplotGrob(map1))  
pushViewport(viewport(x=0.683,y=0.232,width=0.45,height=0.45))
grid.draw(ggplotGrob(map2))  
popViewport()






###2.3
##maps of the six common virus distribution
c1<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Kobuvirus')+
  geom_point(data=d%>%filter(Kobuvirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Kobuvirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('coral1'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

c2<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Mamastrovirus')+
  geom_point(data=d%>%filter(Mamastrovirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Mamastrovirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('gold'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

c3<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Mastadenovirus')+
  geom_point(data=d%>%filter(Mastadenovirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Mastadenovirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('olivedrab3'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

c4<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Norovirus')+
  geom_point(data=d%>%filter(Norovirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Norovirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('skyblue1'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

c5<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Rotavirus')+
  geom_point(data=d%>%filter(Rotavirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Rotavirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('rosybrown1'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

c6<-ggmap(get_map(location ='dongthap' ,zoom=10,maptype = 'terrain'))+
  labs(x='Longitude',y='Latitude',col='Sapovirus')+
  geom_point(data=d%>%filter(Sapovirus==T),aes(x=LONGITUDE,y=LATITUDE,colour=as.factor(Sapovirus)),alpha=0.51,size=4)+
  scale_color_manual(values = c('#999999'))+
  geom_polygon(data=province_boundary[-c(1870:1874),],aes(x=long,y=lat),colour='#999999',fill=NA,size=1.2)+
  theme(legend.position = 'top')

plot_grid(c1,c2,c3,ncol=3,nrow=1,label_size = 10)
plot_grid(c4,c5,c6,ncol=3,nrow=1,label_size = 10)




#####coinfectation 
comb<-d%>%count(is_coinf)
label<-paste(comb$is_coinf,paste('(',round(comb$n/sum(comb$n)*100,1),'%)',sep=''),sep=' ')
g1<-ggplot(comb,aes(x="",y=n,fill=as.factor(is_coinf)))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.23,.9),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) +  
  scale_fill_discrete('The number of coinfection',breaks = comb$is_coinf, labels = label) +
  theme(axis.text.x = element_blank()) 



##general a new variable contains the information of if there is a common virus
d<-d%>%mutate(num_common=0)
for (i in 1:707){
  if(d$Kobuvirus[i]==1){
    d$num_common[i]=1
  }
  else{d$num_common[i]=0}
  if(d$Mamastrovirus[i]==1){
    d$num_common[i]=d$num_common[i]+1
  }
  else{d$num_common[i]=d$num_common[i]+0}
  if(d$Mastadenovirus[i]==1){
    d$num_common[i]=d$num_common[i]+1
  }
  else{d$num_common[i]=d$num_common[i]+0}
  if(d$Norovirus[i]==1){
    d$num_common[i]=d$num_common[i]+1
  }
  else{d$num_common[i]=d$num_common[i]+0}
  if(d$Rotavirus[i]==1){
    d$num_common[i]=d$num_common[i]+1
  }
  else{d$num_common[i]=d$num_common[i]+0}
  if(d$Sapovirus[i]==1){
    d$num_common[i]=d$num_common[i]+1
  }
  else{d$num_common[i]=d$num_common[i]+0}
}
d<-d%>%mutate(num_uncommon=as.numeric(is_coinf)-1-num_common)
d$num_common<-as.factor(d$num_common)
d$num_uncommon<-as.factor(d$num_uncommon)


##common

comb2<-d%>%count(num_common)
label2<-paste(comb2$num_common,paste('(',round(comb2$n/sum(comb2$n)*100,1),'%)',sep=''),sep=' ')
g2<-ggplot(comb2,aes(x="",y=n,fill=num_common))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.28,.95),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) +  
  scale_fill_discrete('Coinfection with Conmmon virus',breaks = comb2$num_common, labels = label2) +
  theme(axis.text.x = element_blank()) 


##just one common virus
comb3<-data.frame(name=c('Kob.','Mam.','Mas.','Nor.','Rot.','Sap.'),
                  number=c((d%>%dplyr::select(Kobuvirus,is_coinf,num_common)%>%filter(Kobuvirus==1,is_coinf==1,num_common==1)%>%count(Kobuvirus))[[1,2]],
                           (d%>%dplyr::select(Mamastrovirus,is_coinf,num_common)%>%filter(Mamastrovirus==1,is_coinf==1,num_common==1)%>%count(Mamastrovirus))[[1,2]],
                           (d%>%dplyr::select(Mastadenovirus,is_coinf,num_common)%>%filter(Mastadenovirus==1,is_coinf==1,num_common==1)%>%count(Mastadenovirus))[[1,2]],
                           (d%>%dplyr::select(Norovirus,is_coinf,num_common)%>%filter(Norovirus==1,is_coinf==1,num_common==1)%>%count(Norovirus))[[1,2]],
                           (d%>%dplyr::select(Rotavirus,is_coinf,num_common)%>%filter(Rotavirus==1,is_coinf==1,num_common==1)%>%count(Rotavirus))[[1,2]],
                           (d%>%dplyr::select(Sapovirus,is_coinf,num_common)%>%filter(Sapovirus==1,is_coinf==1,num_common==1)%>%count(Sapovirus))[[1,2]]))
label3<-paste(comb3$name,paste('(',round(comb3$number/sum(comb3$number)*100,1),'%)',sep=''),sep=' ')

g3<-ggplot(comb3,aes(x="",y=number,fill=name))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.27,.9),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) +  
  scale_fill_discrete('A single common virus infection',breaks = comb3$name, labels = label3) +
  theme(axis.text.x = element_blank()) 


##eah common virus
comb4<-data.frame(name=c('Kob.','Mam.','Mas.','Nor.','Rot.','Sap.'),
                  number=c((d%>%count(Kobuvirus))[[1,2]],(d%>%count(Mamastrovirus))[[1,2]],(d%>%count(Mastadenovirus))[[1,2]],
                           (d%>%count(Norovirus))[[1,2]],(d%>%count(Rotavirus))[[1,2]],(d%>%count(Sapovirus))[[1,2]]))

label4<-paste(comb4$name,paste('(',round(comb4$number/sum(comb4$number)*100,1),'%)',sep=''),sep=' ')
g4<-ggplot(comb4,aes(x="",y=number,fill=name))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.35,.9),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) +  
  scale_fill_discrete('Contain at least the fixed common virus',breaks = comb4$name, labels = label4) +
  theme(axis.text.x = element_blank()) 






##a fixed common+another common

comb5<-data.frame(name=c('Kob.','Mam.','Mas.','Nor.','Rot.','Sap.'),
                  number=c((d%>%dplyr::select(Kobuvirus,is_coinf)%>%filter(Kobuvirus==1,is_coinf!=1)%>%count(Kobuvirus))[[1,2]],
                           (d%>%dplyr::select(Mamastrovirus,is_coinf)%>%filter(Mamastrovirus==1,is_coinf!=1)%>%count(Mamastrovirus))[[1,2]],
                           (d%>%dplyr::select(Mastadenovirus,is_coinf)%>%filter(Mastadenovirus==1,is_coinf!=1)%>%count(Mastadenovirus))[[1,2]],
                           (d%>%dplyr::select(Norovirus,is_coinf)%>%filter(Norovirus==1,is_coinf!=1)%>%count(Norovirus))[[1,2]],
                           (d%>%dplyr::select(Rotavirus,is_coinf)%>%filter(Rotavirus==1,is_coinf!=1)%>%count(Rotavirus))[[1,2]],
                           (d%>%dplyr::select(Sapovirus,is_coinf)%>%filter(Sapovirus==1,is_coinf!=1)%>%count(Sapovirus))[[1,2]]))
label5<-paste(comb5$name,paste('(',round(comb5$number/sum(comb5$number)*100,1),'%)',sep=''),sep=' ')

g5<-ggplot(comb5,aes(x="",y=number,fill=name))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.28,.9),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) + 
  scale_fill_discrete('A fixed common virus with others',breaks = comb5$name, labels = label5) +
  theme(axis.text.x = element_blank()) 


##a fixed common+another common
comb6<-data.frame(name=c('Kob.','Mam.','Mas.','Nor.','Rot.','Sap.'),
                  number=c(0,
                           (d%>%dplyr::select(Mamastrovirus,is_coinf,num_common)%>%filter(Mamastrovirus==1,is_coinf==2,num_common==2)%>%count(Mamastrovirus))[[1,2]],
                           (d%>%dplyr::select(Mastadenovirus,is_coinf,num_common)%>%filter(Mastadenovirus==1,is_coinf==2,num_common==2)%>%count(Mastadenovirus))[[1,2]],
                           (d%>%dplyr::select(Norovirus,is_coinf,num_common)%>%filter(Norovirus==1,is_coinf==2,num_common==2)%>%count(Norovirus))[[1,2]],
                           (d%>%dplyr::select(Rotavirus,is_coinf,num_common)%>%filter(Rotavirus==1,is_coinf==2,num_common==2)%>%count(Rotavirus))[[1,2]],
                           (d%>%dplyr::select(Sapovirus,is_coinf,num_common)%>%filter(Sapovirus==1,is_coinf==2,num_common==2)%>%count(Sapovirus))[[1,2]]))
label6<-paste(comb6$name,paste('(',round(comb6$number/sum(comb6$number)*100,1),'%)',sep=''),sep=' ')

g6<-ggplot(comb6,aes(x="",y=number,fill=name))+
  geom_bar(stat='identity',width=1, position = 'stack',alpha=0.6)+
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) +
  theme(legend.position = c(.43,.9),legend.background = element_blank(),legend.text = element_text(size = 10),legend.key.size = unit(12, "pt")) +  
  scale_fill_discrete('A fixed common virus with another common virus',breaks = comb6$name, labels = label6) +## set the lable
  theme(axis.text.x = element_blank()) 


##get together
plot_grid(g1,g2,g3,g4,g5,g6,labels=c('(A)','(B)','(C)','(D)','(E)','(F)'),ncol=3,nrow=2,label_size = 10)
