###4.1

##4.1.1

d$is_coinf[is.na(d$is_coinf)]<-0


d$water_source<-as.factor(d$water_source)
d$ContactDiar<-as.factor(d$ContactDiar)
d$KeepAnimal<-as.factor(d$KeepAnimal)
d$KillingAnimal<-as.factor(d$KillingAnimal)
d$EatCookRawMeat<-as.factor(d$EatCookRawMeat)
d$SiteRecruitment<-as.factor(d$SiteRecruitment)
d$is_coinf<-as.factor(d$is_coinf)
d$Tap<-as.factor(d$Tap)
d$Well<-as.factor(d$Well)
d$Rain<-as.factor(d$Rain)
d$River<-as.factor(d$River)
d$Pond<-as.factor(d$Pond)
d$Bottled<-as.factor(d$Bottled)

##tidy data for plotting
per_water<-d%>%group_by(is_coinf,water_source)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_contact<-d%>%group_by(is_coinf,ContactDiar)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_keep<-d%>%group_by(is_coinf,KeepAnimal)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_kill<-d%>%group_by(is_coinf,KillingAnimal)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_eat<-d%>%group_by(is_coinf,EatCookRawMeat)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_site<-d%>%group_by(is_coinf,SiteRecruitment)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_age<-d%>%group_by(is_coinf,Age_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
per_gender<-d%>%group_by(is_coinf,Gender)%>%summarise(count=n())%>%mutate(percent=count/sum(count))



g1<-ggplot(per_water,aes(x=is_coinf,y=percent*100,fill=water_source))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of water source", fill = "water_source") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g2<-ggplot(per_contact,aes(x=is_coinf,y=percent*100,fill=ContactDiar))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of contact patients", fill = "ContactDiar") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g3<-ggplot(per_keep,aes(x=is_coinf,y=percent*100,fill=KeepAnimal))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of keep animals", fill = "KeepAnimal") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g4<-ggplot(per_kill,aes(x=is_coinf,y=percent*100,fill=KillingAnimal))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of kill animals", fill = "KillingAnimal") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g5<-ggplot(per_eat,aes(x=is_coinf,y=percent*100,fill=EatCookRawMeat))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of EatCookRawMeat", fill = "EatCookRawMeat") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g6<-ggplot(per_site,aes(x=is_coinf,y=percent*100,fill=SiteRecruitment))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of site", fill = "SiteRecruitment") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g7<-ggplot(per_age,aes(x=is_coinf,y=percent*100,fill=Age_cut))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of Age", fill = "Age_cut") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g8<-ggplot(per_gender,aes(x=is_coinf,y=percent*100,fill=Gender))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of gender", fill = "Gender") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))

plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,labels=c('(A)','(B)','(C)','(D)','(E)','(F)','(G)','(H)'),ncol=4,nrow=2,label_size = 10)

##4.1.2
library(MASS)
library(nnet)
library(lme4)


##multinom
multi<-multinom(data=d,is_coinf~Age_cut+Gender+SiteRecruitment+Tap+Well+Rain+River+Pond+Bottled+ContactDiar+KillingAnimal+KeepAnimal+ EatCookRawMeat )
summary(multi)
multi0<-step(multi)
summary(multi0)

##2-tailed z test
z <- summary(multi0)$coefficients/summary(multi0)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
##ratio
exp(coef(multi0))


##4.1.3
##polr
polr1<-polr(is_coinf~Age_cut+Gender+Tap+Well+Rain+River+Pond+Bottled+ContactDiar+KeepAnimal+KillingAnimal+EatCookRawMeat+SiteRecruitment,data=d)
summary(polr1)
polr0<-step(polr1,trace = 0)
summary(polr0)
##test
ctable <- coef(summary(polr0))
p_polr <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
p_polr
##ratio
exp(coef(polr0))
##CI
confint(polr0)


##4.1.4
##general a nwe variable to show whether there are coinfection
d<-d%>%
  mutate(if_coinf=ifelse(is_coinf==0,0,1))

mixed<-glmer(if_coinf~Age+Gender+Tap+Well+Rain+River+Pond+Bottled+ContactDiar+KeepAnimal+KillingAnimal+EatCookRawMeat+(1|SiteRecruitment),data=d,family = binomial)
summary(mixed)
anova(mixed)

mixed1<-glmer(if_coinf~Gender+(1|SiteRecruitment),data=d,family = binomial, nAGQ=25)
summary(mixed1)

mixed2<-glmer(if_coinf~Age_cut+Gender+(1|SiteRecruitment),data=d,family = binomial, nAGQ=25)
summary(mixed2)

mixed3<-glmer(if_coinf~Age_cut+Gender+KillingAnimal+(1|SiteRecruitment),data=d,family = binomial)
summary(mixed3)

mixed4<-glm(if_coinf~Age_cut+Gender+KillingAnimal+SiteRecruitment,data=d,family = binomial)
summary(mixed4)

mixed5<-glm(if_coinf~Gender+SiteRecruitment+Age,data=d,family = binomial, nAGQ=25)
summary(mixed5)

##univariable analysis
k<-c('Age_cut','Gender','Tap','Well','Rain','River','Pond','Bottled','ContactDiar','KeepAnimal','KillingAnimal','EatCookRawMeat')
p_value<-matrix(c(k,rep(0,12)),2,12,byrow=T)
for(i in k){
  p_value[2,grep(i,k,value = F)]<-summary(glmer(if_coinf~get(i)+(1|SiteRecruitment),data=d,family = binomial))$coefficients[2,4]
}
p_value



###4.2

##4.2.1
###disease sevirity VS coinfection
diar_coinf<-d%>%group_by(is_coinf,Diar_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))

g_diar_coinf<-ggplot(diar_coinf,aes(x=is_coinf,y=percent*100,fill=as.factor(Diar_cut)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of Diar_cut", fill = "If diar") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))

pain_coinf<-d%>%group_by(is_coinf,AbdominalPain)%>%summarise(count=n())%>%mutate(percent=count/sum(count))

g_pain_coinf<-ggplot(pain_coinf,aes(x=is_coinf,y=percent*100,fill=as.factor(AbdominalPain)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of AbdominalPain", fill = "If pain") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))


fever_coinf<-d%>%group_by(is_coinf,ThreeDaysFever)%>%summarise(count=n())%>%mutate(percent=count/sum(count))

g_fever_coinf<-ggplot(fever_coinf,aes(x=is_coinf,y=percent*100,fill=as.factor(ThreeDaysFever)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "The number of coinfection", y = "The percentage of ThreeDaysFever", fill = "If fever") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))

ggplot(fever_coinf,aes(x=is_coinf,y=percent*100,group=as.factor(ThreeDaysFever),linetype=as.factor(ThreeDaysFever)))+
  geom_line()

##get together
plot_grid(g_diar_coinf,g_pain_coinf,g_fever_coinf,labels=c('(A)','(B)','(C)'),ncol=3,nrow=1,label_size = 10)




##ploteach common variable VS coinfection
##diar
diar_Mastadenovirus<-d%>%group_by(Mastadenovirus,Diar_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
diar_Norovirus<-d%>%group_by(Norovirus,Diar_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
diar_Rotavirus<-d%>%group_by(Rotavirus,Diar_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
diar_Sapovirus<-d%>%group_by(Sapovirus,Diar_cut)%>%summarise(count=n())%>%mutate(percent=count/sum(count))


g_diar_mas<-ggplot(diar_Mastadenovirus,aes(x=Mastadenovirus,y=percent*100,fill=as.factor(Diar_cut)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Mastadenovirus", y = "The percentage of diarrhoel episodes", fill = "Diar_cut") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_diar_nor<-ggplot(diar_Norovirus,aes(x=Norovirus,y=percent*100,fill=as.factor(Diar_cut)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Norovirus", y = "The percentage of diarrhoel episodes", fill = "Diar_cut") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_diar_rot<-ggplot(diar_Rotavirus,aes(x=Rotavirus,y=percent*100,fill=as.factor(Diar_cut)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Rotavirus", y = "The percentage of diarrhoel episodes", fill = "Diar_cut") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_diar_sap<-ggplot(diar_Sapovirus,aes(x=Sapovirus,y=percent*100,fill=as.factor(Diar_cut)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Sapovirus", y = "The percentage of diarrhoel episodes", fill = "Diar_cut") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('coral1','khaki1','olivedrab3','skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))

##pain
pain_Mastadenovirus<-d%>%group_by(Mastadenovirus,AbdominalPain)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
pain_Norovirus<-d%>%group_by(Norovirus,AbdominalPain)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
pain_Rotavirus<-d%>%group_by(Rotavirus,AbdominalPain)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
pain_Sapovirus<-d%>%group_by(Sapovirus,AbdominalPain)%>%summarise(count=n())%>%mutate(percent=count/sum(count))


g_pain_mas<-ggplot(pain_Mastadenovirus,aes(x=Mastadenovirus,y=percent*100,fill=as.factor(AbdominalPain)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Mastadenovirus", y = "The percentage of AbdominalPain", fill = "If Pain") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_pain_nor<-ggplot(pain_Norovirus,aes(x=Norovirus,y=percent*100,fill=as.factor(AbdominalPain)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Norovirus", y = "The percentage of AbdominalPain", fill = "If Pain") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_pain_rot<-ggplot(pain_Rotavirus,aes(x=Rotavirus,y=percent*100,fill=as.factor(AbdominalPain)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Rotavirus", y = "The percentage of AbdominalPain", fill = "If Pain") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_pain_sap<-ggplot(pain_Sapovirus,aes(x=Sapovirus,y=percent*100,fill=as.factor(AbdominalPain)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Sapovirus", y = "The percentage of AbdominalPain", fill = "If Pain") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))


##fever
fever_Mastadenovirus<-d%>%group_by(Mastadenovirus,ThreeDaysFever)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
fever_Norovirus<-d%>%group_by(Norovirus,ThreeDaysFever)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
fever_Rotavirus<-d%>%group_by(Rotavirus,ThreeDaysFever)%>%summarise(count=n())%>%mutate(percent=count/sum(count))
fever_Sapovirus<-d%>%group_by(Sapovirus,ThreeDaysFever)%>%summarise(count=n())%>%mutate(percent=count/sum(count))


g_fever_mas<-ggplot(fever_Mastadenovirus,aes(x=Mastadenovirus,y=percent*100,fill=as.factor(ThreeDaysFever)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Mastadenovirus", y = "The percentage of ThreeDaysFever", fill = "If fever") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_fever_nor<-ggplot(fever_Norovirus,aes(x=Norovirus,y=percent*100,fill=as.factor(ThreeDaysFever)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Norovirus", y = "The percentage of ThreeDaysFever", fill = "If fever") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_fever_rot<-ggplot(fever_Rotavirus,aes(x=Rotavirus,y=percent*100,fill=as.factor(ThreeDaysFever)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Rotavirus", y = "The percentage of ThreeDaysFever", fill = "If fever") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))
g_fever_sap<-ggplot(fever_Sapovirus,aes(x=Sapovirus,y=percent*100,fill=as.factor(ThreeDaysFever)))+
  geom_bar(stat='identity', width = 0.7)+
  labs(x = "Whether has the Sapovirus", y = "The percentage of ThreeDaysFever", fill = "If fever") +
  theme_minimal(base_size = 14)+
  scale_fill_manual(values=c('skyblue1','rosybrown1','#999999'))+
  theme(legend.text=element_text(size=8), legend.title=element_text(size=10),
        axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 9))

##together
plot_grid(g_diar_mas,g_diar_nor,g_diar_rot,g_diar_sap,g_pain_mas,g_pain_nor,g_pain_rot,g_pain_sap,
          g_fever_mas,g_fever_nor,g_fever_rot,g_fever_sap,
          labels=c('(A)','(B)','(C)','(D)','(E)','(F)','(G)','(H)','(I)','(J)','(K)','(L)'),ncol=4,nrow=3,label_size = 10)



##4.2.2

##multinom for NumberDiarEpi
multi_diar_cut2<-multinom(data=d,Diar_cut~Kobuvirus+Mamastrovirus+Mastadenovirus+Norovirus+Rotavirus+Sapovirus+is_coinf)
summary(multi_diar_cut2)
multi_diar_cut<-step(multi_diar_cut2,trace = 0)
summary(multi_diar_cut)
##2-tailed z test
z <- summary(multi_diar_cut)$coefficients/summary(multi_diar_cut)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
##ratio
exp(coef(multi_diar_cut))

##multinom for AbdominalPain
multi_pain2<-multinom(data=d,AbdominalPain~Kobuvirus+Mamastrovirus+Mastadenovirus+Norovirus+Rotavirus+Sapovirus+is_coinf)
summary(multi_pain2)
multi_pain<-step(multi_pain2,trace = 1)
summary(multi_pain)
##2-tailed z test
z <- summary(multi_pain)$coefficients/summary(multi_pain)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
##ratio
exp(coef(multi_pain))


##multinom for ThreeDaysFever
multi_fever2<-multinom(data=d,ThreeDaysFever~Kobuvirus+Mamastrovirus+Mastadenovirus+Norovirus+Rotavirus+Sapovirus+is_coinf)
summary(multi_fever2)
multi_fever<-step(multi_fever2,trace = 1)
summary(multi_fever)
##2-tailed z test
z <- summary(multi_fever)$coefficients/summary(multi_fever)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
##ratio
exp(coef(multi_fever))
