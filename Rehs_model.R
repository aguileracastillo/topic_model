library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)


plot_prep<-plot_prep%>%
  group_by(variable,year)%>%
  summarise(mean = ci(value)[1],
            lowCI = ci(value)[2],
            hiCI = ci(value)[3],
            sd = ci (value)[4])

#some test plot
ggplot(plot_prep, aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable))) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle("Topic 23")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 0.20))

#now you create the two plots
plot1<-ggplot(subset(plot_prep,variable%in%c("X11")), aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable)))+geom_ribbon(data=subset(plot_prep,variable%in%c("X11")),aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle("Topic 11")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(year, mean, group=factor(variable)), subset(plot_prep,variable%in%c("X11")))+coord_cartesian(ylim = c(0, 0.25))+labs(y = "Mean probability")
plot1
plot2<- ggplot(subset(plot_prep,variable%in%c("X40")), aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable)))+geom_ribbon(data=subset(plot_prep,variable%in%c("X40")),aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle("Topic 40")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(year, mean, group=factor(variable)), subset(plot_prep,variable%in%c("X40")))+coord_cartesian(ylim = c(0, 0.1)) +labs(y = "Mean probability")
plot3<-ggplot(subset(plot_prep,variable%in%c("X41")), aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable)))+geom_ribbon(data=subset(plot_prep,variable%in%c("X41")),aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle("Topic 41")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(year, mean, group=factor(variable)), subset(plot_prep,variable%in%c("X41")))+coord_cartesian(ylim = c(0, 0.1)) +labs(y = "Mean prob.")
plot4<- ggplot(subset(plot_prep,variable%in%c("X60")), aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable)))+geom_ribbon(data=subset(plot_prep,variable%in%c("X60")),aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle("Topic 60")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(year, mean, group=factor(variable)), subset(plot_prep,variable%in%c("X60")))+coord_cartesian(ylim = c(0, 0.06)) +labs(y = "Mean probability")
unique(plot_prep$variable)[1]

plot_list<-list()

i=5
#function if you want to change the topics
plot_list<-lapply(c(11,40),function (i) ggplot(subset(plot_prep,variable%in%c(levels(plot_prep$variable)[i])), aes(year, mean, group=factor(variable))) + geom_line(aes(color=factor(variable)))+geom_ribbon(data=subset(plot_prep,variable%in%c(unique(plot_prep$variable))),aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +theme(legend.title=element_blank())+ theme(legend.position="none")+ggtitle(paste0("Topic ",i))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_line(aes(year, mean, group=factor(variable)), subset(plot_prep,variable%in%c(levels(plot_prep$variable)[i])))+coord_cartesian(ylim = c(0, 0.1)) +labs(y = "Mean probability"))
grid.arrange(grobs= plot_list,ncol2)


#merge the two plots into one
grid.arrange(grobs = list(plot1,plot2),ncol=2