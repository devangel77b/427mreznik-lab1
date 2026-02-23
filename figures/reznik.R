library(ggplot2)
library(dplyr)

raw <- read.csv('reznik.csv',header=TRUE)
data <- tibble(raw)
data <- mutate(data,tsquared=t^2)

fig2 <- ggplot(data,aes(x=t,y=y,color=object))+
     geom_hline(yintercept=0,color="gray70")+
     geom_point()+	
     geom_smooth(method="lm",formula=y~I(x^2),se=FALSE)+
     xlab('$t$, \\unit{\\second}')+
     ylab('$y$, \\unit{\\meter}')+
     theme_bw(base_size=8)+
     theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig2.svg',plot=fig2,width=3.4167,height=2,units="in")

fig3 <- ggplot(data,aes(x=t,y=v_y,color=object))+
     geom_hline(yintercept=0,color="gray70")+
     geom_point()+
     geom_smooth(method="lm",formula=y~x+0,se=FALSE)+
     xlab('$t$, \\unit{\\second}')+
     ylab('$v_y$, \\unit{\\meter\\per\\second}')+
     theme_bw(base_size=8)+
     theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig3.svg',plot=fig3,width=3.4167,height=2,units="in")

fig4 <- ggplot(data,aes(x=tsquared,y=y,color=object))+
     geom_hline(yintercept=0,color="gray70")+
     geom_point()+
     geom_smooth(method="lm",formula=y~x,se=FALSE)+
     xlab('$t^2$, \\unit{\\second\\squared}')+
     ylab('$y$, \\unit{\\meter}')+
     coord_cartesian(xlim=c(0,1))+
     theme_bw(base_size=8)+
     theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig4.svg',plot=fig4,width=3.4167,height=2,units="in")

# then do stats
datat <- tibble(read.csv('times.csv',header=TRUE))
modelt <- lm(t_s~type,datat)
print(anova(modelt))

# check accelerations
model1 <- lm(v_y~t,data)
model2 <- lm(v_y~t:object,data)
print(anova(model1,model2))
print(summary(model2))



