library(ggplot2)
library(dplyr)

raw <- read.csv('reznik.csv',header=TRUE)
data <- tibble(raw)
data <- mutate(data,tsquared=t^2)

fig2 <- ggplot(data,aes(x=t,y=y,color=object))+
    geom_point()+
    geom_smooth(method="lm",formula=y~I(x^2),se=FALSE)+
    xlab('$t$, \\unit{\\second}')+
    ylab('$y$, \\unit{\\meter}')+
    theme_bw(base_size=8)

fig3 <- ggplot(data,aes(x=t,y=v_y,color=object))+
    geom_point()+
    geom_smooth(method="lm",formula=y~x+0,se=FALSE)+
    xlab('$t$, \\unit{\\second}')+
    ylab('$v_y$, \\unit{\\meter\\per\\second}')+
    theme_bw(base_size=8)

fig4 <- ggplot(data,aes(x=tsquared,y=y,color=object))+
     geom_point()+
     geom_smooth(method="lm",se=FALSE)+
     xlab('$t^2$, \\unit{\\second\\squared}')+
     ylab('$y$, \\unit{\\meter}')+
     theme_bw(base_size=8)

# then do stats
