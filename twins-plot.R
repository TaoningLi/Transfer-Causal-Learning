library(reshape2)
library(ggplot2)



data.frame(ATEmat.OO1)->df.OO1
df.OO1 <- melt(df.OO1)
plot.OO <- ggplot(data=df.OO1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("OO")+
  ylab("ATE")

data.frame(ATEmat.RR1)->df.RR1
df.RR1 <- melt(df.RR1)
plot.RR <- ggplot(data=df.RR1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("RR")+
  ylab("ATE")
#---------------------
data.frame(ATEmat.RO[,c(7,8,2)])->df.RO1
df.RO1 <- melt(df.RO1)
plot.RO <- ggplot(data=df.RO1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("OO")+
  ylab("ATE") +   
  geom_boxplot(aes(fill =variable), notch = FALSE, size =0.4) +
  scale_fill_brewer(palette ="Set2") +   
  theme_bw()+   
  guides(fill=guide_legend(title=NULL))+
  theme(axis.text.x = element_text(angle = 90))

data.frame(ATEmat.RO[,c(10,9,2)])->df.RO1
df.RO1 <- melt(df.RO1)
plot.RO <- ggplot(data=df.RO1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("RO")+
  ylab("ATE") +   
  geom_boxplot(aes(fill =variable), notch = FALSE, size =0.4) +
  scale_fill_brewer(palette ="Set2") +   
  theme_bw()+   
  guides(fill=guide_legend(title=NULL))+
  theme(axis.text.x = element_text(angle = 90))

data.frame(ATEmat.RO[,c(4,6,2)])->df.RO1
df.RO1 <- melt(df.RO1)
plot.RO <- ggplot(data=df.RO1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("RO")+
  ylab("ATE") +   
  geom_boxplot(aes(fill =variable), notch = FALSE, size =0.4) +
  scale_fill_brewer(palette ="Set2") +   
  theme_bw()+   
  guides(fill=guide_legend(title=NULL))+
  theme(axis.text.x = element_text(angle = 90))

data.frame(ATEmat.RO[,c(3,4,5,6,2)])->df.RR1
df.RR1 <- melt(df.RR1)
plot.RR1 <- ggplot(data=df.RR1, aes(x=variable,y=value))+
  geom_boxplot()+
  xlab("RR")+
  ylab("ATE") +   
  geom_boxplot(aes(fill =variable), notch = FALSE, size =0.4) +
  scale_fill_brewer(palette ="Set2") +   
  theme_bw()+   
  guides(fill=guide_legend(title=NULL))+
  theme(axis.text.x = element_text(angle = 90))