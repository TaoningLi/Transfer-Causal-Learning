library(dplyr)
library(ggplot2)
library(patchwork)

#use read.csv to read files
twin.Y <- read.csv("data_twins/twin_pairs_Y_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
twin.X <- read.csv("data_twins/twin_pairs_X_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
twin.T <- read.csv("data_twins/twin_pairs_T_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#create ID for each pair of twins
colnames(twin.X)[1]<-"ID"
colnames(twin.T)[1]<-"ID"
colnames(twin.Y)[1]<-"ID"

twin.X <- twin.X[,-2]

#check the head of covariates
names(twin.X)

#check the description of covariates
readLines("data_twins/covar_desc.txt")->cov.desc
print(cov.desc)

#check the type of covariates
readLines("data_twins/covar_type.txt")->cov.type
print(cov.type)

#check the heads of X,T,Y
head(twin.X)
head(twin.T)
head(twin.Y)


####
m1m0 <- length(which(twin.Y[,2]==1 & twin.Y[,3]==1))
m1s0 <- length(which(twin.Y[,2]==0 & twin.Y[,3]==1))
s1m0 <- length(which(twin.Y[,2]==1 & twin.Y[,3]==0))
s1s0 <- length(which(twin.Y[,2]==0 & twin.Y[,3]==0))

mort.total<- data.frame(
  category=c("mortality", "survival"),
  count=c(m1m0+m1s0+s1m0, s1s0)
)

mort.total$fraction <- mort.total$count / sum(mort.total$count)

# Compute the cumulative percentages (top of each rectangle)
mort.total$ymax <- cumsum(mort.total$fraction)

# Compute the bottom of each rectangle
mort.total$ymin <- c(0, head(mort.total$ymax, n=-1))

# Compute label position
mort.total$labelPosition <- (mort.total$ymax + mort.total$ymin) / 2

# Compute a good label
mort.total$label <- paste0(mort.total$category, "\n value: ", mort.total$count)

# Make the plot
mort.plot1<- ggplot(mort.total, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

mort <- data.frame(
  category=c("both", "heavier(treatment)", "lighter(control)"),
  count=c(m1m0, m1s0, s1m0)
)

mort$fraction <- mort$count / sum(mort$count)

# Compute the cumulative percentages (top of each rectangle)
mort$ymax <- cumsum(mort$fraction)

# Compute the bottom of each rectangle
mort$ymin <- c(0, head(mort$ymax, n=-1))

# Compute label position
mort$labelPosition <- (mort$ymax + mort$ymin) / 2

# Compute a good label
mort$label <- paste0(mort$category, "\n value: ", mort$count)

# Make the plot
mort.plot2<- ggplot(mort, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
             geom_rect() +
             geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
             scale_fill_brewer(palette=4) +
             coord_polar(theta="y") +
             xlim(c(2, 4)) +
             theme_void() +
             theme(legend.position = "none")
mort.plot <- mort.plot1 + mort.plot2


risk.factor<-c('alcohol','anemia','cardiac','chyper','diabetes', 'drink5','eclamp','hemo','herpes','hydra','lung','uterine',      
                'cardiac','pre4000','preterm','uterine','rh','tobacco','cigar6','uterine','gestat10')
risk.factor.1 <- c('alcohol','tobacco','drink5','cigar6')
risk.factor.2 <- c('preterm','diabetes','eclamp','hemo')


twin.X.clean<-na.omit(twin.X)
twin.Y.clean<-twin.Y[which(twin.Y$ID %in% twin.X.clean$ID),]
twin.T.clean<-twin.T[which(twin.T$ID %in% twin.X.clean$ID),]

#mortality set and survival set
mort.set <- which((twin.Y[,2] == 1) | (twin.Y[,3] == 1))
surv.set <- which((twin.Y[,2] == 0) & (twin.Y[,3] == 0))

twin.total.clean <- merge(twin.X.clean, twin.Y.clean, by="ID")
twin.total.clean <- merge(twin.total.clean, twin.T.clean, by="ID")
attach(twin.total.clean)
twin.total.clean$mort <- 1- (1-mort_1) * (1-mort_0) 

twin.mort.clean <- twin.total.clean[which(twin.total.clean$ID %in% mort.set),]
twin.surv.clean <- twin.total.clean[which(twin.total.clean$ID %in% surv.set),]

twin.total.clean$mort <- as.factor(twin.total.clean$mort)
#twin.total.clean$drink5 <- as.factor(twin.total.clean$drink5)
ggplot(twin.total.clean,aes(x = drink5 ,fill = mort))+
             geom_bar(position = "fill")


library(ggradar)#you can download this package by commanding <remotes::install_github("ricardo-bion/ggradar")>
library(tidyverse)
library(scales)
library(showtext)

mean_cigar6 <- mean(twin.total.clean$cigar6)
mean_drink5 <- mean(twin.total.clean$drink5)
mean_tobacco <- mean(twin.total.clean$tobacco)
mean_alcohol <- mean(twin.total.clean$alcohol)
mean_gestat10 <- mean(twin.total.clean$gestat10)
mean_cardiac <- mean(twin.total.clean$cardiac)
mean_diabetes <- mean(twin.total.clean$diabetes)
mean_anemia <- mean(twin.total.clean$anemia)

Twin.radar <- twin.total.clean %>%
  group_by(mort_0, mort_1) %>%
  summarise(
    avg_cigar6 = mean(cigar6)/mean_cigar6,
    avg_drink5 = mean(drink5)/mean_drink5,
    avg_tobacco = mean(tobacco)/mean_tobacco,
    avg_alcohol = mean(alcohol)/mean_alcohol,
    avg_gestat10 = mean(gestat10)/mean_gestat10,
    avg_cardiac = mean(cardiac)/mean_cardiac,
    avg_diabetes = mean(diabetes)/mean_diabetes,
    avg_anemia = mean(anemia)/mean_anemia,
  ) 

Twin.radar$mort.twin <-c("neither", "heaviear", "lighter", "both")
Twin.radar$mort.twin <- as.factor(Twin.radar$mort.twin)
Twin.radar <- Twin.radar %>%
  group_by(mort.twin) 
Twin.radar <- Twin.radar[,-grep("mort_0|mort_1",colnames(Twin.radar))]
Twin.radar <- Twin.radar[,c(9,seq(1,8))]

radar.plot <- Twin.radar %>%
  ggradar(
    grid.label.size = 4,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 4, # Afftects the names of the variables
    group.point.size = 5,   # Simply the size of the point 
    grid.min = 0.5,
    grid.mid = 1,
    grid.max = 2,
    values.radar = c("50%","100%","200%")
  )
plot(radar.plot)


risk.factor<-c('alcohol','anemia','cardiac','cigar6','chyper','diabetes', 'drink5','eclamp','gestat10','hemo','herpes','hydra','lung',      
               'pre4000','preterm','rh','tobacco','uterine')
twin.cov <- twin.X.clean %>%
            select(all_of(risk.factor))
twin.cor <- cor(twin.cov)
m=par(no.readonly = TRUE)
corrplot::corrplot(twin.cor, method = "color", type = "upper",tl.cex = 0.6 
                   ,number.font = 2 )
corrplot::corrplot.mixed(twin.cor, upper = 'shade', lower = 'number',tl.cex = 0.6 
                         , tl.col="black",number.font = 2,diag = 'u')

twin.X.clean<-na.omit(twin.X)
twin.Y.clean<-twin.Y[which(twin.Y$ID %in% twin.X.clean$ID),]
twin.T.clean<-twin.T[which(twin.T$ID %in% twin.X.clean$ID),]


twin.den.plot <- ggplot(twin.total.clean, aes(x = dbirwt_0, y=mort_0))+
                 geom_histogram(bins=10)
sum(twin.total.clean$mort_0)/dim(twin.total.clean)[1] -> mort_0_tot
sum(twin.total.clean$mort_1)/dim(twin.total.clean)[1] -> mort_1_tot

mort_less2000 <- twin.total.clean %>% 
  filter(dbirwt_0 <= 2000 & dbirwt_1 <= 2000) %>% 
  summarise(control = sum(mort_0)/dim(twin.total.clean[which(dbirwt_0 <= 2000 & dbirwt_1 <= 2000),])[1],
            treatment = sum(mort_1)/dim(twin.total.clean[which(dbirwt_0 <= 2000 & dbirwt_1 <= 2000),])[1])

mort_total<- twin.total.clean %>% 
  summarise(control = sum(mort_0)/dim(twin.total.clean)[1],
            treatment = sum(mort_1)/dim(twin.total.clean)[1])

rbind(mort_less2000,mort_total) -> mort_bar1
ggplot(mort_bar1, aes(x = treat)) +
geom_col(position = "dodge")
