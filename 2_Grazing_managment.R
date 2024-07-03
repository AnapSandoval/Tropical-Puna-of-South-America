
#####################################################################
##########ANALYSIS CAMELID GRAZING INTENSITY ~ MINING################
##########Ana P.Sandoval 22/10/2023##################################
library(plyr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(readxl)
require("ggplot2")
library(emmeans)
library(AICcmodavg)
##Looking at community herd densities at indigenous communities with intenisve herding & mining disturbance######## 
df <- read_excel("C:/Users/anaps/Desktop/Chapter management/Data analysis/herd_mining.xlsx")
View(df)
##hist(df$logMinning_con)
df$logMining_con<- log(df$Minning_con)
df$logMining_con[is.infinite(df$logMining_con)] <- NA
head(df)
####check relationship %area mining and number of concession###
####can we extrapolate the nr of concessions to area of land?####
lm1<- lm(Decrease_Area~logMining_con, data=df, na.action = na.omit)
lm1
summary(lm1)
###plot
p<-ggplot(df, aes(x = logMining_con, y = Decrease_Area, col=community)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(Mining concessions)",
       y = "Area Community (ha)") +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkblue", 
                                "Agua_Blanca" = "red", "Nubepampa" = "orange", 
                                "Puyo Puyo" = "purple", 
                                "Cololo" = "magenta")) +
  ##scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2)) +
  theme_minimal()
p
#####lm mining concessions## WITH INTERACTIONS
# Filter out rows with missing values in Mining_con
df_filtered <- df[complete.cases(df$logMining_con), ]
df_filtered$year<-as.numeric(df_filtered$year)
str(df_filtered$year)
##View(df_filtered)
lm_mining <- lm(logMining_con ~ year*community, data=df_filtered)
anova(lm_mining)
summary(lm_mining)
emm= emtrends(lm_mining, pairwise~community, var="year")
emm 
###SIMPLE plot
p <- ggplot() +
  geom_point(aes(x = year, y = logMining_con, col = community), data = df, pch = 19, size =2, alpha = 0.5) +
  geom_smooth(aes(x = year, y = logMining_con), method = "lm", color = "black", data = df) +
  labs(x = "Year",
       y = "Nr mining concessions",
       color = "Mining Intensity") +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2)) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "brown", "Nubepampa" = "orange", 
                                "Puyo Puyo" = "purple", 
                                "Cololo" = "magenta"), "Canahuma"= "red", "Medallani"= "blue") +
  guides(color = guide_legend(title = "Community"))
p
###
# Create a data frame for predictions
new_data <- expand.grid(year = unique(df_filtered$year), community = unique(df_filtered$community))
new_data$logMining_con <- predict(lm_mining, new_data)
# Identify communities with solid lines
solid_linesM <- c("Puyo Puyo", "Nubepampa")
# Plot the interaction effect
Mining<-ggplot(df_filtered, aes(x = year, y = logMining_con, color = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_line(data = new_data, aes(y = logMining_con, linetype = factor(community %in% solid_linesM)), size = 1) +
  labs(x = "Year", y = "log(Mining_concession)", color = "Community") +
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Cololo" = "brown", "Puyo Puyo" = "purple", "Canahuma"= "red", "Medallani"= "blue")) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2))+
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
  theme_bw()+
  theme(aspect.ratio = 1)
Mining

df_filtered$year <- as.numeric(df_filtered$year)
###PERMANOVA tests to check dependencies of the time series###
###NOT working check with Yann##
library(vegan)
library("permute")
str(df_filtered)
h <- how(plots = Plots(strata = df_filtered$community, type = "none"),
         nperm = 499)
df_filtered$year<-as.factor(df_filtered$year)

adonis(logMining_con  ~ year*community, data=df_filtered, permutations=99)

library(dplyr)
####herd size A###WITH INTERACTION
str(df)
df$year<-as.numeric(df$year)##
df$logherd_size_A<-log(df$herd_size_A)#herd size_A contains all livestock animals while herd_sizeb, only alpacas
lm_herd <- lm(logherd_size_A ~ year*community,  data=df, na.action = na.omit)
anova(lm_herd)
summary(lm_herd)
emm1= emtrends(lm_herd, pairwise~community, var="year")
emm1 
require("ggplot2")
# Create a data frame for predictions
new_data <- expand.grid(year = unique(df$year), community = unique(df$community))
new_data$logherd_size_A <- predict(lm_herd, new_data)
# Identify communities with solid lines
solid_linesH <- c("Antaquilla", "Puyo Puyo", "Nubepampa")###significant relationships
# Plot the interaction effect
Herd<-ggplot(df, aes(x = year, y = logherd_size_A, color = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_line(data = new_data, aes(y = logherd_size_A, linetype = factor(community %in% solid_linesH)), size = 1) +
  labs(x = "Year", y = "log(Herd Size)", color = "Community") +
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Cololo" = "brown", "Puyo Puyo" = "purple", "Canahuma"= "red", "Medallani"= "blue"))+
  scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2))+
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
  theme_bw()+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)

Herd
####herd densities analysis####
str(df)
hist(df$herd_density)
df_filtered$logherd_density<-log(df_filtered$herd_density)
lm_herd_den <- lm(logherd_density ~ year*community,  data=df, na.action = na.omit)
summary(lm_herd_den)
emm1= emtrends(lm_herd_den, pairwise~community, var="year")
emm1 
require("ggplot2")
# Create a data frame for predictions
new_data <- expand.grid(year = unique(df$year), community = unique(df$community))
new_data$herd_density <- predict(lm_herd_den, new_data)
# Identify communities with solid lines
solid_linesHD <- c("Antaquilla", "Puyo Puyo", "Nubepampa")###significant relationships
# Plot the interaction effect
Herd_den<-ggplot(df, aes(x = year, y = herd_density, color = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_line(data = new_data, aes(y = herd_density, linetype = factor(community %in% solid_linesHD)), size = 1) +
  labs(x = "Year", y = "Livestock_density", color = "Community") +
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Cololo" = "brown", "Puyo Puyo" = "purple", "Canahuma"= "red", "Medallani"= "blue"))+
  scale_x_continuous(breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2))+
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
  theme_bw()+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)
Herd_den

###Relationship herd size/densities and number of mining concessions###
# Specify the three variables (columns) of interest
lm3 <- lm(logherd_density ~ logMining_con*community,  data=df, na.action = na.omit)
summary(lm3)
emm3= emtrends(lm3, pairwise~community, var="logMining_con")
emm3 
# Identify communities with solid lines##significant 
Min_herd <- ggplot(df, aes(x = logMining_con, y = logherd_density, col = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_smooth(aes(linetype = community), method = "lm", se = FALSE) +
  labs(x = "Nr. Mining concessions", y = "Livestock_density(An/ha)") +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Puyo Puyo" = "purple", 
                                "Cololo" = "brown")) +
  scale_linetype_manual(values = c("Antaquilla" = "dashed", 
                                   "Katantika" = "dashed", 
                                   "Agua_Blanca" = "solid", 
                                   "Nubepampa" = "dashed", 
                                   "Puyo Puyo" = "solid", 
                                   "Cololo" = "dashed"))+
  theme_bw()+
  theme(aspect.ratio = 1)
Min_herd
###LME###
gmm1 <- lme(logherd_density ~ logMining_con, random=list(~1|community) ,data=df, na.action = na.omit)###preferred
gmm2 <- lme(logherd_density ~ logMining_con, random=list(~1+logMining_con|community), control = list(maxit = 1000, tol = 1e-6), data=df,na.action = na.omit )
anova(gmm1, gmm2)
summary(gmm1)
plot(gmm1)
intervals(gmm1,  which = "fixed")
vf1Fixed<- varFixed(~(logMining_con))
vf3 <- varPower(form= ~(logMining_con))
vf4 <- varExp(form =~(logMining_con))
vf5 <- varConstPower(form =~(logMining_con))
gmm1 <- lme(herd_density ~ logMining_con, random=list(~1|community), data=df_filtered)
gmm2 <- lme(herd_density ~ logMining_con, random=list(~1|community), weights =vf1Fixed , data=df_filtered)
gmm3 <- lme(herd_density ~ logMining_con, random=list(~1|community), weights = vf3,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000), data=df_filtered)
gmm4 <- lme(herd_density ~ logMining_con, random=list(~1|community), weights = vf4,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000), data=df_filtered)
gmm5 <- lme(herd_density ~ logMining_con, random=list(~1|community), weights = vf5,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000), data=df_filtered)
anova(gmm1, gmm4, gmm5)
fixef(gmm1)
ranef(gmm1)
df_filtered$F0 <- fitted(gmm1, level = 0) # to get the fitted values for the fixed effects
df_filtered$F1 <- fitted(gmm1, level = 1) # to get the fitted values for the random effects
# percentage of variance explained
lm1 <- lm(df_filtered$F0 ~ df_filtered$logherd_density, na.action = na.omit)
summary(lm1)
###Plot the relationship
library(ggplot2)
library(effects)
Herd_logMining_con<-as.data.frame(effect("logMining_con",gmm1, xlevels=20))
df_filtered$Minning_con <-as.numeric(df_filtered$Minning_con)
View(df_filtered)
HERD_MIN<-ggplot()+
  geom_point(aes(x=Minning_con, y=herd_density,color=community),data=df_filtered, pch=19, size=1.5)+
  geom_ribbon(data=Herd_logMining_con, aes(x=exp(logMining_con),y=exp(fit),
                                     ymin=exp(lower), ymax=exp(upper)), alpha=0.2)+
  stat_smooth(data=Herd_logMining_con,aes(x=exp(logMining_con),y=exp(fit)),size=1.4, color= "black")+
  xlab("Nr. mining concessions")+
  ylab("Livestock density")+
  theme_bw()+
  theme(aspect.ratio = 1)
HERD_MIN

Livestock<- qplot(logMining_con, logherd_density, ylab = "log(herd density)", xlab= "log(Nr. mining concessions)", colour=community, data=df_filtered)
LIV<-Livestock + theme_bw()+
  geom_line(aes(y=F0), size=2, colour='black') + # line for the fixed effect
  geom_line(aes(y=F1, group=community), size=1)+ 
  scale_color_manual(values=c("purple", "green","brown", "lightgray",  "orange", "darkgreen"),
                     limits = c("Puyo Puyo","Antaquilla", "Cololo", "Katantika","Nubepampa", "Agua_blanca"))+
  theme_bw()+
  theme(aspect.ratio = 1)
LIV
###Vicunhas####
#####lm mining concessions## WITH INTERACTIONS
# Filter out rows with missing values in Minning_con
df_vic <- read_excel("C:/Users/anaps/Desktop/Chapter management/Data analysis/vic_mining.xlsx")
View(df_vic)
hist(df_vic$Vichuna_size)
df_vic$logVichuna_size<-log(df_vic$Vichuna_size)
hist(df_vic$logVichuna_size)
df_vic$year<-as.numeric(df_vic$year)
df_vic$logMinning_con <- log(df_vic$Minning_con )
lm_vicsize <- lm(Vichuna_size ~ year*community, data=df_vic)
anova(lm_vicsize)
summary(lm_vicsize)
emm2= emtrends(lm_vicsize, pairwise~community, var="year")
emm2
R.Version()
# Create a data frame for predictions
new_data <- expand.grid(year = unique(df_vic$year), community = unique(df_vic$community))
new_data$logVichuna_size <- predict(lm_vicsize, new_data)
# Identify communities with solid lines
solid_linesV<- c("Antaquilla", "Cololo", "Katantika", "Nubepampa")
# Plot the interaction effect
Vic<-ggplot(df_vic, aes(x = year, y = logVichuna_size, color = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_line(data = new_data, aes(y = logVichuna_size, linetype = factor(community %in% solid_linesV)), size = 1) +
  labs(x = "Year", y = "log(Vicunha_size)", color = "Community") +
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Cololo" = "brown", "Puyo Puyo" = "purple", "Canahuma"= "red", "Medallani"= "blue"))+
  scale_x_continuous(breaks = seq(2018, 2022, by = 1), labels = seq(2018, 2022, by = 1))+
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
  theme_bw()+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)
Vic
####relationship with vichuna density####
hist(df_vic$vichuna_density)
df_vic$logvichuna_density <-log(df_vic$vichuna_density)
hist(df_vic$logvichuna_density)
df_vic$year<-as.numeric(df_vic$year)
df_vic$logMinning_con <- log(df_vic$Minning_con )
lm_vicden <- lm(logvichuna_density  ~ year*community, data=df_vic)
summary(lm_vicden)
emm2= emtrends(lm_vicden, pairwise~community, var="year")
emm2

# Create a data frame for predictions
new_data <- expand.grid(year = unique(df_vic$year), community = unique(df_vic$community))
new_data$vichuna_density <- predict(lm_vicden, new_data)
# Identify communities with solid lines
solid_linesV<- c("Canahuma", "Cololo", "Nubepampa")
# Plot the interaction effect
Vic<-ggplot(df_vic, aes(x = year, y = vichuna_density, color = community)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5)) +
  geom_line(data = new_data, aes(y = vichuna_density, linetype = factor(community %in% solid_linesV)), size = 1) +
  labs(x = "Year", y = "Vicunha_density", color = "Community") +
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkgray", 
                                "Agua_Blanca" = "darkgreen", "Nubepampa" = "orange", 
                                "Cololo" = "brown", "Puyo Puyo" = "purple", "Canahuma"= "red", "Medallani"= "blue"))+
  scale_x_continuous(breaks = seq(2018, 2022, by = 1), labels = seq(2018, 2022, by = 1))+
  scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
  theme_bw()+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)
Vic



###Plot Figure 3
library(cowplot)
library(grid)
library(gridExtra)
Fig2a<-plot_grid( Herd_den, Vic,Mining,  labels=c("A", "B", "C"), ncol=3, nrow=1, rel_widths=c(1.1,1.1,1.6)) #d1<-plot_grid(d,e,f, rel_widths=c(0.8,0.8,1.2), labels=c("A","B", "C"), ncol=3, nrow=1)
Fig2a
ggsave("C:/Users/anaps/Desktop/Chapter management/Figures/Figure2a.pdf", Fig2a, width = 10, height = 5, dpi=300)

######Relationship vicunha population size and number of mining concessions###
view(df_vic)
df_vic$logMinning_con[is.infinite(df_vic$logMinning_con)] <- NA
lm4 <- lm(logvichuna_density ~ logMinning_con*community,  data=df_vic,  na.action = na.omit)
summary(lm4)
emm4= emtrends(lm4, pairwise~community, var="logMinning_con")
emm4 
# Identify communities with solid lines##significant 
Min_vic <- ggplot(df_vic, aes(x = logMinning_con, y = logvichuna_density, col = community)) +
  geom_point() +
  geom_smooth(aes(linetype = community), method = "lm", se = FALSE) +
  labs(x = "Mining concessions", y = "Vicunha_density (An/ha)") +
  scale_color_manual(values = c("Antaquilla" = "green", "Katantika" = "darkblue", "Nubepampa" = "orange", 
                                "Puyo Puyo" = "purple", 
                                "Cololo" = "brown")) +
  scale_linetype_manual(values = c("Antaquilla" = "dashed", 
                                   "Katantika" = "dashed", 
                                   "Nubepampa" = "dashed", 
                                   "Puyo Puyo" = "dashed", 
                                   "Cololo" = "dashed"))+  
  theme_bw()+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)
Min_vic
Fig3a<-plot_grid(Min_vic,Min_herd,labels=c("A", "B"), ncol=2, nrow=1, rel_widths=c(1,1.3)) #d1<-plot_grid(d,e,f, rel_widths=c(0.8,0.8,1.2), labels=c("A","B", "C"), ncol=3, nrow=1)
Fig3a
ggsave("C:/Users/anaps/Desktop/Chapter management/Figures/Figure3aFINALFINAL.jpeg", Fig3a, width = 10, height = 5, dpi=300)

###LME###
df_filtered
View(df_vic)
# Remove rows with -Inf values
gmm1 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community) ,data=df_vic,  na.action = na.omit)###preferred
gmm2 <- lme(logvichuna_density ~ logMinning_con, random=list(~1+logMinning_con|community), data=df_vic, control = lmeControl(msMaxIter = 1000000, msMaxEval = 1000000),   na.action = na.omit)
anova(gmm1, gmm2)
summary(gmm1)
plot(gmm1)
intervals(gmm1,  which = "fixed")
vf1Fixed<- varFixed(~(logMinning_con))
vf3 <- varPower(form= ~(logMinning_con))
vf4 <- varExp(form =~(logMinning_con))
vf5 <- varConstPower(form =~(logMinning_con))
gmm1 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community),na.action = na.omit, data=df_vic)
gmm2 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community), weights =vf1Fixed , na.action = na.omit,data=df_vic)
gmm3 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community), weights = vf3,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000),  na.action = na.omit, data=df_vic)
gmm4 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community), weights = vf4,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000),  na.action = na.omit, data=df_vic)
gmm5 <- lme(logvichuna_density ~ logMinning_con, random=list(~1|community), weights = vf5,control = lmeControl(msMaxIter = 1000, msMaxEval = 1000),  na.action = na.omit, data=df_vic)
anova(gmm1, gmm2, gmm3,gmm4, gmm5)
dfvic_filtered <- df_vic[complete.cases(df_vic$logMinning_con), ]
fixef(gmm1)
ranef(gmm1)
dfvic_filtered$F0 <- fitted(gmm1, level = 0) # to get the fitted values for the fixed effects
dfvic_filtered$F1 <- fitted(gmm1, level = 1) # to get the fitted values for the random effects
# percentage of variance explained
lm1 <- lm(dfvic_filtered$F0 ~ dfvic_filtered$logvichuna_density)
summary(lm1)
###Plot the relationship
library(ggplot2)
library(effects)
Vic_logMining_con<-as.data.frame(effect("logMinning_con",gmm1, xlevels=10))
VIC_Min<-ggplot()+
  geom_point(aes(x=Minning_con, y=vichuna_density ,color=community),data=dfvic_filtered, pch=19, size=1.5)+
  geom_ribbon(data=Vic_logMining_con, aes(x=exp(logMinning_con),y=exp(fit),
                                           ymin=exp(lower), ymax=exp(upper)), alpha=0.2)+
  stat_smooth(data=Vic_logMining_con,aes(x=exp(logMinning_con),y=exp(fit)),size=1.4, color= "black")+
  xlab("Nr. mining concessions")+
  ylab("Vicunha density")+
  theme_bw()+
  theme(aspect.ratio = 1)+
  theme(legend.position = "none")
View(dfvic_filtered)
VIC <- qplot(logMinning_con, logvichuna_density, ylab = "log(Vicunha density)", xlab= "log(Nr. mining concessions)", colour=community, data=dfvic_filtered)
VIC<-VIC + theme_bw()+
  geom_line(aes(y=F0), size=2, colour='black') + # line for the fixed effect
  geom_line(aes(y=F1, group=community), size=1)+ 
  scale_color_manual(values=c("purple", "green","brown", "lightgray",  "orange", "darkgreen"),
                     limits = c("Puyo Puyo","Antaquilla", "Cololo", "Katantika","Nubepampa", "Agua_blanca"))+
  theme_bw()+
  theme(aspect.ratio = 1)+
  theme(legend.position = "none")
VIC
VIC_Min
Fig3b<-plot_grid(VIC,LIV,labels=c("A", "B"), ncol=2, nrow=1, rel_widths=c(1,1.3)) #d1<-plot_grid(d,e,f, rel_widths=c(0.8,0.8,1.2), labels=c("A","B", "C"), ncol=3, nrow=1)
Fig3b
ggsave("C:/Users/anaps/Desktop/Chapter management/Figures/Figure3MEM.jpeg", Fig3b, width = 10, height = 5, dpi=300)
