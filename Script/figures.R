
library("ggplot2")
library("qqplotr")
library("dplyr")
library("DescTools")
library("FSA")
library("PMCMRplus")

####################################################################################
### Figure 2: Comparing the areas covered by each of the models ################################################
## Proto3 vs Proto4 ####################################################################################

setwd("../Data")
dat_A <- read.csv("dat.csv", fileEncoding = 'UTF-8-BOM', header=T)
dat <- dat_A[which(dat_A$Category != "Total"),]

summary(dat$Area[dat$Category == "Proto3"])
summary(dat$Area[dat$Category == "negl"])
summary(dat$Area[dat$Category == "low"])
summary(dat$Area[dat$Category == "mod"])
summary(dat$Area[dat$Category == "high"])

### CHECKING FOR NORMALITY   They are not normal
par(mfrow=c(1,1))
qqnorm(dat$Area[dat$Category == "Proto3"], pch = 1, frame = FALSE)
qqline(dat$Area[dat$Category == "Proto3"], col = "steelblue", lwd = 2)
shapiro.test(dat$Area[dat$Category == "Proto3"])

qqnorm(dat$Area[dat$Category == "negl"], pch = 1, frame = FALSE)
qqline(dat$Area[dat$Category == "negl"], col = "steelblue", lwd = 2)
shapiro.test(dat$Area[dat$Category == "negl"])

qqnorm(dat$Area[dat$Category == "low"], pch = 1, frame = FALSE)
qqline(dat$Area[dat$Category == "low"], col = "steelblue", lwd = 2)
shapiro.test(dat$Area[dat$Category == "low"])

qqnorm(dat$Area[dat$Category == "mod"], pch = 1, frame = FALSE)
qqline(dat$Area[dat$Category == "mod"], col = "steelblue", lwd = 2)
shapiro.test(dat$Area[dat$Category == "mod"])

qqnorm(dat$Area[dat$Category == "high"], pch = 1, frame = FALSE)
qqline(dat$Area[dat$Category == "high"], col = "steelblue", lwd = 2)
shapiro.test(dat$Area[dat$Category == "high"])

### Running shapiro wilk to compare between Proto3 and Proto4

dat <- dat_A[which(dat_A$Category != "Total"),]

dat2a <- dat[which(dat$Category == "Proto3"),]
dat2 <- rbind(dat2a, dat[which(dat$Category == "high"),])
dat2$Category <- ifelse(dat2$Category == "high", "Proto4", "Proto3")

wilcox.test(dat2$Area[which(dat2$Category == "Proto3")], dat2$Area[which(dat2$Category == "Proto4")])
## p = 0.064

## Making plot comparing just Proto3 and Proto4 (FIGURE 1)

dat2$Category <- factor(dat2$Category, levels = c("Proto3", "Proto4"))   ##Need to reorder them for easier viewing
boxplot(dat2$Area ~ dat2$Category,  las=1, ylab="Area (million sq km)", xlab="", yaxt= "n", ylim= c(0,11000000), col=c("white", "grey"))
axis(2, seq(0,11000000, by = 1000000), seq(0, 11, by = 1), las=1)

points(jitter(rep(1, length(dat2$Area[dat2$Category == "Proto3"])), f=4), 
       dat2$Area[dat2$Category == "Proto3"], col="grey48", pch=16)
points(jitter(rep(2, length(dat2$Area[dat2$Category == "Proto4"])), f=4), 
       dat2$Area[dat2$Category == "Proto4"], col="grey48", pch=16)

## Plot comparing just uncertainty levels in Proto4 (FIGURE 2)

dat3 <- dat[which(dat$Category != "Proto3"),]
m1<-kruskal.test(Area ~ Category, data=dat3)
print(m1)    #p=0.001

posthocs1<-dscfAllPairsTest(Area ~ Category, data=dat3)
print(posthocs1)   # negl -> high :: A, A, B, B 

dat3$Category <- factor(dat3$Category, levels = c("negl", "low", "mod", "high"))   ##Need to reorder them for easier viewing
boxplot(dat3$Area ~ dat3$Category,  las=1, ylab="Area (million sq km)", xlab="", yaxt= "n", ylim= c(0,11000000), outline=FALSE)
axis(2, seq(0,11000000, by = 1000000), seq(0, 11, by = 1), las=1)
text(x=1, y=10500000, labels = "A")
text(x=2, y=10500000, labels = "A")
text(x=3, y=10500000, labels = "B")
text(x=4, y=10500000, labels = "B")

points(jitter(rep(1, length(dat3$Area[dat3$Category == "negl"])), f=4), 
       dat3$Area[dat3$Category == "negl"], col="grey48", pch=16)
points(jitter(rep(2, length(dat3$Area[dat3$Category == "low"])), f=4), 
       dat3$Area[dat3$Category == "low"], col="grey48", pch=16)
points(jitter(rep(3, length(dat3$Area[dat3$Category == "mod"])), f=4), 
       dat3$Area[dat3$Category == "mod"], col="grey48", pch=16)
points(jitter(rep(4, length(dat3$Area[dat3$Category == "high"])), f=4), 
       dat3$Area[dat3$Category == "high"], col="grey48", pch=16)

### Comparing area differences between the two models
area_diff <- dat$Area[dat$Category == "Proto3"] - dat$Area[dat$Category == "high"]

boxplot(area_diff, las=1, ylab="Area (million sq km)", xlab="", yaxt = "n", ylim = c(0, 2500000), col= c("white", "grey"), outline=FALSE)
axis(2, seq(0,2500000, by = 1000000), seq(0, 2, by = 1), las=1)
points(jitter(rep(1, length(area_diff)), f=4), 
       area_diff, col="grey48", pch=16)

####################################################################################
### Figure 3: Comparing accuracies of each model ################################################
## Proto3 vs Proto4 ####################################################################################

## Making plot comparing just Proto3 and Proto4 (FIGURE 3)

summary(dat$Percent_Counties[dat$Category == "Proto3"])
summary(dat$Percent_Counties[dat$Category == "negl"])
summary(dat$Percent_Counties[dat$Category == "low"])
summary(dat$Percent_Counties[dat$Category == "mod"])
summary(dat$Percent_Counties[dat$Category == "high"])

wilcox.test(dat2$Percent_Counties[which(dat2$Category == "Proto3")], dat2$Percent_Counties[which(dat2$Category == "Proto4")])
## p = 0.056

dat2$Category <- factor(dat2$Category, levels = c("Proto3", "Proto4"))   ##Need to reorder them for easier viewing
boxplot(dat2$Percent_Counties ~ dat2$Category,  las=1, ylab="Accuracy (%)", xlab="", yaxt= "n", ylim= c(0.9,1), col=c("white", "grey"), outline=FALSE)
axis(2, seq(0.9,1, by = 0.02), seq(90, 100, by = 2), las=1)

points(jitter(rep(1, length(dat2$Percent_Counties[dat2$Category == "Proto3"])), f=4), 
       dat2$Percent_Counties[dat2$Category == "Proto3"], col="grey48", pch=16)
points(jitter(rep(2, length(dat2$Percent_Counties[dat2$Category == "Proto4"])), f=4), 
       dat2$Percent_Counties[dat2$Category == "Proto4"], col="grey48", pch=16)

## Plot comparing just uncertainty levels in Proto4 (FIGURE 2)

m2<-kruskal.test(Percent_Counties ~ Category, data=dat3)
print(m2)   #p < 0.001

posthocs2<-dscfAllPairsTest(Percent_Counties ~ Category, data=dat3)
print(posthocs2)   # negl -> high :: A, A, B, B 

dat3$Category <- factor(dat3$Category, levels = c("negl", "low", "mod", "high"))   ##Need to reorder them for easier viewing
boxplot(dat3$Percent_Counties ~ dat3$Category,  las=1, ylab="Accuracy (%)", xlab="", yaxt= "n", ylim= c(0,1), outline=FALSE)
axis(2, seq(0,1, by = 0.1), seq(0, 100, by = 10), las=1)

text(x=1, y=0.1, labels = "A")
text(x=2, y=0.1, labels = "A")
text(x=3, y=0.1, labels = "B")
text(x=4, y=0.1, labels = "B")

points(jitter(rep(1, length(dat3$Percent_Counties[dat3$Category == "negl"])), f=4), 
       dat3$Percent_Counties[dat3$Category == "negl"], col="grey48", pch=16)
points(jitter(rep(2, length(dat3$Percent_Counties[dat3$Category == "low"])), f=4), 
       dat3$Percent_Counties[dat3$Category == "low"], col="grey48", pch=16)
points(jitter(rep(3, length(dat3$Percent_Counties[dat3$Category == "mod"])), f=4), 
       dat3$Percent_Counties[dat3$Category == "mod"], col="grey48", pch=16)
points(jitter(rep(4, length(dat3$Percent_Counties[dat3$Category == "high"])), f=4), 
       dat3$Percent_Counties[dat3$Category == "high"], col="grey48", pch=16)

##################################################################
##### FIGURE 4 #########
#####################################################################

dat_E <- read.csv("expanded_dat.csv", fileEncoding = 'UTF-8-BOM', header=T)

dat_uncertainty <- dat_E[which(dat_E$Category == "Proto4_low" | dat_E$Category == "Proto4_mod" | dat_E$Category == "Proto4_high"),]

#Area
kruskal.test(Area_change_band ~ Category, data=dat_uncertainty)

#Accuracy
kruskal.test(Accuracy_change_band ~ Category, data=dat_uncertainty)

# % Area
kruskal.test(Area_percent_change_band ~ Category, data=dat_uncertainty)

dat_uncertainty_area <- data.frame(species <- dat_uncertainty$Species,
                                   category <- dat_uncertainty$Category,
                                   percent_change <- dat_uncertainty$Area_percent_change_band,
                                   note <- "Area")

dat_uncertainty_area$species <- dat_uncertainty_area$species....dat_uncertainty.Species
dat_uncertainty_area$category <- dat_uncertainty_area$category....dat_uncertainty.Category
dat_uncertainty_area$percent_change <- dat_uncertainty_area$percent_change....dat_uncertainty.Area_percent_change_band
dat_uncertainty_area$note <- dat_uncertainty_area$note.....Area.
dat_uncertainty_area[,c(1,2,3,4)] <- list(NULL)

dat_uncertainty_accuracy <- data.frame(species <- dat_uncertainty$Species,
                                       category <- dat_uncertainty$Category,
                                       percent_change <- dat_uncertainty$Accuracy_percent_change_band,
                                       note <- "Accuracy")

dat_uncertainty_accuracy$species <- dat_uncertainty_accuracy$species....dat_uncertainty.Species
dat_uncertainty_accuracy$category <- dat_uncertainty_accuracy$category....dat_uncertainty.Category
dat_uncertainty_accuracy$percent_change <- dat_uncertainty_accuracy$percent_change....dat_uncertainty.Accuracy_percent_change_band
dat_uncertainty_accuracy$note <- dat_uncertainty_accuracy$note.....Accuracy.
dat_uncertainty_accuracy[,c(1,2,3,4)] <- list(NULL)

dat_figure <- rbind(dat_uncertainty_area, dat_uncertainty_accuracy)
dat_figure$category <- factor(dat_figure$category, levels = c("Proto4_low", "Proto4_mod", "Proto4_high"))

cols <- rainbow(3, s = 0.5)
par(mar=c(5.1, 4.1,4.1, 6.1), xpd=TRUE)
boxplot(percent_change  ~ category + note, data = dat_figure,
        at = c(1:3, 5:7), col = "grey48",
        names = c("", "Area", "", "", "Accuracy", ""), xaxs = FALSE,
        las=1, ylab="% Change", xlab="", yaxt= "n", ylim= c(0,0.9), outline = FALSE)

axis(2, seq(0,0.9, by = 0.1), seq(0, 90, by = 10), las=1)

legend("top", inset=c(0,-0.25), fill = cols, legend = c("low","mod","high"), horiz = T, cex=0.75)


##################################################################
##### FIGURE 5 #########
#####################################################################

dat_intermediary <- dat_E[which(dat_E$Category == "Proto3_elev" | dat_E$Category == "Proto3_kgc" | dat_E$Category == "Proto3_phz"),]

#Area
kruskal.test(Area_change ~ Category, data=dat_intermediary)

#Accuracy
kruskal.test(Accuracy_change_Proto3 ~ Category, data=dat_intermediary)

# % Area
kruskal.test(Area_percent_change ~ Category, data=dat_intermediary)

# % Accuracy
kruskal.test(Accuracy_percent_change_Proto3 ~ Category, data=dat_intermediary)

# Raw area differences are significant. Making plots

dat_intermediary$Category <- factor(dat_intermediary$Category, levels = c("Proto3_elev", "Proto3_kgc", "Proto3_phz"))   ##Need to reorder them for easier viewing
boxplot(dat_intermediary$Area_change ~ dat_intermediary$Category,  las=1, ylab="Change in Area (million sq km)", xlab="", yaxt= "n", ylim= c(0,2200000), outline = FALSE)
axis(2, seq(0,2000000, by = 500000), seq(0, 2, by = 0.5), las=1)

points(jitter(rep(1, length(dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_elev"])), f=4), 
       dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_elev"], col="grey48", pch=16)
points(jitter(rep(2, length(dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_kgc"])), f=4), 
       dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_kgc"], col="grey48", pch=16)
points(jitter(rep(3, length(dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_phz"])), f=4), 
       dat_intermediary$Area_change[dat_intermediary$Category == "Proto3_phz"], col="grey48", pch=16)

text(x=1, y=2000000, labels = "A")
text(x=2, y=2000000, labels = "B")
text(x=3, y=2000000, labels = "A")

#### SUPPLEMENTAL FIGURES ##############333
### Figure 2 ###############################################################3

dat_E <- read.csv("expanded_dat.csv", fileEncoding = 'UTF-8-BOM', header=T)

######### AREAS #####################

summary(dat_E$Area[dat_E$Category == "Proto3"])
summary(dat_E$Area[dat_E$Category == "Proto3_elev"])
summary(dat_E$Area[dat_E$Category == "Proto3_kgc"])
summary(dat_E$Area[dat_E$Category == "Proto3_phz"])
summary(dat_E$Area[dat_E$Category == "Proto4_high"])

wilcox.test(dat_E$Area[dat_E$Category == "Proto3"], dat_E$Area[dat_E$Category == "Proto3_elev"])
wilcox.test(dat_E$Area[dat_E$Category == "Proto3"], dat_E$Area[dat_E$Category == "Proto3_kgc"])
wilcox.test(dat_E$Area[dat_E$Category == "Proto3"], dat_E$Area[dat_E$Category == "Proto3_phz"])
wilcox.test(dat_E$Area[dat_E$Category == "Proto3"], dat_E$Area[dat_E$Category == "Proto4_high"])   #Wilcoxon giving me a warning message?

m3a <- kruskal.test(Area ~ Category, data=dat_E)
print(m3a)   #p=0.3103

posthocs3a<-dscfAllPairsTest(Percent_Counties ~ Category, data=dat_E)
print(posthocs3a)   # negl -> high :: A, A, A, A

#Removing Proto3 and Proto4 to compare just the intermediaries
dat_E_intermediary <- dat_E[which(dat_E$Category == "Proto3_elev" | dat_E$Category == "Proto3_kgc" | dat_E$Category == "Proto3_phz"),]

m3I <- kruskal.test(Area ~ Category, data=dat_E_intermediary)
print(m3I)   #p=0.7627

m3I2 <- kruskal.test(Percent_Counties ~ Category, data=dat_E_intermediary)
print(m3I2)   #p=0.7627

dat_E_intermediary2 <- dat_E[which(dat_E$Category == "Proto3_elev" | dat_E$Category == "Proto3_kgc" | dat_E$Category == "Proto3_phz" | dat_E$Category == "Proto3" | dat_E$Category =="Proto4_high"),]
m3Ia <- kruskal.test(Area ~ Category, data=dat_E_intermediary2)
print(m3Ia) 

m3Ia2 <- kruskal.test(Percent_Counties ~ Category, data=dat_E_intermediary2)
print(m3Ia2) 

dat_E$Category <- factor(dat_E$Category, levels = c("Proto3", "Proto3_elev", "Proto3_kgc", "Proto3_phz", "Proto4_high"))   ##Need to reorder them for easier viewing
boxplot(dat_E$Area ~ dat_E$Category,  las=1, ylab="Area (million sq km)", xlab="", yaxt= "n", xaxt="n", ylim= c(0,10000000), col=c("white", "grey", "grey", "grey", "grey"), outline=FALSE)
axis(2, seq(0,10000000, by = 1000000), seq(0, 10, by = 1), las=1)
axis(1, seq(1,5), by = 1, c("Proto3", "Proto3+elev", "Proto3+kgc", "Proto3+phz", "Proto4"))

points(jitter(rep(1, length(dat_E$Area[dat_E$Category == "Proto3"])), f=4), 
       dat_E$Area[dat_E$Category == "Proto3"], col="grey48", pch=16)
points(jitter(rep(2, length(dat_E$Area[dat_E$Category == "Proto3_elev"])), f=4), 
       dat_E$Area[dat_E$Category == "Proto3_elev"], col="grey48", pch=16)
points(jitter(rep(3, length(dat_E$Area[dat_E$Category == "Proto3_kgc"])), f=4), 
       dat_E$Area[dat_E$Category == "Proto3_kgc"], col="grey48", pch=16)
points(jitter(rep(4, length(dat_E$Area[dat_E$Category == "Proto3_phz"])), f=4), 
       dat_E$Area[dat_E$Category == "Proto3_phz"], col="grey48", pch=16)
points(jitter(rep(5, length(dat_E$Area[dat_E$Category == "Proto4_high"])), f=4), 
       dat_E$Area[dat_E$Category == "Proto4_high"], col="grey48", pch=16)

# Percent area ##################
dat_E$Category <- factor(dat_E$Category, levels = c("Proto3", "Proto3_elev", "Proto3_kgc", "Proto3_phz", "Proto4_high"))   ##Need to reorder them for easier viewing
boxplot(dat_E$Percent_Counties ~ dat_E$Category,  las=1, ylab="Accuracy (%)", xlab="", yaxt= "n", xaxt="n", ylim= c(0.95,1), col=c("white", "grey", "grey", "grey", "grey"), outline=FALSE)
axis(2, seq(0.95,1, by = 0.01), seq(95, 100, by = 1), las=1)
axis(1, seq(1,5), by = 1, c("Proto3", "Proto3+elev", "Proto3+kgc", "Proto3+phz", "Proto4"))

points(jitter(rep(1, length(dat_E$Percent_Counties[dat_E$Category == "Proto3"])), f=4), 
       dat_E$Percent_Counties[dat_E$Category == "Proto3"], col="grey48", pch=16)
points(jitter(rep(2, length(dat_E$Percent_Counties[dat_E$Category == "Proto3_elev"])), f=4), 
       dat_E$Percent_Counties[dat_E$Category == "Proto3_elev"], col="grey48", pch=16)
points(jitter(rep(3, length(dat_E$Percent_Counties[dat_E$Category == "Proto3_kgc"])), f=4), 
       dat_E$Percent_Counties[dat_E$Category == "Proto3_kgc"], col="grey48", pch=16)
points(jitter(rep(4, length(dat_E$Percent_Counties[dat_E$Category == "Proto3_phz"])), f=4), 
       dat_E$Percent_Counties[dat_E$Category == "Proto3_phz"], col="grey48", pch=16)
points(jitter(rep(5, length(dat_E$Percent_Counties[dat_E$Category == "Proto4_high"])), f=4), 
       dat_E$Percent_Counties[dat_E$Category == "Proto4_high"], col="grey48", pch=16)

########### FIGURE 3 #########################################3
dat_E <- read.csv("expanded_dat.csv", fileEncoding = 'UTF-8-BOM', header=T)

dat_uncertainty <- dat_E[which(dat_E$Category == "Proto4_low" | dat_E$Category == "Proto4_mod" | dat_E$Category == "Proto4_high"),]

#Area
kruskal.test(Area_change_band ~ Category, data=dat_uncertainty)

#Accuracy
kruskal.test(Accuracy_change_band ~ Category, data=dat_uncertainty)

# % Area
kruskal.test(Area_percent_change_band ~ Category, data=dat_uncertainty)

dat_uncertainty$Category <- factor(dat_uncertainty$Category, levels = c("Proto4_low", "Proto4_mod", "Proto4_high"))   ##Need to reorder them for easier viewing
boxplot(dat_uncertainty$Area_percent_change_band ~ dat_uncertainty$Category,  las=1, ylab="% Change", xlab="", yaxt= "n", ylim= c(0,1.1), outline = FALSE)
axis(2, seq(0,1, by = 0.1), seq(0, 100, by = 10), las=1)

points(jitter(rep(1, length(dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_low"])), f=4), 
       dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_low"], col="grey48", pch=16)
points(jitter(rep(2, length(dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_mod"])), f=4), 
       dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_mod"], col="grey48", pch=16)
points(jitter(rep(3, length(dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_high"])), f=4), 
       dat_uncertainty$Area_percent_change_band[dat_uncertainty$Category == "Proto4_high"], col="grey48", pch=16)

# % Accuracy
kruskal.test(Accuracy_percent_change_band ~ Category, data=dat_uncertainty)

m_uncertainty <- kruskal.test(Accuracy_percent_change_band ~ Category, data=dat_uncertainty)
print(m_uncertainty)   #p=0.04847
kruskalmc(Accuracy_percent_change_band ~ Category, data=dat_uncertainty, probs=0.05)

dat_uncertainty$Category <- factor(dat_uncertainty$Category, levels = c("Proto4_low", "Proto4_mod", "Proto4_high"))   ##Need to reorder them for easier viewing
boxplot(dat_uncertainty$Accuracy_percent_change_band ~ dat_uncertainty$Category,  las=1, ylab="% Change", xlab="", yaxt= "n", ylim= c(0,0.25), outline = FALSE)
axis(2, seq(0,0.25, by = 0.1), seq(0, 25, by = 10), las=1)

points(jitter(rep(1, length(dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_low"])), f=4), 
       dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_low"], col="grey48", pch=16)
points(jitter(rep(2, length(dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_mod"])), f=4), 
       dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_mod"], col="grey48", pch=16)
points(jitter(rep(3, length(dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_high"])), f=4), 
       dat_uncertainty$Accuracy_percent_change_band[dat_uncertainty$Category == "Proto4_high"], col="grey48", pch=16)

text(x=1, y=0.21, labels = "A")
text(x=2, y=0.21, labels = "AB")
text(x=3, y=0.21, labels = "B")
