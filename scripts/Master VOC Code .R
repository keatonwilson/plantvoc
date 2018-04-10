##Final Field VOC Code from Dissertation Work - Updated and modified from existed code on 11/20/17
library(car)

library(tidyverse)
library(gdata)
library(MASS)
library(stringr)

#Reading in the data
volmaster = read.csv("/Users/KeatonWilson/Documents/Projects/plantvoc/data/VolatilesMasterEdits(Master).csv")
colnames(volmaster) = c("Peak#", "RetTime", "Letter", "Type", "Width", "Area", "StartTime", 
                        "EndTime", "SampleNumber", "Treatment", "File", "LeafArea", "Compound", 
                        "TetralinCorrected", "LeafAreaCorrected", "PlantVolume", "WaterStatus", "Temp", 
                        "MaxTemp", "Humidity", "X", "Site", "Date")

#Transforming to Julian Date
jul.date = strptime(volmaster$Date, "%m/%d/%Y")$yday+1
volmaster = cbind(volmaster[,-c(21,23)], jul.date)
colnames(volmaster) = c("Peak#", "RetTime", "Letter", "Type", "Width", "Area", "StartTime", "EndTime", 
                        "SampleNumber", "Treatment", "File", "LeafArea", "Compound", "TetralinCorrected", 
                        "LeafAreaCorrected", "PlantVolume", "WaterStatus", "Temp", "MaxTemp", "Humidity", 
                        "Site", "Date")


#setting up the alphabet these are the "bins" #Modified alphabet from original. This alphabet is different from lab alphabet
alphabet = c(0, 8.05, 10.19, 12.7, 13.13, 13.26, 13.3, 13.46, 13.5, 13.57, 13.65, 14.05, 14.1, 
             14.25, 14.3, 14.33, 14.39, 14.65, 14.85, 14.9, 15.00, 15.3, 17.01, 17.9, 18.66, 18.72, 
             18.95, 19.2, 19.3, 19.5, 20.47, 20.51, 20.69, 20.73, 20.83, 20.97, 21.02, 21.55, 21.64, 21.77, 
             21.85, 22.3, 22.31, 22.49, 23.1, 25)

#"chemical" letters
vollabels = c("E-Z-hexenal", "m-xylene, 2-ethyl", "Z-3-Hexenyl Acetate", "Limonene", "Benzyl alcohol", 
              "Trans-B-Ocimene", "p-diethylbenzene1", "Ethylhexanol", "Ocimene", "6-ethyl-2-methyloctane", 
              "2-bromo-octane", "1,3-diethyl benzene", "5-ethyl-2,2,3-trimethylheptane", "2,3,6,7-tetramethyloctane", 
              "p-diethylbenzene", "a-pinene", "3,6-dimethylundecane", "Z-3-Hexenyl Propionate", "B-Linalool", "Nonanal",
              "2,2,7,7-tetramethyl octane", "Methyl Salicylate", "Tetralin", "Cuminol", "2,4-dimethylacteophenone", 
              "p-acetylethylbenzene", "2-dodecanol", "p-acetylethyl phenone", "4-ethyl benzoic acid, 2-ethylhexylester", 
              "1-pentadecene", "2-methyldecane", "2,6,10-trimethyl tetradecane", "3-ethyloctane", "Butylated hydroxytoluene", 
              "DEET", "propanoic acid, 2-methyl-1-(1,1,dimethylethyl)-2-methyl-1, 3-propamedielester", "Heptadecane", 
              "2-ethylhexylester benzoic acid", "cis-Z-a-bisabolene epoxide", "ethylhexyl benzoate", "didodecyl phthalate", 
              "Diisobutyl phthalate", "2-ethylhexylsalicylate", "Homomenthyl salicylate", "Oxybenzone")


#Using the cut function to bin the data into each group
x = cut(volmaster$RetTime, breaks = alphabet, labels = vollabels)

#binding the newly created alphabet vector back onto the original dataframe
volmaster = cbind(volmaster[,-3], x)
colnames(volmaster) = c("Peak#", "RetTime", "Type", "Width", "Area", "StartTime", "EndTime", "SampleNumber", "Treatment", 
                        "File", "LeafArea", "Compound", "TetralinCorrected", "LeafAreaCorrected", "PlantVolume", "WaterStatus", 
                        "Temp", "MaxTemp", "Humidity", "Site", "Date", "CompoundName")
#Reordering the dataframe to that the letter column is back where it should be. 
volmaster = volmaster[,c("Peak#", "RetTime", "CompoundName", "Type", "Width", "Area", "StartTime", "EndTime", "SampleNumber", 
                         "Treatment", "File", "LeafArea", "TetralinCorrected", "LeafAreaCorrected", "PlantVolume", "WaterStatus", 
                         "Temp", "MaxTemp", "Humidity", "Site", "Date")]

#Getting rid of any standard runs in the data
volmaster1 = volmaster[which(volmaster$SampleNumber != "1 ppm" | volmaster$SampleNumber != "10 ppm" |
                               volmaster$SampleNumber != "100 ppm" | volmaster$SampleNumber != "1000 ppm" |
                               volmaster$SampleNumber != "100ppm" | volmaster$SampleNumber != " 1ppm" | volmaster$Treatment != "STD"),]

volmaster = volmaster[volmaster$Treatment != "STD",]

#Coercing blanks into NAs
volmaster = volmaster[!(is.na(volmaster$Treatment) | volmaster$Treatment ==""),]

#Correcting for leaf area
LeafCorrectedArea = ifelse(volmaster$CompoundName == "Tetralin", volmaster$Area, volmaster$Area/volmaster$LeafArea)
volmaster = cbind(LeafCorrectedArea, volmaster)

library(tidyverse)
#Cutting out compounds
badvocs = c("1,3-diethyl benzene", "p-diethylbenzene", vollabels[36], "DEET", "2-ethylhexylester benzoic acid", "p-acetylethylbenzene", "didodecyl phthalate",
            "diisobutyl phthalate", "toluene")
patterns = c("benzene", "DEET", vollabels[36], "benzoic acid", "phthalate", "Oxybenzone", "propanoic acid", " butylated hydroxytoluene", "NA", 
             "2,6,10-trimethyl tetradecane", "5-ethyl-2,2,3-trimethylheptane", "2,3,6,7-tetramethyloctane", "2,2,7,7-tetramethyl octane", "2-bromo-octane")
volmaster$CompoundName = as.character(volmaster$CompoundName)
volmaster = volmaster[!grepl(paste(patterns, collapse = "|"), volmaster$CompoundName),]

volmaster = volmaster %>%
  filter(!is.na(CompoundName), CompoundName != "Butylated hydroxytoluene", CompoundName != "ethylhexyl benzoate", CompoundName != "3,6-dimethylundecane")


#Widening the data set
longvolmaster2 = volmaster[,c("CompoundName", "SampleNumber", "LeafCorrectedArea", "Treatment", "Humidity", "PlantVolume", "Temp", "WaterStatus", "Date", "Site", "MaxTemp")]
wide2 = reshape(longvolmaster2, v.names = c("LeafCorrectedArea"), idvar = "SampleNumber", timevar = "CompoundName", direction = "wide")



newalphabet = gsub("LeafCorrectedArea.", "",names(wide2)[10:30])
colnames(wide2) = c("SampleNumber", "Treatment", "Humidity", "PlantVolume", "Temp", "WaterStatus", "Date", "Site", "MaxTemp", newalphabet)

#Coercing all NAs to 0s for chemical amounts
wide2[,10:30][is.na(wide2[,10:30])] = 0


#Ok, wide 2 looks pretty good now.
glimpse(wide2)


#Summing Areas a la Ray Calloway
sumdat = apply(wide2[,-c(1:10)], 1, function(x) sum(x))
moresumdat = cbind(sumdat, wide2[,1:10])
moresumdat = transform(moresumdat, sumdat = (sumdat*0.0175)/7)
moresumdat = transform(moresumdat, Temp = (Temp-32)*(5/9))

#Samples 47 and 45 look like outliers, let's remove them.
moresumdatedit = subset(moresumdat, moresumdat$SampleNumber != "45" & moresumdat$SampleNumber != "47")
moresumdatedit$MaxTemp[which(moresumdatedit$MaxTemp == 0)] = NA
moresumdateditnoNA = moresumdatedit[complete.cases(moresumdatedit),]


#Let's look at some models of total VOC output and our different factors
herblm = lm(sumdat ~ Treatment, data = moresumdatedit)
lm0 = lm(sumdat ~ 1, data = moresumdatedit)
lm1 = lm(sumdat ~Temp*Humidity, data = moresumdatedit)
lm2 = lm(sumdat ~ Temp, data = moresumdatedit)
lm3 = lm(sumdat ~ Temp+Humidity, data = moresumdatedit)
lm4 = lm(sumdat ~ Humidity, data = moresumdatedit)

summary(herblm)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

AIC(lm0)
AIC(lm2)
AIC(lm4)
AIC(lm3)
AIC(lm1)

#The gist here is that the best model is lm1 - Temp and Humidity with interaction. 
###Ok, so figures...
library(ggplot2)

Fig.2a = ggplot(moresumdatedit, aes(x = Temp, y =sumdat)) +
  geom_jitter(size = 4, alpha = 0.6, width = 0.05) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  geom_smooth(method=lm, colour = "black") +
  xlab("Mean 3 Day Temperature (ºC)") +
  ylab(bquote('Area Specific Emission Rate ('*µL ~ Hour^-1 ~ cm^-2*')')) +
  geom_hline(yintercept = 0, size = 0.25)

Fig.2b = ggplot(moresumdatedit, aes(x = Humidity, y =sumdat)) +
  geom_jitter(size = 4, alpha = 0.6, width = 0.05) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  geom_smooth(method="lm", colour = "black", formula = y ~ poly(x,2)) +
  xlab("Mean 3 Day Relative Humidity (%)") +
  ylab(bquote('Area Specific Emission Rate ('*µL ~ Hour^-1 ~ cm^-2*')')) +
  geom_hline(yintercept = 0, size = 0.25)



##Don't run this below - your file path will be different
ggsave(filename = "Fig.2a.pdf", plot = Fig.2a,
       path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/",
       width = 8, 
       height = 7,
       units = "in")
ggsave(filename = "Fig.2b.pdf", plot = Fig.2b,
       path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/",
      width = 8, 
       height = 7,
      units = "in")



##Normalizing and Aitchison Transforming for the rest of the analysis

#Normalizing across rows for all data
#Building the normalizing function
normalize <- function(x) { 
  x <- sweep(x, 1, apply(x, 1, min)) 
  sweep(x, 1, apply(x, 1, max), "/") 
} 
#Applying the normalize function
normwide = normalize(wide2[,-c(1:9)])
normwide[normwide == "0"] = 0.00000001

#Building the Aitchison Function
aitchison1 = function(x){
  x = log(x/normwide$Tetralin)
}
aitchison2 = function(x){
  x = log(x/goodnormwide$extracted)
}

#Applying the Aitchison Transformation to all data
aitchisontrnsfrmd = apply(normwide, 2, aitchison1)
aitchisontrnsfrmd = cbind(wide2[,1:9], aitchisontrnsfrmd)
aitchisontrnsfrmd = aitchisontrnsfrmd[!(is.na(aitchisontrnsfrmd$Treatment) | aitchisontrnsfrmd$Treatment ==""),]
aitchisontrnsfrmd = drop.levels(aitchisontrnsfrmd)
aitchisontrnsfrmd[aitchisontrnsfrmd == 0] = NA

glimpse(aitchisontrnsfrmd)
#Getting rid of tetralin
aitchisontrnsfrmd = aitchisontrnsfrmd[,-10]


#MANOVA Exploration
#Generating complete cases (because MANOVA doesn't like NAs, apparently)
tempgood = aitchisontrnsfrmd[complete.cases(aitchisontrnsfrmd$Temp) == "TRUE",]
humgood = aitchisontrnsfrmd[complete.cases(aitchisontrnsfrmd$Humidity) == "TRUE",]
bothgood = tempgood[complete.cases(tempgood$Humidity) == "TRUE",]

#Still some NAs
sum(is.na(bothgood))

#Let's find them
apply(bothgood, 2, function(x) sum(is.na(x)))

#They're in waterStatus
glimpse(aitchisontrnsfrmd)

Y = cbind(aitchisontrnsfrmd[-c(1:9)])
Z = bothgood[,-c(1:9)]
V = cbind(humgood[,-c(1:9)])
X = tempgood[,10:29]
length(names(aitchisontrnsfrmd))

all_good = aitchisontrnsfrmd[complete.cases(aitchisontrnsfrmd),]
sum(is.na(all_good))


model1 = lm(as.matrix(bothgood[,-c(1:9)]) ~ 1, data = bothgood)
model2 = manova(as.matrix(bothgood[,-c(1:9)]) ~ Temp+Humidity+Treatment, data = bothgood)
summary(model2)
#Let's see if there some shitty factors that I can remove
uniqexplor = apply(Z, 2, unique)
bothgoodnew = bothgood


######GOLDEN BIT OF CODE!##################
#So the best models include Temp and Humidity, and the lowest AIC score is one with Temp*Humidity*WaterStatus

model0 = manova(as.matrix(all_good[,-c(1:9)]) ~ 1, data = all_good)
extractAIC(model0)
summary(model0, tol = 0)
model1 = manova(as.matrix(all_good[,-c(1:9)]) ~ Treatment, data = all_good)
extractAIC(model1)
summary(model1, tol = 0)
model2 = manova(as.matrix(all_good[,-c(1:9)]) ~ Temp, data = all_good)
extractAIC(model2)
summary(model2, tol = 0)
model3 = manova(as.matrix(all_good[,-c(1:9)]) ~ Temp+Humidity, data = all_good)
extractAIC(model3)
summary(model3, tol = 0)
model4 = manova(as.matrix(all_good[,-c(1:9)]) ~ Temp*Humidity, data = all_good)
extractAIC(model4)
summary(model4, tol = 0)
model5 = manova(as.matrix(all_good[,-c(1:9)]) ~ Temp*Humidity+WaterStatus, data = all_good)
summary(model5, tol = 0)
extractAIC(model5)
model6 = manova(as.matrix(all_good[,-c(1:9)]) ~ Temp*Humidity*WaterStatus, data = all_good)
extractAIC(model6)
summary(model6, tol = 0)

#Best model is the last one - most complex, but lowest AIC.

comp_df_index = complete.cases(aitchisontrnsfrmd[,-c(1:9)])
comp_df = aitchisontrnsfrmd[comp_df_index,]
#PCA Stuff
pca.model = prcomp(comp_df[,-c(1:9)])
summary(pca.model)

PC1 = predict(pca.model)[,1]
PC2 = predict(pca.model)[,2]
PC3 = predict(pca.model)[,3]
PC4 = predict(pca.model)[,4]
PC5 = predict(pca.model)[,5]
PC6 = predict(pca.model)[,6]
PC7 = predict(pca.model)[,7]
PC8 = predict(pca.model)[,8]

plot(comp_df$Treatment, PC1)
plot(comp_df$Treatment, PC2)
plot(comp_df$Treatment, PC3)

#Looks like none of the PCAs correlate with herbivory treatments, just like MANOVA. 

#Let's look at other variables
lm1 = lm(PC1 ~ Humidity, data = comp_df)

summary(lm1)
##OK PC1 loads with humidity, good p-value, bad R2, but whatever. Let's make a nice ggplot 
##PC1 no longer loads with humidity

Fig.2d = ggplot(all_good, aes(x = Humidity, y =PC1)) +
  geom_jitter(size = 4, alpha = 0.6, width = 0.5) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  geom_smooth(method=lm, colour = "black") +
  xlab("Mean 3 Day Relative Humidity (%)") +
  ylab("PC1")

#Don't run this on your computer, the path will be different
ggsave(file = "Fig.2d.pdf", plot = Fig.2d,
       path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/",
       width = 7, 
       height = 6,
       units = "in")



lm2 = lm(PC2 ~ Humidity, data = comp_df)
summary(lm2)

#Nice, strong relationship between temperature and PC2 - Plot!
Fig.2c = ggplot(comp_df, aes(x = ((Temp-32)*(5/9)), y =PC2)) +
  geom_jitter(size = 4, alpha = 0.6, width = 0.5) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  geom_smooth(method=lm, colour = "black") +
  xlab("Mean 3 Day Relative Temperature (ºC)") +
  ylab("PC2")

#Don't run
ggsave(file = "Fig.2c.pdf", plot = Fig.2c, 
      path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/",
       width = 7, 
       height = 6,
       units = "in")

#Let's check water status
lm3 = lm(PC3 ~ WaterStatus, data = comp_df)
summary(lm3)



#Discriminant Analysis
lda.model = lda(aitchisontrnsfrmd$Treatment[-13]~., aitchisontrnsfrmd[-13,-c(1:9)])
summary(lda.model)
lda.model
#Assessing the LDA - have to get rid of row 13 first, it's blank and fucking stuff up.

table.1 = table(Original = aitchisontrnsfrmd$Treatment[-13], Predicted = predict(lda.model)$class)
sum(diag(prop.table(table.1)))
diag(prop.table(table.1,1))


datVOC<-data.frame(Treatment=predict(lda.model)$class,predict(lda.model)$x) 
library(ellipse)
dat_ell <- data.frame() 
for(i in as.factor(levels(datVOC$Treatment))){ 
  dat_ell <- rbind(dat_ell, cbind(as.data.frame(with(datVOC[datVOC$Treatment==i,], ellipse(cor(ld1, ld2), 
                                                                                           scale=c(sd(ld1),sd(ld2)), 
                                                                                           centre=c(mean(ld1),mean(ld2))))),Treatment=i)) 
} 
###This doesn't work, let's try and split things manually maybe?
datVOCcontrol = subset(datVOC, datVOC$Treatment == "Control")
datVOClow = subset(datVOC, datVOC$Treatment == "Low")
datVOChigh = subset(datVOC, datVOC$Treatment == "High")

ell_control = as.data.frame(ellipse(cor(datVOCcontrol$LD1, datVOCcontrol$LD2), scale = c(sd(datVOCcontrol$LD1), sd(datVOCcontrol$LD2)), centre = c(mean(datVOCcontrol$LD1), mean(datVOCcontrol$LD2))))
ell_control$Treatment = c("Control")
ell_low = as.data.frame(ellipse(cor(datVOClow$LD1, datVOClow$LD2), scale = c(sd(datVOClow$LD1), sd(datVOClow$LD2)), centre = c(mean(datVOClow$LD1), mean(datVOClow$LD2))))
ell_low$Treatment = c("Low")
ell_high = as.data.frame(ellipse(cor(datVOChigh$LD1, datVOChigh$LD2), scale = c(sd(datVOChigh$LD1), sd(datVOChigh$LD2)), centre = c(mean(datVOChigh$LD1), mean(datVOChigh$LD2))))
ell_high$Treatment = c("High")

ell_master = rbind(ell_control, ell_low, ell_high)

Fig.3  = ggplot(datVOC, aes(x=LD1, y=LD2, color = Treatment) ) + 
  geom_point(size = 6, aes(shape = Treatment), alpha = 0.5) +
  theme_classic() +
  geom_path(data = ell_master, aes(x=x, y=y), size = 1, linetype = 2) + 
  labs(x = "LD1 (62%)", y = "LD2 (38%)")

#Don't run
#ggsave(plot = Fig.3, file = "Fig.3.pdf",
 #      path = "/Users/KeatonWilson/Desktop/ePortfolio")

#New Stuff for Andre

#Ok, we're going plot the loadings from the LDA against the loadings from the PCA

#Need to extract the loadings
#Here are our objects
lda.model
pca.model

#I think I need to standardize this shit, so for PCA stuff...
#Need to try and use a different package to generate the structure matrix
install.packages("candisc")
library(candisc)

#The candisc package wants the data in a different form, a model...
d = as.data.frame(aitchisontrnsfrmd[,-c(1:9)])
lda.mod = lm(as.matrix(aitchisontrnsfrmd[,-c(1:9)]) ~ Treatment, data = aitchisontrnsfrmd)
lda.model1 = MASS::lda(aitchisontrnsfrmd$Treatment~., aitchisontrnsfrmd[,-c(1:9)], na.action=na.omit)

test.canl = candisc(lda.mod, term = "Treatment")
test.canl$coeffs.std
heplot(test.canl)
#Ok, so the values below are the actual loadings - the covariances between variables and standardized components

loadings = (pca.model$rotation *pca.model$sdev)

dim(loadings)

#Building a new dataframe to do the Stimulus-Space plots
newdata = (cbind((abs(test.canl$coeffs.std[,1])),(abs(loadings[,1])),(abs(test.canl$coeffs.std[,2])),(abs(loadings[,2])), rownames(test.canl$coeffs.std)))
colnames(newdata) = c("LDA1", "PC1", "LDA2", "PC2", "compound")


newdata = as_data_frame(newdata)
newdata$LDA1 = as.numeric(newdata$LDA1)
newdata$PC1 = as.numeric(newdata$PC1)
newdata$PC2 = as.numeric(newdata$PC2)
newdata$LDA2 = as.numeric(newdata$LDA2)

#Cleaning
newdata$compound = str_replace(newdata$compound, "\n", "")
newdata$compound = trimws(newdata$compound, "right")
rownames(newdata) = NULL
newdata = newdata[-c(27:36),]

#Reordering and adding ID column
detach(package:MASS)
newdata[,1:4] = round(newdata[,1:4], digits = 4)
newdata$ID = 1:length(newdata$LDA1)
newdata = newdata %>%
  select(compound, ID, LDA1, PC1, LDA2, PC2)


#plotting
install.packages("ggrepel")
library(ggrepel)

dat_lims = apply(newdata[,3:6], 2, function(v) c(min(v), max(v)))
plot_lims = ggplot_build(g)$layout$panel_ranges[[1]][c("x.range", "y.range")]


g = ggplot(data = newdata, aes(x = LDA1, y = PC1)) +
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon") +
  geom_label_repel(aes(label = ID),size = 5) +
  scale_y_continuous(limits = c(-6,6)) +
  scale_x_continuous(limits = c(-6,6)) +
  coord_cartesian(xlim = c(-0.1, 1.75), ylim = c(-0.1, 0.7)) +
  theme_classic() +
  geom_hline(yintercept = 0.35) +
  geom_vline(xintercept = 0.875) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) 

ggsave(plot = g, file = "Fig.6a.pdf",
      path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/", limitsize = FALSE)


g1 = ggplot(data = newdata, aes(x = LDA1, y = PC2)) +
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon") +
  geom_label_repel(aes(label = ID),size = 5) +
  scale_y_continuous(limits = c(-6,6)) +
  scale_x_continuous(limits = c(-6,6)) +
  coord_cartesian(xlim = c(-0.1, 1.75), ylim = c(-0.1, 0.7)) +
  theme_classic() +
  geom_hline(yintercept = 0.35) +
  geom_vline(xintercept = 0.875) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) 

ggsave(plot = g1, file = "Fig.6b.pdf",
      path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/")

g2 = ggplot(data = newdata, aes(x = LDA2, y = PC1)) +
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon") +
  geom_label_repel(aes(label = ID),size = 5) +
  scale_y_continuous(limits = c(-6,6)) +
  scale_x_continuous(limits = c(-6,6)) +
  coord_cartesian(xlim = c(-0.1, 1.75), ylim = c(-0.1, 0.7)) +
  theme_classic() +
  geom_hline(yintercept = 0.35) +
  geom_vline(xintercept = 0.875) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1)  

ggsave(plot = g2, file = "Fig.6c.pdf",
      path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/")

g3 = ggplot(data = newdata, aes(x = LDA2, y = PC2)) +
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon") +
  geom_label_repel(aes(label = ID),size = 5) +
  scale_y_continuous(limits = c(-6,6)) +
  scale_x_continuous(limits = c(-6,6)) +
  coord_cartesian(xlim = c(-0.1, 1.75), ylim = c(-0.1, 0.7)) +
  theme_classic() +
  geom_hline(yintercept = 0.35) +
  geom_vline(xintercept = 0.875) +
  geom_hline(yintercept = 0, size = 0.1) +
  geom_vline(xintercept = 0, size = 0.1) 

ggsave(plot = g3, file = "Fig.6d.pdf",
      path = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/")

Supp_Table_1 = newdata
write.csv(Supp_Table_1, 
          file = "/Users/KeatonWilson/Documents/Projects/plantvoc/output/compound_table.csv")


#Exporting wide2 for other purposes
write.csv(wide2, file = "/Users/KeatonWilson/Documents/Projects/plantvoc/data/wide2.csv")
