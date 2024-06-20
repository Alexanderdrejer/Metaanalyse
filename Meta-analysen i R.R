rm(list=ls())

library(metafor)
library(meta)
library(readxl)
dat <- read_excel("C:/Users/alexa/Desktop/Idrætsprojekt 2.0/Data/Samlet Data.xlsx")
PDUPDRS <- read_excel("C:/Users/alexa/Desktop/Idrætsprojekt 2.0/Data/Kun PD - disease severity, UPDRS.xlsx")
PDALDER <- read_excel("C:/Users/alexa/Desktop/Idrætsprojekt 2.0/Data/Kun PD.xlsx")
Stroke <- read_excel("C:/Users/alexa/Desktop/Idrætsprojekt 2.0/Data/Kun stroke sorteret efter alder.xlsx")
AD <- read_excel("C:/Users/alexa/Desktop/Idrætsprojekt 2.0/Data/Kun AD.xlsx")
ref <- dat[1,] #Denne linje i datasættet er for referencepopulationen. 
#Den bliver altså gemt som et objekt der hedder "ref" for reference.
#Da ref ikke skal bruge i den videre analyse, fjernes den række via koden nedenunder.
dat <- dat[-c(1),]

# Data and ies calculation for overall (ies = Individual effect size)
dat <- as.data.frame(dat)
cases <- dat$Events
total <- dat$Population
authoryear <- dat$Authoryear
ies <- escalc(xi = cases, ni = total, data = dat, measure = "PFT")

# Pooled individual effect size for overall data
yi <- ies$yi
vi <- ies$vi
pes.da <- rma(yi, vi, data = ies)

pes <- predict(pes.da, transf = transf.ipft.hm, targs = list(ni = dat$Population))
print(pes)

# Heterogeneity for overall data
confint(pes.da)
pes.summary <- metaprop(cases, total, authoryear, data = dat, sm = "PFT")
forest(pes.summary, xlim = c(0, 0.7), comb.fixed = FALSE, col.square.lines = "navy", 
       col.square = "navy", weight.study = "random", ref = ref$Porportion[1])

# Outliers z-score (2-3) for overall data
stud.res <- rstudent(pes.da)
abs.z <- abs(stud.res$z)
stud.res[order(-abs.z)]

# Outliers leave one out plot for overall data
L1O <- leave1out(pes.da, transf = transf.ipft.hm, targ = list(ni = dat$Population)); print(L1O)
l1o <- leave1out(pes.da)
yi <- l1o$estimate
vi <- l1o$se^2
forest(yi, vi, transf = transf.ipft.hm, targ = list(ni = dat$Population),
       slab = dat$Authoryear,
       refline = pes$pred,
       xlab = "Summary proportions leaving out each study")

inf <- influence(pes.da)
print(inf); 

# Adjusting margins
par(mar = c(5, 4, 4, 2) + 0.1)

# Plotting influence plot for overall data
plot(inf)

###############################################################
###############################################################
# Subgroup analysis for PD sorted by UPDRS score (low-high)
###############################################################
###############################################################

dat <- PDUPDRS

# Data and ies calculation for PD sorted by UPDRS
dat <- as.data.frame(dat)
cases <- dat$Events
total <- dat$Population
authoryear <- dat$Authoryear
ies <- escalc(xi = cases, ni = total, data = dat, measure = "PFT")

# Pooled individual effect size for PD sorted by UPDRS
yi <- ies$yi
vi <- ies$vi
pes.da <- rma(yi, vi, data = ies)

pes <- predict(pes.da, transf = transf.ipft.hm, targs = list(ni = dat$Population))
print(pes)

# Heterogeneity for PD sorted by UPDRS
confint(pes.da)
pes.summary <- metaprop(cases, total, authoryear, data = dat, sm = "PFT")
forest(pes.summary, xlim = c(0, 0.7), comb.fixed = FALSE, col.square.lines = "navy", 
       col.square = "navy", weight.study = "random", ref = ref$Porportion[1])

# Outliers z-score (2-3) for PD sorted by UPDRS
stud.res <- rstudent(pes.da)
abs.z <- abs(stud.res$z)
stud.res[order(-abs.z)]

# Outliers leave one out plot for PD sorted by UPDRS
L1O <- leave1out(pes.da, transf = transf.ipft.hm, targ = list(ni = dat$Population)); print(L1O)
l1o <- leave1out(pes.da)
yi <- l1o$estimate
vi <- l1o$se^2
forest(yi, vi, transf = transf.ipft.hm, targ = list(ni = dat$Population),
       slab = dat$Authoryear,
       refline = pes$pred,
       xlab = "Summary proportions leaving out each study")

inf <- influence(pes.da)
print(inf); plot(inf)

###############################################################
###############################################################
# PD sorted by age
###############################################################
###############################################################

dat <- PDALDER

# Data and ies calculation for PD sorted by age
dat <- as.data.frame(dat)
cases <- dat$Events
total <- dat$Population
authoryear <- dat$Authoryear
ies <- escalc(xi = cases, ni = total, data = dat, measure = "PFT")

# Pooled individual effect size for PD sorted by age
yi <- ies$yi
vi <- ies$vi
pes.da <- rma(yi, vi, data = ies)

pes <- predict(pes.da, transf = transf.ipft.hm, targs = list(ni = dat$Population))
print(pes)

# Heterogeneity for PD sorted by age
confint(pes.da)
pes.summary <- metaprop(cases, total, authoryear, data = dat, sm = "PFT")
dev.new() # Åbner et nyt grafikapparat
forest(pes.summary, xlim = c(0, 0.7), comb.fixed = FALSE, col.square.lines = "navy", 
       col.square = "navy", weight.study = "random", ref = ref$Porportion[1])


# Outliers z-score (2-3) for PD sorted by age
stud.res <- rstudent(pes.da)
abs.z <- abs(stud.res$z)
stud.res[order(-abs.z)]

# Outliers leave one out plot for PD sorted by age
L1O <- leave1out(pes.da, transf = transf.ipft.hm, targ = list(ni = dat$Population)); print(L1O)
l1o <- leave1out(pes.da)
yi <- l1o$estimate
vi <- l1o$se^2
forest(yi, vi, transf = transf.ipft.hm, targ = list(ni = dat$Population),
       slab = dat$Authoryear,
       refline = pes$pred,
       
       