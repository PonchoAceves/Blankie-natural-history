# 
# Getting ready -----------------------------------------------

#clear workspace:
rm(list=ls())
graphics.off()

# installing packages
# remotes::install_github("rmaia/pavo")
# install.packages("tidverse")
# install.packages("tibble")

# Libraries ---------------------------------------------------------------

library(pavo)
library(tidyverse)
library(dplyr)
library(tibble)
library(stringr)

# 01 Loading clear blankie data ----------------------------------------------

## collect all ".jaz" files from various subfolders
c.specs <- getspec(where = "Data/Raw SpecData Clear blankies", ext="jaz",subdir=T,subdir.names=T)
# plot(c.specs)
## optional check ups
# is.rspec(specs)
# names(specs)
# head(specs)

# Fix column names --------------------------------------------------------

names(c.specs) <- gsub(x = names(c.specs), pattern = "SPECTRUM", replacement = "")
sample <- sub("\\/.*", "", colnames(c.specs))[-1]
sample
unique(sample)

# Exploring RAW data with plots -------------------------------------------

## One individual sample per plot
## this funcitons plots reflectance spectra

# par(mfrow=c(3,5))
# explorespec(specs)

## Per sample
par(mfrow=c(1,2))
explorespec(c.specs, by = sample, ylim = c(0, 100))

## saving exploratory plots as PDF
# pdf("Output/ExploringRawSpec_bySample.pdf")
# par(mfrow=c(1,2))
# explorespec(specs, by = sample, ylim = c(0, 100))
# dev.off()
# Exists in Output

# Loading decorated blankie data ------------------------------------------

# collect all .jaz files from various subfolders
d.specs <- getspec(where = "Data/Raw SpecData Tree blankies", ext="jaz",subdir=T,subdir.names=T)
# plot(d.specs)
head(d.specs)
names(d.specs)

# Fix column names --------------------------------------------------------

names(d.specs) <- gsub(x = names(d.specs), pattern = "SPECTRUM", replacement = "")
head(d.specs)
names(d.specs)

blankie <- sub("\\/.*", "", colnames(d.specs))[-1]
blankie
unique(blankie)

explorespec(d.specs, by = blankie, ylim = c(0, 100))
dev.off()
# 
# pdf("Output/ExploringTreeSpec_bySample.pdf")
# par(mfrow=c(1,2))
# explorespec(tspecs, by = blankie, ylim = c(0, 100))
# dev.off()
# Exists in Output


# Writing clean datasets to files -----------------------------------------
# "c" for clear and "d" for decorated

c.specs <- write.csv(c.specs, file = "Data/clean_specsClear.csv", row.names = F)
d.specs <- write.csv(d.specs, file = "Data/clean_specsBlankie.csv", row.names = F)
# Both exist in Data

# 02 Loading data from exported csv files  -------------------------------

## "check names = F", stops R from replacing the special characters in the names from the previously exported files

c.specs <-read.csv("Data/clean_specsClear.csv", check.names=FALSE)
c.specs <-as.rspec(c.specs)
# is.rspec(c.specs)
# names(specs)
# check names = F, stops R from replacing the special characters in the names

d.specs <-read.csv("Data/clean_specsBlankie.csv", check.names=FALSE)
d.specs <-as.rspec(d.specs)
# is.rspec(d.specs)
# names(tspecs)


# Averaging per sample ----------------------------------------------------

# Average samples > clear
sample <- sub("\\/.*", "", colnames(c.specs))[-1]
mc.specs <- aggspec(c.specs, by = sample, FUN = "mean")
names(mc.specs)[-1] <- unique(sample)

# names(mc.specs)
# head(mc.specs)
# is.rspec(mc.specs)

par(mfrow=c(1,2))
explorespec(mc.specs)
par(mfrow=c(1, 1))

####
####

# Average samples > Decorated Blankie
blankie <- sub("\\/.*", "", colnames(d.specs))[-1]
md.specs <- aggspec(d.specs, by = blankie, FUN = "mean")
names(md.specs)[-1] <- unique(blankie)

# head(md.specs)
# is.rspec(md.specs)
# names(md.specs)

par(mfrow=c(1,2))
explorespec(md.specs)
par(mfrow=c(1, 1))

###
###

# Smoothing ---------------------------------------------------------------

# should be performed to the averaged per sample values

# plotsmooth(mc.specs, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)
c.specs <- procspec(mc.specs, opt = "smooth", span = 0.05)

# plotsmooth(mtspecs, minsmooth = 0.05, maxsmooth = 0.5, curves = 4, ask = FALSE)
d.specs <- procspec(md.specs, opt = "smooth", span = 0.05)



# Summary Files per Type --------------------------------------------------

summary(c.specs)
summary(d.specs)

# cspecssummary <- summary(c.specs)
# c.specs.summary <- write.csv(cspecssummary, file = "Data/ClearSummary.csv", row.names = T)
# 
# dspecssummary <- summary(d.specs)
# c.specs.summary <- write.csv(dspecssummary, file = "Data/DecoratedSummary.csv", row.names = T)


# Sample Spectra Plots per Type -------------------------------------------

plot(c.specs, ylim = c(10, 60), main = "clear")
plot(d.specs, ylim = c(10, 60), main = "debris")


# Comparative spectra Plots -----------------------------------------------

par(mfrow=c(1, 2))
plot(c.specs, ylim = c(10, 60), main = "Clear silk")
plot(d.specs, ylim = c(10, 60), main = "Decorated silk with debris")


# Binding Datasets --------------------------------------------------------

both <- merge(c.specs, d.specs)
is.rspec(both)
head(both)

# Grouping per sample type ------------------------------------------------

# names(tspecs)[-1]
# names(specs)[-1]
# b = blankie , cc = clear
decorated.B <- sub(".*_", "", colnames(md.specs)[-1])
clear.B <- sub("_.*", "", colnames(mc.specs)[-1])
groups <- c(clear.B, decorated.B)


# Figure for reflectance curves -------------------------------------------

# aggplot to visualize the mean reflectance curves for each sample type (i.e. clear blankies = samples and normal debris covered blankies = blankies)
par(mfrow=c(1, 1))
aggplot(both, by = groups, ylim = c(10, 70))
legend(530, 70,
       legend= c("Clear retreats", "Retreats with debris"), 
       y.intersp=0.5, 
       cex = 1.5, 
       pt.cex = 0.5, 
       lty = c(1, 1), 
       col = c("red", "blue"), 
       bty = "n", 
       lwd = 2)


# pdf("Output/MeanReflectanceClearVsNormalBlankie.pdf")
# par(mfrow=c(1,1))
# aggplot(both, by = groups)
# legend(600, 56,
#        legend= c("With debris", "Clear"),
#        lty = c(1, 1),
#        col = c("red", "blue"),
#        bty = "n",
#        lwd = 6,
#        cex = 1,
#        pt.cex = 2)
# dev.off()

# The automatic version
# par(mfrow=c(1, 1))
# aggplot(both, by = groups, alpha = 0.3, legend = TRUE)


# NOT TO BE PUBLISHED -----------------------------------------------------
# Below lies everything that is currently a failed attempt or exploration lines that are on hold until we are sure none of those test or functions will be included in any of the manuscripts

# Comparison per wavelength chroma ----------------------------------------

clear.chromas <- summary(c.specs, subset = c("S1U", "S1B", "S1G", "S1Y", "S1R"))

decorated.chromas <- summary(d.specs, subset = c("S1U", "S1B", "S1G", "S1Y", "S1R"))


par(mfrow = c(1,2))
# non-coloured
# boxplot(clear.chromas, main = "Clear")
# boxplot(debris.chromas, main = "Debris")

# coloured
boxplot(clear.chromas, ylim = c(0.15, 0.35), main = "Clear", col = c("#A377DA", "#597DDA", "#94DC89", "#fff496", "#ED6A5D"))
boxplot(decorated.chromas, ylim = c(0.15, 0.35), main = "Decorated", col = c("#6C4E8F", "#3A528E", "#62925B", "#E7D576", "#BD544B"))

# side by side arrangement
clear.chromas$type <- "clear"
decorated.chromas$type <- "decorated"

all.chromas <- rbind(clear.chromas, decorated.chromas)

par(mfrow = c(1, 5))

boxplot(all.chromas$S1U~all.chromas$type, ylab = "Chroma UV", col = c("#A377DA", "#6C4E8F"))
boxplot(all.chromas$S1B~all.chromas$type, ylab = "Chroma Blue", col = c("#597DDA", "#3A528E"))
boxplot(all.chromas$S1G~all.chromas$type, ylab = "Chroma Green", col = c("#94DC89", "#62925B"))
boxplot(all.chromas$S1Y~all.chromas$type, ylab = "Chroma Yellow", col = c("#fff496", "#E7D576"))
boxplot(all.chromas$S1R~all.chromas$type, ylab = "Chroma Red", col = c("#ED6A5D", "#BD544B"))
par(mfrow = c(1, 1))


#### Tom's comments ####
clear.B2 <- summary(c.specs, subset = c("B2"))
debris.B2 <- summary(d.specs, subset = c("B2"))
clear.B2$type <- "clear"
debris.B2$type <- "debris"
tom.B2 <- rbind(clear.B2, debris.B2)
par(mfrow = c(1, 1))
boxplot(tom.B2$B2 ~ tom.B2$type, ylab = "B2")

#
clear.h4 <- summary(c.specs, subset = c("H4"))
debris.h4 <- summary(d.specs, subset = c("H4"))
clear.h4$type <- "clear"
debris.h4$type <- "debris"
tom.h4 <- rbind(clear.h4, debris.h4)
par(mfrow = c(1, 1))
boxplot(tom.h4$H4 ~ tom.h4$type, ylab = "H4")
#
clear.s5 <- summary(c.specs, subset = c("S5"))
debris.s5 <- summary(d.specs, subset = c("S5"))
clear.s5$type <- "clear"
debris.s5$type <- "debris"
tom.s5 <- rbind(clear.s5, debris.s5)
boxplot(tom.s5$S5 ~ tom.s5$type, ylab = "S5")




m.specs.inPlot <- aggspec(both, by = groups, FUN = "mean")
head(m.specs.inPlot)
tail(m.specs.inPlot)
