##########################################################################################
######################### Sourcing the "mcperturb" functions
##########################################################################################

dir = getwd()
source(paste(dir, "densPlots.R", sep = "/"))
source(paste(dir, "diagOut.R", sep = "/"))
source(paste(dir, "densPlots.R", sep = "/"))
source(paste(dir, "implausStats.R", sep = "/"))
source(paste(dir, "rsqdPlots.R", sep = "/"))
source(paste(dir, "randomNoiseMat.R", sep = "/"))
source(paste(dir, "regModelStats.R", sep = "/"))
source(paste(dir, "noiseLevelDiagOutList.R", sep = "/"))
source(paste(dir, "noiseLevelsList.R", sep = "/"))
source(paste(dir, "overallDiagsDiffs.R", sep = "/"))
source(paste(dir, "overallDiagsout.R", sep = "/"))
source(paste(dir, "overallDiagsRank.R", sep = "/"))
source(paste(dir, "boxplotsAllVars.R", sep = "/"))
source(paste(dir, "BoxplotAllPerc.R", sep = "/"))
source(paste(dir, "rateOfChange.R", sep = "/"))
source(paste(dir, "isBestFit.R", sep = "/"))
source(paste(dir, "isRateofChange.R", sep = "/"))
source(paste(dir, "overallDiagsPlots.R", sep = "/"))

##########################################################################################
######################### Initiating library packages
##########################################################################################

install.packages(c("corrgram", "car", "perturb", "mctest", "readr"))
library(corrgram)
library(car)
library(perturb)
library(mctest)
library(readr)

##########################################################################################
######################### Initiating the parameters
##########################################################################################

# Reading in the data
body_dat = read.table("C:/Users/Ryan/Desktop/body.dat.txt", sep = " ", strip.white = TRUE)
# Response variable
y = body_dat[,23]
# X-matrix
x = body_dat[,c(10, 11, 16, 17, 21, 22, 24)]
colnames(x) = c("shoulder", "chest", "bicep", "forearm", "wrist", "age", "height")
special.Vars = c("shoulder")
# Making the noiselevels
noiseStart = 0.05
noiseEnd = 0.25
noiseSteps = 0.05
noiseLevs = seq(noiseStart, noiseEnd, by = noiseSteps) 
# The amount of iterations at each noise level
iteration = 50

##########################################################################################
######################### Calling the density plots function
########################################################################################## 

# Figure 4
densPlots(x,T)

# Figure 5
densPlots(x[,1:2],T)

##########################################################################################
#########################  Calling the implausestats function
##########################################################################################

# Table 10
implausStats(xmat = x, response = y)

##########################################################################################
######################### Calling the rsqdPlots function
##########################################################################################

# Figure 6
rsqdPlots(x,y)
rsqdPlots(x,y,T)

##########################################################################################
######################### Calling Overall Diagnostics plots
##########################################################################################

# Figure 7 
for (i in 1:dim(x)[2])
{
  special.Vars = colnames(x)[i]
  print(special.Vars)
  overallDiagsPlots(xmat = x, yvar = y, noiseLevels = noiseLevs, 
                    spec.Vars = special.Vars, iter = iteration, choice =  c("d"))
}

##########################################################################################
######################### Calling Boxplots all percentage function
##########################################################################################

boxplotoutperc = BoxplotAllPerc(xmatrix = x, y = y, noiseLevs = noiseLevs, 
                                special.Vars = special.Vars, iteration = iteration)

##########################################################################################
######################### Calling Boxplots all variables function
##########################################################################################

boxplotout = boxplotsAllVars(xmatrix = x, y = y, noiseLevs = noiseLevs, 
                             special.Vars = special.Vars, iteration = iteration)

##########################################################################################
######################### Calling OverallDiagsOut 
##########################################################################################

overallDiagsOut(x = x, y = y, noiseLevs = noiseLevs, iteration = iteration)

##########################################################################################
######################### Calling rateOfChange 
##########################################################################################

summaryTableList = rateOfChange(x = x, y = y, noiseLevs = noiseLevs, 
                                special.Vars = special.Vars, iteration = iteration)

##########################################################################################
######################### Calling overallDiagsRank 
##########################################################################################

Ranks = overallDiagsRank(xmat = x, resp = y, noiseL = noiseLevs, itera = iteration)

##########################################################################################
######################### Calling isBestfit 
##########################################################################################

bestfitvalues = isBestFit(x = x, y = y, noiseLevs = noiseLevs, 
                          special.Vars = special.Vars, iteration = iteration)
vifBestFit = bestfitvalues[[1]]

##########################################################################################
######################### Calling isRateofChange
##########################################################################################

rateofchangevalues = isRateofChange(x = x, y = y, noiseLevs = noiseLevs, 
                                    special.Vars = special.Vars, iteration = iteration)
vifRateofChange = rateofchangevalues[[1]]

##########################################################################################
######################### Calling isBestFit, isRateofChange
##########################################################################################

fullx = body_dat[,c(1:22,24)]
colnames(fullx) = c("bicromial", "biiliac", "bitrochanteric", "chestDepth",
                    "chestDim", "elbowDim", "wristDim", "kneeDim", "ankleDim", 
                    "shoulderDim", "chestGir", "waistGir", "navelGir", "hipGir", 
                    "thighGir", "bicepGir", "forearmGir", "kneeGir", "calfGir", 
                    "ankleGir", "wistGir", "age", "height")
                    
bestfitvaluesAll = isBestFit(x = fullx, y = y, noiseLevs = noiseLevs, special.Vars = special.Vars, iteration = iteration)
vifBestFitALL = bestfitvaluesAll[[1]]
