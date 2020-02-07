library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(tidyr)


#########################################prescreen process###############
###read in all prescreens

fall16 <- read.csv("Fall16.csv")
fall17 <- read.csv("Fall17.csv")
fall18 <- read.csv("Fall18.csv")
Spr17 <- read.csv("Spr17.csv")
Spr18 <- read.csv("Spr18.csv")
Sum17 <- read.csv("Sum17.csv")
Sum18 <- read.csv("Sum18.csv")
Win17 <- read.csv("Win17.csv")
Win18 <- read.csv("Win18.csv")

allPrescreen <- rbindlist(list(fall16,fall17,fall18,Spr17,Spr18,Sum17,Sum18,Win17,Win18), fill = TRUE)


##reduce and reorder variables
reducedPrescreen <- allPrescreen[,c("id_code","cohort", "math1", "math2", "math3", "math4", "math5", "math6", "math7", "math8", "math9", "chairparts",  "spatialrel", "diagrams","howmanymath","mathposthigh","ratereading","gender","FathEd","MothEd","class","Ethnic","engfirst")]

##rename variables (ga: general attitude; ma: math attitude; sa: spatial attitude)
colnames(reducedPrescreen) <- c("repID","cohort","MA.GoodatMath", "MA.ImportantDoWell", "MA.NaturallyBetter", "MA.EffortImportant", "MA.UnderstandQuickly", "MA.ThinkCarefully", "MA.PlugInNum", "MA.PatternsOverCalc", "MA.ExcitingIntimidating", "SA.ChairParts", "SA.SpatialRel", "SA.Diagrams","howmanymath","mathposthigh","ratereading","gender","FathEd","MothEd","class","Ethnic","engfirst")


###################

#read in users 
users <- read.csv("AllUsers.csv")

reducedPrescreen <- merge(users,reducedPrescreen,by="repID",all=FALSE)

#########
#read in pretest results (or calculate from above code)
pretest <- read.csv("AllREPPretest.csv")

##reorder pretest variables
pretest <- pretest[c("user", "EmbeddedFigure", "FigureRotation", "LetterSets", "Vocab", "AddSubtract", "MarkXYI", "MarkThetaI", "Right.Triangle.Trig.I", "Right.Triangle.Trig.II", "Right.Triangle.Trig.IV", "TrigIDs", "MarkXYII", "MarkThetaII", "Right.Triangle.Trig.III", "Right.Triangle.Trig.V")]

##rename/recode pretest variables
colnames(pretest) <- c("user", "A.Figure", "A.Rotate", "A.Letters", "A.Vocab", "PM.Arith", "PM.XY", "PM.Theta", "PM.TTSinCos", "PM.TTSides", "PM.CTSinCos", "PM.TrigRs", "SM.XY", "SM.Theta", "SM.TTSinCos", "SM.CTSinCos")


allData <- merge(reducedPrescreen,pretest,by="user")

#remove incomplete cases
allData <- na.omit(allData)


#check for duplicate IDs
n_occur <- data.frame(table(allData$user))
dups <- n_occur[n_occur$Freq > 1,]
dupDetail <- allData[allData$user %in% n_occur$Var1[n_occur$Freq > 1],]


#calculate cohort by date
dupDetail$date_joined = as.POSIXct(dupDetail$date_joined, tz=Sys.timezone(), format="%m/%d/%Y %H:%M")
dupDetail$date_joined_month = month(dupDetail$date_joined)
dupDetail$date_joined_year = year(dupDetail$date_joined)
cohort_from_date <- function (month, year) {
  if (is.na(month)){
    return("Winter18")
  }
  if (year==2017 & month <=3) {
    return("Win17")
  }
  if (year==2018 & month <=3) {
    return("Winter18")
  }
  if (year==2016 & month >=9) {
    return("Fall16")
  }
  if (year==2017 & month >=9) {
    return("Fall17")
  }
  if (year==2018 & month >=9) {
    return("Fall18")
  }
  if (year==2017 & (month >=4 & month <=6)) {
    return("Spr17")
  }
  if (year==2018 & (month >=4 & month <=6)) {
    return("Spr18")
  }
  if (year==2017 & (month==7 | month==8)) {
    return("Sum17")
  }
  if (year==2018 & (month==7 | month==8)) {
    return("Sum18")
  }
  return(NULL)
}
dupDetail$cohort_from_date <- mapply(cohort_from_date, dupDetail$date_joined_month, dupDetail$date_joined_year)


#show me instances where stated cohort does not match cohort login
dupDetailMismatch <- dupDetail[dupDetail$cohort!=dupDetail$cohort_from_date,]

#remove dup data from all data
allData <- allData[!(allData$user %in% dupDetail$user),]


#remove mismatched dups from dup data
correctedDup <- dupDetail[dupDetail$cohort==dupDetail$cohort_from_date,]
correctedDup <- correctedDup[,1:41]

#combine dup data and all data
allDataFixed <- rbind(allData,correctedDup)

#save as .csv
write.csv(allDataFixed,"REPPretestPrescreen2-4-20.csv")
