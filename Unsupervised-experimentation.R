# JonJon Clark
# World Quant University

# Getting the excel data in
dir <- getwd()
setwd(dir)
require(gdata)
df = read.xls (("CountryData.xlsx"), sheet = 1, header = TRUE)
# Removing blank rows generated
df<- df[c(1:21)]

# 192 obs. of  21 variables:
head(df)
str(df)

# Renaming the vairbles to be human readable :)
names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")

#Looking at the names and everything now :)
head(df)
str(df)


df2 = read.xls (("CountryData.xlsx"), sheet = 2, header = TRUE)
head(df2)
str(df2)

df2<- df2[c(1:21)]
# 192 obs. of  21 variables:
head(df2)
str(df2)

names(df2)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")
head(df2)
str(df2)


mapping = read.xls (("CLASS.xls"), sheet = 2, header = TRUE)
head(mapping)
str(mapping)

str(df)
str(df2)
str(mapping)

index<- which(rowSums(is.na(df))>2)
index
df<-df[-index,]
str(df)
sum(is.na(df))



# Possible averaging algorithm
for( i in 3:4){
  for( j in 1:183){
  if (is.na(df[j,i])) {
    index<- which(as.character(df$Country[j])==as.character(mapping$CountryCode))
    myvar<- mapping$GroupCode[index]
    #print(myvar)
    myvalue<-0
    trig<-0
    
    for(x in 1:(length(myvar)-1)){
      tempval<-df2[which(as.character(df2$Country)==as.character(myvar[x])),i]
      if(is.na(tempval)){
          trig<-trig+1
      }else{
          myvalue<-myvalue+tempval
      }  
    }
    df[j,i]<-myvalue/(length(myvar)-trig-1)
    }
  }
}


variables <- df[,3:21]
rownames(variables)<-df[,2] 
head(variables)
pca.out<-prcomp(variables,scale=TRUE)
pca.out
biplot(pca.out,scale = 0, cex=0.35)

