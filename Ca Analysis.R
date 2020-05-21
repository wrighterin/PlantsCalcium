### Data Analysis/Visualization ESE6 Final Project ####

getwd()
setwd("C:/Users/ubcwr/OneDrive/Documents/Harvard/ESE-6/Final Project")

library(displayHTS)
library(arm)

Top5Data <- read.csv("Top5 Data.csv", stringsAsFactors = FALSE)
Top5Data <- Top5Data[2:4858,]
Top5Data$Top5_Ca <- as.numeric(Top5Data$Top5_Ca)
Top5Data$Latitude <- as.numeric(Top5Data$Latitude)
Top5Data$Longitude <- as.numeric(Top5Data$Longitude)
Top5Data$LandCover1 <- as.factor(Top5Data$LandCover1)
Top5Data$LandCover2 <- as.factor(Top5Data$LandCover2)
View(Top5Data)


Top5Data$Top5_Ca <- as.character(Top5Data$Top5_Ca)
Top5Data$Top5_Ca[Top5Data$Top5_Ca == 0.005] <- "<0.01" 


First10 <- Top5Data[1:10,]
View(First10)
First10$Top5_Ca <- as.numeric(First10$Top5_Ca)
TestModel <- lm(First10$Top5_Ca~First10$Latitude)
display(TestModel)


lat <- Top5Data$Latitude
long <- Top5Data$Longitude
LU1 <- Top5Data$LandCover1
LU2 <- Top5Data$LandCover2
Ca <- Top5Data$Top5_Ca

plot(x = lat, y = Ca)
plot(x = LU1, y = Ca)
plot(x = LU2, y = Ca)

## Ca~lat
mod1 <- lm(Ca~lat)
plot(lat, Ca, main = "Soil Calcium West to East", xlab = "Latitude (degrees)", ylab = "Soil Calcium (wt%)")
abline(mod1, col='red', lwd=2)
text(x=40, y=25, paste('Ca = ', round(mod1$coefficients[2],digits =2), 'latitude + ', round(mod1$coefficients[1], digits=3)))
summary(mod1)

## Ca~long
mod2 <- lm(Ca~long)
plot(long, Ca)
abline(mod2, col='red', lwd=2)

## Ca~lat+long
mod3 <- lm(Ca~lat+long)
display(mod3)
summary(mod3)

## Ca~LU1
mod4 <- lm(Ca~LU1)
display(mod4)

## Ca~Lu2
mod5 <- lm(Ca~LU2)
display(mod5)

## Ca~lat+long+LU1
mod6 <- lm(Ca~lat+long+LU1)
display(mod6)

## Ca~lat+long+LU2
mod7 <- lm(Ca~lat+long+LU2)
display(mod7)

## LU1~LU2
mod8 <- lm(LU1~LU2)
display(mod8)

## LU1~Ca
mod9 <- lm(LU1~Ca)
display(mod9)

unique(Top5Data$LandCover2)
## Coastal v Non-Coastal##

#subset coastal#
coast <- subset(Top5Data, Top5Data$StateID =="AL" |Top5Data$StateID == "FL"
                |Top5Data$StateID == "CA"|Top5Data$StateID == "LA"|Top5Data$StateID == "TX"
                |Top5Data$StateID == "NC"|Top5Data$StateID == "OR"|Top5Data$StateID == "ME"
                |Top5Data$StateID == "MA"|Top5Data$StateID == "SC"|Top5Data$StateID == "WA"
                |Top5Data$StateID == "NJ"|Top5Data$StateID == "NY"|Top5Data$StateID == "VA"
                |Top5Data$StateID == "GA"|Top5Data$StateID == "CT"|Top5Data$StateID == "MI"
                |Top5Data$StateID == "RI"|Top5Data$StateID == "MD"|Top5Data$StateID == "DE"
                |Top5Data$StateID == "NH", 
                select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
coast$Top5_Ca <- as.numeric(coast$Top5_Ca)
coast$Latitude <- as.numeric(coast$Latitude)
coast$Longitude <- as.numeric(coast$Longitude)
coast$LandCover1 <- as.factor(coast$LandCover1)
coast$LandCover2 <- as.factor(coast$LandCover2)

#subset non-coastal#
noncoast <- subset(Top5Data, Top5Data$StateID == "VT"|Top5Data$StateID == "WV"
                   |Top5Data$StateID == "KY"|Top5Data$StateID == "TN"|Top5Data$StateID == "AK"
                   |Top5Data$StateID == "MO"|Top5Data$StateID == "IA"|Top5Data$StateID == "ND"
                   |Top5Data$StateID == "SD"|Top5Data$StateID == "NE"|Top5Data$StateID == "MT"
                   |Top5Data$StateID == "NV"|Top5Data$StateID == "OK"|Top5Data$StateID == "NM"
                   |Top5Data$StateID == "AZ"|Top5Data$StateID == "NV"|Top5Data$StateID == "UT"
                   |Top5Data$StateID == "ID"|Top5Data$StateID == "WY"|Top5Data$StateID == "CO",
                   select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
noncoast$Top5_Ca <- as.numeric(noncoast$Top5_Ca)
noncoast$Latitude <- as.numeric(noncoast$Latitude)
noncoast$Longitude <- as.numeric(noncoast$Longitude)
noncoast$LandCover1 <- as.factor(noncoast$LandCover1)
noncoast$LandCover2 <- as.factor(noncoast$LandCover2)

#subset Great Lakes#
GL <- subset(Top5Data, Top5Data$StateID == "MN" |Top5Data$StateID == "WI"|Top5Data$StateID =="IL"
             |Top5Data$StateID =="IN"|Top5Data$StateID == "OH"|Top5Data$StateID == "MI"
             |Top5Data$StateID =="PA"|Top5Data$StateID =="NY", 
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
GL$Top5_Ca <- as.numeric(GL$Top5_Ca)
GL$Latitude <- as.numeric(GL$Latitude)
GL$Longitude <- as.numeric(GL$Longitude)
GL$LandCover1 <- as.factor(GL$LandCover1)
GL$LandCover2 <- as.factor(GL$LandCover2)

View(coast)
View(noncoast)
View(GL)

summary(coast$Top5_Ca)
summary(noncoast$Top5_Ca)
summary(GL$Top5_Ca)

boxplot(coast$Top5_Ca, noncoast$Top5_Ca, GL$Top5_Ca, main = "Soil Calcium Content by Coastline",
        ylab = "Soil Calcium (wt%", xlab = "Type of Coastline", at = c(1,2,3),
        names = c("Coastal", "Noncoastal", "Great Lakes"), col = c("dark blue", "white", "dodger blue"),
        ylim = c(0,10))


### Things left to do ###
# Subset by Land Use Type #
unique(LU1)
Planted <- subset(Top5Data, Top5Data$LandCover1 == "Planted/Cultivated")
Planted$Top5_Ca <- as.numeric(Planted$Top5_Ca)
Planted$Latitude <- as.numeric(Planted$Latitude)
Planted$Longitude <- as.numeric(Planted$Longitude)
Planted$LandCover1 <- as.factor(Planted$LandCover1)
Planted$LandCover2 <- as.factor(Planted$LandCover2)
unique(Planted$LandCover2)

Developed <- subset(Top5Data, Top5Data$LandCover1 == "Developed")
Developed$Top5_Ca <- as.numeric(Developed$Top5_Ca)
Developed$Latitude <- as.numeric(Developed$Latitude)
Developed$Longitude <- as.numeric(Developed$Longitude)
Developed$LandCover1 <- as.factor(Developed$LandCover1)
Developed$LandCover2 <- as.factor(Developed$LandCover2)
unique(Developed$LandCover2)

Forested <- subset(Top5Data, Top5Data$LandCover1 == "Forested Upland")
Forested$Top5_Ca <- as.numeric(Forested$Top5_Ca)
Forested$Latitude <- as.numeric(Forested$Latitude)
Forested$Longitude <- as.numeric(Forested$Longitude)
Forested$LandCover1 <- as.factor(Forested$LandCover1)
Forested$LandCover2 <- as.factor(Forested$LandCover2)
unique(Forested$LandCover2)

Herbaceous <- subset(Top5Data, Top5Data$LandCover1 == "Herbaceous Upland")
Herbaceous$Top5_Ca <- as.numeric(Herbaceous$Top5_Ca)
Herbaceous$Latitude <- as.numeric(Herbaceous$Latitude)
Herbaceous$Longitude <- as.numeric(Herbaceous$Longitude)
Herbaceous$LandCover1 <- as.factor(Herbaceous$LandCover1)
Herbaceous$LandCover2 <- as.factor(Herbaceous$LandCover2)
unique(Herbaceous$LandCover2)

Shrubland <- subset(Top5Data, Top5Data$LandCover1 == "Shrubland")
Shrubland$Top5_Ca <- as.numeric(Shrubland$Top5_Ca)
Shrubland$Latitude <- as.numeric(Shrubland$Latitude)
Shrubland$Longitude <- as.numeric(Shrubland$Longitude)
Shrubland$LandCover1 <- as.factor(Shrubland$LandCover1)
Shrubland$LandCover2 <- as.factor(Shrubland$LandCover2)
unique(Shrubland$LandCover2)

Barren <- subset(Top5Data, Top5Data$LandCover1 == "Barren")
Barren$Top5_Ca <- as.numeric(Barren$Top5_Ca)
Barren$Latitude <- as.numeric(Barren$Latitude)
Barren$Longitude <- as.numeric(Barren$Longitude)
Barren$LandCover1 <- as.factor(Barren$LandCover1)
Barren$LandCover2 <- as.factor(Barren$LandCover2)

NonNatWoody <- subset(Top5Data, Top5Data$LandCover1 == "Non-Natural Woody")
NonNatWoody$Top5_Ca <- as.numeric(NonNatWoody$Top5_Ca)
NonNatWoody$Latitude <- as.numeric(NonNatWoody$Latitude)
NonNatWoody$Longitude <- as.numeric(NonNatWoody$Longitude)
NonNatWoody$LandCover1 <- as.factor(NonNatWoody$LandCover1)
NonNatWoody$LandCover2 <- as.factor(NonNatWoody$LandCover2)
unique(NonNatWoody$LandCover2)
# box and whisker for land use type #
boxplot(Barren$Top5_Ca, Forested$Top5_Ca, Planted$Top5_Ca, Developed$Top5_Ca, Herbaceous$Top5_Ca,
        NonNatWoody$Top5_Ca, Shrubland$Top5_Ca, ymax = 15, ylab = 'Soil Calcium (wt%)', xlab = "Type of Land Cover", main = "Soil Calcium by Land Cover",
        at = c(1,2,3,4,5,6,7), names = c("Barren", "Forested Upland", "Planted/Cultivated", 
                                         "Developed", "Herbaceous Upland", "Non-Natural Woody", "Shrubland"), 
        ylim = c(0,15), col = c("deep sky blue", "dodger blue", "royal blue", "blue", "medium blue", "dark blue", "midnight blue"))


# Barren v Forested #
boxplot(Barren$Top5_Ca, Forested$Top5_Ca, ylab = "Soil Calcium (wt%)", xlab = "Type of Soil Cover",
        at = c(1,2), names = c("Barren", "Forested Upland"), ylim = c(0,15), 
        col = c("dark olive green", "olivedrab"), main = "Barren Versus Forested Upland Soils")

# ttest for barren v forests #
ttest_BF=t.test(Barren$Top5_Ca,Forested$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_BF


## Different types of Barren Land ##
unique(Barren$LandCover2)
Bare <- subset(Barren, Barren$LandCover2 == "Bare Rock/Sand/Clay")
Bare$Top5_Ca <- as.numeric(Bare$Top5_Ca)
Bare$Latitude <- as.numeric(Bare$Latitude)
Bare$Longitude <- as.numeric(Bare$Longitude)
Bare$LandCover1 <- as.factor(Bare$LandCover1)
Bare$LandCover2 <- as.factor(Bare$LandCover2)

Transitional <- subset(Barren, Barren$LandCover2 == "Transitional")
Transitional$Top5_Ca <- as.numeric(Transitional$Top5_Ca)
Transitional$Latitude <- as.numeric(Transitional$Latitude)
Transitional$Longitude <- as.numeric(Transitional$Longitude)
Transitional$LandCover1 <- as.factor(Transitional$LandCover1)
Transitional$LandCover2 <- as.factor(Transitional$LandCover2)

boxplot(Bare$Top5_Ca, Transitional$Top5_Ca, main = "Different Types of Barren Soils",
        ylab = "Soil Calcium (wt%)", xlab = "Land Cover Type", at = c(1,2), 
        names = c("Bare Rock/Sand/Clay", "Transitional"), col = c("chocolate4", "sienna3"))

## Different Types of Forested Uplands ##
unique(Forested$LandCover2)
Evergreen <- subset(Forested, Forested$LandCover2 == "Evergreen Forest")
Evergreen$Top5_Ca <- as.numeric(Evergreen$Top5_Ca)
Evergreen$Latitude <- as.numeric(Evergreen$Latitude)
Evergreen$Longitude <- as.numeric(Evergreen$Longitude)
Evergreen$LandCover1 <- as.factor(Evergreen$LandCover1)
Evergreen$LandCover2 <- as.factor(Evergreen$LandCover2)

Deciduous <- subset(Forested, Forested$LandCover2 == "Deciduous Forest")
Deciduous$Top5_Ca <- as.numeric(Deciduous$Top5_Ca)
Deciduous$Latitude <- as.numeric(Deciduous$Latitude)
Deciduous$Longitude <- as.numeric(Deciduous$Longitude)
Deciduous$LandCover1 <- as.factor(Deciduous$LandCover1)
Deciduous$LandCover2 <- as.factor(Deciduous$LandCover2)

Mixed <- subset(Forested, Forested$LandCover2 == "Mixed Forest")
Mixed$Top5_Ca <- as.numeric(Mixed$Top5_Ca)
Mixed$Latitude <- as.numeric(Mixed$Latitude)
Mixed$Longitude <- as.numeric(Mixed$Longitude)
Mixed$LandCover1 <- as.factor(Mixed$LandCover1)
Mixed$LandCover2 <- as.factor(Mixed$LandCover2)

boxplot(Evergreen$Top5_Ca, Mixed$Top5_Ca, Deciduous$Top5_Ca, xlab = "Type of Forest",
        main = "Soil Calcium Content by Forest Type", ylab = "Soil Calcium (wt%)", 
        at = c(1,2,3), names = c("Evergreen Forest", "Mixed Forest", "Deciduous Forest"), 
        col = c("darkolivegreen", "darkolivegreen4", "darkolivegreen2"), ylim = c(0,4))

boxplot(Bare$Top5_Ca, Transitional$Top5_Ca, Evergreen$Top5_Ca, Mixed$Top5_Ca, 
        Deciduous$Top5_Ca, main = "Different Types of Barren and Forested Soils",
        ylab = "Soil Calcium (wt%)", xlab = "Land Cover Type", at = c(1,2,3,4,5), 
        names = c("Bare Rock/Sand/Clay", "Transitional","Evergreen Forest", "Mixed Forest", "Deciduous Forest"),
        col = c("chocolate4", "sienna3", "darkolivegreen", "darkolivegreen4", "darkolivegreen2"))

### Subsetting by Type of Plants ###
Pasture <- subset(Planted, Planted$LandCover2 == "Pasture/Hay")
Pasture$Top5_Ca <- as.numeric(Pasture$Top5_Ca)
Pasture$Latitude <- as.numeric(Pasture$Latitude)
Pasture$Longitude <- as.numeric(Pasture$Longitude)
Pasture$LandCover1 <- as.factor(Pasture$LandCover1)
Pasture$LandCover2 <- as.factor(Pasture$LandCover2)

Fallow <- subset(Planted, Planted$LandCover2 == "Fallow")
Fallow$Top5_Ca <- as.numeric(Fallow$Top5_Ca)
Fallow$Latitude <- as.numeric(Fallow$Latitude)
Fallow$Longitude <- as.numeric(Fallow$Longitude)
Fallow$LandCover1 <- as.factor(Fallow$LandCover1)
Fallow$LandCover2 <- as.factor(Fallow$LandCover2)

Urban<- subset(Planted, Planted$LandCover2 == "Urban/Recreational Grasses")
Urban$Top5_Ca <- as.numeric(Urban$Top5_Ca)
Urban$Latitude <- as.numeric(Urban$Latitude)
Urban$Longitude <- as.numeric(Urban$Longitude)
Urban$LandCover1 <- as.factor(Urban$LandCover1)
Urban$LandCover2 <- as.factor(Urban$LandCover2)

RowCrops<- subset(Planted, Planted$LandCover2 == "Row Crops"| Planted$LandCover2 == "Row crops")
RowCrops$Top5_Ca <- as.numeric(RowCrops$Top5_Ca)
RowCrops$Latitude <- as.numeric(RowCrops$Latitude)
RowCrops$Longitude <- as.numeric(RowCrops$Longitude)
RowCrops$LandCover1 <- as.factor(RowCrops$LandCover1)
RowCrops$LandCover2 <- as.factor(RowCrops$LandCover2)

SmallGrains <- subset(Planted, Planted$LandCover2 == "Small Grains")
SmallGrains$Top5_Ca <- as.numeric(SmallGrains$Top5_Ca)
SmallGrains$Latitude <- as.numeric(SmallGrains$Latitude)
SmallGrains$Longitude <- as.numeric(SmallGrains$Longitude)
SmallGrains$LandCover1 <- as.factor(SmallGrains$LandCover1)
SmallGrains$LandCover2 <- as.factor(SmallGrains$LandCover2)

boxplot(Pasture$Top5_Ca, Fallow$Top5_Ca, Urban$Top5_Ca, RowCrops$Top5_Ca, SmallGrains$Top5_Ca,
        Deciduous$Top5_Ca, Mixed$Top5_Ca, Evergreen$Top5_Ca, Shrubland$Top5_Ca, Herbaceous$Top5_Ca,
        NonNatWoody$Top5_Ca, at = c(1,2,3,4,5,6,7,8,9,10,11), names = c("Pasture/Hay", "Fallow",
                                                                        "Urban Grass",
                                                                        "Row Crops", "Small Grains",
                                                                        "Deciduous", "Mixed Forest",
                                                                        "Evergreen", "Shrubland",
                                                                        "Herbaceous", "Non-Natural Woody"),
        main = "Soil Calcium Levels by Plant Coverage", xlab = "Narrow Land Cover", ylab = "Soil Calcium (wt%)",
        col = c(grDevices::rainbow(n = 11)), ylim = c(0,15))






## subsetting by region ##
NE <- subset(Top5Data, Top5Data$StateID == "ME"|Top5Data == "VT" |Top5Data == "NH" 
             |Top5Data == "NY" |Top5Data == "RI" |Top5Data == "MA" |Top5Data == "CT"
             |Top5Data == "PN" |Top5Data == "NJ",
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
NE$Top5_Ca <- as.numeric(NE$Top5_Ca)
NE$Latitude <- as.numeric(NE$Latitude)
NE$Longitude <- as.numeric(NE$Longitude)
NE$LandCover1 <- as.factor(NE$LandCover1)
NE$LandCover2 <- as.factor(NE$LandCover2)

SE <- subset(Top5Data, Top5Data$StateID == "MD" |Top5Data$StateID == "DE"|Top5Data$StateID == "VA"
                |Top5Data$StateID == "WV"|Top5Data$StateID == "KY"|Top5Data$StateID == "TN"
                |Top5Data$StateID == "AK"|Top5Data$StateID == "LA" |Top5Data$StateID =="MI" 
                |Top5Data$StateID =="AL" |Top5Data$StateID =="GA" |Top5Data$StateID =="FL"
                |Top5Data$StateID == "NC" |Top5Data$StateID =="SC",
                select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
SE$Top5_Ca <- as.numeric(SE$Top5_Ca)
SE$Latitude <- as.numeric(SE$Latitude)
SE$Longitude <- as.numeric(SE$Longitude)
SE$LandCover1 <- as.factor(SE$LandCover1)
SE$LandCover2 <- as.factor(SE$LandCover2)

MW <- subset(Top5Data, Top5Data$StateID == "OH" |Top5Data$StateID == "IL"|Top5Data$StateID == "IN" 
             |Top5Data$StateID =="WI" |Top5Data$StateID =="MI" |Top5Data$StateID =="MN" 
             |Top5Data$StateID =="IA" |Top5Data$StateID =="MO"|Top5Data$StateID == "ND"
             |Top5Data$StateID == "SD" |Top5Data$StateID =="NE" |Top5Data$StateID =="KS",
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
MW$Top5_Ca <- as.numeric(MW$Top5_Ca)
MW$Latitude <- as.numeric(MW$Latitude)
MW$Longitude <- as.numeric(MW$Longitude)
MW$LandCover1 <- as.factor(MW$LandCover1)
MW$LandCover2 <- as.factor(MW$LandCover2)

SW <- subset(Top5Data, Top5Data$StateID == "OK" |Top5Data$StateID == "TX" 
             |Top5Data$StateID == "NM" |Top5Data$StateID == "AZ" ,
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
SW$Top5_Ca <- as.numeric(SW$Top5_Ca)
SW$Latitude <- as.numeric(SW$Latitude)
SW$Longitude <- as.numeric(SW$Longitude)
SW$LandCover1 <- as.factor(SW$LandCover1)
SW$LandCover2 <- as.factor(SW$LandCover2)

RM <- subset(Top5Data, Top5Data$StateID == "MT"|Top5Data$StateID == "ID"|Top5Data$StateID == "UT"
             |Top5Data$StateID == "CO"|Top5Data$StateID == "WY" |Top5Data$StateID == "NV",
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
RM$Top5_Ca <- as.numeric(RM$Top5_Ca)
RM$Latitude <- as.numeric(RM$Latitude)
RM$Longitude <- as.numeric(RM$Longitude)
RM$LandCover1 <- as.factor(RM$LandCover1)
RM$LandCover2 <- as.factor(RM$LandCover2)

PC <- subset(Top5Data, Top5Data$StateID == "WA"|Top5Data$StateID == "OR" |Top5Data$StateID == "CA",
             select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
PC$Top5_Ca <- as.numeric(PC$Top5_Ca)
PC$Latitude <- as.numeric(PC$Latitude)
PC$Longitude <- as.numeric(PC$Longitude)
PC$LandCover1 <- as.factor(PC$LandCover1)
PC$LandCover2 <- as.factor(PC$LandCover2)

boxplot(NE$Top5_Ca, SE$Top5_Ca, MW$Top5_Ca, SW$Top5_Ca, RM$Top5_Ca, PC$Top5_Ca, 
        xlab = "Region", ylab = "Soil Calcium (wt%)", main = "Soil Calcium by Region",
        ylim = c(0,8), at = c(1,2,3,4,5,6), names = c("North East", "South East", "Midwest",
                                                       "Southwest", "Rocky Mountains",
                                                       "Pacific"),
        col = c("red", "orange", "yellow", "green", "blue", "purple"))

## Subset by Ecoregion Domain ##
East <- subset(Top5Data, Top5Data$StateID == "ME"|Top5Data == "VT" |Top5Data == "NH" 
               |Top5Data == "NY" |Top5Data == "RI" |Top5Data == "MA" |Top5Data == "CT"
               |Top5Data == "PN" |Top5Data == "NJ"|Top5Data$StateID == "MD" |Top5Data$StateID == "DE"
               |Top5Data$StateID == "VA"|Top5Data$StateID == "WV"|Top5Data$StateID == "KY"|Top5Data$StateID == "TN"
               |Top5Data$StateID == "AK"|Top5Data$StateID == "LA" |Top5Data$StateID =="MI" 
               |Top5Data$StateID =="AL" |Top5Data$StateID =="GA" |Top5Data$StateID =="FL"
               |Top5Data$StateID == "NC" |Top5Data$StateID =="SC"|Top5Data$StateID == "OH" 
               |Top5Data$StateID == "IL"|Top5Data$StateID == "IN" 
               |Top5Data$StateID =="WI" |Top5Data$StateID =="MI" |Top5Data$StateID =="MN" 
               |Top5Data$StateID =="IA" |Top5Data$StateID =="MO", 
               select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
East$Top5_Ca <- as.numeric(East$Top5_Ca)
East$Latitude <- as.numeric(East$Latitude)
East$Longitude <- as.numeric(East$Longitude)
East$LandCover1 <- as.factor(East$LandCover1)
East$LandCover2 <- as.factor(East$LandCover2)

West <- subset(Top5Data, Top5Data$StateID == "OK" |Top5Data$StateID == "TX" 
               |Top5Data$StateID == "NM" |Top5Data$StateID == "AZ"|Top5Data$StateID == "ND"
               |Top5Data$StateID == "SD" |Top5Data$StateID =="NE" |Top5Data$StateID =="KS"
               |Top5Data$StateID == "MT"|Top5Data$StateID == "ID"|Top5Data$StateID == "UT"
               |Top5Data$StateID == "CO"|Top5Data$StateID == "WY" |Top5Data$StateID == "NV",
               select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
West$Top5_Ca <- as.numeric(West$Top5_Ca)
West$Latitude <- as.numeric(West$Latitude)
West$Longitude <- as.numeric(West$Longitude)
West$LandCover1 <- as.factor(West$LandCover1)
West$LandCover2 <- as.factor(West$LandCover2)

 # PC is Pacific
boxplot(PC$Top5_Ca, West$Top5_Ca, East$Top5_Ca, xlab = "Ecoregion Domain", ylab = "Soil Calcium (wt%)",
        at = c(1,2,3), names = c("Pacific Domain", "Western Domain", "Eastern Domain"), ylim = c(0,6),
        col = c("cyan4", "orangered", "olivedrab"), main = "Soil Calcium by Ecoregion Domains")

ttest_eco=t.test(PC$Top5_Ca,East$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_eco 
## DESERT AND NON DESERT ##
Desert <- subset(Top5Data, Top5Data$StateID == "TX" |Top5Data$StateID == "NM"|Top5Data$StateID == "AZ"
                 |Top5Data$StateID == "NV" |Top5Data$StateID =="OK"|Top5Data$StateID == "CA" 
                 |Top5Data$StateID =="UT" |Top5Data$StateID == "CO", 
                 select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
Desert$Top5_Ca <- as.numeric(Desert$Top5_Ca)
Desert$Latitude <- as.numeric(Desert$Latitude)
Desert$Longitude <- as.numeric(Desert$Longitude)
Desert$LandCover1 <- as.factor(Desert$LandCover1)
Desert$LandCover2 <- as.factor(Desert$LandCover2)

Nondesert <- subset(Top5Data, Top5Data$StateID == "ME"|Top5Data$StateID == "VT" |Top5Data$StateID == "NH" 
                    |Top5Data$StateID == "NY" |Top5Data$StateID == "RI" |Top5Data$StateID == "MA" 
                    |Top5Data$StateID == "CT"|Top5Data$StateID == "PN" |Top5Data$StateID == "NJ"
                    |Top5Data$StateID == "MD"|Top5Data$StateID == "DE" ## 11
                    |Top5Data$StateID == "VA"|Top5Data$StateID == "WV"|Top5Data$StateID == "KY"
                    |Top5Data$StateID == "TN"|Top5Data$StateID == "AK"|Top5Data$StateID == "LA" 
                    |Top5Data$StateID =="MI" |Top5Data$StateID =="AL" |Top5Data$StateID =="GA" 
                    |Top5Data$StateID =="FL"|Top5Data$StateID == "NC" |Top5Data$StateID =="SC"
                    |Top5Data$StateID == "OH"|Top5Data$StateID == "IL"|Top5Data$StateID == "IN" 
                    |Top5Data$StateID =="WI" |Top5Data$StateID =="MI" |Top5Data$StateID =="MN" 
                    |Top5Data$StateID =="IA" |Top5Data$StateID =="MO" |Top5Data$StateID == "WA"
                    |Top5Data$StateID == "OR"|Top5Data$StateID == "MT"|Top5Data$StateID == "ID"
                    |Top5Data$StateID == "WY"|Top5Data$StateID == "ND" |Top5Data$StateID =="SD" 
                    |Top5Data$StateID =="NE" |Top5Data$StateID =="KS", 
                    select = c("StateID", "Latitude", "Longitude", "LandCover1", "LandCover2", "Top5_Ca"))
Nondesert$Top5_Ca <- as.numeric(Nondesert$Top5_Ca)
Nondesert$Latitude <- as.numeric(Nondesert$Latitude)
Nondesert$Longitude <- as.numeric(Nondesert$Longitude)
Nondesert$LandCover1 <- as.factor(Nondesert$LandCover1)
Nondesert$LandCover2 <- as.factor(Nondesert$LandCover2)

boxplot(Desert$Top5_Ca, Nondesert$Top5_Ca)
ttest_desert=t.test(Desert$Top5_Ca,Nondesert$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_desert

## Other ttests ##
#Evergreen and Deciduous #
ttest_trees=t.test(Evergreen$Top5_Ca,Deciduous$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_trees
#Pacific v East#
ttest_PacEast=t.test(PC$Top5_Ca,East$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_PacEast
#Bare v Transitional#
ttest_Barren=t.test(Bare$Top5_Ca,Transitional$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_Barren


### Just two random states because I am absurdly frustrated ##
UT <- subset(Top5Data, Top5Data$StateID == "UT")
UT$Top5_Ca <- as.numeric(UT$Top5_Ca)
UT$Latitude <- as.numeric(UT$Latitude)
UT$Longitude <- as.numeric(UT$Longitude)
UT$LandCover1 <- as.factor(UT$LandCover1)
UT$LandCover2 <- as.factor(UT$LandCover2)

GA <- subset(Top5Data, Top5Data$StateID == "GA")
GA$Top5_Ca <- as.numeric(GA$Top5_Ca)
GA$Latitude <- as.numeric(GA$Latitude)
GA$Longitude <- as.numeric(GA$Longitude)
GA$LandCover1 <- as.factor(GA$LandCover1)
GA$LandCover2 <- as.factor(GA$LandCover2)

boxplot(GA$Top5_Ca, UT$Top5_Ca)
ttest_random=t.test(UT$Top5_Ca,GA$Top5_Ca,alternative='two.sided',var.equal = FALSE,conf.level = 0.95)
ttest_random

# just having some fun ##
plot(long, lat, col = c(grDevices::rainbow(n = 4858)))
plot(lat, long)
