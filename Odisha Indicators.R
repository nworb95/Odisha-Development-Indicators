setwd("C:/Users/nworb95/Desktop/R Programming/Social Cops Assignment/SocialCopsTest")

#-------------------------------------------------------------------------------------------
# My usual packages
#-------------------------------------------------------------------------------------------

checkAndDownload <- function(packageNames) {
        for(packageName in packageNames) {
                if(!isInstalled(packageName)) {
                        install.packages(packageName,repos="http://lib.stat.cmu.edu/R/CRAN") 
                } 
                library(packageName, character.only=TRUE,quietly=TRUE,verbose=FALSE)
        }
}

isInstalled <- function(mypkg){
        is.element(mypkg, installed.packages()[,1])
}

packages <- c("sp", "sandwich", "gplots", "zoo", "ggmap", "plm", "car", "ggplot2",
              "plyr", "rgeos", "sqldf", "RColorBrewer", "dplyr", "dtplyr", "rgdal", "xtable",
              "data.table", "corrplot", "tseries", "lmtest", "psych", "stargazer",
              "Hmisc", "raster",  "urca", "nlme", "robust", "sjstats", "ggrepel", "devtools",
              "gridExtra", "tables", "maptools", "colorspace", "xlsx", "directlabels", "gdata", "magrittr")
checkAndDownload(packages)

#-------------------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------------------

df.Begin <- read.xlsx2("ODISHA.xlsx", 2, stringsAsFactors = F, 
                       colClasses = c("character", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric"), as.data.frame = T)


names(df.Begin)
df.Odisha <- df.Begin
df.Odisha$Literacy_Differential <- df.Odisha$Male_Literacy-df.Odisha$Female_Literacy

df.Values <- subset(df.Odisha, select=-District)
max <- data.frame(apply(df.Values, 2, max))
min <- data.frame(apply(df.Values, 2, min))
range <- (max - min)
names(range) <- c('Denominator')

#-------------------------------------------------------------------------------------------
# Positive characteristics
#-------------------------------------------------------------------------------------------


df.Odisha$Villages_Electrified_Score <- ((df.Odisha$Villages_Electrified-min["Villages_Electrified",])/range["Villages_Electrified",])

df.Odisha$Fertiliser_Consumption_Score <- ((df.Odisha$Fertiliser_Consumption-min["Fertiliser_Consumption",])/range["Fertiliser_Consumption",])

df.Odisha$Total_Cereal_Yield_Score <- ((df.Odisha$Total_Cereal_Yield-min["Total_Cereal_Yield",])/range["Total_Cereal_Yield",])

df.Odisha$Bank_Branches_Score <- ((df.Odisha$Bank_Branches-min["Bank_Branches",])/range["Bank_Branches",])

df.Odisha$Health_SCs_Score <- ((df.Odisha$Health_SCs-min["Health_SCs",])/range["Health_SCs",])

df.Odisha$Modern_Contraceptive_Score <- ((df.Odisha$Modern_Contraceptive-min["Modern_Contraceptive",])/range["Modern_Contraceptive",])

df.Odisha$Immunization_Score <- ((df.Odisha$Immunization-min["Immunization",])/range["Immunization",])

df.Odisha$Male_LFP_Score <- ((df.Odisha$Male_LFP-min["Male_LFP",])/range["Male_LFP",])

df.Odisha$Female_LFP_Score <- ((df.Odisha$Female_LFP-min["Female_LFP",])/range["Female_LFP",])

df.Odisha$Qualified_Teachers_Score <- ((df.Odisha$Qualified_Teachers-min["Qualified_Teachers",])/range["Qualified_Teachers",])

df.Odisha$NER_Primary_Score <- ((df.Odisha$NER_Primary-min["NER_Primary",])/range["NER_Primary",])

df.Odisha$Schools_water_Score <- ((df.Odisha$Schools_water-min["Schools_water",])/range["Schools_water",])

df.Odisha$ST_Primary_Score <- ((df.Odisha$ST_Primary-min["ST_Primary",])/range["ST_Primary",])

df.Odisha$HH_Elec_Score <- ((df.Odisha$HH_Elec-min["HH_Elec",])/range["HH_Elec",])

df.Odisha$Rural_HH_Water_Score <- ((df.Odisha$Rural_HH_Water-min["Rural_HH_Water",])/range["Rural_HH_Water",])

df.Odisha$Urban_HH_Water_Score <- ((df.Odisha$Urban_HH_Water-min["Urban_HH_Water",])/range["Urban_HH_Water",])

df.Odisha$Toilets_Score <- ((df.Odisha$Toilets-min["Toilets",])/range["Toilets",])

df.Odisha$Rural_Mobile_Score <- ((df.Odisha$Rural_Mobile-min["Rural_Mobile",])/range["Rural_Mobile",])

df.Odisha$Male_Literacy_Score <- ((df.Odisha$Male_Literacy-min["Male_Literacy",])/range["Male_Literacy",])

df.Odisha$NDDP_Score <- ((df.Odisha$NDDP-min["NDDP",])/range["NDDP",])

df.Odisha$MSME_Score <- ((df.Odisha$MSME-min["MSME",])/range["MSME",])

df.Odisha$NER_Secondary_Score <- ((df.Odisha$NER_Secondary-min["NER_Secondary",])/range["NER_Secondary",])

df.Odisha$NER_Secondary_Female_Score <- ((df.Odisha$NER_Secondary_Female-min["NER_Secondary_Female",])/range["NER_Secondary_Female",])

#-------------------------------------------------------------------------------------------
# Negative characteristics
#-------------------------------------------------------------------------------------------

df.Odisha$Rural_Lorenz_Ratio_Score <- ((df.Odisha$Rural_Lorenz_Ratio-min["Rural_Lorenz_Ratio",])/range["Rural_Lorenz_Ratio",])

df.Odisha$Urban_Lorenz_Ratio_Score <- ((df.Odisha$Urban_Lorenz_Ratio-min["Urban_Lorenz_Ratio",])/range["Urban_Lorenz_Ratio",])

df.Odisha$Marginal_Workers_Score <- ((df.Odisha$Marginal_Workers-min["Marginal_Workers",])/range["Marginal_Workers",])

df.Odisha$PTR_Primary_Score <- ((df.Odisha$PTR_Primary-min["PTR_Primary",])/range["PTR_Primary",])

df.Odisha$IMR_Score <- ((df.Odisha$IMR-min["IMR",])/range["IMR",])

df.Odisha$Underage_Female_Marriage_Score <- ((df.Odisha$Underage_Female_Marriage-min["Underage_Female_Marriage",])/range["Underage_Female_Marriage",])

df.Odisha$Literacy_Gap_Score <- ((df.Odisha$Literacy_Differential-min["Literacy_Differential",])/range["Literacy_Differential",])

#===========================================================================================
# Marginalized Groups Composite Index
#===========================================================================================
# Comprised of a 2/3 weighting of women's issues and a 1/3 weighting of ST issues
#-------------------------------------------------------------------------------------------

df.Index <- data.frame(df.Odisha$District)
names(df.Index) <- c('District')

df.Index$Marginalized_Groups <- ((2/15)*df.Odisha$Underage_Female_Marriage_Score
                                 + (2/15)*df.Odisha$Modern_Contraceptive_Score
                                 + (2/15)*df.Odisha$Female_LFP_Score
                                 + (2/15)*df.Odisha$Literacy_Gap_Score
                                 + (2/15)*df.Odisha$NER_Secondary_Female_Score
                                 +(1/3)*df.Odisha$ST_Primary_Score)

#===========================================================================================
# Food Security Composite Index
#===========================================================================================
# Comprised of a 1/3 weighting of fertiliser consumption and 2/3 weighting of cereal yield
#-------------------------------------------------------------------------------------------

df.Index$Food_Security <- ((1/3)*df.Odisha$Fertiliser_Consumption_Score
                           + (2/3)*df.Odisha$Total_Cereal_Yield_Score)

#===========================================================================================
# Amenities Composite Index
#===========================================================================================
# Comprised of a 1/6 weighting of rural mobile phones, village electrification, toilets, 
# houses with electricity as main power source, while the remaining 1/3 is split between 
# water access within premises for rural (2/3 weighting) and urban (1/3 weighting) homes
#-------------------------------------------------------------------------------------------

df.Index$Amenities <- ((1/6)*df.Odisha$Rural_Mobile_Score
                       + (1/6)*df.Odisha$Villages_Electrified_Score
                       + (1/6)*df.Odisha$Toilets_Score
                       + (1/6)*df.Odisha$HH_Elec_Score
                       + (1/9)*df.Odisha$Urban_HH_Water_Score
                       + (2/9)*df.Odisha$Rural_HH_Water_Score)

#===========================================================================================
# Education Composite Index
#===========================================================================================
# Comprised of a 1/5 weighting of NER Primary, NER Secondary, drinking water at schools, PTR,
# and primary teachers with at least secondary education qualifications
#-------------------------------------------------------------------------------------------

df.Index$Education <- ((1/5)*df.Odisha$NER_Primary_Score
                       + (1/5)*df.Odisha$NER_Secondary_Score
                       + (1/5)*df.Odisha$Schools_water_Score
                       + (1/5)*df.Odisha$PTR_Primary_Score
                       + (1/5)*df.Odisha$Qualified_Teachers_Score)

#===========================================================================================
# Health Composite Index
#===========================================================================================
# Comprised of a 1/3 weighting of sub-centres per capita, immunization rates, and IMR
#-------------------------------------------------------------------------------------------

df.Index$Health <- ((1/3)*df.Odisha$Health_SCs_Score
                    + (1/3)*df.Odisha$Immunization_Score
                    + (1/3)*df.Odisha$IMR_Score)

#===========================================================================================
# Financial Capability Composite Index
#===========================================================================================
# Comprised of a 1/3 weighting of income and a 2/15 weighting of rural inequality, urban 
# inequality, bank branches per capita, MSMEs, and marginal workers
#-------------------------------------------------------------------------------------------

df.Index$Financial <- ((1/3)*df.Odisha$NDDP_Score
                       + (2/15)*df.Odisha$Rural_Lorenz_Ratio_Score
                       + (2/15)*df.Odisha$Urban_Lorenz_Ratio_Score
                       + (2/15)*df.Odisha$Bank_Branches_Score
                       + (2/15)*df.Odisha$MSME_Score
                       + (2/15)*df.Odisha$Marginal_Workers_Score)

#===========================================================================================
# Comprehensive Composite Index
#===========================================================================================
# Comprised of a 1/6 weight of each dimension of development
#-------------------------------------------------------------------------------------------

df.Index$Final <- ((1/6)*df.Index$Marginalized_Groups
                   + (1/6)*df.Index$Food_Security
                   + (1/6)*df.Index$Amenities
                   + (1/6)*df.Index$Education
                   + (1/6)*df.Index$Health
                   + (1/6)*df.Index$Financial)

df.Index <- transform(df.Index, Final_Rank = ave(Final,
                                                 FUN = function(x) rank(-x, ties.method = "first")))

#===========================================================================================
# Visualizing the Index
#===========================================================================================
# Going to give it a go...
#-------------------------------------------------------------------------------------------

india <- readShapePoly(file.choose())

odisha <- subset(india, ST_NM == "Odisha")

spplot(odisha, "DISTRICT", colorkey = F)
odisha$DISTRICT = as.factor(odisha$DISTRICT)

colo <- as.numeric(df.Index$Final)
odisha$colo = colo
myColors.heat = heat_hcl(12, h = c(0, -100), l = c(75, 40), c = c(40, 80), power = 1)
myColors.seq = sequential_hcl(n = 30)
myColors.brew = brewer.pal(9, "Blues")

Odisha.map <- spplot(odisha, "colo", col.regions = myColors.seq, col = grey(0.9), colorkey = T)
# Success

names <- as.character(df.Index$District)
df.HeatIndex <- subset(df.Index, select = -c(District, Final_Rank))
rownames(df.HeatIndex) <- c(names)

install_github("rlbarter/superheat")
library(superheat)

df.SuperHeat <- subset(df.HeatIndex, select = - Final)
colnames(df.SuperHeat) <- c('Marginal', 'Food', 'Amenities', 'Education', 'Health', 'Financial')

df.Index <- transform(df.Index, Reverse_Rank = ave(Final,
                                                   FUN = function(x) rank(x, ties.method = "first")))

Odisha.heat <- superheat(as.matrix(df.SuperHeat),
                         scale = F,
                         smooth.heat = T,
                         heat.pal = myColors.seq,
                         yr = df.Index$Reverse_Rank,
                         yr.axis = F,
                         yr.plot.type = "bar",
                         grid.hline.size = 0,
                         grid.vline.size = 0,
                         left.label.text.alignment = "right",
                         grid.hline.col = grey(0.9),
                         bottom.label.text.size = 4,
                         left.label.text.size = 4,
                         padding = .5,
                         grid.vline.col = grey(0.9))

df.Pretty <- subset(df.Index, select = -c(District, Final_Rank, Reverse_Rank))
rownames(df.Pretty) <- c(names)
colnames(df.Pretty) <- c('Marginal', 'Food', 'Amenities', 'Education',
                         'Health', 'Financial', 'Composite')
xtable(df.Pretty)