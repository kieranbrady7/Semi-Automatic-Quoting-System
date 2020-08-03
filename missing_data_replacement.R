#Kieran Brady 
#12343851
#Semi-Automatic Quoting System for the Fabrication Engineering Sector

#Replace missing value of the training data with mean value of respective variable

#load packages
library(dplyr)
library(tidyr)

#read in training data
read.csv("Training Data1.csv", fileEncoding="UTF-8-BOM")->trainingdata

#repalce mising values with average of variable thats missing
trainingdata <- trainingdata %>% replace_na(list(Weigth = mean(trainingdata$Weight), Weld = mean(trainingdata$Weld),
                                                 DrilledHoles = mean(trainingdata$DrilledHoles), Bends = mean(trainingdata$Bends),
                                                 PlasmaCutting = mean(trainingdata$PlasmaCutting), PriceMS= mean(trainingdata$PriceMS),
                                                 PriceW = mean(trainingdata$PriceW), PriceB = mean(trainingdata$PriceB),
                                                 PriceDH = mean(trainingdata$PriceDH), PricePC = mean(trainingdata$PricePC)))

#save updated training data.csv
write.csv(trainingdata, file = file.path("TrainingData2.csv"), row.names = F, quote = T)
