# Import stream sediment geochemical dataset of Paariz area, Iran as an input excel file #

library(readxl)
Paariz <- read_excel("C:/Users/maghsoudimoudF/Desktop/Paariz.xlsx")
View(Paariz)

# Check the data types #

library(ggplot2)
my.data<- Paariz
str(my.data)

# As could be seen, all the columns are defined as num, so do not require for conversion #

# Subset Mo (Molibdenium) values. The values with -999 must be removed from the dataset (measurement errors and mistakes) : (32,6), (59,6), (70,6), (97,6), (98,6), (103,6), (171,6), (232,6), (233,6), (243,6), (297,6) #

my.data.subset<-my.data[c(1:31,33:58,60:69,71:96,99:102,104:170,172:231,234:242,244:296,298:382), ]

# Parameter definition #

zn<-my.data.subset[[3]]
pb<-my.data.subset[[4]]

# Correlation assessment #

cor(zn,pb)

plot(my.data.subset$zn, my.data.subset$pb)

# Linear regression #

lm(my.data.subset$zn~my.data.subset$pb)
lm_result <- lm(my.data.subset$zn~my.data.subset$pb)
lm_result
summary(lm_result)
confint(lm_result)

# Plot output #

abline(lm(my.data.subset$zn~my.data.subset$pb))

# Anomaly detection by using boxplot # Numvers above 200 are anomalous #

boxplot(my.data.subset$zn)

# Conditional function. Detect anomalous points #

for (i in 1:length(zn)){
  if (zn[i] > 200) {
    print ("anomaly")
  } 
  else {
    print ("background")
  } 
}

# End #