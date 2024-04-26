#load libraries
library(ggplot2)
library(gridExtra)
library(cowplot)

#set directory
setwd("C:/G2F_orthomosaic_and_shape/NDVI")

#load files
unmasked22<-read.csv("2022_phenos_and_unmaskedNDVI.csv",head=T)
masked22<-read.csv("2022_phenos_and_maskedNDVI.csv",head=T)
unmasked23<-read.csv("2023_phenos_and_unmaskedNDVI.csv",head=T)
masked23<-read.csv("2023_phenos_and_maskedNDVI.csv",head=T)

#pull out NDVI columns *Using means here, can change grep to sum if need be
NDVI_unmasked22<-unmasked22[,grep("mean",colnames(unmasked22))]
NDVI_masked22<-masked22[,grep("mean",colnames(unmasked22))]
NDVI_unmasked23<-unmasked23[,grep("mean",colnames(unmasked23))]
NDVI_masked23<-masked23[,grep("mean",colnames(unmasked23))]

#Check dimensions of NDVI vectors 
#*Inconsistent number of rows, masked22 has 1056 rows with and extra 256 rows of NA
#and masked23 has 2 extra rows, seems like it took extra rows from the csv files for some reason
dim(NDVI_unmasked22)
dim(NDVI_masked22)
dim(NDVI_unmasked23)
dim(NDVI_masked23)

#Truncate NDVI_masked22 and NDVI_masked23 to 800 rows (plots), and NDVI_unmasked23 and NDVI_unmasked23 to 10 columns (flights) just for efficiency
NDVI_masked22<-NDVI_masked22[1:800,]
NDVI_unmasked23<-NDVI_unmasked23[, 1:10]
NDVI_masked23<-NDVI_masked23[1:800, 1:10]


#Recheck dimensions of NDVI vectors 
dim(NDVI_unmasked22)
dim(NDVI_masked22)
dim(NDVI_unmasked23)
dim(NDVI_masked23)

#Days after planting for each year
dap2022<-c(20,26,33,40,55,60,70,77,83,89,103)
dap2023<-c(28,35,44,49,58,76,90,99,113,124)

# Extracting data for the 2022 plot
unmasked22_ndvi_values <- unlist(NDVI_unmasked22[1, 1:11])
masked22_ndvi_values <- unlist(NDVI_masked22[1, 1:11])
dap22_values <- dap2022

# Creating a data frame for ggplot
unmasked22_data <- data.frame(dap22 = dap22_values, unmasked22_ndvi = unmasked22_ndvi_values)
masked22_data <- data.frame(dap22 = dap22_values, masked22_ndvi = masked22_ndvi_values)

# Creating the plot
p1 <- ggplot() +
  geom_point(data = unmasked22_data, aes(x = dap22, y = unmasked22_ndvi, color = "Unmasked")) +
  geom_point(data = masked22_data, aes(x = dap22, y = masked22_ndvi, color = "Masked")) +
  geom_smooth(data = unmasked22_data, aes(x = dap22, y = unmasked22_ndvi), method = "loess", se = FALSE, color = "darkblue", linetype = "solid", linewidth = 0.5) +
  geom_smooth(data = masked22_data, aes(x = dap22, y = masked22_ndvi), method = "loess", se = FALSE, color = "red", linetype = "solid", linewidth = 0.5) +
  labs(x = "Days after planting",
       y = "Mean NDVI",
       title = "2022",
       color = "Legend") +
  scale_color_manual(values = c("Unmasked" = "darkblue", "Masked" = "red"),
                     labels = c("Masked", "Unmasked"),
                     name = "Legend") +
  theme_grey()



# Extracting data for the 2023 plot
unmasked23_ndvi_values <- unlist(NDVI_unmasked23[1, 1:10])
masked23_ndvi_values <- unlist(NDVI_masked23[1, 1:10])
dap23_values <- dap2023

# Creating a data frame for ggplot
unmasked23_data <- data.frame(dap23 = dap23_values, unmasked23_ndvi = unmasked23_ndvi_values)
masked23_data <- data.frame(dap23 = dap23_values, masked23_ndvi = masked23_ndvi_values)

# Creating the plot
p2 <- ggplot() +
  geom_point(data = unmasked23_data, aes(x = dap23, y = unmasked23_ndvi, color = "Unmasked")) +
  geom_point(data = masked23_data, aes(x = dap23, y = masked23_ndvi, color = "Masked")) +
  geom_smooth(data = unmasked23_data, aes(x = dap23, y = unmasked23_ndvi), method = "loess", se = FALSE, color = "darkblue", linetype = "solid", linewidth = 0.5) +
  geom_smooth(data = masked23_data, aes(x = dap23, y = masked23_ndvi), method = "loess", se = FALSE, color = "red", linetype = "solid", linewidth = 0.5) +
  labs(x = "Days after planting",
       y = "Mean NDVI",
       title = "2023",
       color = "Legend") +
  scale_color_manual(values = c("Unmasked" = "darkblue", "Masked" = "red"),
                     labels = c("Masked", "Unmasked"),
                     name = "Legend") +
  theme_grey()

