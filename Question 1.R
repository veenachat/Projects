
install.packages("factoextra")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(factoextra)
####******* a) Data pre processing

#Set working directory
setwd("/Users/veenachatterjee/Desktop/Tilburg_SEM2/DSM_ass_1/")

#read data
data <- read.csv("co2emissions.csv")

#countries to keep
countries_to_keep <- c("Australia","Austria","Belgium","Bulgaria","China","Croatia","Czechia",
                       "Denmark", "Estonia", "Finland","France", "Germany", "Greece", "Hungary",
                       "India", "Indonesia", "Ireland","Italy","Latvia","Lithuania","Netherlands",
                       "Poland", "Portugal","Romania", "Russia", "Slovakia", "Slovenia", "Spain",
                       "Sweden", "Switzerland", "Ukraine", "United Kingdom", "United States")

country_filtered <- data[data$Entity %in% countries_to_keep, ]

#dates to keep
finaldata <- country_filtered[country_filtered$Year >= 1907 & country_filtered$Year <= 2022, ]

#check for omission
finaldata <- finaldata %>%
  group_by(Entity) %>%
  filter(n() >= 116) %>%
  ungroup()

unique_values <- unique(finaldata$Entity)
print(length(unique_values))


####*********** Question b


# Plot income across time for different countries
ggplot(finaldata, aes(x = finaldata$Year, y = finaldata$AnnualCOEmissionsperCapita, color = finaldata$Entity)) +
  geom_line() +
  labs(x = "Year", y = "Emissions", title = "Emissions Across Time for Different Countries") +
  theme_minimal()

######*****Question d
unique_count <- length(unique(finaldata$Entity))
print(unique_count)

unique_count <- length(unique(finaldata$Year))
print(unique_count)



#####Question f
demeaned_data <- scale(finaldata$AnnualCOEmissionsperCapita, center = TRUE, scale = FALSE)
pca_result <- prcomp(demeaned_data)

# Plot PCA results
fviz_pca_ind(pca_result, 
             col.ind = finaldata$AnnualCOEmissionsperCapita, 
             palette = "jco",
             addEllipses = TRUE)

####Question g

finaldata2=finaldata[,c(-1,-2)]
pr.out=prcomp(finaldata2,scale=TRUE)
pr.out$rotation
summary(pr.out)

biplot(pr.out, scale = 0)

####Question i

plot(pr.out, type = "l", main = "Scree Plot")


####Question j
finaldata_UK<- finaldata[finaldata$Entity != "United Kingdom", ]
finaldata_UK2=finaldata_UK[,c(-1,-2)]

pr.out=prcomp(finaldata_UK2,scale=TRUE)
pr.out$rotation
summary(pr.out)

plot(pr.out, type = "l", main = "Scree Plot")




