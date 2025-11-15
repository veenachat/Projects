
install.packages("factoextra")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(factoextra)

####******* a) Data pre processing
rm(list=ls()) 
df<-list()
#Set working directory
setwd("/Users/veenachatterjee/Desktop/Tilburg_SEM2/DSM_ass_1/co2emissions.csv")

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

# plot co2 emissions over time for all countries

max_values <- finaldata %>%
  group_by(Entity) %>%
  filter(AnnualCOEmissionsperCapita == max(AnnualCOEmissionsperCapita))

min_year <- min(finaldata$Year)
max_year <- max(finaldata$Year)
x_range <- max_year - min_year
padding <- 0.05 * x_range  
x_min <- min_year + padding 
x_max <- max_year + padding

Q1_b <- ggplot(finaldata, aes(x = Year, y = AnnualCOEmissionsperCapita, color = Entity)) +
  geom_line(linewidth = 0.5) +
  geom_text(data = max_values, aes(label = Entity), hjust = -0.1, vjust = 0.5, size = 3, color = "black") +
  labs(title = "Emissions Over Time for All Countries",
       x = "Year", y = "CO2 Emissions") +
  theme_light() +
  theme(legend.position = "bottom") +
  xlim(x_min, x_max)  
print(Q1_b)
ggsave("Q1_b.pdf", plot = Q1_b, width = 8, height = 6)

######*****Question c

# remove Estonia from the data and replot

finaldata <- finaldata[finaldata$Entity != "Estonia", ]
unique_values <- unique(finaldata$Entity)
print(length(unique_values))

max_values <- finaldata %>%
  group_by(Entity) %>%
  filter(AnnualCOEmissionsperCapita == max(AnnualCOEmissionsperCapita))

Q1_c <- ggplot(finaldata, aes(x = Year, y = AnnualCOEmissionsperCapita, color = Entity)) +
  geom_line(linewidth = 0.5) +
  geom_text(data = max_values, aes(label = Entity), hjust = -0.1, vjust = 0.5, size = 3, color = "black") +
  labs(title = "Emissions Over Time for All Countries",
       x = "Year", y = "CO2 Emissions") +
  theme_light() +
  theme(legend.position = "bottom") +
  xlim(x_min, x_max)  
print(Q1_c)
ggsave("Q1_c.pdf", plot = Q1_c, width = 8, height = 6)

######*****Question d

unique_count <- length(unique(finaldata$Entity))
print(unique_count)

unique_count <- length(unique(finaldata$Year))
print(unique_count)

######*****Question e

######*****Question f

date <-finaldata[1:116, c(3)]
datas1 <- finaldata[1:116, c(4)]
datas2 <- finaldata[117:232, c(4)]
datas3 <- finaldata[233:348, c(4)]
datas4 <- finaldata[349:464, c(4)]
datas5 <- finaldata[465:580, c(4)]
datas6 <- finaldata[581:696, c(4)]
datas7 <- finaldata[697:812, c(4)]
datas8 <- finaldata[813:928, c(4)]
datas9 <- finaldata[929:1044, c(4)]
datas10 <- finaldata[1045:1160, c(4)]
datas11 <- finaldata[1161:1276, c(4)]
datas12 <- finaldata[1277:1392, c(4)]
datas13 <- finaldata[1393:1508, c(4)]
datas14 <- finaldata[1509:1624, c(4)]
datas15 <- finaldata[1625:1740, c(4)]
datas16 <- finaldata[1741:1856, c(4)]
datas17 <- finaldata[1857:1972, c(4)]
datas18 <- finaldata[1973:2088, c(4)]
datas19 <- finaldata[2089:2204, c(4)]
datas20 <- finaldata[2205:2320, c(4)]
datas21 <- finaldata[2321:2436, c(4)]
datas22 <- finaldata[2437:2552, c(4)]
datas23 <- finaldata[2553:2668, c(4)]
datas24 <- finaldata[2669:2784, c(4)]
datas25 <- finaldata[2785:2900, c(4)]
datas26 <- finaldata[2901:3016, c(4)]
datas27 <- finaldata[3017:3132, c(4)]
datas28 <- finaldata[3133:3248, c(4)]
datas <- data.frame(datas1, datas2, datas3, datas4, datas5, datas6, datas7, datas8, datas9, datas10,
                    datas11, datas12, datas13, datas14, datas15, datas16, datas17, datas18, datas19,
                    datas20, datas21, datas22, datas23, datas24, datas25, datas26, datas27, datas28)
rm(datas1, datas2, datas3, datas4, datas5, datas6, datas7, datas8, datas9, datas10,
   datas11, datas12, datas13, datas14, datas15, datas16, datas17, datas18, datas19,
   datas20, datas21, datas22, datas23, datas24, datas25, datas26, datas27, datas28)
colnames(datas) <-as.vector(unique(finaldata$Entity))

# perform PCA without standardization
pr.out.datas = prcomp(datas[], center=TRUE, scale=FALSE)
pdf(file="Q1_f.pdf",width=10, height=10)
biplot(pr.out.datas, scale=0, expand=1, choices = c(1, 2), col=c("white","black"), main="PCA")
dev.off() 

######*****Question g

# now use the standardized data

pr.stand.datas = prcomp(datas[], center=TRUE, scale=TRUE)

pdf(file="Q1_g1.pdf",width=10, height=10)
biplot(pr.stand.datas, scale=0, expand=1, choices = c(1, 2), col=c("white","black"), main="PCA")
dev.off() 

pdf(file="Q1_g2.pdf",width=10, height=10)
biplot(pr.stand.datas, scale=0, expand=1, choices = c(2, 3), col=c("white","black"), main="PCA")
dev.off() 

pdf(file="Q1_g3.pdf",width=10, height=10)
biplot(pr.stand.datas, scale=0, expand=1, choices = c(1, 3), col=c("white","black"), main="PCA")
dev.off() 

######*****Question h

######*****Question i

# set the maximum number of PC to 10 from now
# perform new PCA

pr.new.datas = prcomp(datas[], center=TRUE, scale=TRUE, rank. = 10)

#plot of proportion of variance
pr.new.var = pr.new.datas$sdev^2
PVE = pr.new.var/sum(pr.new.var)

PVE_first_10 <- PVE[1:10]

pdf("Q1_i.pdf",width=10, height=10)
plot(cumsum(PVE_first_10), type = 'b', ylab = "cumulative PVE", ylim = c(0, 1))
text(x = 1:10, y = cumsum(PVE_first_10), labels = round(cumsum(PVE_first_10), 2), pos = 3, cex = 0.8)
dev.off()


######*****Question j

#remove the UK from the analysis and redo i
datas_j<- datas[, !colnames(datas) %in% "United Kingdom"]

pr.new.datas_j = prcomp(datas_j[], center=TRUE, scale=TRUE, rank. = 10)

#plot of proportion of variance
pr.new.var_j = pr.new.datas_j$sdev^2
PVE = pr.new.var_j/sum(pr.new.var_j)

PVE_first_10_j <- PVE[1:10]

pdf("Q1_j.pdf",width=10, height=10)
plot(cumsum(PVE_first_10_j), type = 'b', ylab = "cumulative PVE", ylim = c(0, 1))
text(x = 1:10, y = cumsum(PVE_first_10_j), labels = round(cumsum(PVE_first_10_j), 2), pos = 3, cex = 0.8)
dev.off()


######*****Question k
install.packages("ellipse")
library(ellipse)

#i've tried something from this site https://www.benjaminbell.co.uk/2018/02/principal-components-analysis-pca-in-r.html?m=1#anchor4
tab <- matrix(c(pr.new.datas_j$x[,1], pr.new.datas_j$x[,2]), ncol=2)
# Calculate correlations
c1 <- cor(tab[1:16,])
c2 <- cor(tab[17:30,])
c3 <- cor(tab[31:33,])
pdf("Q1_k.pdf",width=10, height=10)
biplot(pr.new.datas_j)
polygon(ellipse(c1*(max(abs(pr.new.datas_j$rotation))*20), centre=colMeans(tab[1:16,]), level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
polygon(ellipse(c2*(max(abs(pr.new.datas_j$rotation))*20), centre=colMeans(tab[1:16,]), level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
dev.off()
