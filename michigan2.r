library(rgdal)
library(spdep)
library(rgeos)
library(tmap)
library(cartogram)
library(tidyverse)
library(gridExtra)
library(GGally)
library(GWmodel)
library(spdep)
library(cluster)
library(reshape2)
library(stringr)
library(raster)
library(lattice)

#Read the shape files and change the FIPS Code:
boundaries <- readOGR(dsn = "MichiganCounties", layer = "Counties_v17a")
boundaries@data$FIPSCODE=paste0("26",boundaries@data$FIPSCODE)
boundaries@data$FIPSCODE <- as.numeric(boundaries@data$FIPSCODE)
wisconsin <- readOGR(dsn = "WisconsinCounties", layer = "County_Boundaries_24K")
wisconsin@data$COUNTY_FIP <- str_pad(wisconsin@data$COUNTY_FIP, 3, pad = "0")
wisconsin@data$COUNTY_FIP=paste0("55",wisconsin@data$COUNTY_FIP)
wisconsin@data$COUNTY_FIP <- as.numeric(wisconsin@data$COUNTY_FIP)

#Read the csv file and join:
michigan_data<- read_csv("michigan_election_results3.csv")
boundaries@data <- inner_join(boundaries@data, michigan_data,  by=c("FIPSCODE" =  "County Code"))
michigan <- boundaries

#Read the csv file and join:
wisc_data<- read_csv("wisconsin_data_clean.csv")
wisconsin@data <- inner_join(wisconsin@data, wisc_data,  by=c("COUNTY_FIP" =  "County Code"))

#Add some new variables:
michigan@data$pop_density <- michigan@data$Population / michigan@data$SQMILES
michigan@data$Trump <- michigan@data$Trump *100
michigan@data$male <- michigan@data$male / michigan@data$Population
names(michigan@data)[names(michigan@data) == 'median age'] <- 'median_age'
names(michigan@data)[names(michigan@data) == 'mean_income'] <- 'median_income'
names(wisconsin@data)[names(wisconsin@data) == 'mean_income'] <- 'median_income'
michigan@data$pct_construction_mining <- NULL
michigan@data$pct_transport <- NULL

#Add some new variables:
wisconsin@data <- wisconsin@data %>%
  mutate(
    Trump = (Trump/Total)*100,
    Male = (Male/Population)*100,
    pop_density = Population/SHAPEAREA
  )

#Histograms (do some log transforms?)
mich <- michigan@data
mich <- mich[, -c(1:16)]
mich <- mich[, -c(2:3)]
mich2 <- melt(mich)  
ggplot(data = mich2, mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
wisc <- wisconsin@data
wisc <- wisc[, -c(1:9)]
wisc <- wisc[, -c(2)]
wisc2 <- melt(wisc)  
ggplot(data = wisc2, mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#Some log transforms:
michigan@data$pop_density <- log(michigan@data$pop_density)
wisconsin@data$pop_density <- log(wisconsin@data$pop_density)

#Correlation matrix:
cormich <- round(cor(mich),2)
cormich <- melt(cormich)
ggplot(data = cormich, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#007FFF", high = "#BE2625", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1), 
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)

corwisc <- round(cor(wisc),2)
corwisc <- melt(corwisc)
ggplot(data = corwisc, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#007FFF", high = "#BE2625", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1), 
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)

#Clean up workspace:
rm(michigan_data)
rm(boundaries)
rm(mich)
rm(mich2)
rm(cormich)
rm(wisc)
rm(wisc2)
rm(corwisc)
rm(wisc_data)


#Some plots:


tm_shape(michigan) +  
  tm_fill(col="Trump",palette="YlOrRd",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="% of Trump vote",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

#Some plots:
tm_shape(wisconsin) +  
  tm_fill(col="Trump",palette="YlOrRd",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="% of Trump vote",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

#Correlation plots:
michigan@data %>%
  gather(c(median_income,unemployment_rate,median_age,veteran_rate,pct_white, pct_bluecollar, pct_degree, pop_density), 
key = "vars", value = "value") %>%
  ggplot(aes(x=value, y=Trump))+ 
  geom_point(aes(fill=Trump, size=Population),pch=21) +
  scale_fill_gradient(low = "#F08080", high = "#8C1717") +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  facet_wrap(~vars, scales="free")+
  theme_bw() +
  theme(legend.position="none")

wisconsin@data %>%
  gather(c(median_income,unemployment,median_age,veteran_rate,pct_white, pct_bluecollar, pct_degree, pop_density), 
         key = "vars", value = "value") %>%
  ggplot(aes(x=value, y=Trump))+ 
  geom_point(aes(fill=Trump, size=Population),pch=21) +
  scale_fill_gradient(low = "#F08080", high = "#8C1717") +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  facet_wrap(~vars, scales="free")+
  theme_bw() +
  theme(legend.position="none")

#Check for multicollinearity, eigenvalues of the correlation matrix
regressors <- subset(michigan@data, select=c("median_income", "median_age", "pct_white", "pct_bluecollar", "pct_degree", "pop_density", "veteran_rate"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 

#Drop veteran_rate
regressors <- subset(wisconsin@data, select=c("pop_density", "median_age", "pct_white", "pct_degree"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 
rm(regressors)
rm(cormatrix)

#Linear model:
mich_model <- lm(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan@data)
summary(mich_model)
wisc_model <- lm(Trump ~  median_age + pct_white + pct_degree  , data=wisconsin@data)
summary(wisc_model)



#Plot the residuals
michigan@data$resids <- resid(mich_model)
tm_shape(michigan) +  
  tm_fill(col="resids",palette="RdBu",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="Residuals",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)

#Plot the residuals
wisconsin@data$resids <- resid(wisc_model)
tm_shape(wisconsin) +  
  tm_fill(col="resids",palette="RdBu",style="cont", size=0.2, id="geo_label", title="") + 
  tm_layout(
    title="Residuals",
    title.snap.to.legend=TRUE,
    title.size=0.8,
    legend.text.size=0.6,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)



#Normalize with Z-score:
michigan@data <- michigan@data %>%
  mutate(
    Trump = (Trump-mean(Trump))/sd(Trump),
    mean_income = (median_income-mean(median_income))/sd(median_income),
    median_age = (median_age-mean(median_age))/sd(median_age),
    pct_white = (pct_white-mean(pct_white))/sd(pct_white),
    pct_degree = (pct_degree-mean(pct_degree))/sd(pct_degree),
    pop_density = (pop_density-mean(pop_density))/sd(pop_density)
  ) 

#Find optimal bandwidth (71):
bandwidth <- bw.gwr(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan, approach = "AICc", kernel = "bisquare", adaptive = TRUE)

#Geographically-weighted model:
gw_model <- gwr.basic(Trump ~ median_income + median_age + pct_white + pct_degree + pop_density, data=michigan, bw = bandwidth,  kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)


#Geographically-weighted spatial statistics:
gw_ss <- gwss(michigan, vars  =  c("Trump", "median_income", "median_age" , "pct_white", "pct_degree", "pop_density"),
              kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)

#Plot GWSS
tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[46:50]), title="correlation coefficients", style="cont",palette="RdBu", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.show=TRUE,
    panel.labels=c("median income", "median age", "% white", "% degree educated", "population density"),
    panel.label.bg.color="white",
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

#Normalize with Z-score:
wisconsin@data <- wisconsin@data %>%
  mutate(
    Trump = (Trump-mean(Trump))/sd(Trump),
    median_age = (median_age-mean(median_age))/sd(median_age),
    pct_white = (pct_white-mean(pct_white))/sd(pct_white),
    pct_degree = (pct_degree-mean(pct_degree))/sd(pct_degree)
  ) 

#Find optimal bandwidth (71):
wisc_bandwidth <- bw.gwr(Trump ~ median_age + pct_white + pct_degree, data=wisconsin, approach = "AICc", kernel = "bisquare", adaptive = TRUE)

#Geographically-weighted model:
wisc_gw_model <- gwr.basic(Trump ~  median_age + pct_white + pct_degree , data=wisconsin, bw = wisc_bandwidth,  kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)


#Geographically-weighted spatial statistics:
wisc_gw_ss <- gwss(wisconsin, vars  =  c("Trump", "median_age" , "pct_white", "pct_degree"),
              kernel = "bisquare", adaptive = TRUE, bw = wisc_bandwidth, quantile = TRUE)

#Plot GWSS
tm_shape(wisc_gw_ss$SDF) +
  tm_fill(col=colnames(wisc_gw_ss$SDF@data[27:29]), title="correlation coefficients", style="cont",palette="RdBu", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.show=TRUE,
    panel.labels=c("median age", "% white", "% degree educated"),
    panel.label.bg.color="white",
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)




#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- gw_ss$SDF@data[, c(46:50)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow method for K means clustering, Michigan")

#K-means clustering:
michigan@data$cluster <- NULL
michCluster <- kmeans(data, 3, nstart = 20)
michigan@data <- cbind(michigan@data, michCluster$cluster)
names(michigan@data)[names(michigan@data) == 'michCluster$cluster'] <- 'cluster'

#Plot the clusters:
colors <- c('#1b9e77','#d95f02','#7570b3')
colors2 <- c('#8dd3c7','#ffffb3','#bebada')
tm_shape(michigan) +  
  tm_fill(col="cluster",style="cont", palette=colors, size=0.2, id="geo_label", title.show=FALSE, legend.show=FALSE) + 
  tm_layout(frame=FALSE)

#Same for Wisconin:
#Elbow Method for finding the optimal number of clusters
# Compute and plot wss for k = 2 to k = 15.
data <- wisc_gw_ss$SDF@data[, c(27:29)]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow method for K means clustering, Wisconsin")

#K-means clustering:
wisconsin@data$cluster <- NULL
wiscCluster <- kmeans(data, 3, nstart = 20)
wisconsin@data <- cbind(wisconsin@data, wiscCluster$cluster)
names(wisconsin@data)[names(wisconsin@data) == 'wiscCluster$cluster'] <- 'cluster'

#Plot the clusters:
tm_shape(wisconsin) +  
  tm_fill(col="cluster",style="cont", palette=colors, size=0.2, id="geo_label", title.show=FALSE, legend.show=FALSE) + 
  tm_layout(frame=FALSE)
