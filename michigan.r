library(rgdal)
library(spdep)
library(rgeos)
library(tmap)
library(cartogram)
library(ggplot2)
library(gridExtra)
library(GGally)
library(readr)
library(dplyr)
library(tidyr)
library(GWmodel)
library(spdep)
library(cluster)
library(reshape2)

#Read the shape files and change the FIPS Code:
boundaries <- readOGR(dsn = "MichiganCounties", layer = "Counties_v17a")
boundaries@data$FIPSCODE=paste0("26",boundaries@data$FIPSCODE)
boundaries@data$FIPSCODE <- as.numeric(boundaries@data$FIPSCODE)

#Read the csv file and join:
michigan_data<- read_csv("michigan_election_results3.csv")
boundaries@data <- inner_join(boundaries@data, michigan_data,  by=c("FIPSCODE" =  "County Code"))
michigan <- boundaries

#Add some new variables:
michigan@data$pop_density <- michigan@data$Population / michigan@data$SQMILES
michigan@data$Trump <- michigan@data$Trump *100
michigan@data$male <- michigan@data$male / michigan@data$Population
names(michigan@data)[names(michigan@data) == 'median age'] <- 'median_age'
michigan@data$pct_construction_mining <- NULL
michigan@data$pct_transport <- NULL

#Histograms (do some log transforms?)
mich <- michigan@data
mich <- mich[, -c(1:16)]
mich <- mich[, -c(2:3)]
mich2 <- melt(mich)  
ggplot(data = mich2, mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#Some log transforms:
michigan@data$pop_density <- log(michigan@data$pop_density)

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

#Clean up workspace:
rm(michigan_data)
rm(boundaries)
rm(mich)
rm(mich2)
rm(cormich)

#Some plots:
tm_shape(michigan) +  
  tm_fill(col="Trump",palette="OrRd",style="cont", size=0.2, id="geo_label", title="") + 
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
  gather(c(mean_income,unemployment_rate,median_age,veteran_rate,pct_white, pct_bluecollar, pct_degree, pop_density), 
key = "vars", value = "value") %>%
  ggplot(aes(x=value, y=Trump))+ 
  geom_point(aes(fill=Trump, size=Population),pch=21) +
  scale_fill_gradient(low = "#F08080", high = "#8C1717") +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  facet_wrap(~vars, scales="free")+
  theme_bw() +
  theme(legend.position="none")

#Check for multicollinearity, eigenvalues of the correlation matrix
regressors <- subset(michigan@data, select=c("mean_income", "median_age", "pct_white", "pct_bluecollar", "pct_degree", "pop_density", "veteran_rate"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 

#Drop veteran_rate
regressors <- subset(michigan@data, select=c("mean_income", "median_age", "pct_white", "pct_degree", "pop_density"))
cormatrix <- round(cor(regressors),2)
eigen(cormatrix)$values 
rm(regressors)
rm(cormatrix)

#Linear model:
model <- lm(Trump ~ mean_income + median_age + pct_white + pct_bluecollar + pct_degree + pop_density, data=michigan@data)
summary(model)
model <- lm(Trump ~ mean_income + median_age + pct_white + pct_degree + pop_density, data=michigan@data)
summary(model)

#Plot the residuals
michigan@data$resids <- resid(model)
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

#Normalize with zero mean:
michigan@data <- michigan@data %>%
  mutate(
    Trump = (Trump-mean(Trump))/sd(Trump),
    mean_income = (mean_income-mean(mean_income))/sd(mean_income),
    median_age = (median_age-mean(median_age))/sd(median_age),
    pct_white = (pct_white-mean(pct_white))/sd(pct_white),
    pct_degree = (pct_degree-mean(pct_degree))/sd(pct_degree),
    pop_density = (pop_density-mean(pop_density))/sd(pop_density)
  )

#Find optimal bandwidth (71):
bandwidth <- bw.gwr(Trump ~ mean_income + median_age + pct_white + pct_degree + pop_density, data=michigan, approach = "AICc", kernel = "bisquare", adaptive = TRUE)

#Geographically-weighted model:
gw_model <- gwr.basic(Trump ~ mean_income + median_age + pct_white + pct_degree + pop_density, data=michigan, bw = bandwidth,  kernel = "bisquare", adaptive = TRUE, F123.test = TRUE)


#Geographically-weighted spatial statistics:
gw_ss <- gwss(michigan, vars  =  c("Trump", "mean_income", "median_age" , "pct_white", "pct_degree", "pop_density"),
              kernel = "bisquare", adaptive = TRUE, bw = 30, quantile = TRUE)

#Plot GWSS
tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[46:50]), title="correlation coefficients", style="cont",palette="RdBu", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.show=TRUE,
    panel.labels=c("Mean income", "Median age", "% white", "% degree educated", "Population density"),
    panel.label.bg.color="white",
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

