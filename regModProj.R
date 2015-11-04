

library(dplyr)
library(corrplot)
library(car) #for vif()


#Store mtcars as a data frame in df
df<- tbl_df(mtcars)

#df$am<- as.factor(df$am)
#levels(df$am)<- c("Auto","Man")

#----------------------------
#      Exploratory Analysis
#----------------------------


#Corrplot to visualize correlations
# Note that there is strong correlation between many variables
corrplot.mixed(cor(df))

#Pairs plot, colored by transmission type
pairs(df,col=3 + (df$am ==1))




#-----------------------
#       Model Selection Method 1 
#-----------------------

#Build a base model, with mpg ~ am
#Show r.squared value
fit0<- lm(mpg~am,df)
summary(fit0)
#summary(fit0)$adj.r.squared


#Find correlation between model residuals and remaining variables
#since patterns in residuals (errors) imply the predictors are missing
#predictive info
cor(fit0$residuals,select(df,-mpg,-am))


#Show correlation between variables and mpg
#Every variable appears to have "some" correlation
#cor(df)[,1]

##Plot Correlations of remaining variables
par(mfrow=c(3,3))
for(i in names(select(df,-mpg,-am))){
      plot(unlist(df[i]),fit0$residuals, xlab= paste(i),ylab="Model Residuals")  
      }

#hp has highest correlation, add it to the model
#show new r.squared for model
#Check correlations for new model

fit1<- lm(mpg~am+hp,df)
summary(fit1)
summary(fit1)$adj.r.squared
cor(fit1$residuals,select(df,-mpg,-am,-hp))
vif(fit1)

#wt has highest correlation, add to model
fit2<- lm(mpg~am+hp+wt,df)
summary(fit1)$adj.r.squared
cor(fit2$residuals,select(df,-mpg,-am,-hp,-wt))
vif(fit2)

#qsec has highest correlation, add to model
fit3<- lm(mpg~am+hp+wt+qsec,df)
summary(fit3)$adj.r.squared
cor(fit3$residuals,select(df,-mpg,-am,-hp,-wt,-qsec))
vif(fit3)

#adding qsec shows variance inflation in hp
#see what happens when we keep qsec but get rid of hp
fit4<- lm(mpg~am+wt+qsec,df)
summary(fit4)$adj.r.squared
cor(fit4$residuals,select(df,-mpg,-am,-wt,-qsec))
vif(fit4)

#this doesn't affect am, which is good, and also reduces
#wt variance; r.squared isn't affected much
#Add carb in model since it has highest variance
fit5<- lm(mpg~am+wt+qsec+carb,df)
summary(fit5)$adj.r.squared
cor(fit5$residuals,select(df,-mpg,-am,-wt,-qsec,-carb))
vif(fit5)

#This model (fit4) is better than if we included all variables
summary(lm(mpg~.,df))$adj.r.squared

#Boxplot to show difference between manual/transmission
boxplot(mtcars$mpg~as.factor(mtcars$am),col="green",ylab="mpg",xlab="am",main="MPG vs AM")

mpg.adj<- resid(lm(mpg~wt+qsec,df))
am.adj<- resid(lm(am~wt+qsec,df))
t.test(mpg.adj,am.adj)

#t-test for
#adjusted mpg for each transmission type
t.test(mpg.adj[df$am=="Auto"],mpg.adj[df$am=="Man"])

#-------------------------------------------------------
#    Plot code
#-----------------------------------------------------

# Correlation Level Plot
# Source: http://rstudio-pubs-static.s3.amazonaws.com/360_4e37e3ec5346481c88ce5d44f02d1cbe.html

library("RColorBrewer")
library("grid")
library(lattice)
brewer.div = colorRampPalette(brewer.pal(11, "PRGn"), interpolate = "spline")

levelplot(cor(df),at=do.breaks(c(-1.01,1.01),20),
          main="Correlation Level Plot", xlab="",ylab="",aspect=1,
          #panel=panel.corrgram.2,
          colorkey=list(space="top"),pretty=TRUE, 
          #col.regions=colorRampPalette(c("red","white","blue")),
          col.regions=brewer.div(200),
          scales=list(x=list(rot=90)))

#-------------------------------------------
#-------------------------------------------

#Build a model with variable, look at variable inflation
fit<- lm(mpg~.,df)
vif(fit)

#Remove highest variances, create new model, check vif
fit2<- lm(mgp~.-cyl-disp)


#cor(df)[,1]>1.75*mean(cor(df))
#cor(df)[,1]>mean(cor(df)) +c(1,-1)* sd(cor(df))

#Variable interactions color coded based on transmission type
pairs(df, panel = panel.smooth, main = "mtcars data", col = 3 + (df$am == 1))

# Models
fit<- lm(mpg~am,df)
fit2<- lm(mpg~am+qsec,df)