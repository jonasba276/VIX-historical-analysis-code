#go to the right directory
setwd('~/Vix')
vix_data = read.csv("VIX_History.csv")
#to call a row type data['row name']
#Note that it also gives the index, to remove the index write data[,number of row]
# or write data['row name][,1]
#or data$name

#To get column names: colnames(data)

#Since data of vix is constant during the day: "OPEN"  "HIGH"  "LOW"   "CLOSE" are all equal
#THIS IS INCORRECT!

#However the data seem quite similar, let's check this with PCA
#PCA only works on numerical data, so the DATE column (= vix_data[1])will be left out of the PCA
vix_pca = prcomp(vix_data[2:length(vix_data)], center=TRUE, scale=TRUE)
summary(vix_pca)
#PC2 only explains 0.004 = 0.4% of the variance, we can thus only work with PC1
#For a scree plot the library factoextra is needed
library(factoextra)
fviz_eig(vix_pca)

#Plotting Variables -PCA
fviz_pca_var(vix_pca,
             #col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800"),
             repel = TRUE     # Avoid text overlapping
)

#Ploting Biplot (VERY LABOR INTENSIvE)
#fviz_pca_biplot(vix_pca, repel = TRUE,
#                col.var = "#2E9FDF", # Variables color
#                col.ind = "#696969"  # Individuals color
#                )

#PCA1 is thus enough to explain all the other data. But notice that the variables themselves are almost equal to PC1,
#so it is possible to only work with one variable to represent all data

#Henceforth shall be worked with CLOSE, since it gives the market some time to mature and is more representative of normal VIX compared to HIGH
head(vix_data)

tot_days= seq(1,length(vix_data$DATE))

plot(tot_days, vix_data$OPEN, col = 'red', pch=10, xlab=paste("Time since", vix_data$DATE[1], "[Days]"), ylab='Vix [$^2]', main="Historical Data Vix")
legend("topleft", c("Data points"), col=c("red"), pch=c(10))
#Using indeci is not really easy to digest for the reader, hence a different approach is needed

#Code template taken form earthdatascience.org

library(ggplot2)
library(patchwork)


options(stringsAsFactors = FALSE)

ggplot(data = vix_data, aes(x = DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1990-2022")

#The x-axis is still quite unclear, this is due to the fact all text is overlapping
#Changing datatype of DATE from str to a date class might improve it

#Capital Y since 4 number format for years
vix_data$DATE = as.Date(vix_data$DATE, format = "%m/%d/%Y")

#Demonstrating change of format
head(vix_data)

#Plot
ggplot(data = vix_data, aes(x = DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1990-2022")

#plot 2019-2022
#vix_data[which(vix_data$DATE>"2018-12-31"),] can be used
plot_2019_2022 =ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
                    geom_point() +
                  labs(x = "Date",
                  y = "Vix/Volatility [$^2]",
                  title = "The Vix History",
                  subtitle = "2019-2022")+
                  scale_x_date(limit=c(as.Date("2018-12-31"), as.Date("2023-01-01"))) +
                  ylim(10, 100)

plot_2019_2022

#plot 2016-2019
plot_2016_2019=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
              geom_point() +
              labs(x = "Date",
                    y = "Vix/Volatility [$^2]",
                    title = "The Vix History",
                    subtitle = "2016-2019") +
              scale_x_date(limit=c(as.Date("2015-12-31"), as.Date("2020-01-01"))) +
              ylim(10, 60)
plot_2016_2019

#Plot 2013-2016
plot_2013_2016=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "2013-2016") +
  scale_x_date(limit=c(as.Date("2012-12-31"), as.Date("2017-01-01"))) +
  ylim(10, 60)
plot_2013_2016

#Plot 2010-2013
plot_2010_2013=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "2010-2013") +
  scale_x_date(limit=c(as.Date("2009-12-31"), as.Date("2014-01-01"))) +
  ylim(10, 60)
plot_2010_2013

#Plot 2007-2010
plot_2007_2010=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "2007-2010") +
  scale_x_date(limit=c(as.Date("2006-12-31"), as.Date("2011-01-01"))) +
  ylim(10, 100)
plot_2007_2010

#Plot 2004-2007
plot_2004_2007=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "2004-2007") +
  scale_x_date(limit=c(as.Date("2003-12-31"), as.Date("2008-01-01"))) +
  ylim(10, 40)
plot_2004_2007

#Plot 2001-2004
plot_2001_2004=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "2004-2007") +
  scale_x_date(limit=c(as.Date("2000-12-31"), as.Date("2005-01-01"))) +
  ylim(10, 60)
plot_2001_2004

#Plot 1998-2001
plot_1998_2001=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1998-2001") +
  scale_x_date(limit=c(as.Date("1997-12-31"), as.Date("2002-01-01"))) +
  ylim(10, 60)
plot_1998_2001

#Plot 1995-1998
plot_1995_1998=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1995-1998") +
  scale_x_date(limit=c(as.Date("1994-12-31"), as.Date("1999-01-01"))) +
  ylim(10, 60)
plot_1995_1998

#Plot 1992-1995
plot_1992_1995=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1992-1995") +
  scale_x_date(limit=c(as.Date("1991-12-31"), as.Date("1996-01-01"))) +
  ylim(10, 40)
plot_1992_1995

#Plot 1990-1992
plot_1990_1992=ggplot(data = vix_data, aes(DATE, y = CLOSE)) +
  geom_point() +
  labs(x = "Date",
       y = "Vix/Volatility [$^2]",
       title = "The Vix History",
       subtitle = "1990-1992") +
  scale_x_date(limit=c(as.Date("1989-12-31"), as.Date("1993-01-01"))) +
  ylim(10, 60)
plot_1990_1992


close_mean = mean(vix_data$CLOSE)
close_var = sqrt(var(vix_data$CLOSE))


all_plot =ggplot(data = vix_data, aes(x = DATE, y = CLOSE)) +
        geom_point() +
        geom_line() +
        labs(x = "Date",
         y = "Vix/Volatility [$^2]",
         title = "The Vix History",
        subtitle = "1990-2022")

#Plot of data points with horizontal "cut-off" line (= average +- 1.96 sigma, aka confidence interval)
all_plot+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_hline(yintercept =  close_mean - 1.96*close_var)
plot_2019_2022+ geom_hline(yintercept =  close_mean + 1.96*close_var)

#Take dates that cross threshold
extreme_val = data.frame()
extreme_index = c()

for (i in c(1:length(vix_data$DATE))){
  if (vix_data$CLOSE[i] > (close_mean + 1.96*close_var)){
    extreme_val = rbind(extreme_val, data.frame(DATE= vix_data$DATE[i], CLOSE=vix_data$CLOSE[i]))
    extreme_index = append(extreme_index, i)
  }
}

#Plot to show selected points
all_plot+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2019_2022+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2016_2019+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2013_2016+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2010_2013+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2007_2010+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2004_2007+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_2001_2004+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_1998_2001+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_1995_1998+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')
plot_1990_1992+geom_hline(yintercept =  close_mean + 1.96*close_var) + geom_point(data= extreme_val, aes(DATE, CLOSE), color='red')

#sum = 0
#for (i in c(1: length(vix_data$DATE))){
#  if((vix_data$OPEN[i] -vix_data$CLOSE[i]) != 0){
#    print(i)
#    print(vix_data$OPEN[i])
#    print(vix_data$CLOSE[i])
#    print("------------")
#  if(i >200){
#    break()
#  }
#  }
#}

print(extreme_val)

diff_days = c()

for (i in c(1: length(extreme_val$DATE)-1)){
  diff_days = c(diff_days, (extreme_val$DATE[i+1]-extreme_val$DATE[i]))
}

mean_diff_days = mean(diff_days)
var_diff_days = sqrt(var(diff_days))
#To determine separate events we will find outlines using the 1.5* IQR (i.e. outside whiskers)
#Since the variation is too large; a quick glance at diff_days will show why

IQR(diff_days)
#A possible method for outrliers according to R
#whisker_diff_days = median(diff_days) + 1.58*IQR(diff_days)/(sqrt(length(diff_days)))
#More Classically
whisker_diff_days = quantile(diff_days)[4] + 1.5*IQR(diff_days)
print(whisker_diff_days)

#Another possibly(!) more accurate is looking at difference of indici, this way holidays and weekends are accounted for
diff_index = c()

for (i in c(1: length(extreme_index)-1)){
  diff_index = c(diff_index, (extreme_index[i+1]-extreme_index[i]))
}

mean_diff_index = mean(diff_index)
var_diff_index = sqrt(var(diff_index))

whisker_diff_index = quantile(diff_index)[4] + 1.5*IQR(diff_index)
print(whisker_diff_index)
#Not a good method sinze IQR = 0

#Find First/last day of every event and maximum value
days_events = data.frame(DATE= extreme_val$DATE[1], CLOSE= extreme_val$CLOSE[1])
max_index = 1



print("NEW")
print(extreme_val$DATE[1])
print(extreme_val$CLOSE[1])

#Note that the code below can be made better by splitting it up in 3 seperate for loops
for (i in c(2:length(extreme_val$DATE))){
  
  if ((extreme_index[i] - extreme_index[i-1])> 1){
  #if ((extreme_val$DATE[i]-extreme_val$DATE[i-1])>30){
    print("MAX")
    print(extreme_val$DATE[max_index])
    print(extreme_val$CLOSE[max_index])
    print("LAST")
    print(extreme_val$DATE[i-1])
    print(extreme_val$CLOSE[i-1])
    print("---------")
    print("NEW")
    print(extreme_val$DATE[i])
    print(extreme_val$CLOSE[i])
    
    #Add the most extreme day
    days_events = rbind(days_events, data.frame(DATE= extreme_val$DATE[max_index], CLOSE=extreme_val$CLOSE[max_index]))
    #The most extreme is tracked within each event period, so new extreme day has to be updated to first day of the new event period
    max_index = i
    #Add last day of the event
    days_events = rbind(days_events, data.frame(DATE= extreme_val$DATE[i-1], CLOSE=extreme_val$CLOSE[i-1]))
    #Add first day of the new event
    days_events = rbind(days_events, data.frame(DATE= extreme_val$DATE[i], CLOSE=extreme_val$CLOSE[i]))
    
  } else if (extreme_val$CLOSE[max_index] < extreme_val$CLOSE[i-1]){
    #track most extreme day within a event time
    max_index = i-1
  } else if(i == length(extreme_val$DATE)){
    #Last maximum
    days_events = rbind(days_events, data.frame(DATE= extreme_val$DATE[max_index], CLOSE=extreme_val$CLOSE[max_index]))
    # The very last has to be added
    days_events = rbind(days_events, data.frame(DATE= extreme_val$DATE[i], CLOSE=extreme_val$CLOSE[i]))
  } 
}
print(days_events)

#After sorting events manually, the highest vix will be noted such that poisson model can be found 
highest_vix = c(as.Date("1990-08-06"), as.Date("1990-08-23"), as.Date("1991-01-14"), as.Date("1997-10-30"), 
                as.Date("1998-10-08"), as.Date("2001-09-20"), as.Date("2001-10-12"), as.Date("2002-08-05"),
                as.Date("2002-10-07"), as.Date("2008-11-20"), as.Date("2010-05-20"), as.Date("2011-08-08"),
                as.Date("2011-10-03"), as.Date("2015-08-24"), as.Date("2018-02-05"), as.Date("2018-12-24"),
                as.Date("2020-03-16"), as.Date("2020-06-11"), as.Date("2021-01-27"), as.Date("2022-03-07"))

#The time between poisson events is expontially distributed
diff_highest = c()

for (i in c(1: length(highest_vix)-1)){
  diff_highest = c(diff_highest, (highest_vix[i+1]-highest_vix[i]))
}

#The MLE of the exponential distribution is 1/E[X]
mean_diff_highest = mean(diff_highest)
lambda_rate = 1/mean_diff_highest
print(lambda_rate)

#Cumulative distr.
x_expgraph = c(0:max(diff_highest)*(1.01))
plot(x_expgraph, pexp(x_expgraph, rate=lambda_rate), type = "l", col= 'red',
     main = "Predicted Cumulative distribution function of Days between highest measured vix in seperate events",
     xlab = "Amount of Days since last event",
     ylab= "Probability of event recurring", )
points(diff_highest, pexp(diff_highest, rate=lambda_rate), col='blue')

legend("bottomright", legend = c("Cum. Distr. Function", "Days between max vix days"), col= c("red", "blue"), lty = c(1, NA), pch = c(NA, 1))

#Cum. distr. with emp.
plot( ecdf(diff_highest), col='red',
      main = "Predicted Cumulative distribution function of Days between highest measured vix in seperate events",
      xlab = "Amount of Days since last event",
      ylab= "Probability of event recurring", )
lines(x_expgraph, pexp(x_expgraph, rate=lambda_rate), col = 'blue')
legend("bottomright", legend = c( "Empirical distr.","Theoretical Cum. Distr. Function"), col= c("red", "blue"), lty = c(NA, 1), pch= c(10, NA))

#Drawing 95% confidence curves from kolmogorov
library(sfsmisc)
ecdf.ksCI(diff_highest, main ="Empirical Distribution Function and Confidence Bands", xlab= "Difference of days")
lines(x_expgraph, pexp(x_expgraph, rate=lambda_rate), col = 'blue')


#Kolmogorov-Smirnov Test; note that sup< 0.301 which is the test statistic for alpha 0.05 hence null hypothesis is not thrown out

#Predict with 95% confidence in which interval the next event will happen
qexp(0, rate = lambda_rate)
qexp(0.95, rate = lambda_rate)

#Check whether the time series is mean-reverting by performing an augmented Dickey-Fuller test 
#Check this claim! Sources say it checks unit root!
library(tseries)
adf.test(vix_data$CLOSE)

#Check whether time series is stationary by performing Kwiatkowski-Phillips-Schmidt-Shin test
kpss.test(vix_data$CLOSE, null = "Level")
#these give contradictory answers; a more powerfull test is needed
library(urca)
summary(ur.ers(vix_data$CLOSE, type = c("DF-GLS"),model = c("constant")))

#To determine order of AR model, A partial correlation function can be used; Theoretically past the order of the AR model, it should become zero
pacf(vix_data$CLOSE, 30, main = "Partial Autocorrelation Function of the VIX")
acf(vix_data$CLOSE, 30, main = "Autocorrelation Function of the VIX")

#What about pacf of only 1 year?
#To select a specific you can use the format vix_data[vix_data$DATE< as.Date("year month day"),]
pacf(vix_data[vix_data$DATE< as.Date("1991-01-01"),]$CLOSE, 30)
acf(vix_data[vix_data$DATE< as.Date("1991-01-01"),]$CLOSE)

#Try to find parameters for AR(1) model
arima0(vix_data$CLOSE, order=c(1,0,0))
alpha_full_100 = arima0(vix_data$CLOSE, order=c(1,0,0))$coef[2]

#AR(11) model
arima0(vix_data$CLOSE, order=c(11,0,0))

#Arima(1,1,1)
arima0(vix_data$CLOSE, order=c(1,1,1))


#Script to plot VIX Half-Lives in case of the AR(1)

AR_half_time = function(start_day, end_day, vix_dataframe){
  #The vix_dataframe has $DATE and $CLOSE
  #start_day and end_day are of as.Date type
  beta_ar = arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0))$coef[1]
  #Half life is given by ln(1/2)/ln(beta); This is since R works with X_t = a + beta*X_{t-1} instead of delta X_t =...
  #Also in this case we work with AR(1)
  #ln is given by log in R
  #print(beta_ar)
  #This printing is to show that the algorithm sometimes returns values with impossible errors i.e. NaN or 0
  #if(log(1/2)/log(beta_ar)>10^10){
  #  print("---=-------")
  #  print(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0)))
  #  print(start_day)
  #  print(log(1/2)/log(beta_ar))
  #}
  
  if (!(is.nan(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0))$var[1])| (arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0))$var[1] <10^{-5}))){
    #Checking for strange errors of arima0 algorithm
    if(log(1/2)/log(beta_ar)>10**5){
      print("---=-------")
      print(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0)))
      print(start_day)
      print(log(1/2)/log(beta_ar))
      print("Is NaN?")
      print(is.nan(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0))$var[1]))
    }
    return(log(1/2)/log(beta_ar))
  }
  #return(log(1/2)/log(beta_ar))
}


AR_half_time_log = function(start_day, end_day, vix_dataframe){
  #The vix_dataframe has $DATE and $CLOSE
  #start_day and end_day are of as.Date type
  beta_ar = arima0(x = log(vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day]), order=c(1,1,1))$coef[1]
  #Half life is given by ln(1/2)/ln(beta); This is since R works with X_t = a + beta*X_{t-1} instead of delta X_t =...
  #Also in this case we work with AR(1)
  #ln is given by log in R
  #print(beta_ar)
  if(log(1/2)/log(beta_ar)>2000){
    print("---=-------")
    print(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,1,1)))
    print(start_day)
    print(log(1/2)/log(beta_ar))
  }
  return(log(1/2)/log(beta_ar))
}

AR_half_time_alpha = function(start_day, end_day, vix_dataframe, alpha_intercept){
  #The vix_dataframe has $DATE and $CLOSE
  #start_day and end_day are of as.Date type
  #alpha_intercept is a float
  beta_ar = arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0), fixed = c(NA, alpha_intercept))$coef[1]
  #Half life is given by ln(1/2)/ln(beta); This is since R works with X_t = a + beta*X_{t-1} instead of delta X_t =...
  #Also in this case we work with AR(1)
  #ln is given by log in R
  #print(beta_ar)
  #if(log(1/2)/log(beta_ar)>10^(10)){
  #  print("---=-------")
  #  print(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0), fixed = c(NA, alpha_intercept)))
  #  print(start_day)
  #  print(log(1/2)/log(beta_ar))
  #}
  if (!(is.nan(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0), fixed = c(NA, alpha_intercept))$var[1])| (arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0), fixed = c(NA, alpha_intercept))$var[1] <10^{-5}))){
    #Checking for strange errors of arima0 algorithm
    if(log(1/2)/log(beta_ar)>10**5){
      print("---=-------")
      print(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0)))
      print(start_day)
      print(log(1/2)/log(beta_ar))
      print("Is NaN?")
      print(is.nan(arima0(x = vix_dataframe$CLOSE[vix_dataframe$DATE<= end_day & vix_dataframe$DATE>=start_day], order=c(1,0,0))$var[1]))
    }
    return(log(1/2)/log(beta_ar))
  }
  #return(log(1/2)/log(beta_ar))
}

ar1_beta= data.frame()

#251 since we count from day 1 to 252
#That runs into problems (matrices that are non-invertable); tryCatch has to be used
for (i in c(1:(length(vix_data$DATE)-251))){
  #print(".-.-.-.-.-.-.-.-.-.-.")
  #print(vix_data$DATE[i])
  #print('--------')
  #print(vix_data$DATE[i+251])
  tryCatch({
  beta_252 = AR_half_time(vix_data$DATE[i], vix_data$DATE[i+251], vix_data) 
  #beta_251 = try(AR_half_time(vix_data$DATE[i], vix_data$DATE[i+251], vix_data))
  ar1_beta = rbind(ar1_beta, data.frame(DATE= vix_data$DATE[i], BET=beta_252))
  }, error=function(e){})
}
plot(ar1_beta$DATE, ar1_beta$BET, pch = 21, bg = "red",
     xlab = "Date", ylab = "Half life (Days)", main = "VIX Half life")
lines(ar1_beta$DATE, ar1_beta$BET, type='l')

plot(ar1_beta$DATE, ar1_beta$BET, xlim = c(as.Date("2000-01-01"), as.Date("2015-01-01")), ylim=c(0, 100))


#Arima of log
#This is imcomplete! The values with impossible variances have NOT been removed
ar1_beta_log= data.frame()

for (i in c(1:(length(vix_data$DATE)-251))){
  #print(".-.-.-.-.-.-.-.-.-.-.")
  #print(vix_data$DATE[i])
  #print('--------')
  #print(vix_data$DATE[i+251])
  tryCatch({
    beta_252 = AR_half_time_log(vix_data$DATE[i], vix_data$DATE[i+251], vix_data) 
    #beta_251 = try(AR_half_time(vix_data$DATE[i], vix_data$DATE[i+251], vix_data))
    ar1_beta_log = rbind(ar1_beta_log, data.frame(DATE= vix_data$DATE[i], BET=beta_252))
  }, error=function(e){})
}
plot(ar1_beta_log$DATE, ar1_beta_log$BET, pch= 10)
plot(ar1_beta_log$DATE, ar1_beta_log$BET, xlim = c(as.Date("2000-01-01"), as.Date("2016-01-01")))

#Fixed Alpha/intercept
ar1_beta_alpha= data.frame()

for (i in c(1:(length(vix_data$DATE)-251))){
  #print(".-.-.-.-.-.-.-.-.-.-.")
  #print(vix_data$DATE[i])
  #print('--------')
  #print(vix_data$DATE[i+251])
  tryCatch({
    beta_252 = AR_half_time_alpha(vix_data$DATE[i], vix_data$DATE[i+251], vix_data, alpha_full_100) 
    #beta_251 = try(AR_half_time(vix_data$DATE[i], vix_data$DATE[i+251], vix_data))
    ar1_beta_alpha = rbind(ar1_beta_alpha, data.frame(DATE= vix_data$DATE[i], BET=beta_252))
  }, error=function(e){})
}
plot(ar1_beta_alpha$DATE, ar1_beta_alpha$BET,pch = 21, bg = "deepskyblue",
      xlab = "Date", ylab = "Half life (Days)", main = expression(paste("VIX Half life with c =  19.52282 ")))
#lines(ar1_beta_alpha$DATE, ar1_beta_alpha$BET, type='l')

#Plotting vega of options and and a portfolio of options to show that 1/K^2 is correct weight

d1 <- function(s,k,o,t) {
  d <- (log(s/k) + o*o*t/2)/(o*sqrt(t))
  return(d)
}

vega <- function(s,k,o,t) {
  v <- s/(2*o) * exp(-d1(s,k,o,t)*d1(s,k,o,t)/2)/(sqrt(2*pi))
  return(v)
}

sumvega <- function(s,o,t) {
  sum <- 0
  for (i in 5*(4:20)){
    sum <- sum + vega(s,i,o,t)/(i*i)
  }
  return(sum)
}

x <- seq(10,100,.01)
plot(x,vega(x,100,.3,.1),
     xlab="S_t",
     ylab="Vega",
     type="l",
     col="blue")
for (i in 5*(4:19)) {
  lines(x,vega(x,i,.3,.1),
        col="blue")
}
plot(x,
     sumvega(x,.3,.1),
     xlab="S_t",
     ylab="Vega",
     type="l",
     col="blue")


