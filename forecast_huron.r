#DEPENDENCES 
library(forecast)
library(fpp2)
library(GGally)
library(ggplot2)

#  A first view 

plot(huron)
#  forecast horizon 
f <- 8



###        Generate  linear regression
tslm_huron <- tslm(huron ~ trend)
#  forecast with linear regression
fc_lm_huron <- forecast(tslm_huron, h=f)


### a log transformation is now perfomed
tslm_log_huron <- tslm(huron ~ trend, 
                       lambda = 0)

fc_log_huron <- forecast(tslm_log_huron, h=f)




### piecewise linear regression
#we take the time
# a change is made in approximately 1915

# first the max of time of huron data 
t <- time(huron)
#  the knot is generate
t.break <- 1915

# now we convert the data before 1915  to zero
t_piece <- ts(pmax(0,t-t.break), start=1875)



tslm_pw_huron <- tslm(huron ~ t + t_piece)

l=length(t)  # 98
t[l]   #  1972 
t_new <-t[l] + seq(h)  #  the data to forecast 
t_piece_new <- t_piece[length(t_piece)]+seq(h)


newdata <- cbind(t=t_new,
                 t_piece=t_piece_new) %>%
    as.data.frame()
newdata

# forecast
fc_tslm_pw_huron <- forecast(
    tslm_pw_huron,
    newdata = newdata
)


tslm_spline_huron <- splinef(huron, lambda = 0)
fc_tslm_spline_huron <- forecast(
    tslm_spline_huron,
    newdata = newdata
)





#  Plot

# plot the results
autoplot(huron) +
    autolayer(fitted(tslm_huron), series = "Linear") +
    autolayer(fitted(tslm_log_huron), series="Logarithm") +
    autolayer(fitted(tslm_pw_huron), series = "Piecewise") +
    autolayer(fitted(tslm_spline_huron), series = "Cubic Spline") +
    autolayer(fc_tslm_pw_huron, series="Piecewise") +
    autolayer(fc_tslm_huron$mean, series = "Linear") +
    autolayer(fc_tslm_log_huron$mean, series="Logarithm") +
    autolayer(fc_tslm_spline_huron$mean, series="Cubic Spline") +
    xlab("Year") +  ylab("Water level") +
    ggtitle("Lake Huron water level change") +
    guides(colour=guide_legend(title=" "))
