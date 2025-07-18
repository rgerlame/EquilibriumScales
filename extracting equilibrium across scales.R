install.packages(deSolve)
library(deSolve)
require(tidyverse)
require(cowplot)

#predator prey L-V

par(mfrow=c(1,2)) #this code means that each plot window will fit two plots

#parameter values
alpha <-1
beta <- 0.1
gamma <- 0.4
delta <- 0.1

(2*pi)/sqrt(alpha*gamma) #this is an approximation


yini <- c(X = 10, Y = 10) #these are the initial population sizes

#this code constructs the model
Lot_Vol <- function (t, y, parms) {
  with(as.list(y), {
    dX <- alpha * X - beta * X * Y #prey equation
    dY <- delta * X * Y - gamma * Y #predator equation
    list(c(dX, dY))
  }) }

times <- seq(from = 0, to = 100, by = 0.01) #this code sets the time interval population size will be sampled
out   <- ode(y = yini, times = times, func = Lot_Vol, parms = NULL) #this code runs the model

head(out) #here you can see the data frame created by the model

#the code below plots the data from the data frame
plot(y=out[, "X"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Animals (#)")
lines(y=out[, "Y"], x = out[, "time"], type = 'l', col = "red")
abline(v=(2*pi)/sqrt(alpha*gamma),lwd=2) #note that this is APPROXIMATELY but not PERFECTLY the cycle lengt, i.e., the vertical line hits close to 10 but not exactly
abline(h=10,lwd=2) 

#get derivatives, make them absolute - then average at different points and windows


out<-as.data.frame(out)
out$deriv<-NA


for(i in 1:nrow(out)) {
  
out[i,4]<-out[i+1,3] - out[i,3] 
  
}

out$lambda<-out$deriv+1

#the code below plots the data from the data frame

par(mfrow=c(2,1))
plot(y=out[, "Y"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Population size")
#lines(y=out[, "Y"], x = out[, "time"], type = 'l', col = "red")

#the code below plots the data from the data frame
plot(y=out[, "lambda"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Per capita rate of change")
abline(a=1,b=0,lty="dashed",col="grey")
segments (y0=1.02,y1=1.02,x0=c(15,35,55,75),x1=c(25,45,65,85), lwd=2)
segments (y0=0.98,y1=0.98,x0=c(10,40,70),x1=c(35,65,95), lwd=2)

#sample 10,000 time windows of different starting and ending points

data <- data.frame(window_size=NA,start=sample(1:10001,100000, replace=TRUE), end=sample(1:10001,100000, replace=TRUE), gm_mean=NA)

data <- data %>% mutate(min =  pmin(start, end), max = pmax(start, end)) %>%
  mutate(start=min,end=max) %>% select(-min,-max) %>%
  mutate(window_size=end-start) %>%
  filter(window_size!=0) %>%
  distinct() #prevents repeats

#want roughly equal in all
ggplot(data,aes(x=window_size)) +
  geom_histogram(binwidth=10) +
  theme_classic() +
  labs(y="Frequency",x="Window size") +
  scale_x_continuous(breaks=seq(0,10000,1000))


gm_mean = function(a){prod(a)^(1/length(a))}

for(i in 1:nrow(data)) {

  tmp<-gm_mean(out[data$start[i]:data$end[i],5])

  data$gm_mean[i]<-abs(tmp-1)

}

data


plot1<-ggplot(data,aes(x=window_size, y=gm_mean)) +
  geom_point(alpha=0.01) +
  #geom_vline(xintercept=(2*pi)/sqrt(alpha*gamma)*100,color="red")+
  labs(y="Time-averaged per capita growth rate",x="Window size") +
  scale_x_continuous(breaks=seq(0,10000,1000)) +
  theme_classic()


par(mfrow=c(1,2)) #this code means that each plot window will fit two plots

#parameter values
alpha <-0.5
beta <- 0.1
gamma <- 0.4
delta <- 0.1

(2*pi)/sqrt(alpha*gamma) #this is an approximation


yini <- c(X = 10, Y = 10) #these are the initial population sizes

#this code constructs the model
Lot_Vol <- function (t, y, parms) {
  with(as.list(y), {
    dX <- alpha * X - beta * X * Y #prey equation
    dY <- delta * X * Y - gamma * Y #predator equation
    list(c(dX, dY))
  }) }

times <- seq(from = 0, to = 100, by = 0.01) #this code sets the time interval population size will be sampled
out   <- ode(y = yini, times = times, func = Lot_Vol, parms = NULL) #this code runs the model

head(out) #here you can see the data frame created by the model

#the code below plots the data from the data frame
plot(y=out[, "X"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Animals (#)")
lines(y=out[, "Y"], x = out[, "time"], type = 'l', col = "red")
abline(v=(2*pi)/sqrt(alpha*gamma),lwd=2) #note that this is APPROXIMATELY but not PERFECTLY the cycle lengt, i.e., the vertical line hits close to 10 but not exactly
abline(h=10,lwd=2) 

#get derivatives, make them absolute - then average at different points and windows


out<-as.data.frame(out)
out$deriv<-NA


for(i in 1:nrow(out)) {
  
  out[i,4]<-out[i+1,3] - out[i,3] 
  
}

out$lambda<-out$deriv+1

#the code below plots the data from the data frame

par(mfrow=c(2,1))
plot(y=out[, "Y"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Population size")
#lines(y=out[, "Y"], x = out[, "time"], type = 'l', col = "red")

#the code below plots the data from the data frame
plot(y=out[, "lambda"], x = out[, "time"], type = 'l', col = "blue", xlab = "Time", ylab = "Per capita rate of change")
abline(a=1,b=0,lty="dashed",col="grey")
segments (y0=1.02,y1=1.02,x0=c(15,35,55,75),x1=c(25,45,65,85), lwd=2)
segments (y0=0.98,y1=0.98,x0=c(10,40,70),x1=c(35,65,95), lwd=2)

#sample 10,000 time windows of different starting and ending points

data <- data.frame(window_size=NA,start=sample(1:10001,100000, replace=TRUE), end=sample(1:10001,100000, replace=TRUE), gm_mean=NA)

data <- data %>% mutate(min =  pmin(start, end), max = pmax(start, end)) %>%
  mutate(start=min,end=max) %>% select(-min,-max) %>%
  mutate(window_size=end-start) %>%
  filter(window_size!=0) %>%
  distinct() #prevents repeats

#want roughly equal in all
ggplot(data,aes(x=window_size)) +
  geom_histogram(binwidth=10) +
  theme_classic() +
  labs(y="Frequency",x="Window size") +
  scale_x_continuous(breaks=seq(0,10000,1000))


gm_mean = function(a){prod(a)^(1/length(a))}

for(i in 1:nrow(data)) {
  
  tmp<-gm_mean(out[data$start[i]:data$end[i],5])
  
  data$gm_mean[i]<-abs(tmp-1)
  
}

data


plot2<-ggplot(data,aes(x=window_size, y=gm_mean)) +
  geom_point(alpha=0.01) +
  #geom_vline(xintercept=(2*pi)/sqrt(alpha*gamma)*100,color="red")+
  labs(y="Time-averaged per capita growth rate",x="Window size") +
  scale_x_continuous(breaks=seq(0,10000,1000)) +
  theme_classic()

plot_grid(plot1,plot2, labels=c("(A)","(B)",label_fontface = "bold"))
