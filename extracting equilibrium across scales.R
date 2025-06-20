#a bunch of patches
#a dispersal kernel
#a bunch of species
#transient part - fluxes over different windows
#different niches 
#


#Will's thing - 

#locally are in vs out 
#or some larger scale

#a timescale specific thing

#limit cycle

#absolute growth rate


install.packages(deSolve)
library(deSolve)

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

# gm_mean(out[1500:2500,"lambda"]) #0.997163
# gm_mean(out[3500:4500,"lambda"]) #0.9902805
# gm_mean(out[5500:6500,"lambda"]) #1.00875
# gm_mean(out[7500:8500,"lambda"]) #1.005231
# 
# points(x=c(20,40,60,80),y=c(0.997163, 0.9902805, 1.00875, 1.005231), pch=19)
# mean(0.997163, 0.9902805, 1.00875, 1.005231)
# 
# gm_mean(out[1000:3500,"lambda"]) #1.00875
# gm_mean(out[4000:6500,"lambda"]) #1.005231
# gm_mean(out[7000:9500,"lambda"]) #1.005231
# 
# points(x=c(22.5,52.5,82.5),y=c(1.003568, 1.003719, 0.9989107), pch=19)
# mean(1.003568, 1.003719, 0.9989107)

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


ggplot(data,aes(x=window_size, y=gm_mean)) +
  geom_point(alpha=0.01) +
  geom_vline(xintercept=(2*pi)/sqrt(alpha*gamma)*100,color="red")+
  labs(y="Frequency",x="Window size") +
  scale_x_continuous(breaks=seq(0,10000,1000)) +
  theme_classic()

#averaging window sizes to exclude trivial variation
tmp <- data %>% group_by(window_size) %>%
  summarize(gm_sum=mean(gm_mean, na.rm=TRUE))


out2<-as.data.frame(tmp)
out2$deriv<-NA

for(i in 1:nrow(out2)) {
  
  out2[i,3]<-out2[i+1,2] - out2[i,2] 
  
}

tmp<-out2 %>% arrange(abs(deriv)) %>%
  head(n=20) %>%
  arrange(window_size)

value<-tmp[1,1]

ggplot(out2,aes(x=window_size, y=deriv)) +
  geom_point(alpha=0.01) 

ggplot(data,aes(x=window_size, y=gm_mean)) +
  geom_point(alpha=0.01) +
  geom_vline(xintercept=value,color="red")

#start position = can identify transient dynamics, i.e., equ only at that scale for windows that exclude transient
