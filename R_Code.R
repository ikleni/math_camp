
library(MASS)
library(reshape2)
library(car)
library(lfe)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(tidyverse)
library(skimr)
library(ggpubr)
library(roll)
library(stargazer)
library(stringr)
library(naniar)
library(profvis)
library(bench)
library(microbenchmark)
library(dlnm)

### R scripts

# install/update packags
install.packages("plotrix")
update.packages("ggplot2")
# use packages
library('ggplot2')

# basic syntax

# string
myString <- "Hello, World!"
print(myString)

# if-else
x <- 1
if(x>=1) {
  "Yes"
} else {
  "No"
}

# for loop
for(i in 1:10){
  print(i)
}


# vectors
apple <- c('red','green',"yellow")
print(apple)

# Get the class of the vector
print(class(apple))

# list
list1 <- list(c(2,5,3),21.3,sin)

# Print the list.
print(list1)

# get the list
list[[2]]

# matrix
M <- matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3)
print(M)

# array
a <- array(c('green','yellow'),dim = c(1,1,2))
print(a)
a[1,1,1]

# factors
apple_colors <- c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple <- factor(apple_colors)

# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))


# data frame
BMI <- 	data.frame(
  gender = c("Male", "Male","Female"), 
  height = c(152, 171.5, 165), 
  weight = c(81,93, 78),
  Age = c(42,38,26)
)
print(BMI)

# functions
myfunc <- function(x)
{
  y <- x + 1
  return(y)
}

# return a list
myfunc <- function(x)
{
  mymin <- min(x)
  mymax <- max(x)
  return(list(min = mymin, max = mymax))
}
x <- rnorm(100, mean = 0, sd = 1)
k <- myfunc(x)
k$min
k$max

# import 
source("myfunc_external.R")
k <- myfunc_external(x)


# data reshaping


# Create vector objects.
city <- c("Tampa","Seattle","Hartford","Denver")
state <- c("FL","WA","CT","CO")
zipcode <- c(33602,98104,06161,80294)

# Combine above three vectors into one data frame.
addresses <- cbind(city,state,zipcode)

# Print the data frame.
print(addresses)

# Create another data frame with similar columns
new.address <- data.frame(
  city = c("Lowry","Charlotte"),
  state = c("CO","FL"),
  zipcode = c("80230","33949"),
  stringsAsFactors = FALSE
)


# Print the data frame.
print(new.address)

# Combine rows form both the data frames.
all.addresses <- rbind(addresses,new.address)

# Print the result.
print(all.addresses)

# merge
merged.Pima <- merge(x = Pima.te, y = Pima.tr,
                     by.x = c("bp", "bmi"),
                     by.y = c("bp", "bmi")
)
print(merged.Pima)
nrow(merged.Pima)

# melt
molten.ships <- melt(ships, id = c("type","year"))
print(molten.ships)

# cast
recasted.ship <- cast(molten.ships, type+year~variable,sum)
print(recasted.ship)

# read data
setwd("/Users/terrywu/Desktop/Math_Camp/Scripts/R")
data <- read.csv("SP500.csv")
head(data)

print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

# subset
data_sub <- data[data$Earnings.Share >= 5,]
data_sub <- data[data$Sector %in% c("Technology", "Health Care"),]

# save
write.csv(data,"output.csv")

# save to RData
save(data, data_sub, file = "sample.RData")
load("sample.RData")

# plots

# pie chart
x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

# Plot the chart.
pie(x,labels)

# data
H <- c(7,12,28,3,41)
M <- c("Mar","Apr","May","Jun","Jul")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Month",ylab="Revenue",col="blue",
        main="Revenue chart",border="red")

# histogram
hist(data$Price,xlab = "Price",col = "yellow",border = "blue")
hist(data$Price[data$Price<=100],xlab = "Price",col = "yellow",border = "blue",breaks = 5)

# line chart
plot(data$Market.Cap)
plot(data$Market.Cap, type="o")

plot(data$X52.Week.High[1:10],type = "o",col = "red")
lines(data$X52.Week.Low[1:10], type = "o", col = "blue")

# generate random num
runif(10)
runif(10, min=10, max = 13)
sample(1:100, 3, replace = T)
sample(1:100, 3, replace = F)

# normal
dnorm(10, mean = 0, sd = 1) # density
pnorm(1, mean = 0, sd = 1) # CDF
rnorm(100, mean = 0, sd = 1) # random generator

# similarly, gamma
rgamma(100, shape = 1, rate = 1)

# summary stats
summary(data$Price)
table(data$Sector)
aggregate(Price~Sector, data = data, FUN=mean)

# missing values
x <- c(12,7,3,4.2,18,2,54,-21,8,-5,NA)

# Find mean.
result.mean <-  mean(x)
print(result.mean)

# Find mean dropping NA values.
result.mean <-  mean(x,na.rm = TRUE)
print(result.mean)

# linear regression
model <- lm(Price ~ Dividend.Yield, data=data)
summary(model)

model <- lm(Price ~ Dividend.Yield - 1, data=data)
summary(model)

model <- lm(Price ~ Dividend.Yield * Earnings.Share, data=data)
summary(model)

model <- lm(Price ~ Dividend.Yield : Earnings.Share, data=data)
summary(model)

# get model results
model$fitted.values
predict(model, data)
predict(model, data, interval="prediction")

# fixed effect
model <- lm(Price ~ Dividend.Yield + factor(Sector), data=data)
model <- felm(Price ~ Dividend.Yield | Sector, data=data)
summary(model)

# logistic
input <- mtcars[,c("am","cyl","hp","wt")]
model <- glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)
summary(model)


# nls
xvalues <- c(1.6,2.1,2,2.23,3.71,3.25,3.4,3.86,1.19,2.21)
yvalues <- c(5.19,7.43,6.94,8.11,18.75,14.88,16.06,19.12,3.21,7.58)

# Plot these values.
plot(xvalues,yvalues)

# Take the assumed values and fit into the model.
model <- nls(yvalues ~ b1*xvalues^2+b2,start = list(b1 = 1,b2 = 3))
summary(model)

###### ggplot2

theme_set(theme_bw())  # pre-set the bw theme.

# scatter plot
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Scatterplot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

# jitter plot
data(mpg, package="ggplot2") # alternate source: "http://goo.gl/uEeRGu")

ggplot(mpg, aes(cty, hwy)) + 
geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")

# correlation plot
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

# Diverging Barcharts
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

# barchart
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)


# Draw plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


# hist
ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral") +
  geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  


# density
ggplot(mpg, aes(cty))+ 
  geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")


# get data
data("economics", package = "ggplot2")

# Compute % Returns
economics$returns_perc <- c(0, diff(economics$psavert)/economics$psavert[-length(economics$psavert)])
economics_m <- economics[1:24, ]



# labels and breaks for X axis text
lbls <- paste0(month.abb[month(economics_m$date)], " ", lubridate::year(economics_m$date))
brks <- economics_m$date

# plot
ggplot(economics_m, aes(x=date)) + 
  geom_line(aes(y=returns_perc)) + 
  labs(title="Monthly Time Series", 
       subtitle="Returns Percentage from Economics Dataset", 
       caption="Source: Economics", 
       y="Returns %") +  # title and caption
  scale_x_date(labels = lbls, 
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())

# multiple line
data(economics_long, package = "ggplot2")
head(economics_long)

df <- economics_long[economics_long$variable %in% c("psavert", "uempmed"), ]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)

# plot
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Economics", 
       y="Returns %", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_color_manual(labels = c("psavert", "uempmed"), 
                     values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

# some advanced tools

# regression table to latex code # library(stargazer)
stargazer(model,digits=5, omit.stat=c("adj.rsq","ser","f"), no.space=TRUE,report = "vc*t", out = "model.tex")

# loop to apply
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)
a_m1

movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)
str(movies_lower)
unlist(lapply(movies,tolower))

dt <- cars
lapply(dt, min)
sapply(dt, min)

avg <- function(x) {  
  ( min(x) + max(x) ) / 2}
fcars <- sapply(dt, avg)
fcars

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

# clean the environment
rm(list = ls())

# tidyverse package

# read a sample dataset
X <- read.csv("hedge_funds_list.csv")
class(X$Firm_Name_Country)
X <- read.csv("hedge_funds_list.csv", stringsAsFactors = F)
class(X$Firm_Name_Country)

# create new variables
X <- X %>%
  mutate(AUM = as.numeric(sapply(X$Firm_Name_AUM_in_millions, gsub, pattern = ",", replacement= ".")))

# subset
Y <- X %>%
  filter(AUM >= 15)

# rename
X <- X %>%
  rename(AUM_1 = AUM)

# replace
X <- X %>%
  replace_with_na(replace = list(Firm_Name_annual_change_in_AUM= 0))

# change data type
Y <- Y %>%
  mutate_if(is.character, as.factor)

# select a set of variables
Y <- Y %>%
  select(AUM, Firm_Name_City)

# summarize
Y <- X  %>% 
  group_by(Firm_Name_Country) %>%
  summarize(total_AUM = sum(AUM_1, na.rm = TRUE)) 

# remove duplicates
X <- X %>%
  distinct()

# merge
X <- X %>% left_join(Y, by = "Firm_Name_Country")




### profiling and benchmarking

gofvis({
  data(diamonds, package = "ggplot2")
  
  plot(price ~ carat, data = diamonds)
  m <- lm(price ~ carat, data = diamonds)
  abline(m, col = "red")
})


# compare functions
mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

# a random vector
x <- runif(100)

# benchmark
microbenchmark(
  mean1(x),
  mean2(x)
)


# compare loop and tidyverse

# We need a function that can identify days that meet two conditions: 
# (1) the temperature equals or exceeds a threshold temperature (27 degrees Celsius in the examples) and
# (2) the temperature equals or exceeds the hottest temperature in the data before that day. 


# Function that uses a loop 
find_records_1 <- function(datafr, threshold){
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold & 
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
  return(datafr)
}

# Function that uses tidyverse functions
find_records_2 <- function(datafr, threshold){
  datafr <- datafr %>%
    mutate_(over_threshold = ~ temp >= threshold,
            cummax_temp = ~ temp == cummax(temp),
            record_temp = ~ over_threshold & cummax_temp) %>%
    select_(.dots = c("-over_threshold", "-cummax_temp"))
  return(as.data.frame(datafr))
}


# create a dataset
example_data <- data.frame(date = c("2015-07-01", "2015-07-02",
                                    "2015-07-03", "2015-07-04",
                                    "2015-07-05", "2015-07-06",
                                    "2015-07-07", "2015-07-08"),
                           temp = c(26.5, 27.2, 28.0, 26.9, 
                                    27.5, 25.9, 28.0, 28.2))

(test_1 <- find_records_1(example_data, 27))
(test_2 <- find_records_2(example_data, 27))

# compare
record_temp_perf <- microbenchmark(find_records_1(example_data, 27), 
                                   find_records_2(example_data, 27))
record_temp_perf


# for larger datasets
data("chicagoNMMAPS")

record_temp_perf_2 <- microbenchmark(find_records_1(chicagoNMMAPS, 27), 
                                     find_records_2(chicagoNMMAPS, 27))
record_temp_perf_2



# plot
autoplot(record_temp_perf)
autoplot(record_temp_perf_2)




