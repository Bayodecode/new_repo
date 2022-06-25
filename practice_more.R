#simulating data
set.seed(100)
x<-runif(50, 10, 400)
x

#Functions - Ex - calculating average
average <- function(x){
  length(x)
  sum(x)
  ave <- sum(x)/length(x)
  return(paste("The average(mean) of the above stimulation is", ave))
}

average(x)

#learning cbind and rbind
average <- function(x){
  n = length(x)
  ave <- sum(x)/n
  result <- cbind(n, ave)
  #note that r does not take multi-argument returns
  result2 <- rbind(n, ave)
  return(result)
}

average(x)

average <- function(x){
  n = length(x)
  ave <- sum(x)/n
  result <- cbind(n, ave)
  return(result)
}

average(x)

#if/else statements are used to set conditions
x <- c(1,2,-3,4)

if(all(x>0)){
  print("All Postives")
} else{
  print("Not all positives")
}


x <- c(1,2,-3,4)

if(all(x>0)){
  print("All Postives")
} else{
  print("Not all positives")
}


####
range(x <- sort(round(stats::rnorm(10) - 1.2, 1)))
if(all(x < 0)) cat("all x values are negative\n")

all(logical(0))  # true, as all zero of the elements are true.

###$


a <- 3
b <- 2
c <- -1
(-b + sqrt(b^2 - 4*a*c)) / (2*a)
(-b - sqrt(b^2 - 4*a*c)) / (2*a)


install.packages("dslabs")
library(dslabs)
data(murders)

str(murders)
names(murders)

murders$state


#exacting strings
state_abv <- substr(murders$state, 1, 2)
list(state_abv)
as.vector(state_abv)
as.data.frame(state_abv)


a <- murders$abb
class(a)

b <- murders[[2]]

identical(a, b)

#vectors and coercion

cities <- c("Beijing", "Lagos", "Paris", "Rio_de_Janeiro", "San_Juan", "Toronto")
temp <- c(35, 88, 42, 84, 81, 30)
names(temp) <-  cities
temp

names(cities) <- temp

12:73

x  <- 1:100
for (i in x) {
  if (i %% 2 != 0) 
    print(i)
  
}

#sorting
library(dslabs)
data(murders)

total_murders <- murders$total
total_murders
sort(murders$total)

#order (Using index)
x <- c(31, 4, 15, 92, 65)
sort(x)

index = order(x)
x[index]

ind <- order(murders$total)
murders$abb[ind]

#max and which.max
max(murders$total)
i_max <- which.max(murders$total)
murders$state[i_max]



murders$population
pop <- murders$population 
sort(pop)
                 
index <- order(pop)
pop[index]
city <- murders$abb[index]
names(pop) <- city
pop
rank(pop)
max(murders$population)
city_max <- which.max(murders$population)
murders$state[city_max]

#murder rate
murder_rate <- murders$total / murders$population * 100000

murders$abb[order(murder_rate)]
ind <- murder_rate < 0.71
ind <- murder_rate <= 0.71
murders$state[ind]
murders$state[murder_rate >= 7]


#safe(murder rate = 1 in 100,000) state in the western region 
west <- murders$region == "West"
safe <- murder_rate <= 1

ind <- safe & west
murders$state[ind]

#California’s murder rate
ind <- which(murders$state == "California")
murder_rate[ind]


murder_rate[ind]

#plot of total murders versus population.

pop_x <- murders$population/10^6
murd <- murders$total

plot(pop_x, murd)

#histogram
murder_rate
x<- with(murders, total / population * 100000)
hist(x)
which.max(x)
which.min(x)


ind_r <- order(murder_rate)
murder_rate[ind_r]
xity <- murders$abb[ind_r]
names(murder_rate) <- xity
sort(murder_rate)

#boxplot
murders$rate <- with(murders, total / population * 100000)
boxplot(rate~region, data = murders)



#Exercise

yn <- "Statess"
xn <- 90
mode(xn)
vector(yn)
nchar(yn)

for(i in yn){
  print (i)
}






#for loop (to iterate over a set/list of numbers)
#For-loops perform the same task over and over while
#changing the variable.  They let us define
#the range that our variable takes, and then
#changes the value with each loop and evaluates
#the expression every time inside the loop.

# creating a function that computes the sum of integers 1 through n
sum_n <- function (n){
  x <- 1:n
  y <- sum(x)
  return(y)
}
sum_n(5)

# Ex. of a very simple for-loop
for(i in 1:5){
  print(i)
}

#for-loop for our summation
m <- 30
s_n <- vector(length = m) 
for(n in 1:m){
  s_n[n] <- sum_n(n)
}

s_n[n]
sum_n(n)

# creating a plot for our summation function
n <- 1:m
n
s_n
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)


grading<-function(y){
  new<-list()
  for (k in 1:length(y)){
    if (y[k]<1.5){new[k]<-"fail"}
    else if(y[k]<2.5){new[k]<-"third class"}
    else if(y[k]<3.5){new[k]<-"second class lower"}
    else if(y[k]<4.5){new[k]<-"second class upper"}
    else {new[k]<-"first class"}
  }
  result<-matrix(new)
  result1<-data.frame(result)
  result1$CGPA<-y
  return(result1)
}

set.seed(1)
data<-runif(10, 0, 5)
grading(data)
