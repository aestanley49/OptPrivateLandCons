
####################################################################
## Series of tutorials to learn GAs

####################################################################

### Should make table of contents for different tutorials... 


## Problem - running out of GenSA tutorials to work on that are begginer level with dfs... 
# solution - work on the same knapsack problem in both GA and SA (and see how to convert code/logic from different coding language like python..)


### try working through the following (might have to do this without package..) 

#### These are both in python 
## https://towardsdatascience.com/optimization-techniques-simulated-annealing-d6a4785a1de7
### https://towardsdatascience.com/simulated-annealing-for-clustering-problems-part-1-3fa8994a3ebb



### published info with GenSA package
# https://journal.r-project.org/archive/2013/RJ-2013-002/RJ-2013-002.pdf
# information for how to use GenSA package

# Example down on page 9 on risk 


library("quantmod")
tickers <- c("GE", "IBM", "JPM", "MSFT", "WMT")
getSymbols(tickers, from = "2000-12-01", to = "2010-12-31")
#[1] "GE" "IBM" "JPM" "MSFT" "WMT"
P <- NULL
for(ticker in tickers) {
  tmp <- Cl(to.monthly(eval(parse(text = ticker))))
  P <- cbind(P, tmp)
  }
colnames(P) <- tickers
R <- diff(log(P))
R <- R[-1,]

mu <- colMeans(R)
sigma <- cov(R)
library("PerformanceAnalytics")
pContribCVaR <- ES(weights = rep(0.2, 5),
                     method = "gaussian", portfolio_method = "component",
                     mu = mu, sigma = sigma)$pct_contrib_ES
obj <- function(w) {
  fn.call <<- fn.call + 1
  if (sum(w) == 0) { w <- w + 1e-2 }
  w <- w / sum(w)
  CVaR <- ES(weights = w,
               method = "gaussian", portfolio_method = "component",
               mu = mu, sigma = sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
  out <- tmp1 + 1e3 * tmp2
  return(out)
}

set.seed(1234)
fn.call <<- 0
out.GenSA <- GenSA(fn = obj, lower = rep(0, 5), upper = rep(1, 5),
                     control = list(smooth = FALSE, max.call = 3000))

fn.call.GenSA <- fn.call
out.GenSA$value
##[1] 0.1141484884
out.GenSA$counts
##[1] 3000
cat("GenSA call functions", fn.call.GenSA, "times.\n")
### GenSA call functions 3000 times.
wstar.GenSA <- out.GenSA$par
wstar.GenSA <- wstar.GenSA / sum(wstar.GenSA)
rbind(tickers, round(100 * wstar.GenSA, 2))
### [,1] [,2] [,3] [,4] [,5]
### tickers "GE" "IBM" "JPM" "MSFT" "WMT"
### "18.92" "21.23" "8.33" "15.92" "35.6"
100 * (sum(wstar.GenSA * mu) - mean(mu))
## [1] 0.03790568876



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###








### circle weighted.. see how set up SA in code
# https://jmsallan.netlify.app/blog/coding-simulated-annealing-in-r/


# function returns the distance to the origin of the center of mass of a possible solution
mass_center <- function(v){
  n <- length(v)
  
  angles <- 0:(n-1) * 2 * pi / n
  x <- v*cos(angles)
  y <- v*sin(angles)
  dcm <- c(sum(x), sum(y))/sum(v)
  dcm <- sqrt(sum(dcm^2))
  
  return(dcm)
}

## How to test different solutions (swap them..?)
swap <- function(v, i , j){
  aux <- v[i]
  v[i] <- v[j]
  v[j] <- aux
  return(v)
}



## Now setting up SA... 

sa_circle <- function(inisol, iter=1000, alpha = 0.9, p0 = 0.9, eval=FALSE){
  
  #setting up tracking of evolution if eval=TRUE
  if(eval){
    evalfit <- numeric()
    evalbest <- numeric()
    temp <- numeric()
  }
  
  n <- length(inisol)
  count <- 1
  
  #initialization of explored solution sol and best solution bestsol
  #and objective funciton values fit  and bestfit
  sol <- inisol
  bestsol <- inisol
  fit <- mass_center(sol) ## run through function to find mass center for that solution
  bestfit <- fit
  
  #estimation of initial temperature
  sol_a <- sapply(1:100, function(x) mass_center(c(1, sample(2:n, n-1))))
  sol_b <- sapply(1:100, function(x) mass_center(c(1, sample(2:n, n-1))))
  delta_f0 <- mean(abs(sol_a - sol_b))
  Tmax <- -delta_f0/log(p0)
  T <- Tmax
  
  ## the simulated annealing loop
  while(count < iter){
    
    #obtaining the testing solution x'
    move <- sample(2:n, 2)
    testsol <- swap(sol, move[1], move[2])
    testfit <- mass_center(testsol)
    
    #checking if we replace x by x'
    if(exp(-(testfit-fit)/T) > runif(1)){
      sol <- testsol
      fit <- testfit
    }
    #updating the best solution
    if(testfit <= bestfit){
      bestsol <- testsol
      bestfit <- testfit
      count <- 1
    }else{
      count <- count + 1
    }
    
    #keeping record of evolution of fit, bestfit and temperature if eval=TRUE
    if(eval){
      evalfit <- c(evalfit, fit)
      evalbest <- c(evalbest, bestfit)
      temp <- c(temp, T)
    }
    
    T <- alpha*T
    
  }
  #returning the solution
  if(eval)
    return(list(sol=bestsol, fit=bestfit, evalfit=evalfit, evalbest=evalbest, temp=temp))
  else
    return(list(sol=bestsol, fit=bestfit))
}



set.seed(1313)
test <- sa_circle(1:37, 
                  iter = 1000, 
                  alpha = 0.99, 
                  p0 = 0.5, 
                  eval = TRUE)

test$fit



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###





### Example of how to write SA in r code without package 
# https://rpubs.com/mstefan-rpubs/salesman

## Create fake cities 
# "not in" operator
'%!in%' <- function(x,y)!('%in%'(x,y))
# extend "LETTERS" function to run from "A" to "ZZ"
MORELETTERS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
# example
MORELETTERS[20:30]

## Put fake cities' coordinates in df 
# function to simulate cities
simCities <- function(n_cities = 5) {
  cities <- matrix(runif(2*n_cities,-1,1),ncol=2)
  rownames(cities) <- MORELETTERS[1:n_cities]
  colnames(cities) <- c("x","y")
  return(cities)
}
# example
cities <- simCities(n = 10)
cities 


## plot cities 
# function to plot cities
plotCities <- function(cities, 
                       main="",
                       bg="white", 
                       main_col="black",
                       point_col="deepskyblue") {
  par(bg=bg)
  plot(cities, 
       pch=16, cex=3,
       col=point_col, 
       ylim=c(-1,1.1), xlim=c(-1,1),
       yaxt="n", xaxt="n",
       ylab="", xlab="",
       bty="n",
       main=main, col.main=main_col)
}

# example
plotCities(cities)


## Function stat generates random routes that start and end with city A
# function to generate a random route
randomRoute <- function(cities) {
  start <- rownames(cities)[1]
  route <- sample(rownames(cities)[-1])
  route <- c(start,route,start) # return back home
  return(route)
}

# example
route <- randomRoute(cities)
route


## Distances between cities..
# function to compute adjacency matrix
compAdjMat <- function(cities) return(as.matrix(dist(cities)))

# example
cities <- simCities(5)
adjmat <- compAdjMat(cities)
adjmat

# function to compute the distance of a route
distRoute <- function(adjmat, route) {
  d <- 0
  for(i in 2:nrow(adjmat)) {
    d <- d + adjmat[route[i-1],route[i]]
  }
  return(d)
}

# example
route1 <- c("A","C","E","B","D","A")
distRoute(adjmat, route1)

### simulated annealing 

simAnneal <- function(cities, temp=1e4,
                      cooling=5e-3, break_after=1e2,
                      sleep=0.05) {
  
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # start with random route
  best_route <- randomRoute(cities)
  min_d <- distRoute(adjmat,best_route)
  
  # variable tracking
  track_temp <- c()
  track_prob <- c()
  track_dist <- c()
  
  # iterative loop
  stable_count <- 0
  while(stable_count < break_after) {
    
    # conduct swap
    ik <- sort(sample(2:nrow(cities),2))
    new_route <- swap(best_route, i=ik[1], k=ik[2])
    new_d <- distRoute(adjmat,new_route)
    
    # probability of adjusting route
    improvement <- min_d - new_d
    p_adjust <- ifelse(improvement > 0, 1, exp(improvement/temp))
    
    # adjust route?
    adjust <- ifelse(p_adjust >= runif(1,0,1), T, F)
    
    # if adjustment
    if(adjust) {
      best_route <- new_route
      min_d <- new_d
      stable_count <- 0
      plotRoute(cities, best_route, min_d)
      legend("topright", legend=round(temp,4), 
             bg="transparent", 
             bty="n",
             text.col="black")
      Sys.sleep(sleep)
    } else {
      stable_count <- stable_count+1
    }
    
    # update on variable tracking
    track_temp <- c(track_temp,temp)
    track_prob <- c(track_prob,p_adjust)
    track_dist <- c(track_dist,new_d)
    
    # cool down
    temp <- temp*(1-cooling)
    
  } # end of iterative loop
  
  # return
  return(list(distance = min_d,
              route = best_route,
              track_temp = track_temp,
              track_prob = track_prob,
              track_dist = track_dist)
  )
  
  
}
# example
simAnneal(cities)





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### sampling chees board with queens example 
# https://github.com/v-iashin/SamplingChessboardWithQueens

## looks intensive to dig into - lots of scripts feeding in.. 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Knapsack problem (done with SA and GA)
# https://stackoverflow.com/questions/62012037/gensa-and-sa-giving-nonsense-output-for-knapsack-problem

# Note - GenSA "This function searches for global minimum ..."
# Not sure if this is the best example - doesn't include temperature or maxit 

library(GenSA)
library(GA)

df <- read.table(text = ",gewichten(gr),waarde
Voorwerp 1,70,135
Voorwerp 2,73,139
Voorwerp 3,77,149
Voorwerp 4,80,150
Voorwerp 5,82,156
Voorwerp 6,87,163
Voorwerp 7,90,173
Voorwerp 8,94,184
Voorwerp 9,98,192
Voorwerp 10,106,201
Voorwerp 11,110,210
Voorwerp 12,113,214
Voorwerp 13,115,221
Voorwerp 14,118,229
Voorwerp 15,120,240", sep = ",", header = T
)


#Define function
knapsack <- function(x) {
  f <- sum(x * df[3])
  penalty <- sum(df[2]) * abs(sum(x*df[2]) - 750)
  -(f - penalty) # SIMPLY ADDED A MINUS SIGN
}

init <- runif(1, -5000, 5000)

onder <- rep(-5000, length(init))
boven <- rep(5000, length(init)) 



controlelijst <- list(max.time=25, nb.stop.improvement = 100)

resultaatSA <- GenSA(par=init, lower = onder, upper = boven, fn=knapsack, control=controlelijst)

resultaatSA$par # 0.5233775
head(resultaatSA$trace.mat)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### Concrete data (done using multiple different optimization methods but only looking at SA)

#! Issue with this tutorial - avNNet_model_final not provided online... 
# https://rpubs.com/jeandsantos88/Concrete_Mixture_Optimization

## Have to set the problem up by loading in packages and datasets... 
# Install and load packages
if (!require(pacman)) {install.packages("pacman", verbose = F, quiet = T)} else require(pacman, quietly = T)
suppressWarnings(pacman::p_load(plyr, caret, tidyverse, tidyselect, readr, readxl, parallel, doParallel, gridExtra, pso, GA, GenSA, DEoptim, GGally, ggfortify, broom, knitr, kableExtra, install = T))
# Load library
download.file(url = "http://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls", destfile = "Concrete_Data.xls", method = "curl", quiet = TRUE)

# Import Data
concrete_data <- read_xls(path = "Concrete_Data.xls", sheet = 1)

# Rename variables
colnames(concrete_data) <- c("Cement", "Slag", "Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Strength")

ingredients <- c("Cement", "Slag", "Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate")

# Recalculate composition as proportions
concrete_data[, ingredients] <- t(apply(X = concrete_data[, ingredients], MARGIN = 1, FUN = function(x) {x/sum(x)}))


## 5 Obtain optimal concrete mixture 
# Import predictive model
avNNet_model_final<- readRDS(file = "Models/avNNet_model.rds")

# Define minimum and maximum values for each input
margin <- 0.05 
Cement_min_max <- c(min(concrete_data$Cement)*(1-margin), 
                    max(concrete_data$Cement)*(1+margin)) %>% round(4)
Slag_min_max <- c(min(concrete_data$Slag)*(1-margin), 
                  max(concrete_data$Slag)*(1+margin)) %>% round(4)
Ash_min_max <- c(min(concrete_data$Ash)*(1-margin), 
                 max(concrete_data$Ash)*(1+margin)) %>% round(4)
Superplasticizer_min_max <- c(min(concrete_data$Superplasticizer)*(1-margin),
                              max(concrete_data$Superplasticizer)*(1+margin)) %>% round(4)
Coarse_Aggregate_min_max <- c(min(concrete_data$Coarse_Aggregate)*(1-margin),
                              max(concrete_data$Coarse_Aggregate)*(1+margin)) %>% round(4)
Fine_Aggregate_min_max <- c(min(concrete_data$Fine_Aggregate)*(1-margin),
                            max(concrete_data$Fine_Aggregate)*(1+margin)) %>% round(4)

lower_limits <- c(Cement_min_max[1], Slag_min_max[1], Ash_min_max[1], Superplasticizer_min_max[1], Coarse_Aggregate_min_max[1], Fine_Aggregate_min_max[1])
upper_limits <- c(Cement_min_max[2], Slag_min_max[2], Ash_min_max[2], Superplasticizer_min_max[2], Coarse_Aggregate_min_max[2], Fine_Aggregate_min_max[2])

# Set fixed value for aging
days_aging <- 28

# Set minimum and maxium acceptable amount of water in each mixture
maximum_water <- (max(concrete_data$Water)*1.05) %>% round(4) 
minimum_water <- (min(concrete_data$Water)*0.95) %>% round(4)

n_best_solutions <- 5 # Number of best solutions to keep (for selected methods)

# Optional: Create a starting point for the genetic algorithm
starting_point <- sapply(X = concrete_data, FUN = mean) %>% t() %>% data.frame() %>% select("Cement", "Slag", "Ash", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate") %>% as.matrix()


## Simulated Annealing 5.3

# Set parameter settings for search algorithm
max_iter <- 500 # maximum number of iterations
pop_size <- 10 # population size

# Randomly select one sample from the data as a starting point
set.seed(1)
start_index <- sample(x = 1:nrow(concrete_data), size = 1)
start_point <- concrete_data[start_index, ]

# Select the n most dissimilar observations from the starting point
n_observations <- pop_size
index_starting_observations <- caret::maxDissim(a = start_point, b = concrete_data, n = n_observations)
starting_observations <- concrete_data[index_starting_observations, ]

# Remove water from subset
starting_observations <- starting_observations %>% dplyr::select(-Water) # Because we require all the solutions to add to one, the search procedure will be run without water and the proportion of water will be determined by subtracting the sum of the proportion of all ingredients minus 1


# Create custom function for assessing solutions
eval_function <- function(x, model, min_water, max_water, age = 28) {
  
  x1 <- x[1]; x2 <- x[2]; x3 <- x[3]; x4 <- x[4]; x5 <- x[5]; x6 <- x[6]
  
  # Create dataframe with proportion of each solid component
  solution_df <- data.frame(Cement = x1, 
                            Slag = x2, 
                            Ash = x3, 
                            Superplasticizer = x4, 
                            Coarse_Aggregate = x5, 
                            Fine_Aggregate = x6)
  
  # Calculate proportion of water
  solution_df$Water <- 1-rowSums(solution_df) # Water = 1-sum(solids)
  
  # Create death-penalty score for solutions with water content outside acceptable range
  if(solution_df$Water >= min_water & solution_df$Water <= max_water & rowSums(solution_df) == 1) {
    
    # Add pre-defined age to temporary solution
    solution_df$Age <- age
    
    return(-predict(model, solution_df)) # maximize strength
    
  } else {
    
    return(0)
  }
  
}


set.seed(1)
SA_output <- starting_observations
SA_output$Water <- NA
SA_output$Strength <- NA
i_max <- NA
trace_max_SA <- NA

SA_T0 <- Sys.time() # record start time

# Repeat the process of finding the optimal solution for each starting observation
for(i in 1:nrow(SA_output)) {
  results <- GenSA::GenSA(
    par = SA_output[i, 1:6] %>% as.matrix(),
    fn = eval_function, 
    lower = lower_limits,
    upper = upper_limits,
    control = list(
      maxit = max_iter/pop_size, 
      verbose = F),
    model = avNNet_model_final,
    min_water = minimum_water,
    max_water = maximum_water
  )
  
  # Save the predictions
  SA_output$Strength[i] <- abs(results$value)
  # Save the input variables
  SA_output[i, 1:6] <- results$par
  
  if (SA_output$Strength[i] == max(SA_output$Strength, na.rm = T)) {
    i_max <- i
    trace_SA_max <- results$trace.mat
  }
}

SA_T1 <- Sys.time() # record end time
(SA_Time <-  SA_T1 - SA_T0)




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### 



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# https://rdrr.io/cran/GenSA/man/GenSA.html # has examples of how to use package... 

### might be worth digging into this comparison if can find the code somewhere? 
# https://www.portfolioprobe.com/2012/07/23/a-comparison-of-some-heuristic-optimization-methods/





### someone's homework? 
# https://github.com/pkasela/Decision-Models-Assignments/blob/master/Assignment%204/Assignment4.pdf

### Magic 9 problem with SA
# https://github.com/TomMakesThings/Magic-19




# next should try and search medium.com for tutorials in .r 
# as well as towardsdatascience
# github? 
# blog.datascienceheroes

### try searching explictly using GenSA in search terms - seems to be only good R package. 



# Matejka, Justin, and George Fitzmaurice. “Same stats, different graphs: Generating datasets with varied appearance and identical statistics through simulated annealing.” Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems. ACM, 2017.


# ?https://gist.github.com/robertness/e69127ed752ef78f78db



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## https://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/

# https://stackoverflow.com/questions/61873403/how-to-use-simulated-annealing-in-r-gensa-for-a-function-with-discrete-variabl

# https://www.math.wustl.edu/~feres/Math350Fall2012/Projects/mathproj09.pdf
    # in python.. 

## http://www.sortie-nd.org/lme/R%20Tutorials/Murphy%20-%20Neighborhood%20Models%20Tutorial.pdf


# https://bergant.github.io/nlexperiment/flocking_sa.html

