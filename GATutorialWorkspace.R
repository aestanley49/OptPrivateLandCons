####################################################################
## Series of tutorials to learn GAs

####################################################################

### Should make table of contents for different tutorials... 






### Load in packages used 

library(GA)
library(genalg)



### this looked intersting - fitness function specific..
# https://dl.acm.org/doi/pdf/10.1145/2001858.2001929


### Another knapsack problem 
# https://www.dataminingapps.com/2017/03/solving-the-knapsack-problem-with-a-simple-genetic-algorithm/






### ### R code for writing GA without package 

# https://medium.com/@mblorstad/genetic-algorithm-from-scratch-using-r-a36dc664c46e

## set up 
rm(list=ls(all=TRUE))
#if package not installer 
# install.packages("rlist")
library(rlist)
set.seed(1)

# Data
Gems <- data.frame(
  Color = c("Red", "Blue", "Purple", "Orange", "Green", "Pink", "White", "Black", "Yellow"),
  Weight = round(runif(9,0.5,5),2),
  Value = round(abs(rnorm(9,0,5))+0.5,2)
)

# Task: Gem selection. 
# Aim: Get highest combined value.
# Restriction: Max weight of the gem combined = 10. 


InitilzePop <- function(size){
  #pop<- lapply(1:size, function(pop) round(runif(nrow(Gems),0,1),0))
  pop <- (t(sapply(1:size, function(pop) round(runif(nrow(Gems),0,1),0))))
  colnames(pop)<- Gems$Color
  return(pop)
}
  

# fitness function
fitness_function <- function(population){
  
  score<-0
  for (i in 1:nrow(population)) {
    temp <-apply(Gems[which(population[i,1:nrow(Gems)]==1),2:3],2, sum)
    
    weightRestriction <- ## This controls action sum 
    score[i]<- ifelse(temp[2]>weightRestriction, 0 , temp[1])
    
  }
  pop<- cbind(population,score)
  pop<-pop[order(score, decreasing = T),]
  return(pop)
  
}








### This one actually looks good... 

# https://rpubs.com/Argaadya/550805


df_item <- data.frame(item = c("Tires", "Bumper", "Engine", "Chasis", "Seat"), freq = c(80, 50, 70, 50, 70), weight = c(7, 17, 158, 100, 30))
df_item_long <- df_item[rep(rownames(df_item), df_item$freq), c(1, 3)]

df_item_long

weightlimit <- 10000

evalFunc <- function(x) {
  df <- df_item_long[x == 1, ]
  total_weight <- sum(df$weight)
  total_weight <- if_else(total_weight > weightlimit, 0, total_weight)
  return(total_weight)
  
  
}

gann2 <- ga(type = "binary", fitness = evalFunc, popSize = 100, maxiter = 100, run = 20, 
            nBits = nrow(df_item_long), seed = 123)

summary(gann2)


# let's chose one of the solutions as our optimal choice

df_sol <- df_item_long[gann2@solution[1, ] == 1, ]
df_sol <- df_sol %>% group_by(item, weight) %>% summarise(freq = n()) %>% mutate(total_weigth = freq * 
                                                                                   weight)

df_sol

# Check the total weight 
sum(df_sol$total_weigth)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### another example in .r 

# https://towardsdatascience.com/genetic-algorithm-explained-step-by-step-65358abe2bf



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### some econ example that uses a penalty function 

# https://medium.com/the-trading-scientist/portfolio-optimization-in-r-using-a-genetic-algorithm-8726ec985b6f


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### stocking warehouse example with penalty 
# https://medium.com/genetic-algorithm-ga-using-r-package-rgenoud/genetic-algorithm-ga-with-r-package-rgenoud-d176daa5543e


### ### Start here!! 

#GA to solve discrete data to simulate multiple scenarios and use constrains and penalty.
library("rgenoud")
#Create the tables
Prod <- data.frame(prod=c('P1','P2','P3','P4','P5','P6','P7','P8','P9','P10'),Vol=c(5,10,15,5,20,15,10,20,5,5))
WH <- data.frame(WH=c('1','2','3','4'),WH.Cost=c(5,10,12,15),Capacity=c(25,25,25,55))


### Let's create a mock solution to see how functio works 
sol <- c(2,3,4,1,1,4,2,1,2,2)

func <- function(sol){
  #Add the product-warehouse allignment by GA to the product table
  Prod <- cbind(Prod,sol)
  
  #Obtain the warehouse costs based on the stocking allginments
  Prod <- merge(Prod,WH,by.x="sol",by.y="WH")
  
  #Determine the total warehouse stocking costs
  Prod$cost <- Prod$Vol * Prod$WH.Cost
  
  #Total stocking Cost
  Tot.Cost <- sum(Prod$cost)
  
  #Check for Warehouse Capacity
  Cap <- aggregate(Prod$Vol,by=list(Prod$sol),FUN=sum)
  Cap$Group.1 <- as.factor(Cap$Group.1)
  
  #Apply Penalty if the capacity constrains are not met
  Tot.Cost <- ifelse(Cap[Cap$Group.1=='1',2] > 25 || 
                       Cap[Cap$Group.1=='2',2] > 25 || 
                       Cap[Cap$Group.1=='3',2] > 25 || 
                       Cap[Cap$Group.1=='4',2] > 55,
                     Tot.Cost * 1000,
                     Tot.Cost)
  #Return Value
  return(Tot.Cost)
}
#End of objective function

# Define Boundary Matrix
# Each product should be stocked across any of the 4 warehouses
mat <- matrix(rep(c(1,4),10),10,2,byrow = TRUE)
GA <- genoud(func,nvars = 10,max = FALSE,pop.size = 500,max.generations = 100,wait.generations = 10,Domains = mat,boundary.enforcement = 2,data.type.int = TRUE)
#data.type.int will ensure GA produces only integers between the set domains ie 1 & 4
# Maximum possible solution of the equations 
GA$value
# The parameters of the solutions
GA$par








### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# https://towardsdatascience.com/genetic-algorithm-in-r-the-knapsack-problem-3edc5b07d4a7

### 0-1 Knapsack's Problem

item=c('raincoat','pocket knife','mineral water','gloves','sleeping bag','tent','portable stove','canned food','snacks')
weight=c(2,1,6,1,4,9,5,8,3)
survival=c(5,3,15,5,6,18,8,20,8)
data=data.frame(item,weight,survival)

max_weight=25

## Create chromosome... 
#1 means that we bring the item, while 0 means that we left the item
chromosomes=c(0,1,1,0,0,0,0,0,1)
data[chromosomes==1,]


## Create the function that we want to optimize
fitness=function(x)
{
  current_survpoint=x%*%data$survival
  current_weight=x%*%data$weight
  if(current_weight>max_weight)
  {
    return(0)
  }
  else
  {
    return(current_survpoint)
  }
}

## Run GA
GA=ga(type='binary',fitness=fitness,nBits=nrow(data),maxiter=30,popSize=50,seed=1234,keepBest=TRUE)
summary(GA)
plot(GA)

## Look at fitness value (i.e. the survival points in this case) see that mean and median that tends to increases in each generation
## So want to rerun with bigger generation 


GA2=ga(type='binary',fitness=fitness,nBits=nrow(data),maxiter=50,popSize=50,seed=1234,keepBest=TRUE)
GA3=ga(type='binary',fitness=fitness,nBits=nrow(data),maxiter=100,popSize=50,seed=1234,keepBest=TRUE)
plot(GA2)
plot(GA3)

summary(GA3)

chromosomes_final=c(1,1,1,1,1,0,0,1,1)
cat(chromosomes_final%*%data$weight) ## 25 

data[chromosomes_final==1,]

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### This looks interesting... 
# https://github.com/ropensci/NLMR/issues/60
## See last comment... [code pasted below]


library(raster)
#> Lade nötiges Paket: sp
library(landscapemetrics)
library(NLMR)

# @param landscape Starting landscape. If NULL, a random landscape will be created.
# @param size Only needed if landscape is NULL. Vector with four integer values (i) nrow (ii) ncol (iii) resolution (iv) number classes.
# @param metrics Which landscape metrics are used to optimize. Only landscape level allowed.
# @param targets Target value of selected landscape metrics.
# @param energy_threshold If the relative deviation(energy) between the optimized value and the target value is smaller than threshold, algorithm stops. 
# @param max_runs Maximum number of iterations.
# @param no_change Algorithm stopfs if the deviation (energy) does not decrease.
# @param progress Print progress report
# @param Arguments passed on to landscapemetrics::calculate_lsm

optimize_lsm <- function(landscape = NULL, 
                         size = NULL,
                         metrics, targets, 
                         energy_threshold = 0,
                         max_runs = 1000,
                         no_change = NULL,
                         progress = TRUE,
                         ...) {
  
  if (!all(metrics %in% landscapemetrics::list_lsm(level = "landscape",
                                                   simplify = TRUE, verbose = FALSE))) {
    
    stop("Only landscape level metrics allowed. To see a list, please use  landscapemetrics::list_lsm(level = 'landscape', simplify = TRUE).", 
         call. = FALSE)
  }
  
  if (length(metrics) != length(targets)) {
    
    warning("Length of metric and target not equal. Using same target for all metrics.", 
            call. = FALSE)
    
    targets <- targets[[1]]
  }
  
  if (is.null(landscape)) {
    
    raster_values <- sample(x = 1:size[[4]], 
                            size = size[[1]] * size[[2]], 
                            replace = TRUE)
    
    landscape <- raster::raster(matrix(data = raster_values,
                                       nrow = size[[1]], ncol = size[[2]]))
    
    raster::extent(landscape) <- c(0, ncol(landscape) * size[[3]], 
                                   0, nrow(landscape) * size[[3]])
  }
  
  # set no change to 75% of max runs
  if (is.null(no_change)) {
    
    no_change <- floor(max_runs * 0.75)
  }
  
  # counter if energy changed
  counter <- 0
  
  # calculate landscape metrics
  metrics_result <- calculate_lsm(landscape, what = metrics, 
                                  verbose = FALSE,
                                  full_name = TRUE,
                                  ...)
  
  # same order as input
  metrics_result <- metrics_result[order(match(metrics, metrics_result$function_name)), ]
  
  # calculate energy
  energy <- mean(abs(targets - metrics_result$value) / targets * 100)
  
  # random cell ids (as many as max_runs)
  random_id <- sample(1:raster::ncell(landscape), size = max_runs, replace = TRUE)
  
  # random values
  random_value <- unique(raster::values(landscape))
  
  # it would also be possible to always switch to raster cells. But this would only 
  # configuration and not composition
  
  # composition might be problematic because how to add or remove classes completely?
  
  # simulated annealing - not longer than max_runs
  for (i in seq_len(max_runs)) {
    
    random_lsm <- landscape
    
    # get current cell value
    current_value <- random_lsm[random_id[[i]]]
    
    random_lsm[random_id[[i]]] <- sample(random_value[!random_value %in% current_value], size = 1)
    
    # calculate landscape metric after switching
    metrics_result <- calculate_lsm(random_lsm, what = metrics, 
                                    verbose = FALSE,
                                    full_name = TRUE, 
                                    ...)
    
    # same order as input
    metrics_result <- metrics_result[order(match(metrics, metrics_result$function_name)), ]
    
    # calculate difference
    energy_random <- mean(abs(targets - metrics_result$value) / targets * 100)
    
    # lower difference between target and landscape value -> keep random
    if (energy_random < energy) {
      
      # keep random landscape
      landscape <- random_lsm
      
      # keep enery_random as energy
      energy <- energy_random
    } 
    
    # no improvment
    else {
      
      counter <- counter + 1  
    }
    
    # print progress
    if (progress) {
      
      message("\r> Progress: ", i, "/", max_runs, " || energy = ", round(energy, 2),
              "% (threshold = ", energy_threshold, "%) \t\t\t",
              appendLF = FALSE)
    }
    
    # break if annealing is good enough
    if (energy <= energy_threshold || counter > no_change) {
      break
    } 
  }
  
  if (progress) {
    message("")
  }
  
  return(random_lsm)
}

result_a <- optimize_lsm(landscape = landscape, 
                         metrics = c("lsm_l_split", "lsm_l_core_mn"), 
                         targets = c(1.5, 0.005), 
                         energy_threshold = 5,
                         max_runs = 5000, no_change = 5000, 
                         progress = FALSE)

lsm_l_split(result_a)
#> # A tibble: 1 x 6
#>   layer level     class    id metric value
#>   <int> <chr>     <int> <int> <chr>  <dbl>
#> 1     1 landscape    NA    NA split   1.60
lsm_l_core_mn(result_a)
#> # A tibble: 1 x 6
#>   layer level     class    id metric    value
#>   <int> <chr>     <int> <int> <chr>     <dbl>
#> 1     1 landscape    NA    NA core_mn 0.00485
plot(result_a)



result_b <- optimize_lsm(size = c(30, 30, 20, 5),
                         metrics = c("lsm_l_split", "lsm_l_core_mn"), 
                         targets = c(3.5, 0.005), 
                         energy_threshold = 5,
                         max_runs = 5000, no_change = 5000, 
                         progress = FALSE)

lsm_l_split(result_b)
#> # A tibble: 1 x 6
#>   layer level     class    id metric value
#>   <int> <chr>     <int> <int> <chr>  <dbl>
#> 1     1 landscape    NA    NA split   5.21
lsm_l_core_mn(result_b)
#> # A tibble: 1 x 6
#>   layer level     class    id metric  value
#>   <int> <chr>     <int> <int> <chr>   <dbl>
#> 1     1 landscape    NA    NA core_mn 0.005
plot(result_b)








### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### Running through example

## https://vizerybusiness.wordpress.com/2020/03/23/vendor-consolidation-with-genetic-algorithms-in-r-%F0%9F%A7%AC/


data = data.frame(Supplier = c("Tesco", "Morrisons", "Sainsburys"),
                  Apples = c(1,NA,NA),
                  Oranges = c(1,1,NA),
                  Pears = c(NA,NA,1))
# We know the optimal solution would be Tesco + Sainsburys (Covers all 3 fruits) so we expect (1,0,1) to be chosen 


### Fitness function 

fitFunc <- function(x) {
  current_n_vendors = sum(x, na.rm = T) # No. suppliers in chromosome (x out of 3)
  chromosome_data = data[as.logical(x),-1]
  current_n_products = sum(as.logical(colSums(chromosome_data, na.rm = T))) # How many products do we have (x out of 3)
  
  if (current_n_products == 3) 
    return(current_n_vendors) 
  else
    return(3)
}

### GA

# Run the model
GAmodel <- rbga.bin(size = 3, ## No. things you want to optimize ie no rows in dataset 
                    popSize = 100,  ## No. chromosomes that will be generated for GA to optimize 
                    iters = 100, ## No. times GA will run on popn
                    evalFunc = fitFunc  ## Stick fitness function here 
                    )

# Index of the best solution
best_solution = GAmodel$population[which.min(GAmodel$evaluations),]

# Data corresponding to best solution
data_best = data[as.logical(best_solution),]


### # Okay now try another example... 


data = data.frame(Supplier = c("Tesco", "Morrisons", "Sainsburys"),
                  Apples = c(1,1,NA),
                  Oranges = c(1,1,NA),
                  Pears = c(NA,NA,1))

## Now we could now go with Morrisons + Sainsburys, or Tesco + Sainsburys

# Best 2 solutions
best_solutions = unique(GAmodel$population[order(GAmodel$evaluations), ])[1:2, ]

## !!!! Having error here - only seeing one solution ?? 


### Optimizing cost data 

# New dataset with products and costs
data = data.frame(Supplier = c("Tesco", "Morrisons", "Sainsburys"),
                  Apples = c(.4,.3,NA),
                  Oranges = c(.9,.1,NA),
                  Pears = c(NA,NA,.5))
# Like before have 2 solutions that will provide us with 3 products but Morrisons + Sainsburys is the cheapest 

## New fitness function to optimize for cheapest solution 

fitFunc <- function(x) {
  current_n_vendors = sum(x, na.rm = T)
  chromosome_data = data[as.logical(x),-1]
  current_n_products = sum(as.logical(colSums(chromosome_data, na.rm = T)))
  
  current_cost = sum(chromosome_data, na.rm = T) #Calculate the total cost for the solution
  if (current_n_products == 3) 
    return(current_cost) #Instead of optimising the number of vendors, we are optimising total cost
  else
    return(999) #If the current solution doesn’t meet our criteria of returning 3 products, 
                #we need to tell it what price to return instead of the solution price - arbitrary worst cost soln 
}


# Run the model
GAmodel <- rbga.bin(size = 3, popSize = 100, iters = 100, evalFunc = fitFunc)

# Index of the best solution
best_solution = GAmodel$population[which.min(GAmodel$evaluations),]

# Data corresponding to best solution
data_best = data[as.logical(best_solution),]




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# https://blog.datascienceheroes.com/feature-selection-using-genetic-algorithms-in-r/

## ## see "ExtraTutorialFiles" for scripts and data associated with this one (ended up copying and pasting from Github so didn't have to deal with cloning into a preexisting repository..)




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###





### Writing fitness functions

# https://towardsdatascience.com/how-to-define-a-fitness-function-in-a-genetic-algorithm-be572b9ea3b4


# More of a blog and only example is in java
# Good for conceptualizing




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### Working through tutorial 
# https://cran.r-project.org/web/packages/GA/vignettes/GA.html

##

library(GA)
##   ____    _    
##  / ___|  / \     Genetic 
## | |  _  / _ \    Algorithms
## | |_| |/ ___ \   
##  \____/_/   \_\  version 3.2.2
## Type 'citation("GA")' for citing this R package in publications.



### Function optimisation in one dimension
f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)

GA <- ga(type = "real-valued", fitness = f, lower = c(th = lbound), upper = ubound)
summary(GA)

plot(GA)



### Function optimisation in two dimension

Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors)


GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)

plot(GA)

filled.contour(x1, x2, f, color.palette = bl2gr.colors, 
               plot.axes = { axis(1); axis(2); 
                 points(GA@solution[,1], GA@solution[,2], 
                        pch = 3, cex = 2, col = "white", lwd = 2) }
)





monitor <- function(obj) 
{ 
  contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, pch = 20, col = 2)
  Sys.sleep(0.2)
}

GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         popSize = 50, maxiter = 100, 
         monitor = monitor)

summary(GA)





### ### Setting some members of the initial population



### ### Constrained optimisation


## Fitness function 
f <- function(x)
{ 100 * (x[1]^2 - x[2])^2 + (1 - x[1])^2 }

c1 <- function(x) 
{ x[1]*x[2] + x[1] - x[2] + 1.5 }

c2 <- function(x) 
{ 10 - x[1]*x[2] }


## Plot the function and the feasible regions (coloured areas):

ngrid <- 250
x1 <- seq(0, 1, length = ngrid)
x2 <- seq(0, 13, length = ngrid)
x12 <- expand.grid(x1, x2)
col <- adjustcolor(bl2gr.colors(4)[2:3], alpha = 0.2)
plot(x1, x2, type = "n", xaxs = "i", yaxs = "i")
image(x1, x2, matrix(ifelse(apply(x12, 1, c1) <= 0, 0, NA), ngrid, ngrid), 
      col = col[1], add = TRUE)
image(x1, x2, matrix(ifelse(apply(x12, 1, c2) <= 0, 0, NA), ngrid, ngrid), 
      col = col[2], add = TRUE)
contour(x1, x2, matrix(apply(x12, 1, f), ngrid, ngrid), 
        nlevels = 21, add = TRUE)

## A GA solution can be obtained by defining a penalised fitness function:

fitness <- function(x) 
{ 
  f <- -f(x)                         # we need to maximise -f(x)
  pen <- sqrt(.Machine$double.xmax)  # penalty term
  penalty1 <- max(c1(x),0)*pen       # penalisation for 1st inequality constraint
  penalty2 <- max(c2(x),0)*pen       # penalisation for 2nd inequality constraint
  f - penalty1 - penalty2            # fitness function value
}


GA <- ga("real-valued", fitness = fitness, 
         lower = c(0,0), upper = c(1,13), 
         # selection = GA:::gareal_lsSelection_R,
         maxiter = 1000, run = 200, seed = 123)
summary(GA)


fitness(GA@solution)
## [1] -13584.49
f(GA@solution)
## [1] 13584.49
c1(GA@solution)
## [1] -0.002319273
c2(GA@solution)
## [1] -0.0003015433














