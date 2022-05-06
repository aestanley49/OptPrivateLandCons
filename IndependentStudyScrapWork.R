#############################
### ### ### ### ### ### Creating toy landscape


### ### ### ### Load packages 

#install.packages("landscapeR")
library(landscapeR)
library(truncnorm) #truncated normal distribution (can specify bounds of randomly generated number)
library(raster)
library(ggplot2)
library(here)
library(tidyverse)

library(GA)
library(genalg)



#############################
### ### ### ### ### ### Okay so how do I do this...

## regulat grid with square tiles.. 100 x 100
  # the one tile selected at random, aggregate other tiles around it until meet some sort of condition
  # Would also want variation of small and large clusters 
  # Need to number/have ID for each cluster - okay to have them touching if can ID as different

## Create an empty landscape
## simulate random size values then save these to a integer vector
## Make all patches uniquly labeled with val so can melt? 


### ### Assumptions made
# bgr = 0 (patches can only be generated where there aren't already cells)
# size of patches
# density of patches (how many would be in a given landscape)

## set number of patches
n.patch <- 200
## Specify dimensions of landscape 
m = matrix(0, 100, 100)
## Put that landscape into a raster 
r = raster(m, xmn=0, xmx=100, ymn=0, ymx=100)
## First random value generated for size
k1 <- rtruncnorm(n=1, a=0.1, b=60, mean=10, sd=25.09)
# create first patch then add to landscape
rr = makePatch(r, size = k1, val=1, rast=TRUE) # create first patch then add to landscape

## Loop over landscape to create patches 
for (i in 1:n.patch){
  k1 <- rtruncnorm(n=1, a=0.1, b=60, mean=10, sd=25.09) # Generate a random value for patch size with a min of .1 and max of 60
  ID <- c(i + 1) # To create the ID, add 1 for each run 
  rr = makePatch(rr, size = k1, val=ID, rast=TRUE) 
  plot(rr)
}

rrr <- as.data.frame(rr)

### Save plot for ppt: 
printme <- plot(rr)

ggsave(filename = "RPlot.jpg", plot = printme, path = "/Users/annabellestanley/Documents/R_projects/OptPrivateLandCons")





#############################
### ### ### ### ### ### 

### ### I think if convert this to a dataframe where have each patch ID as independent row (need to consolidate)
### ### then can assign values/attributes to individual patches and run individual patches through GA 
### ### so each patch is a gene.. not sure what cap on number of genes should be 

#r2d2 <- rrr %>% rownames_to_column() %>% rename(ID = rowname) %>%  # this is creating a unique ID for every cell on grid
 # group_by(layer) %>% add_count(layer) %>%  #Create new column of size of each patch called "n" 
  #pivot_wider(names_from = layer, values_from = n)

r2d2 <- rrr %>%   group_by(layer) %>% add_count(layer) %>%  #Create new column of size of each patch called "n" 
  pivot_wider(names_from = layer, values_from = n, values_fn = length) #creating new column where colnames are ID for each patch


r2d2 <- as.data.frame(t(r2d2)) #want to switch patches to be rows  

## building mat with muiltiple actions assigned per patch   
nrow<-length(r2d2$V1) #want same number of rows as we have patches
ncol<- 4 #(number of different actions: Do nothing, graze, timber management, burn)
mat01 <- matrix(rbinom(nrow*ncol,1,.5),nrow,ncol)
Patch_Act <- cbind(r2d2, mat01)
Patch_Act <- rownames_to_column(Patch_Act, "Patch_ID")
colnames(Patch_Act) <- c("Patch_ID", "Size"  ,     "Action_Zzz"     ,   "Action_Graze"  ,      "Action_timMang"   ,     "Action_Burn"  )


################################################################################################################
################################################################################################################
### ### ### ### ### ### GAs


#########
### Let's try applying the previous fitness function from tutorial to the landscape (want 1 of each action types...)

data <- Patch_Act[,3:6]

### Fitness function 

## creating a subset of data where I know what the solution should be... 
data2 <- data[c(18:22),]

maxcount = 5 

fitFunc <- function(x) {
  current_n_patches = sum(x, na.rm = T) # No. actions in solution (x out of 4)
  chromosome_data = data[as.logical(x),]
  current_n_products = sum(as.logical(colSums(chromosome_data, na.rm = T))) # How many products do we have (x out of 3)
  
  if(current_n_patches > maxcount)
    return(0)
  else
    return(current_n_products)
  
  if (current_n_products != 4) 
    return(0) 
  else
    return(current_n_patches)
}


GA=ga(type='binary',
      fitness=fitFunc,
      nBits=nrow(data),
      maxiter=30,
      popSize=50,
      seed=100,
      keepBest=TRUE)

summary(GA)
plot(GA)

## give it a starting popn of 50 individual soln. each soln has no more than 2 1s in it. That satisfies constraintt of no more than 2 patches. 
# Take matrix 22 x 50 matrix of 0s, for each row in matrix, randomly select 2 numbers and change to 1s 
# initpop? 


### Need to create some kind of penalty to limit the number of patches being selected..



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### New Example.... 


## building a df with only one 1 per row 

r2d2$ran=sample(4,nrow(r2d2),T)
Patch_Actions <- rownames_to_column(r2d2, "Patch_ID")
Patch_Actions <- Patch_Actions %>%  
  mutate(Action_Zzz = case_when(ran == 1 ~ 1, ran != 1 ~ 0)) %>% 
  mutate(Action_Graze = case_when(ran == 2 ~ 1, ran != 2 ~ 0)) %>% 
  mutate(Action_timMang = case_when(ran == 3 ~ 1, ran != 3 ~ 0)) %>% 
  mutate(Action_Burn = case_when(ran == 4 ~ 1, ran != 4 ~ 0)) 
Patch_Actions$ran <- NULL ## remove column with randomly assigned numbers 
colnames(Patch_Actions) <- c("Patch_ID", "Size"  ,     "Action_Zzz"     ,   "Action_Graze"  ,      "Action_timMang"   ,     "Action_Burn"  )



### Need to add bird response 

## randomly generate bird abundance per patch 
  for(i in 1:nrow(Patch_Actions)){ #iterate across the rows of mat
    Patch_Actions[i,7] <- ceiling(rtruncnorm(n=1, a=0.1, b=30, mean=15, sd=10.09)) # Generate a random value for bird abundance and round to nearest whole number 
  }


## Now need functions for how actions impact bird abundance 

# 3 different actions to assign (rest is null): Action_Graze", "Action_timMang", "Action_Burn" 

## try proportional relationship between timber mang. and bird (as 1 goes up so does the other)
  # m = .4, x = no cells treated (start with 1) and y = no birds generated (start with current value then add)

Action_Bird_Mod_timMang <- function(Patch_Actions){ ## note this function just creates a new column... 
  # select ONLY the patches that have timbermanagement actions and do linear calculation
  Patch_Actions %>% mutate(TimM_bird_count = case_when(Action_timMang == 1 ~ Patch_Actions$Size * .4 + Patch_Actions$V7, 
                                                       Action_timMang != 1 ~ 0 )) # otherwise place a 0 
 ##  y = m*x + b0
  # For each of these patches... apply 
  #b0[i] <- subsetpatchactions[i,7] #intercept is starting no birds
 # m <- .4 # set proportional amount 
  #x[i] = subsetpatchactions[i,2] #x is patch size 
 # y[i] = m*x[i] + b0[i]
}


## 75% of time burning is benefital for birds and 25% it's not 
  # chose random number of cells within a patch to burn 
  # flip loaded coin to see whether outcome is good or bad 
  # depending on outcome, either add X birds from patch or subtract Y 

## biased coinflip
Action_Bird_Mod_Burn <- function(Fillme){
  x <- rbinom(n = Fillme, size = 1, prob = .65) ## flip a biased coin (can use this as a switch) # n = number of times flip coin 
  birds <- 2 # number of birds we get for burning a single cell  (* number of cells in next line)
  ((x*birds*Fillme + Patch_Actions$V7*x)*Patch_Actions$Action_Burn) # note, for this function, don't get the number of birds started with back if have a bad burn
}

## Step-wise function for grazing? (where x = no acres, y = + bird abundance)
  # For every range of cells treated, bird abundance will increase by A 

Action_Bird_Mod_Graze <- function(x){
  k = 0 # starting value 
  b = 7 #length of step so for every 2 cells modified.... 
  a = .5 # level btw steps ... get additional .5 birds 
  # SO getting additonal .5 birds for every 2 cells modified
  y <- a*floor(x/b) + k 
   ## Will need to add number of additional birds generated to starting value
  (y + Patch_Actions$V7)*Patch_Actions$Action_Graze ##  Also need to add increase got from grazing action to the starting number of birds
}


### ### apply functions to adjust bird numbers across patches... 

# Timber management function
Patch_Actions <- Action_Bird_Mod_timMang(Patch_Actions)

# Burn function
Patch_Actions$burn_birds_count <- Action_Bird_Mod_Burn(Patch_Actions$Size) 

# Grazing function
Patch_Actions$grazing_birds_count  <- Action_Bird_Mod_Graze(Patch_Actions$Size) 



Patch_Actions <- Patch_Actions %>% mutate(Bird_Count_w_Actions = TimM_bird_count + burn_birds_count + grazing_birds_count + (V7*Action_Zzz))


### Notes
# Still have "patch" of all cells that weren't assigned an action in the df
# Need to adjust parameters used to generate patches (too big)
# Might want to lower number of birds generated from burning (rn that's dominating the dataset with highest number of birds generated)


## So probably want to impose a constraint that says can only have 1/3 of selected patches come from burns.. 

## okay so how do we do this... 
  # one option - if create new column where each action is unique number then could specifiy what the value of a chromosome should Not exceed
  # ie, if burn = 4 in column and chosing 10 parcels, can't have value greater than ~28 (didn't do math here)?
  # or flip (might be easier) if burn = 1, the chromosome value has to be greater than 20 (also didn't do math..)

Patch_Actions <- Patch_Actions %>% 
  mutate(act_cat = case_when(Action_Zzz == 1 ~ 4, 
                             Action_Graze == 1 ~ 3,
                             Action_timMang == 1 ~ 2,
                             Action_Burn == 1 ~ 1))

## So the columns we want to focus on in GA are.. 
Patch_Actions_Prep <- Patch_Actions %>% dplyr::select(Bird_Count_w_Actions, act_cat)


### ###

action_distr <- 15 
No_patches <- 10 

evalFunc <- function(x) {
  #Want to enforce the diversity of actions 
  df <- Patch_Actions_Prep[x == 1, ]
  total_weight <- sum(df$act_cat)
  total_weight <- if_else(total_weight < action_distr, 0, total_weight)
  return(total_weight)
  # As well as the number of patches selected 
  no_patch_selected <- length(df$act_cat)
  total_patches <- if_else(no_patch_selected > No_patches, 0, total_patches)
  return(total_patches)
  # Maximize number of birds???
}


### trying from a different example... 

Gems <- Patch_Actions %>% dplyr::select(Patch_ID, Bird_Count_w_Actions, act_cat)

### Creating a random chromosome to check function with... 
Gems$ran=sample(8,nrow(Gems),T)
Gems[which(Gems$ran != 1),4] <- 0

fun_tryme <- function(x){
  value = x*Gems[,2]
  total_value_of_chromosome = sum(value)
  
  # Control the number of patches selected 
  No_patches <- 34
  no_patch_selected <- sum(x)
  total_value_of_chromosome <- if_else(no_patch_selected > No_patches, 0, total_value_of_chromosome)
  
  # controls value of actions (don't want all to be the same action )
  action = sum(x*Gems[,3])
  maxcount <- 15
  if(action < maxcount)
    return(0)
  else
    return(total_value_of_chromosome)
}


# fun_tryme(Gems$ran) # testing to make sure function works 


gann <- ga(type = "binary", fitness = fun_tryme, popSize = 200, maxiter = 1000, run = 250, 
           suggestions = Gems$ran, ### providing initial solution so GA has somewhere to start
            nBits = nrow(Gems), seed = 123)
plot(gann)

summary(gann)

df_sol <- Gems[gann@solution[1, ] == 1, ]

ga_best_solution = gann@solution 

gann@fitnessValue



## adding best solution to df so can see which patches & actions are selected... 

Sol_Gems <- Gems
Sol_Gems$solution <- t(ga_best_solution)
## okay so this code works, but it is mostly chosing actions that are 1s or 2s... why's that?? 




### ### Next step will be to have GA randomly assign different actions (see wearehouse problem), 
  # and then apply bird popn functions to pick best combo 



### Future Ref
# https://github.com/kerschke/flacco




################################################################################################################
################################################################################################################
### ### ### ### ### ### SA




### So let's use the same funciton as before but remove the control for the number of patches (SA will minimize..)

fun_tryme <- function(x){
  value = x*Gems[,2]
  total_value_of_chromosome = sum(value)

  # controls value of actions (don't want all to be the same action )
  action = sum(x*Gems[,3])
  maxcount <- 15
  if(action < maxcount)
    return(0)
  else
    return(-1 * total_value_of_chromosome)
}


## How to test different solutions (swap them..?)
swap <- function(v, i , j, k, l){
  aux <- v[i]
  v[i] <- v[j]
  v[i] <- v[k]
  v[k] <- v[l]
  v[l] <- aux
  return(v)
}


Gems$ran=sample(8,nrow(Gems),T)
Gems[which(Gems$ran != 1),4] <- 0



sa_circle <- function(inisol, iter=1000, T = 1e4, alpha = 0.9, p0 = 0.9, eval=TRUE){
  
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
  fit <- fun_tryme(sol) ## run through function to find mass center for that solution
  bestfit <- fit
  
  
  ## the simulated annealing loop
  while(count < iter){
    
    #obtaining the testing solution x'
    move <- sample(1:n, 4)
    testsol <- swap(sol, move[1], move[2], move[3], move[4]) ## need to increase the number of times swaping things
    testfit <- fun_tryme(testsol)
    
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
test <- sa_circle(Gems$ran, 
                  iter = 250, 
                  T = 1e4,
                  alpha = 0.99, 
                  p0 = 0.5, 
                  eval = TRUE)


### Number of patches being selected 
sum(test$sol)


dftest <- test
dftest2 <- within(dftest, rm(sol,fit)) # removing 2 elements of list so can pull the rest of list into df


df <- as.data.frame( t(data.frame(matrix(unlist(dftest2), nrow=length(dftest2), byrow=TRUE))) )
colnames(df) <- c("evalfit",  "evalbest" , "temp"    )
df$step <- seq(1,252)


#create plot with two lines
ggplot(df, aes(x = step)) + 
  geom_line(aes(y = evalfit, color = 'red')) + 
  geom_line(aes(y = evalbest, color = 'blue')) +
  labs(x = 'Step', y = 'Number of Birds')




#######################################################################################


## Trying to get GenSA to work... 


fun_tryme <- function(x){
  value = x*Gems[,2]
  total_value_of_chromosome = sum(value)
  
  # controls value of actions (don't want all to be the same action )
  action = sum(x*Gems[,3])
  maxcount <- 15
  if(action < maxcount)
    return(0)
  else
    return(-1 * total_value_of_chromosome)
}


controllist <- list(max.time=25, nb.stop.improvement = 50)

fml <- GenSA(par = Gems$ran, 
             fn = fun_tryme, 
             control = controllist, 
             lower = rep(0, 101), 
             upper = rep(1, 101) )
# for length - length(Gems$ran)


fmldat <- as.data.frame(fml$trace.mat)
ggplot(fmldat, aes(x = nb.steps)) + 
  geom_line(aes(y = function.value, color = 'red')) + 
  geom_line(aes(y = current.minimum, color = 'blue')) +
  labs(x = 'Step', y = 'Number of Birds')



######################################################################################################


fun_tryme <- function(x){
  value = x*Gems[,2]
  total_value_of_chromosome = sum(value)
  
  # Control the number of patches selected 
  No_patches <- 34
  no_patch_selected <- sum(x)
  total_value_of_chromosome <- if_else(no_patch_selected > No_patches, -1000, total_value_of_chromosome)

  # controls value of actions (don't want all to be the same action )
  action = sum(x*Gems[,3])
  maxcount <- 15
  if(action < maxcount)
    return(0)
  else
    return(-1 * total_value_of_chromosome)
}

fml2 <- GenSA(par = Gems$ran, 
             fn = fun_tryme, 
             control = controllist, 
             lower = rep(0, 101), 
             upper = rep(1, 101) )


fmldat2 <- as.data.frame(fml2$trace.mat)
ggplot(fmldat2, aes(x = nb.steps)) + 
  geom_line(aes(y = function.value, color = 'red')) + 
  geom_line(aes(y = current.minimum, color = 'blue')) +
  labs(x = 'Step', y = 'Number of Birds')


fml2_sol <- fml2$par

holder <- Gems

holder$sol <- fml2_sol
holder <- holder %>% mutate(testme =Bird_Count_w_Actions*sol)
### UGH this doesn't work - it isn't treating the patches like binary options.... 


################################################################################################################
############################ Scrap work 


#3/14/22

## Old GA 
monitor <- function(obj) {
  minEval = min(obj$evaluations);
  plot(obj, type="hist");
}

# Run the model
GAmodel <- rbga.bin(size = 4, ## No. things you want to optimize ie no rows in dataset 
                    popSize = 100,  ## No. chromosomes that will be generated for GA to optimize 
                    iters = 100, ## No. times GA will run on popn
                    evalFunc = fitFunc,  ## Stick fitness function here 
                    monitorFunc=monitor
)

# Index of the best solution
best_solution = GAmodel$population[which.min(GAmodel$evaluations),]
# Data corresponding to best solution
data_best = data[as.logical(best_solution),]








############################
### ### ### ### ### ### Tutorial from landscapeR

## Create an empty landscape
library(raster)
m = matrix(0, 33, 33)
r = raster(m, xmn=0, xmx=10, ymn=0, ymx=10)

## Make patch 
rr = makePatch(r, size=500, rast=TRUE)
plot(rr)

patchSize = 500
newVal = 3
centre = 545
rr = makePatch(r, patchSize, centre, val=newVal, rast=TRUE)
plot(rr)

rr = makePatch(rr, 100, bgr=0, rast=TRUE, val=5)
plot(rr)



## Make Class
num = 5
size = 15
rr = makeClass(r, num, size)
plot(rr)

num = 75
size = 10
rr = makeClass(r, num, size)
plot(rr)

num = 5
size ~ dunif(0,100)
pts = c(1, 33, 1089, 1057, 545)
rr = makeClass(r, num, size, pts)
plot(rr)


## expandClass

rr = expandClass(rr, 1, 250)
plot(rr)




