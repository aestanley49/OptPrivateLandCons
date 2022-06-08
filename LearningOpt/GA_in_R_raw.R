#### write GA w/o package
# problem xxx in book yyyy


# https://towardsdatascience.com/genetic-algorithm-explained-step-by-step-65358abe2bf



### initial population

intial_popu <- NULL
x <- 1
repeat {
  crm <- runif(2,1,10)
  crm <- as.integer(crm)
  intial_popu <- rbind(intial_popu,crm)
  x = x+1
  if (x == 7){
    break
  }
}
rownames(intial_popu) <- c('Cromosome1','Cromosome2','Cromosome3','Cromosome4','Cromosome5','Cromosome6')
print(intial_popu)
##            [,1] [,2]
## Cromosome1    5    9
## Cromosome2    9    7
## Cromosome3    9    9
## Cromosome4    3    3
## Cromosome5    7    8
## Cromosome6    8    4


### Fitness function

## Function to compute fitness
fit <- function(A){
  a <- A[1]
  b <- A[2]
  return(((2*a^2 + b) - 57))
}
fitness <- apply(intial_popu, 1, FUN = 'fit')
fitting <- 1/(fitness)
probfitting <- fitting/sum(fitting)


### Cross-over

##Function to convert integer to binary
binary <- function(x) {
  i <- 0
  string <- numeric(32)
  while(x > 0) {
    string[32 - i] <- x %% 2
    x <- x %/% 2
    i <- i + 1 
  }
  first <- match(1, string)
  string[first:32] 
}


##create binary matrix of 8 cols and 6 rows
binary_popu <- matrix(NA,nrow = 6,ncol = 8)
for(i in 1:nrow(newpopulation)){
  x <- binary(newpopulation[i,1])
  binary_popu[i,1:length(x)] <- x
  y <- binary(newpopulation[i,2])
  binary_popu[i,5:(4+length(y))] <- y
}
rownames(binary_popu) <- rownames(newpopulation)
cross_paramter <- 0.5
i = 1
crom_cross <- NULL
while(length(crom_cross) < 3){
  cross.crossover.prob <- runif(6,0,1)
  crom_cross <- which(cross.crossover.prob < cross_paramter,arr.ind = T)
  i = i + 1
}
parents <- binary_popu[crom_cross,]
position = 2 ##crossover position
cross_parent <- parents
cross_parent[1,1:position] <- parents[2,1:position]
cross_parent[2,1:position] <- parents[3,1:position]
cross_parent[3,1:position] <- parents[1,1:position]

listofindex <- which(row.names(binary_popu) %in% row.names(cross_parent))
offSpring <- binary_popu
offSpring[listofindex,] <- cross_parent
print(offSpring) ##offspring after crossover
##            [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## Cromosome1    1    0    1   NA    1    0    0    1
## Cromosome2    1    0    1   NA    1    0    0    1
## Cromosome3    1    0    0    1    1    1    1   NA
## Cromosome4    1    0    1   NA    1    0    0    1
## Cromosome5    1    0    1   NA    1    0    0    1
## Cromosome6    1    0    1   NA    1    0    0    1




### Mutation



mutation_paramter <- 0.09
no.of.mutations <- 4  ## Calculated as nrow(offSpring)*ncol(offSpring)*mutation_paramter

randrow <- round(runif(no.of.mutations,1,nrow(offSpring)))
rancol <-  round(runif(no.of.mutations,1,ncol(offSpring)))

## Now get the offsprings by mutating at above position
for(r in 1:length(randrow)){
  if(is.na(offSpring[randrow[r],rancol[r]])){
    offSpring[randrow[r],rancol[r]] <- NA
  }else{
    if(offSpring[randrow[r],rancol[r]] == 0){
      offSpring[randrow[r],rancol[r]] <- 1
    }else{
      offSpring[randrow[r],rancol[r]] <- 0
    }
  }
}



## Now convert binary back to integer
binary_decimal = function(base_number, base = 2) {
  split_base = strsplit(as.character(base_number), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}
offSpring_inter <- matrix(NA,6,2)
for(i in 1:nrow(offSpring)){
  a <- offSpring[i,1:4]
  a <- na.omit(a)
  a <- as.numeric(paste(a, collapse = ""))
  a <- binary_decimal(a)
  b <- offSpring[i,5:8]
  b <- na.omit(b)
  b <- as.numeric(paste(b, collapse = ""))
  b <- binary_decimal(b)
  offSpring_inter[i,1] <- a
  offSpring_inter[i,2] <- b
}
rownames(offSpring_inter) <- rownames(offSpring)
## Chromosomes converted back to integer after end of 1st of generation


### now have offspring 



### need to breed next gen


### put into GA 




##################################################################################################################
##################################################################################################################



##################################################################################################################
##################################################################################################################



# https://medium.com/@mblorstad/genetic-algorithm-from-scratch-using-r-a36dc664c46e



m(list=ls(all=TRUE))

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

#     Color Weight Value
# 1    Red   4.88  9.95
# 2   Blue   1.43  2.73
# 3 Purple   1.52  2.60
# 4 Orange   3.11  0.61
# 5  Green   2.49  0.77
# 6   Pink   3.53  1.99
# 7  White   0.62  9.64
# 8  Black   2.59  1.14
# 9 Yellow   1.77 10.21

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
fitness_function <- function(population, weightRestriction){
  
  score<-0
  for (i in 1:nrow(population)) {
    temp <-apply(Gems[which(population[i,1:nrow(Gems)]==1),2:3],2, sum)
    
    
    score[i]<- ifelse(temp[1]>weightRestriction, 0 , temp[2])
    
  }
  pop<- cbind(population,score)
  pop<-pop[order(score, decreasing = T),]
  return(pop)
  
}


#Selection, Breeding, crossover, mutation

#mutation function
mutate <- function(dna){
  #inverses one random genome in the chromosome
  ind <- sample(1:length(dna),1)
  dna[ind] <- ifelse(dna[ind]==1,0,1)
  return(dna) 
}

#crossover function
crossover <- function(dna1, dna2){
  len <- 1:ceiling(length(dna1)/2)
  offspring <-c(dna1[len],dna2[-len])
  return(offspring)
}


# Making a function to combine genetic information from parents to create offspring
Breed <- function(parent1, parent2, population){
  dna1 <- population[parent1,-ncol(population)]
  dna2 <- population[parent2,-ncol(population)]
  
  H <- 0; W<- 0; S<- 0
  # crossover 95% of the time 
  if(runif(1,0,100)>10) {
    offspring<-crossover(dna1,dna2)
    
  }else{
    # 5% chance for mutation
    offspring <-  mutate(dna1)
  }
  
  return(offspring)
}


# Function to breed the next generation
BreedPopulation <- function(population){
  population<-  fitness_function(population, 10)
  NewPopulation <- list()
  #only top x% of the list allowed the breed.
  len <- floor(nrow(population)/4)
  
  for (i in 1:(len-1)) {
    
    NewPopulation<- list.append(NewPopulation,Breed(i,i+1,population ))
    NewPopulation<- list.append(NewPopulation,Breed(i+1,i,population ))
    NewPopulation<- list.append(NewPopulation,Breed((len+1-i),i,population ))
    NewPopulation<- list.append(NewPopulation,Breed(i,(len+1-i),population ))
    
  }
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  NewPopulation<- list.append(NewPopulation, InitilzePop(1))
  
  pop<-Reduce(rbind, NewPopulation)
  
  return(pop)
}

# Create one function to handle the whole generational process
Genetic_Algorithm <- function(popsize,Gen,BreakCond){
  
  #initilize the population
  population <-InitilzePop(popsize)
  
  #initialize a matrix to store performance data in
  Performance <- matrix(NA, nrow=Gen,ncol=2)
  colnames(Performance) <- c("Average","Best")
  rownames(Performance) <- paste("Generation",seq(nrow(Performance)))
  
  breakCond <- 0
  
  #creating the frame for the plot
  plot(NULL, xlim =c(1,Gen), ylim = c(0,45), ylab = "Value", xlab = "Generation")
  legend("bottomright", c("Best individual", "Average individual"), pch = 20, col = c(1, 2),bty = "n")
  
  #Starting the Generational process
  for (j in 1:Gen) {
    # create the new population
    population <-  BreedPopulation(population)
    
    #In this example, weightRestriction is set to 10 
    score <- fitness_function(population,10)[,nrow(Gems)+1]
    
    #store the performance of the jth generation
    Performance[j,] <- c(mean(score),max(score))
    
    # update plot 
    if(j %in% seq(1,Gen,Gen/20)) {
      Sys.sleep(0.1)
      lines(Performance[,2])
      lines(Performance[,1], col = "red")
    }
    Sys.sleep(0)
    
    
    #Stops the process if the result has not improved the last x generations
    if(Performance[j,2] > max(Performance[1:(j-1),2])){
      #if the performance of this generation is better than the previous generations, the breakCond reset to zero
      breakCond <- 0
    }else
    {
      #if the performace of this generation is not better. Add 1 to the breakCond. 
      breakCond <- breakCond+1
    }
    # if breakCond reaches the Break condition, the process stops.
    if(breakCond >= BreakCond){ break}
    
  }
  
  #Organizing all data into a list
  result <- list(GenerationPerformance = Performance, Solution = list(Genes=population[1,],Result=apply(Gems[which(population[1,] ==1),2:3],2,sum)))
  
  return(result)
  
}


GA_Result<-Genetic_Algorithm(popsize = 20, Gen = 200,BreakCond = 150)


GA_Result$Solution

