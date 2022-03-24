####################################################################
## Script to select best variables for a classification mode using genetic algorithms. 
## Source: https://github.com/jrogol/GenAlg/blob/master/GenAlg-jr2ef.R
## 
####################################################################



# Load the GenAlg library
require(genalg)

# The genetic algorithm will tackle a problem facing elementary school kids for 
# ages - what is the fewest number of coins required to produce change, when the
# cost is rounded to the nearest dollar?

# To do this, each gene on a chromosome will represent a type of coin, with the 
# value being the number of coins used. A vector of weights holds the
# denominations of each coin:

weights <- c(0.01,0.05,0.10,0.25,0.50,1.00)

# The following evaluation function attempts to minimize the number of coins
e <- function(obj){
  # Round the floats, so as to return integers (The Brits *did* kill off the
  # halfpenny in the 60's, after all)
  rounded <- round(obj,digits = 0)
  # Round the cost up to the dollar, and subtract the cost to find the
  # amount of change required.
  change <- ceiling(target) - target
  # Find the total value by taking the dot product of the number of coins and their weights
  totValue <- rounded%*%weights
  # The difference between the required change and solutions in the population 
  # needs to be minimized. Taking the absolute value ensures that 0 will be the 
  # lowest number returned.
  difference <- abs(change-totValue)
  
  # find the cumulative sum of all coin types, from lowest to highest denominations
  values <- cumsum(rounded*weights)
  
  # The number of coins also needs to be minimized. If the cumulative sum of the
  # smaller denominations is greater than a larger denomination, the solution is
  # not optimal. Thus, we penalize the difference by summing the number of
  # suboptimal valuations.
  penalty <- (values[1] > weights[2]) +
    (values[2] > weights[3]) +
    (values[3] > weights[4]) + 
    (values[4] > weights[5]) +
    (values[5] > weights[6])
  
  # Return the difference plus the penalty term
  retVal <- difference + penalty
  retVal 
  
}


# Create a monitor function which outputs a bar plot of the number of coins for
# the fittest solution in each iteration.
monitor <- function(obj) {
  # Find the optimal solution for the iteration
  output <- obj$population[which.min(obj$evaluation),]
  # Round the solution to the nearest integer
  rounded <- round(output)
  # Plot the number of coins in each denomination in a bar plot. Colors obtained
  # via colorbrewer2.org.
  barplot(rounded, 
          names.arg = c("Pennies","Nickels","Dimes","Quarters","Half Dollars", "Dollars"),
          ylab = "Count",
          xlab = "Coin Type",
          las = 2,
          col = rev(c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4')))
}


# Set the target value (cost of an item)
target <- 4.22

# Set the population size and number of iterations
pop <- 200
iters <- 100
# Instead of giving the length of each chromosome, provide a minimum and maximum
# value for each gene in the chromosome. Here, we limit the total number of
# coins in each denomination to 10.
results = rbga(stringMin=rep(0,6), stringMax=rep(10,6), popSize = pop, monitorFunc=monitor, 
               evalFunc=e, verbose=TRUE, mutationChance=0.01, iters = iters)

# Extract the best results, H/T to stack overflow, since the genalg
# documentation is...rubbish. 
# http://stackoverflow.com/questions/20676835/how-to-get-the-best-solution-as-a-column-from-genetic-algorithm-genalg
best <- results$population[which.min(results$evaluations),]
# Sanity check, for the correct amount of change
round(best)%*%weights

# Plot the optimal solution
barplot(round(best), 
        names.arg = c("Pennies","Nickels","Dimes","Quarters","Half Dollars", "Dollars"),
        ylab = "Count",
        xlab = "Coin Type",
        las = 2,
        col = rev(c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4')))
# The minimum number of coins required to make the change.
sum(round(best))