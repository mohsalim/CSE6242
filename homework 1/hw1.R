# Homework 1
# Mohammad Hassan Salim
# msalim7

# Run:
# > source('C:/Users/Mohammad/Documents/GaTech OMSCS/CSE6242/Homework 1/hw1.R')


# Part 2: Log Gamma (Loop)
log_gammma_loop = function(n) {
  total = 0
  i = n - 1
  while(i > 0) {
    total = total + log(i)
    i = i - 1
  }
  return(total)
}

print(log_gammma_loop(5))

# Part 3: Log Gamma (Recursive)
log_gammma_recursive = function(n) {
  if(n <= 1) {
    return(0)
  }
  return(log(n-1) + log_gammma_recursive(n-1))
}

print(log_gammma_recursive(5))

# Part 4: Sum of Log Gamma
sum_log_gamma_loop = function(n) {
  sum = 0
  for(i in 1:n) {
    sum = sum + log_gammma_loop(i)
  }
  return(sum)
}

sum_log_gamma_recursive = function(n) {
  sum = 0
  for(i in 1:n) {
    sum = sum + log_gammma_recursive(i)
  }
  return(sum)
}

print(sum_log_gamma_loop(5))
print(sum_log_gamma_recursive(5))

# Part 5: Compare Results to Built-In R Function
options(expressions = 500000)

sum_lgamma = function(n) {
  sum = 0
  for(i in 1:n) {
    sum = sum + lgamma(i)
  }
  return(sum)
}

print(sum_lgamma(5))

keep_recursive = TRUE
recursive_error = function(e) {
  print("Recursive nested too deeply.")
  keep_recursive = FALSE
}

# TODO vectorize sum functions. it worked for lgamma but not other 2 sum functions. why?
loop = c(); recursive = c(); l_gamma = c()
for(i in seq(1, 5000, by=100)) {
  print(i)
  loop[i] = system.time(sum_log_gamma_loop(i))[1]
  if (keep_recursive) {
    tryCatch({
        recursive[i] = system.time(sum_log_gamma_recursive(i))[1]
      },
      error=recursive_error)
  }
  l_gamma[i] = system.time(sum_lgamma(i))[1]
}

plot(loop, col="red", xlab="N", ylab="Seconds", lwd=2.5,  type="o")
lines(recursive, col="blue", lwd=2.5,  type="o")
lines(l_gamma, col="green", lwd=2.5,  type="o")
title(main="Sum of L Gamma", font.main=4)
legend(x=10, 
       legend=c("Loop","Recursive", "lgamma"), 
       col=c("red", "blue", "green"), 
       lwd=c(2.5, 2.5, 2.5))

print(summary(loop))
print(summary(recursive))
print(summary(l_gamma))
