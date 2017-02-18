# Run:
# > source('C:/Users/Mohammad/Documents/GaTech OMSCS/CSE6242/Quizzes/vector-quiz-1.R')

# y = vector(mode = "logical", length = 4)
# print(y)
# 
# z = vector(mode = "numeric", length = 3)
# print(z)
# 
# x = c(1, 2, 3)
# print(x)
# 
# q = rep(3.2, times = 10)
# print(q)
# 
# w = seq(0, 1, by = 0.1)
# print(w)

w = seq(0, 1, length.out = 11)
# print(w)
# print(w[w < .5])
# 
# print(w <= 0.5)
# print(any(w <= 0.5))
# print(all(w <= 0.5))
# print(which(w <= 0.5))

# 
# print(w[w <= .5])
# print(subset(w, w <= .5))
# w[w <= .5] = 0
# print(w)

z = seq(1, 20, length.out = 20)
x = array(data = z, dim = c(4,5))
# print(x[2,3])
# print(x[2,])
# print(x[-1,])
y = x[c(1,2), c(1,2)]
# print(y)
# print(2*y + 1)
# print(y%*%y)
# 
# print(x[1,] %*% x[1,])
# print(t(x))
# print(outer(x[,1], x[,1]))
# print(rbind(x[1,], x[1,]))
# print(cbind(x[1,], x[1,]))

L = list(name='John', age=55, no.children=2, children.ages=c(15,18))
# print(names(L))
# print(L['name'])
# print(L[[2]])
# print(L$children.ages[2])
# print(L$name)
# print(L[[4]][2])

vecn = c("John Smith", "Jane Doe")
veca = c(42, 45)
vecs = c(50000, 55000)
R = data.frame(name = vecn, age = veca, salary = vecs)
# print(R)
names(R) = c("NAME", "AGE", "SALARY")
# print(R)

data("iris")
print(names(iris))
print(iris[1:4,])
print(iris[1,])
print(iris$Sepal.Length[1:10])

attach(iris, warn.conflicts = FALSE)
# print(Sepal.Length)

# print(iris)
# print(mean(Sepal.Length))
# print(colMeans(iris[,1:4]))
# print(subset(iris, Sepal.Length < 5 & Species == "setosa"))
# print(dim(subset(iris, Species == "setosa"))[1])
# print(summary(iris))

a=10; b=5; c=1
if(a < b) {
  d=1
} else if (a == b) {
  d = 2
} else {
  d = 3
}
#print(d)

#The format for a 'for' loop in R is:
#for(i in 1:100){
#    print("Hello World")
#    print(i*i)
#}

#Everything in the curly braces is executed 100 times. 

#Write a 'for' loop that adds the numbers (num) 1 to 100 and 
#stores it in a variable called 'sum'.

total = function(n){
  sum = 0
  
  ###Put the code for the 'for' loop here.
  for(i in 1:100) {
    sum = sum + i
  }
  
  #print(sum)
  return(sum)
}

total(100)

#A repeat loop must use a break statement to exit the loop.

#Using a repeat loop, write an 'R' program that 
#subtracts the numbers (num) 100 to 1 from a variable called sum.
#If the sum becomes '0' or less, exit the repeat loop. 
#Use a variable called 'num' for the numbers and 'sum' for the sum.

total = function(n){
  sum = 5050
  num = n
  
  repeat { 
    sum = sum - num
    num = num -1
    if(num == 0) {
      break
    }
  }
  
  ###Put the code for the 'repeat' loop here.
  #print(sum)
  return(sum)
}

total(100)

#Given two variables (a,b) and a sum=0, write a while loop to
#perform the following task:

#While b>a, increment the variables sum and 'a' and decrement the variable 'b'.

#a = 1, b = 10

total = function(){
  sum = 0
  a = 1
  b = 10
  
  #Put the while loop here
  while(b > a) {
    sum = sum + 1
    a = a + 1
    b = b - 1
  }
  
  #print(sum)
  return (sum)
}

total()



