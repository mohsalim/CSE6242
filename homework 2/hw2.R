# Homework 2
# Mohammad Hassan Salim
# msalim7

# Run:
# > source('C:/Users/Mohammad/Documents/GaTech OMSCS/CSE6242/Homework 2/hw2.R')

############################################
# Part 1: Professional Employment by State #
############################################
# Get Ggplot2 and midwest data set.
#install.packages('ggplot2')
library(ggplot2)
data(midwest)

# Get each unique state in the data set.
states = unique(midwest$state)

# Creat a repeated list of N number of zeroes as default values.
nz = rep(0, length(states))

# Create empty result data set.
profEmployment = data.frame(state = states, totalPopulation = nz, totalAdultPopulation = nz,
                            profAdultPopulation = nz, profAdultPercent = nz,
                            collegePopulation = nz, collegePercent = nz,
                            highSchoolPopulation = nz, highSchoolPercent = nz)

# Fill in result data set per state.
for(currentState in states) {
  # Data index (d.i.)
  di = midwest$state == currentState
  
  # Result index (r.i.)
  ri = profEmployment$state == currentState
  
  # Get total adult population for this state. Needd for problem 1.
  profEmployment[ri,]$totalAdultPopulation = sum(midwest[di,]$popadults)
  
  # Calculate professional population. Needed for problem 1.
  profEmployment[ri,]$profAdultPopulation = sum(midwest[di,]$popadults*midwest[di,]$percprof)
  profEmployment[ri,]$profAdultPercent = profEmployment[ri,]$profAdultPopulation/profEmployment[ri,]$totalAdultPopulation
  
  # Get total population for this state. Needed for problem 2.
  profEmployment[ri,]$totalPopulation = sum(midwest[di,]$poptotal)
  
  # Calculate college population. Needed for problem 2.
  profEmployment[ri,]$collegePopulation = sum(midwest[di,]$poptotal*midwest[di,]$percollege)
  profEmployment[ri,]$collegePercent = profEmployment[ri,]$collegePopulation/profEmployment[ri,]$totalPopulation
  
  # Calculate high school diploma population. Needed for problem 2.
  profEmployment[ri,]$highSchoolPopulation = sum(midwest[di,]$poptotal*midwest[di,]$perchsd)
  profEmployment[ri,]$highSchoolPercent = profEmployment[ri,]$highSchoolPopulation/profEmployment[ri,]$totalPopulation
}
print(profEmployment)

# Plot bar graph with labels. X-axis is states. Y-axis is professional population percentage.
g = ggplot(data = profEmployment, aes(x = profEmployment$state, y = profAdultPercent)) +
  geom_bar(stat = "identity") +
  labs(title = "Professional Employment by State", x = "State", y = "Professional Adult Population Percentage")
print(g)

# Plot box plot to show relationship between professional population per county and state.
g = ggplot(data = midwest, aes(x = midwest$state, y = percprof)) +
  geom_boxplot() +
  labs(title = "Relationship Between Professional Employment Per County & State", x = "State", y = "Professional Population Per County (Percentage)")
print(g)

#################################################
# Part 2: School and College Education by State #
#################################################
# Get library for GGPairs function.
#install.packages('GGally')
library(GGally)

# TODO This doesn't tell me much. Hard to read/understand graph. Maybe consider making individual graphs.

# Create 3x3 graph across state, total high school percentage per state, and total college percentage per state.
l = c("High School Percent", "College Percent", "State")
g = ggpairs(profEmployment[,c("highSchoolPercent", "collegePercent", "state")], title = "School & College Education by State 1")
print(g)

g = ggplot(data = profEmployment, aes(x = profEmployment$state, y = highSchoolPercent)) +
  geom_bar(stat = "identity") +
  labs(title = "High School Diploma by State", x = "State", y = "High School Population Percentage")
print(g)

# Create 3x3 graph across state, high school percentages, and college percentages
g = ggpairs(midwest[,c("perchsd", "percollege", "state")], title = "School & College Education by State 2")
print(g)

g = ggplot(data = profEmployment, aes(x = profEmployment$state, y = collegePercent)) +
  geom_bar(stat = "identity") +
  labs(title = "College Degree by State", x = "State", y = "College Population Percentage")
print(g)

##################################################
# Part 3: Comparison of Visualization Techniques #
##################################################
# Track clock time of 3 types of graphs.
nList = list()
boxPlotTimes = list()
histogramTimes = list()
qqPlotTimes = list()

# Keep track if we should print graphs or not for this problem.
# We need to do this to get good clock time stats. Else wise it's hard to compare algorithms.
problem3Print = FALSE

# Try different graphs with NxN data.
for(n in seq(1, 5000, 100)) {
  # Keep track of what N we're trying.
  nList = c(unlist(nList), n)
  
  # Randomly create an 2D data frame.
  rx = runif(n, min=0, max=10000)
  ry = runif(n, min=0, max=10000)
  rdf = data.frame(randomX = rx, randomY = ry)
  
  # Box plot clock time.
  st = proc.time()
  g = ggplot(rdf, aes(randomX, randomY)) +
    geom_boxplot()
  if(problem3Print) {
    print(g)
  }
  bpct = (proc.time() - st)[3]
  boxPlotTimes = c(unlist(boxPlotTimes), bpct)
  
  # Histogram clock time.
  st = proc.time()
  g = ggplot(rdf, aes(randomX, randomY)) +
    geom_histogram(stat="identity")
  if(problem3Print) {
    print(g)
  }
  hct = (proc.time() - st)[3]
  histogramTimes = c(unlist(histogramTimes), hct)
  
  
  # QQ plot clock time.
  st = proc.time()
  g = ggplot(rdf) +
    stat_qq(aes(sample = randomX, color = "red")) +
    stat_qq(aes(sample = randomY, color = "blue"))
  if(problem3Print) {
    print(g)
  }
  qqpct = (proc.time() - st)[3]
  qqPlotTimes = c(unlist(qqPlotTimes), qqpct)
}

# Create data frame with all clock times.
clockTimes = data.frame(nIterations = nList, boxPlotTime = boxPlotTimes, histogramTime = histogramTimes, qqPlotTime = qqPlotTimes)
if(problem3Print) {
  print(clockTimes)
}

# Line plot with X as N and 3 lines for clock times. Each own color.
g = ggplot(clockTimes, aes(x = nIterations)) +
  geom_line(aes(y = boxPlotTime, colour="boxPlotTime")) +
  geom_line(aes(y = histogramTime, colour="histogramTime")) +
  geom_line(aes(y = qqPlotTime, colour="qqPlotTime")) +
  xlab("N Data Points (NxN)") +
  ylab("Elapsed Clock Time (Seconds)") +
  scale_colour_manual(breaks = c("boxPlotTime", "histogramTime", "qqPlotTime"), values=c("blue","red","green"))
if(problem3Print) {
  print(g)
}

###############################
# Part 4: Random Scatterplots #
###############################
# Track memory size of 4 types of files.
nList = list()
psSizes = list()
pdfSizes = list()
jpegSizes = list()
pngSizes = list()

# Base path to save file in. NOTE: Currentlty not in use.
basePath = "C:/Users/Mohammad/Documents/GaTech OMSCS/CSE6242/Homework 2/"

# Try different graphs with NxN data.
for(n in seq(1, 10000, 100)) {
  # Keep track of what N we're trying.
  nList = c(unlist(nList), n)
  
  # Randomly create an 2D data frame.
  rx = runif(n, min=0, max=10000)
  ry = runif(n, min=0, max=10000)
  rdf = data.frame(randomX = rx, randomY = ry)
  
  # Generate random scatter plot.
  g = ggplot(rdf, aes(x = randomX, y = randomY)) +
    geom_point() 
  
  # Save the graph in all formats.
  ggsave(filename = "./problem4_sample.ps", plot = g)
  ggsave(filename = "./problem4_sample.pdf", plot = g)
  ggsave(filename = "./problem4_sample.jpg", plot = g)
  ggsave(filename = "./problem4_sample.png", plot = g)
  
  # Get image's disk size in bytes.
  psSizes = c(unlist(psSizes), file.size('./problem4_sample.ps'))
  pdfSizes = c(unlist(pdfSizes), file.size('./problem4_sample.pdf'))
  jpegSizes = c(unlist(jpegSizes), file.size('./problem4_sample.jpg'))
  pngSizes = c(unlist(pngSizes), file.size('./problem4_sample.png'))
}

# Create data frame with all file sizes.
fileSizes = data.frame(nIterations = nList, psSize = psSizes, pdfSize = pdfSizes, jpegSize = jpegSizes, pngSize = pngSizes)

# Line plot with X as N and 4 lines for file sizes Each own color.
g = ggplot(fileSizes, aes(x = nIterations)) +
  geom_line(aes(y = psSize, colour="psSize")) +
  geom_line(aes(y = pdfSize, colour="pdfSize")) +
  geom_line(aes(y = jpegSize, colour="jpegSize")) +
  geom_line(aes(y = pngSize, colour="pngSize")) +
  xlab("N Data Points (NxN)") +
  ylab("File Size (Bytes)") +
  scale_colour_manual(breaks = c("psSize", "pdfSize", "jpegSize", "pngSize"), values = c("blue", "red", "green", "purple"))
print(g)

####################
# Part 5: Diamonds #
####################
# Load up diamonds data set.
data(diamonds)

# Plot histogram for color.
g = ggplot(diamonds, aes(color)) +
  geom_histogram(stat="count")
print(g)

# Plot histogram for carat.
g = ggplot(diamonds, aes(carat)) +
  geom_histogram(stat="count")
print(g)

# Plot histogram for price.
g = ggplot(diamonds, aes(price)) +
  geom_histogram(stat="count")
print(g)

# Create 3x3 graph across coor, carat, and price.
g = ggpairs(diamonds[,c("color", "carat", "price")], title = "Diamonds")
print(g)
