data(srilanka)

# Using all analyses
## calculate the ages; for a reliable estimation, use 'nloops=1000'
## 'seed' is used to make the results reproducible; you can use any number
calculated.ages <- calculateAges(srilanka, nloops=10, seed=12)
## estimate the number of populations (tests for 1 to 3 populations)
res.tests <- tests(calculated.ages, nbmax=3)
## graphic
plot(res.tests)

plot.new()
# Removing the first 8 analyses (control group)
srilanka2 <- srilanka[-(1:8),]
## calculate the ages; for a reliable estimation, use 'nloops=1000'
calculated.ages <- calculateAges(srilanka2, nloops=10, seed=12)
## estimate the number of populations (tests for 1 to 3 populations)
## 1 population is found
res.tests <- tests(calculated.ages, nbmax=3)
## graphic
plot(res.tests)