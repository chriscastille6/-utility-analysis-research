#### STURMAN 2003 ####

#### Step 1. Define the pay per strategy ####

## define the performance rating levels
perf.rating <- seq(1, 5, by = 0.5) # define the performance appraisal distribution

## define the three strategies
strat1 <- rep(0.04, 9)             # 4% across the board increase (8 times, once per rating category)
strat2 <- rep(0.04, length(perf.rating)) 
strat2[perf.rating >= 3] <- 0.04 + 0.01 * (perf.rating[perf.rating >= 3] - 3) * 2 # Perf ratings > 3 get a pay bump
strat3 <- seq(0,.08, by = .01)     # define the increments of strategy 3 (0-8% pay bump)

# define average pay
paylevel.2003 <- 47983             # define the pay level, in this case ~$48K

# Strategy 1
paylevel.2007.s1 <- paylevel.2003 * (1 + strat1)^4

# Stragegy 2
paylevel.2007.s2 <- paylevel.2003 * (1 + strat2)^4

# Strategy 3
paylevel.2007.s3 <- paylevel.2003 * (1 + strat3)^4

# build dataframe for step 1
df1 <- rbind(perf.rating,strat1,strat2,strat3,paylevel.2007.s1,paylevel.2007.s2,paylevel.2007.s3)

#### Step 2. Turnover Probabilities ####

## get ns by performance rating
n.per.perf.rating <- c(60, 97, 1171, 1090, 1667, 672, 317, 46, 23) # list of n by perf.rating
perf_rating_data <- data.frame(
  rating = perf.rating,
  occurrences = n.per.perf.rating
)
# plot it in base R
barplot(n.per.perf.rating, names.arg = perf.rating, xlab = "Performance Rating", ylab = "Number of Occurrences", main = "Distribution of Performance Ratings")
# plot it in ggplot
library(ggplot2)

ggplot(perf_rating_data, aes(x = rating, y = occurrences)) +
  geom_bar(stat = "identity") +
  xlab("Performance Rating") +
  ylab("Number of Occurrences") +
  ggtitle("Distribution of Performance Ratings")

## define annual turnover probabilities for each strategy
strat1.turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.22, 0.27, 0.41, 0.66)
strat2.turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.14, 0.11, 0.11, 0.14)
strat3.turnover <- c(0.99, 0.88, 0.6, 0.35, 0.21, 0.14, 0.11, 0.11, 0.14)

# get number of retained employees for each strategy
strat1.proj.rt <- round((1-strat1.turnover)*n.per.perf.rating, digits=0)
strat2.proj.rt <- round((1-strat2.turnover)*n.per.perf.rating, digits=0)
strat3.proj.rt <- round((1-strat3.turnover)*n.per.perf.rating, digits=0)

proj.rt.by.strat <- c(strat1.proj.rt,strat2.proj.rt,strat3.proj.rt)

# get number of replaced employees for each strategy
strat1.proj.rep <- round((strat1.turnover)*n.per.perf.rating, digits=0)
strat2.proj.rep <- round((strat2.turnover)*n.per.perf.rating, digits=0)
strat3.proj.rep <- round((strat3.turnover)*n.per.perf.rating, digits=0)

n.rep.by.strat <- c(sum(strat1.proj.rep),sum(strat2.proj.rep),sum(strat3.proj.rep))

# build dataframe for step 2
df2 <- rbind(perf.rating,n.per.perf.rating,strat1.turnover,strat2.turnover,strat3.turnover,strat1.proj.rt,strat2.proj.rt,strat3.proj.rt,strat1.proj.rep,strat2.proj.rep,strat3.proj.rep)

#### Step 3. Estimate the 4-year movement costs under different pay strategies

# Express average pay levels across perf's per strategy

paylevel.2003 <- 47983

avg.paylevel.2007.s1 <- weighted.mean(paylevel.2007.s1,strat1.proj.rt)
avg.paylevel.2007.s2 <- weighted.mean(paylevel.2007.s2,strat2.proj.rt)
avg.paylevel.2007.s3 <- weighted.mean(paylevel.2007.s3,strat3.proj.rt)

avg.paylevel.2007 <- c(avg.paylevel.2007.s1,avg.paylevel.2007.s2,avg.paylevel.2007.s3)

# Define movement Cost Multiplier - given
move.cost.mult <- 2

# Calculate the average move cost for each strategy for '03 and '07
avg.move.cost.2003 <- paylevel.2003*move.cost.mult
avg.move.cost.2007 <- avg.paylevel.2007*move.cost.mult

# Calculate yearly average increase for each strategy
avg.yearly.move.cost.increase <- (avg.move.cost.2007-avg.move.cost.2003)/4

# Calculate the average 2004 move cost for each strategy
avg.move.cost.2004 <- avg.move.cost.2003+avg.yearly.move.cost.increase

#Calculate average move cost b/w '04 and '07
avg.move.cost.0407 <- (avg.move.cost.2004+avg.move.cost.2007)/2

#Calculate # of separations for each strategy
n.sep.per.s1 <- sum(strat1.proj.rep)
n.sep.per.s2 <- sum(strat2.proj.rep)
n.sep.per.s3 <- sum(strat3.proj.rep)

n.sep <- c(sum(strat1.proj.rep),sum(strat2.proj.rep),sum(strat3.proj.rep))

#Calculate total move cost b/w '04 and '07
total.move.cost <- avg.move.cost.0407*n.sep

#### Step 4. Estimate the 4-year service costs under different pay strategies

# Define the average service cost multiplier per employee

avg.serv.cost.per.n.mult <- 1.37

# Calculate average service cost for each strategy for '03 and '07
avg.serv.cost.2003 <- paylevel.2003*avg.serv.cost.per.n.mult
avg.serv.cost.2007 <- avg.paylevel.2007*avg.serv.cost.per.n.mult

# Calculate yearly increase in service cost for each strategy
avg.yearly.serv.cost.increase <- (avg.serv.cost.2007-avg.serv.cost.2003)/4

# Calculate the average 2004 service cost for each strategy
avg.serv.cost.2004 <- avg.serv.cost.2003+avg.yearly.serv.cost.increase

# Calculate average service cost b/w '04 and '07
avg.serv.cost.0407 <- (avg.serv.cost.2007+avg.serv.cost.2004)/2

# Calculate the total service cost for each strategy
total.serv.cost <- 4*sum(n.per.perf.rating)*avg.serv.cost.0407

#### Step 5. Computations for estimating individual service value at each performance level

# Calculate weighted mean performance for all employees
avg.perf <- weighted.mean(perf.rating,n.per.perf.rating)

# Calculate # of employees * respective performance level
n.perf.product <- perf.rating*n.per.perf.rating

# Calculate standard deviation of performance
n.times.delta.r.sqared <- n.per.perf.rating*(perf.rating-avg.perf)^2
std.dev.perf <- (sum(n.times.delta.r.sqared)/sum(n.per.perf.rating))^0.5

# Calculate the z-score of each performance rating
z.per.perf <- (perf.rating-avg.perf)/std.dev.perf

#Calculate average service value for each performance level
avg.serv.value.mult <- 1.754
avg.serv.value.per.perf.2003 <- avg.serv.value.mult*paylevel.2003
avg.serv.value.per.perf.2007 <- avg.serv.value.mult*avg.paylevel.2007.s1

# Define SDy levels
sdy.low <- 0.3
sdy.med <- 0.6
sdy.high <- 0.9

# Calculate incremental service value for SDy=0.3
incr.serv.value.2003.sdy.low <- sdy.low*z.per.perf*paylevel.2003
incr.serv.value.2007.sdy.low <- sdy.low*z.per.perf*paylevel.2007.s1

# Calculate incremental service value for SDy=0.6
incr.serv.value.2003.sdy.med <- sdy.med*z.per.perf*paylevel.2003
incr.serv.value.2007.sdy.med <- sdy.med*z.per.perf*paylevel.2007.s1

# Calculate incremental service value for SDy=0.9
incr.serv.value.2003.sdy.high <- sdy.high*z.per.perf*paylevel.2003
incr.serv.value.2007.sdy.high <- sdy.high*z.per.perf*paylevel.2007.s1

# Calculate total individual service value for SDy=0.3
tot.serv.value.per.n.2003.sdy.low <- avg.serv.value.per.perf.2003+incr.serv.value.2003.sdy.low
tot.serv.value.per.n.2007.sdy.low <- avg.serv.value.per.perf.2007+incr.serv.value.2007.sdy.low

# Calculate total individual service value for SDy=0.6
tot.serv.value.per.n.2003.sdy.med <- avg.serv.value.per.perf.2003+incr.serv.value.2003.sdy.med
tot.serv.value.per.n.2007.sdy.med <- avg.serv.value.per.perf.2007+incr.serv.value.2007.sdy.med

# Calculate total individual service value for SDy=0.9
tot.serv.value.per.n.2003.sdy.high <- avg.serv.value.per.perf.2003+incr.serv.value.2003.sdy.high
tot.serv.value.per.n.2007.sdy.high <- avg.serv.value.per.perf.2007+incr.serv.value.2007.sdy.high

#### Step 6. Compute total service value for 2003
tot.serv.value.2003.sdy.low <- n.per.perf.rating*tot.serv.value.per.n.2003.sdy.low
tot.serv.value.2003.sdy.med <- n.per.perf.rating*tot.serv.value.per.n.2003.sdy.med
tot.serv.value.2003.sdy.high <- n.per.perf.rating*tot.serv.value.per.n.2003.sdy.high

#### Step 7. Compute total service value of retained employees for 2007

# Calculate total service value for SDy=0.3
tot.serv.value.2007.sdy.low.s1 <- strat1.proj.rt*tot.serv.value.per.n.2007.sdy.low
tot.serv.value.2007.sdy.low.s2 <- strat2.proj.rt*tot.serv.value.per.n.2007.sdy.low
tot.serv.value.2007.sdy.low.s3 <- strat3.proj.rt*tot.serv.value.per.n.2007.sdy.low

sum.tot.serv.value.2007.sdy.low <- c(sum(tot.serv.value.2007.sdy.low.s1),sum(tot.serv.value.2007.sdy.low.s2),sum(tot.serv.value.2007.sdy.low.s3))

# Calculate total service value for SDy=0.6
tot.serv.value.2007.sdy.med.s1 <- strat1.proj.rt*tot.serv.value.per.n.2007.sdy.med
tot.serv.value.2007.sdy.med.s2 <- strat2.proj.rt*tot.serv.value.per.n.2007.sdy.med
tot.serv.value.2007.sdy.med.s3 <- strat3.proj.rt*tot.serv.value.per.n.2007.sdy.med

sum.tot.serv.value.2007.sdy.med <- c(sum(tot.serv.value.2007.sdy.med.s1),sum(tot.serv.value.2007.sdy.med.s2),sum(tot.serv.value.2007.sdy.med.s3))

# Calculate total service value for SDy=0.9
tot.serv.value.2007.sdy.high.s1 <- strat1.proj.rt*tot.serv.value.per.n.2007.sdy.high
tot.serv.value.2007.sdy.high.s2 <- strat2.proj.rt*tot.serv.value.per.n.2007.sdy.high
tot.serv.value.2007.sdy.high.s3 <- strat3.proj.rt*tot.serv.value.per.n.2007.sdy.high

sum.tot.serv.value.2007.sdy.high <- c(sum(tot.serv.value.2007.sdy.high.s1),sum(tot.serv.value.2007.sdy.high.s2),sum(tot.serv.value.2007.sdy.high.s3))

#### Step 8. Calculate service value of replacement employees for 2007

# Calculate average service value per sdy and pay strategy
avg.serv.value.sdy.low <- rep(sum(tot.serv.value.2007.sdy.low.s1/sum(strat1.proj.rt)),times = 3)
avg.serv.value.sdy.med <- sum(tot.serv.value.2007.sdy.med.s1/sum(strat1.proj.rt))
avg.serv.value.sdy.high <- sum(tot.serv.value.2007.sdy.high.s1/sum(strat1.proj.rt))

# Calculate total service value of replacements by strategy (2007)
n.ret <- c(sum(strat1.proj.rt),sum(strat2.proj.rt),sum(strat3.proj.rt))
tot.serv.value.rep.sdy.low <- avg.serv.value.sdy.low*n.sep
tot.serv.value.rep.sdy.med <- avg.serv.value.sdy.med*n.sep
tot.serv.value.rep.sdy.high <- avg.serv.value.sdy.high*n.sep

#### Step 9. Calculate the total service value of the 2007 workforce

tot.serv.value.2007.sdy.low <- sum.tot.serv.value.2007.sdy.low + tot.serv.value.rep.sdy.low
tot.serv.value.2007.sdy.med <- sum.tot.serv.value.2007.sdy.med + tot.serv.value.rep.sdy.med
tot.serv.value.2007.sdy.high <- sum.tot.serv.value.2007.sdy.high + tot.serv.value.rep.sdy.high

#### Step 10. Compute 4-year total service value

# Compute 2003 service value
serv.value.2003 <- c(sum(tot.serv.value.2003.sdy.low),sum(tot.serv.value.2003.sdy.med),sum(tot.serv.value.2003.sdy.high))

# Compute the average service value increase
avg.serv.value.inc.sdy.low <- (tot.serv.value.2007.sdy.low - serv.value.2003)/4
avg.serv.value.inc.sdy.med <- (tot.serv.value.2007.sdy.med - serv.value.2003)/4
avg.serv.value.inc.sdy.high <- (tot.serv.value.2007.sdy.high - serv.value.2003)/4

# Compute 2004 service value
serv.value.2004.sdy.low <- serv.value.2003 + avg.serv.value.inc.sdy.low
serv.value.2004.sdy.med <- serv.value.2003 + avg.serv.value.inc.sdy.med
serv.value.2004.sdy.high <- serv.value.2003 + avg.serv.value.inc.sdy.high

#Compute the average service value b/w '04 and '07
avg.serv.value.0407.sdy.low <- (serv.value.2004.sdy.low + tot.serv.value.2007.sdy.low)/2
avg.serv.value.0407.sdy.med <- (serv.value.2004.sdy.med + tot.serv.value.2007.sdy.med)/2
avg.serv.value.0407.sdy.high <- (serv.value.2004.sdy.high + tot.serv.value.2007.sdy.high)/2

#Compute the total service value b/w '04 and '07
tot.serv.value.0407.sdy.low <- avg.serv.value.0407.sdy.low*4
tot.serv.value.0407.sdy.med <- avg.serv.value.0407.sdy.med*4
tot.serv.value.0407.sdy.high <- avg.serv.value.0407.sdy.high*4

#### Step 11. Compute the 4-year investment value of different pay strategies
four.yr.invest.value.sdy.low <- tot.serv.value.0407.sdy.low - total.serv.cost - total.move.cost
four.yr.invest.value.sdy.med <- tot.serv.value.0407.sdy.med - total.serv.cost - total.move.cost
four.yr.invest.value.sdy.high <- tot.serv.value.0407.sdy.high - total.serv.cost - total.move.cost
