beads <- rep(c("red", "blue"), times = c(2,3))
beads
sample(beads,1)
B <- 10000
events <- replicate(B, sample(beads, 1))
tab <- table(events)
tab
prop.table(tab)
sample(beads, 5)
sample(beads, 5)
sample(beads, 5) 3/

events <- sample(beads, B, replace=TRUE)
prop.table(table(events))

set.seed(1986)

set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

beads <- rep(c("red", "blue"), times = c(2,3))
beads
mean(beads =="blue")