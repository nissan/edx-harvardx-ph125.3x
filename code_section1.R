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

number <- "Three"
suit <- "Hearts"
paste(number,suit)

letters[1:5]
as.character(1:5)
paste(letters[1:5], as.character(1:5))

expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "placid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck

kings <- paste("King", suits)
kings
deck %in% kings
mean(deck %in% kings)

install.packages("gtools")
library(gtools)
permutations(5,2)

all_phone_numbers <- permutations(10, 7, v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

hands <- permutations(52,2, v=deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# this is the same as #
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

#compare
permutations(3,2) # order matters (3,1) different from (1,3)
combinations(3,2) # order does not matter (3,1) === (1,3)

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52,2, v=deck)

mean(hands[,1] %in% aces & hands[,2] %in% facecard)
## OR ##
mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
  (hands[,2] %in% aces & hands[,1] %in% facecard))

## Using Monte Carlo instead
hand <- sample(deck, 2)
hand
B <- 100000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


n <- 50
bdays <- sample(1:365, n, replace=TRUE)

duplicated(c(1,2,3,1,4,3,5))

any(duplicated(bdays))

B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
})
head(results)
mean(results)


# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

compute_prob <- function(n, B=10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1,60)

compute_prob(n)

prob <- sapply(n, compute_prob)

plot(n, prob)

exact_prob <- function(n) {
  prob_unique <- seq(365, 365-n+1)/365
  1-prod(prob_unique)
}

?prod

eprob <- sapply(n, exact_prob)

plot(n,prob)
lines(n, eprob, col="red")

?lines

B <- 10^seq(1,5, len=100)
compute_prob <- function(B, n=22) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)

plot(log10(B), prob, type="l")


