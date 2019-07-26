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
permutations(3,2)
combinations(3,2)

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


