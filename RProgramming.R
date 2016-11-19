## R PROGRAMMING

deal <- function(cards) {
    cards[1,]
}

deck2 <- deck[1:52,]
head(deck2)
deck3 <- deck[c(2,1,3:52),]
head(deck3)
random <- sample(1:52,size=52)
random
deck4 <- deck[random,]
head(deck4)

shuffle <- function(cards) {
    random <- sample(1:52,size=52)
    cards[random,]
}

deck2 <- deck
deck2[c(13,26,39,52),]
deck2$value[c(13,26,39,52)]
deck2$value[c(13,26,39,52)] <- 14

deck3 <- shuffle(deck)
head(deck3)

sum(deck2$face == "ace")
deck3$face == "ace"
deck3$value[deck3$face=="ace"]
deck3$value[deck3$face=="ace"] <- 14
head(deck3,20)

deck4 <- deck
deck4$value <- 0
head(deck4,13)
deck4$value[deck4$suit=="hearts"] <- 1
tail(deck4)
deck4[deck4$face=="queen",]
queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"]
deck4[queenOfSpades,]
deck4$value[queenOfSpades] <- 13

deck5 <- deck

head(deck5,13)
facecard <- deck5$face %in% c("king","queen","jack")
deck5[facecard,]
deck5$value[facecard] <- 10
head(deck5,13)
deck5$value[deck5$face=="ace"] <- NA
head(deck5,13)

# environments
library(devtools)
parenvs(all=TRUE)
as.environment("package:stats")
parent.env(globalenv())
parent.env(emptyenv())
ls(emptyenv())
ls(baseenv())
ls(globalenv())
head(globalenv()$deck,3)
assign("new","Hello Global",envir=globalenv())
globalenv()$new
environment()
# scoping rules
show_env <- function(x=foo) {
    list(ran.in=environment(),
         parent=parent.env(environment()),
         objects=ls.str(environment()))
}

environment(show_env)
environment(parenvs)

show_env <- function() {
    a <- 1
    b <- 2
    c <- 3
    list(ran.in=environment(),
         parent=parent.env(environment()),
         objects=ls.str(environment()))
}

foo <- "take me to your runtime"
show_env(foo)

DECK <- deck
deck <- deck[-1,]
head(deck,3)
deal <- function() {
    card <- deck[1,]
    assign("deck",deck[-1,],envir=globalenv())
    card
}

shuffle <- function() {
    random <- sample(1:52,size=52)
    assign("deck",DECK[random,],envir=globalenv())
}

shuffle()
deal()
deal()

setup <- function(deck) {
    DECK <- deck
    
    DEAL <- function() {
        card <- deck[1,]
        assign("deck",deck[-1,],envir=parent.env(environment()))
        card
    }
    
    SHUFFLE <- function() {
        random <- sample(1:52,size=52)
        assign("deck",DECK[random,],envir=parent.env(environment()))
    }
    
    list(deal=DEAL,shuffle=SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle

rm(deck)
shuffle()
deal()
deal()

## Project 3
# generate symbols
get_symbols <- function() {
    wheel <- c("DD","7","BBB","BB","B","C","0")
    sample(wheel,size=3,replace=TRUE,prob=c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

# score code
score <- function(symbols) {
# identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B","BB","BBB")
# get prize
  if (same) {
    payouts <- c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0,2,5)[cherries+1]
  }
# adjust for diamonds
diamonds <- sum(symbols == "DD")
prize * 2 ^ diamonds
}

# play function to run slot simulation
play <- function() {
    symbols <- get_symbols()
    print(symbols)
    score(symbols)
}

### to play slot run this
play()



# tests before putting code above
symbols <- c("7","7","7")
symbols[1] == symbols[2] & symbols[2] == symbols[3]
all(symbols==symbols[1]) # either subset like above and do logical test or simplify and use all()

symbols <- c("B","BB","BBB")
symbols <- c("B","0","DD")
all(symbols %in% c("B","BB","BBB")) # check all symbols are all bars

# payout table -- better than writing several if else statements
payouts <- c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
payouts
payouts["DD"]
unname(payouts["DD"])
payouts[symbols[1]]

# count cherries
symbols <- c("C","DD","C")
symbols == "C"
sum(symbols == "C")
sum(symbols == "DD")
# cherry prize -- can either do several if statements, but a simple lookup table is more efficient
cherries + 1
c(0,2,5)[cherries+1]
c(0,2,5)[cherries]
cherries <- 0

#double prize if diamonds result
prize * c(1,2,4,8)[diamonds+3] # my thought
prize * 2 ^ diamonds # better code



# S3
one_play <- play()
one_play
attributes(DECK)
row.names(DECK)
class(DECK)
names(DECK)
row.names(DECK) <- 101:152
levels(DECK) <- c("level 1", "level 2", "level 3")
attributes(DECK)
attributes(one_play)
attr(one_play,"symbols") <- c("B","0","B")
attributes(one_play)
attr(one_play,"symbols")
one_play+1

play <- function() {
    symbols <- get_symbols()
    prize <- score(symbols)
    attr(prize,"symbols") <- symbols
    prize
}
play()
two_play <- play()
two_play

play <- function() {
    symbols <- get_symbols()
    structure(score(symbols),symbols=symbols)
}
three_play <- play()
three_play

slot_display <- function(prize) {
    # extract symbols
    symbols <- attr(prize,"symbols")
    # collapse symbols into a single string
    symbols <- paste(symbols,collapse=" ")
    # combine symbol with prize as reg expr
    # \n is a reg ex for new line
    string <- paste(symbols,prize,sep="\n$")
    # display reg exp in console without quotes
    cat(string)
}

slot_display(one_play)
slot_display(play())

# methods passed to UseMethod (which calls method based on class of print/summary/head/etc first arg)
print.POSIXct
print.factor
print.data.frame
methods(print)

# write S3 method
class(one_play) <- "slots"

args(print)
print.slots <- function(x, ...) {
    cat("I'm using the print.slots method")
}

print(one_play)
one_play
rm(print.slots)

now <- Sys.time()
attributes(now) # UseMethod will hunt down first class, if can't find, go for second, otherwise go to default

print.slots <- function(x,...) {
    slot_display(x)
}
one_play

play <- function() {
    symbols <- get_symbols()
    structure(score(symbols),symbols=symbols,class="slots")
}

class(play())
play()
methods(class="factor")

play1 <- play()
play1

play2 <- play()
play2

c(play1,play2) # R stops using print.slots when combining objects to vector (drops attributes)
play1[1] # as well as when you subset

# Loops
die <- c(1,2,3,4,5,6)
rolls <- expand.grid(die,die)
rolls
expand.grid(die,die,die)
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls,3)
prob <- c("1"=1/8,"2"=1/8,"3"=1/8,"4"=1/8,"5"=1/8,"6"=3/8)
prob
rolls$Var1
prob[rolls$Var1]
rolls$prob1 <- prob[rolls$Var1]
head(rolls,3)
rolls$prob2 <- prob[rolls$Var2]
head(rolls,3)
rolls$prob <- rolls$prob1*rolls$prob2
head(rolls,3)
sum(rolls$value*rolls$prob)

wheel <- c("DD","7","BBB","BB","B","C","0")
combos <- expand.grid(wheel,wheel,wheel,stringsAsFactors=FALSE)
combos
prob <- c("DD"=0.03,"7"=0.03,"BBB"=0.06,"BB"=0.1,"B"=0.25,"C"=0.01,"0"=0.52)
prob[combos$Var1]
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos,3)
combos$prob <- combos$prob1*combos$prob2*combos$prob3
head(combos,3)
sum(combos$prob)
# for loops are for filling up vectors/lists with results of code
combos$prize <- NA
head(combos)
for (i in 1:nrow(combos)) {
    symbols <- c(combos[i,1],combos[i,2],combos[i,3])
    combos$prize[i] <- score(symbols)
}
sum(combos$prize*combos$prob)

# score code
score <- function(symbols) {
    diamonds <- sum(symbols == "DD")
    cherries <- sum(symbols == "C")
    # identify case
    slots <- symbols[symbols!="DD"]
    same <- length(unique(slots)) == 1
    bars <- slots %in% c("B","BB","BBB")
    # get prize
    if (diamonds==3) {
        prize <- 100
    } else if (same) {
        payouts <- c("7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
        prize <- unname(payouts[slots[1]])
    } else if (all(bars)) {
        prize <- 5
    } else if (cherries > 0) {
        # diamonds count as cherries as long as there is one cherry
        prize <- c(0,2,5)[cherries+diamonds+1]
    } else {
        prize <- 0
    }
    # adjust for diamonds
    prize * 2 ^ diamonds
}

# while loop
plays_till_broke <- function(start_with) {
    cash <- start_with
    n <- 0
    while (cash>0) {
        cash <- cash - 1 + play()
        n <- n+1
    }
    n
}
plays_till_broke(100)

# repeat loops
plays_till_broke <- function(start_with) {
    cash <- start_with
    n <- 0
    repeat {
        cash <- cash - 1 + play()
        n <- n+1
        if (cash <= 0) {
            break
        }
    }
    n
}
plays_till_broke(100)


# Vectorized code for speed - logical tests, subsetting, and element-wise selection
abs_loop <- function(vec) {
    for (i in 1:length(vec)) {
        if (vec[i] < 0) {
            vec[i] <- -vec[i]
        }
    }
    vec
}

abs_sets <- function(vec) {
    negs <- vec < 0
    vec[negs] <- vec[negs] * -1
    vec
}

long <- rep(c(-1,1),5000000)
#see which is faster
system.time(abs_loop(long))
system.time(abs_sets(long))

system.time(abs(long))

# how to write vectorized code
vec <- c(1,-2,3,-4,5,-6,7,-8,9,-10)
vec < 0
vec[vec<0]
vec[vec<0]*-1
vec[vec<0] <- vec[vec<0]*-1
vec

vec <- c("DD","C","7","B","BB","BBB","0")
change_vec <- function(vec) {
    vec[vec=="DD"] <- "joker"
    vec[vec=="C"] <- "ace"
    vec[vec=="7"] <- "king"
    vec[vec=="B"] <- "queen"
    vec[vec=="BB"] <- "jack"
    vec[vec=="BBB"] <- "ten"
    vec[vec=="0"] <- "nine"
    vec
}
many <- rep(vec,1000000)
system.time(change_vec(many))

change_vec2 <- function(vec) {
    tb <- c("DD"="joker","C"="ace","7"="king","B"="queen","BB"="jack","BBB"="ten","0"="nine")
    unname(tb[vec])
}
system.time(change_vec2(many))

system.time(
    output <- rep(NA,1000000)
    for (i in 1:1000000) {
        output[i] <- i + 1
    }
)

system.time(
    output <- NA
    for (i in 1:1000000) {
        output[i] <- i + 1
    }
)

#vectorize code practice
winnings <- vector(length=1000000)
for (i in 1:100000) {
    winnings[i] <- play()
} # takes 342 sec
mean(winnings)

get_many_symbols <- function(n) {
    wheel <- c("DD","7","BBB","BB","B","C","0")
    vec <- sample(wheel,size=3*n,replace=TRUE,prob=c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
    matrix(vec,ncol=3)
}
get_many_symbols(5)

play_many <- function(n) {
    symb_mat <- get_many_symbols(n=n)
    data.frame(w1=symb_mat[,1],w2=symb_mat[,2],w3=symb_mat[,3],prize=score_many(symb_mat))
}
plays <- play_many(1000000)
