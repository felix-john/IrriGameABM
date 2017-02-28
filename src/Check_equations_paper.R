# eq 5
x <- 0:10
u <- c(2,5,8,15,19,20,13,7,4,1,0)
plot(x,u, type = "l")

tau <- - 0.001
utau <- exp(tau * u)
pr <- utau / sum(utau)
plot(x, pr, type = "l")

# trembling hand in water extraction
target <- 300
gamma <- 10 # [0, 20]
x <- 0:550
calcprob <- function(watercol, gammawatercol, water0) {(watercol ^ gammawatercol)/ (watercol ^ gammawatercol + water0 ^ gammawatercol)}
turnoff <- calcprob(x, gamma, target)
plot(x, turnoff, type = "l", ylim = c(0, 1))
turnoff[380]
