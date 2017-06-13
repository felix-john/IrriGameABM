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

# lambda

expC <- 0.5
lambda <- 0.5     # sensitivity to update expC due to communication
lambda.ext <- 0.2 # relative update of expC due to extraction
score <- 0.5        # own harvest compared to average harvests

expC <- expC * (1 - lambda) + lambda; (expC <- expC * (1 - lambda.ext) + lambda.ext * score)

# the larger lambda, the faster expC will grow
# the larger lambda.ext, the slower expC will grow
# unless score < 1, expC will always grow