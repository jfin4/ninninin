rm(list = ls())
phi <- (1 + 5**0.5) / 2
psi <- (1 - 5**0.5) / 2
n <- 7
fib <- (phi**(1:n) - psi**(1:n)) / (phi  - psi)
fib <- as.integer(fib)
r <- seq(fib[1], fib[n]) - fib[1]
i <- rep(NA, length(r))
i[fib[1:n]] <- 1:n
theta <- r * (r / max(r)) * (pi / max(r)) 
L = -theta - (pi / 2)
R = L + 2 * theta
tab <- data.frame(i, r, L, R) 
len <- nrow(tab)
tab

maketab <- function(j, tab) {
    # j <- 3
    branch <- tab[[j]]
    r <- tab$r[!is.na(branch)]
    nodes <- r[(r + r2) %in% fib]
    print(nodes + r2)
    if (branch[len] < branch[len - 1]) {
        skip <- 1
        sign <- 1
        minnodes <- 3
    } else {
        skip <- 2
        sign <- -1
        minnodes <- 5
    }
    if (length(nodes) >= minnodes) {
        elems <- seq(1 + skip, length(nodes) - skip, by = skip)
        noderows <- match(nodes, tab$r)[elems]
        offshoots <- sapply(noderows, makeoffshoot, branch, sign)
        tab <- cbind(tab, offshoots)
    }
    if (j + 1 > ncol(tab)) {
        return(tab)
    } else {
        maketab(j + 1, tab)
    }
}
makeoffshoot <- function(noderow, branch, sign) {
    main <- branch[noderow:len]
    offshoot <- rep(NA, len)
    offshoot[noderow:len] <- main + sign * 2 * theta[1:length(main)]
    return(offshoot)
}
tab <- maketab(2, tab)

par(mar = c(0, 0, 0, 0))
plot(c(1, -1), c(1, -1), type = "n")
# plot(c(tab$r, -tab$r), c(tab$r, -tab$r), type = "n")
for (i in 1:len) {
    seg <- tab[i:(i + 1), ]
    for (j in 3:ncol(tab)) {
        r <- seg[, 2]
        twig <- seg[, j]
        x <- r * cos(twig)
        y <- r * sin(twig)
        lines(x, y)
    }
}
