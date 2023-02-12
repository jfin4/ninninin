iter <- 5
smooth <- 5 # 1--5
pal <- "earth"
bg <- "white"
input <- data.frame(iter, smooth, pal, bg)

n <- input$iter # no. iterations
smooth <- input$smooth # smoothness factor; 1 is no smoothing
bg <- input$bg # background color
pal <- input$pal # color palette

# create thetas table 
# define golden ratio
phi <- (1 + 5**0.5) / 2 # 
# create radius vector 
rfib <- phi**(0:n) - min(phi**0:n)
rlen <- 2.2**smooth - 1.2 # smoothness
rint <- seq(rfib[1], rfib[length(rfib)], length.out = rlen)
r <- sort(unique(c(rfib, rint)))
rfibi <- rep(NA, length(r))
rfibi[r %in% rfib] <- 1:length(rfib) # 
# create first column of thetas table
theta <- r / max(r) * pi
tab <- data.frame(l = -theta - pi / 2, r = theta - pi / 2) # 
# fill columns of thetas table
blank <- data.frame(rep(NA, nrow(tab)))
j <- 1 
while (j <= ncol(tab)) {
    col <- tab[j]
    j <- j + 1
    if (grepl("l", names(col))) {
        inc <- 1
        sign <- 1
        name <- "r"
    } else {
        inc <- 2
        sign <- -1
        name <- "l"
    }
    start <- rfibi[!is.na(col)][1] 
    if (start + inc >= max(rfibi, na.rm = TRUE)) {
        next
    }
    for (i in seq(start + inc, n, by = inc)) {
    seq(start + inc, n, by = inc)
        seg <- r >= rfib[i]
        rr <- r[seg] - min(r[seg])
        ttheta <- 
            (rr / max(rr)) *
            (rfib[length(rfib) - (i - 1)] / max(rfib)) *
            2 * pi
        newcol <- blank
        newcol[seg, ] <- col[seg, ] + sign * ttheta
        names(newcol) <- paste(name, j, i, sep = ".")
        tab <- cbind(tab, newcol)
    }
} 
# sort table
colorder <- names(sort(tab[nrow(tab), 1:ncol(tab)]))
tab <- tab[colorder] # 
# make circumfrence
ctab <- unlist(tab[nrow(tab), ])
clen <- 2.9^smooth - 1.9
cint <- seq(ctab[1], ctab[length(ctab)], length.out = clen)
circ <- sort(unique(c(ctab, cint)), decreasing = TRUE)# 

# plot
# open device
par(mar = c(0, 0, 0, 0), bg = bg) # 
# open plot 
plot(0,
    xlim = c(-max(r), max(r)),
    ylim = c(-max(r), max(r)),
    type = "n",
    axes = F,
    xlab = "",
    ylab = ""
) # 

# polgons
getnode <- function(col, row = nrow(tab), keep = "inner", data = tab) {# 
    seg <- data[col]
    if (keep == "inner") {
        seg[nrow(tab):row, ] <- NA
    } else if (keep == "outer") {
        seg[1:row, ] <- NA
    }
    isnode <- apply(data, 2, function(x) seg == x)
    isnode[is.na(isnode)] <- isnode[, col] <- FALSE
    isnodej <- apply(isnode, 2, any)
    nodej <- which(isnodej)[1]
    nodei <- which(data[nodej] == data[col])
    node <- data.frame(row = nodei, col = nodej, row.names = NULL)
    return(node)
}
colors <- hcl.colors(n = ncol(tab),
                    palette = pal)

for (j in 1:(ncol(tab) - 1)) {
    loth <- lith <- rith <- roth <- bth <- NULL
    lor <- lir <- rir <- ror <- br <- NULL
    nnode <- getnode(j)
    loth <- tab[nrow(tab):nnode$row, j]
    lor <- r[nrow(tab):nnode$row]
    pnode <- nnode
    if (pnode$col < j) {
        nnode <- getnode(pnode$col, pnode$row, "inner")
        lith <- tab[pnode$row:nnode$row, pnode$col]
        lir <- r[pnode$row:nnode$row]
        pnode <- nnode
    }
    if (pnode$col > (j + 1)) {
        nnode <- getnode(pnode$col, pnode$row, "outer")
        rith <- tab[pnode$row:nnode$row, pnode$col]
        rir <- r[pnode$row:nnode$row]
        pnode <- nnode
    }
    roth <- tab[pnode$row:nrow(tab), j + 1]
    ror <- r[pnode$row:nrow(tab)]
    bth <- circ[circ > loth[1] & circ < roth[length(roth)]]
    br <- rep(r[length(r)], length(bth))
    sideth <- c(loth, lith, rith, roth, bth)
    sider <- c(lor, lir, rir, ror, br)
    x <- cos(sideth) * sider
    y <- sin(sideth) * sider
    polygon(x, y, 
            col = colors[j],
            # border = bg,
            # density = 60,
            # angle = ((360 / (ncol(tab) - 1)) * j) - 15
    )
}

