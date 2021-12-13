body <- "(x1 + x2) * x3"
args <- "x1, x2, x3"
eval(parse(text = paste('f <- function(', args, ') { return(' , body , ')}')))
f(3,2,5)

