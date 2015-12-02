
#' @export

plot.M4ts <- function(x, xlim = c(tsp(x$past)[1], tsp(x$future)[2]), ylim = range(x$past, x$future),
                        main = x$ID, xlab = "", ylab = "", ...)
{
    freq = frequency(x$past)
    plot(ts(c(x$past, rep(NA, x$H)), end = tsp(x$past)[2] + x$H/freq, frequency = freq), ylim = ylim,
                xlim = xlim, ylab = "", xlab = "", ...)
    
    #if(nchar(x$description) > 70)
    #    title(main=list(main,cex=0.75,font=2))
    #else
    #    title(main=list(main,cex=1,font=2))
    
    lines(ts(x$future, start = tsp(x$past)[2] + 1/freq, frequency = freq), lt = 1, col = 2)
}
