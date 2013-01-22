spattercast <- function(expected,n,runs){
    count = 0.0
    undercurve = 0
    spatters = c()
    for (i in 1:runs) {
        while (count == 0 || (round(((undercurve * (2 + (cos(1))^3))/count),n) != expected)) {
            x = runif(1, min=0, max=1)
            y = runif(1, min=0, max=(2 + (cos(1))^3))
            
            if (y <= (2*(x^8) + (cos(x))^3))
                undercurve = undercurve + 1
            count =  count + 1
        }
        spatters = append(spatters,count)
        print(length(spatters))
        count = 0
        undercurve = 0
    }
    print(mean(spatters))
}