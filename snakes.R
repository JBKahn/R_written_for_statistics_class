snakesandladders <- function(d){
    players = c(1,1)
    turns=1
    while (1)
    {
        for (i in c(1,2)) {
            # The spots of the board 1:10 are sampled based on the probabilities
            # found in the matrix, in the row corresponding to the players current
            # position.
            players[i] = sample(x=c(1:nrow(d)), p=d[players[i],], replace=F, 1)
            # When a player wins, don't let the other players continue the round.
            if ((players[1] == nrow(d)) || (players[2] == nrow(d))) {break}
        }
        # When a player wins, stop playing.
        if ((players[1] == nrow(d)) || (players[2] == nrow(d))) {break}
        # if no player has won, another turn is about to start.
        turns = turns + 1
    }
    return(turns)
}


matrix1 = c(
    1/6,  0/6,  1/6,	1/6,	1/6,	1/6,	0/6,	0/6,	1/6,	0/6,
    1/6,	0/6,	1/6,	1/6,	1/6,	1/6,	0/6,	1/6,	0/6,	0/6,
    1/6,	0/6,	0/6,	1/6,	1/6,	1/6,	0/6,	1/6,	1/6,	0/6,
    1/6,	0/6,	0/6,	0/6,	1/6,	1/6,	0/6,	1/6,	1/6,	1/6,
    1/6,	0/6,	0/6,	0/6,	1/6,	1/6,	0/6,	1/6,	1/6,	1/6,
    1/6,	0/6,	0/6,	0/6,	0/6,	2/6,	0/6,	1/6,	1/6,	1/6,
    3/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	1/6,	1/6,	1/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	4/6,	1/6,	1/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	5/6,	1/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	6/6
)
matrix1 = matrix(matrix1,sqrt(length(matrix1)),sqrt(length(matrix1)),byrow = T)
matrix1list = (c(
    1,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,
    0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,
    1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,1))


matrix2 = c(
    0/6,  0/6,  2/6,  2/6,  0/6,	1/6,	0/6,	0/6,	0/6,	1/6,
    0/6,	0/6,	1/6,	1/6,	0/6,	1/6,	0/6,	1/6,	0/6,	1/6,
    0/6,	0/6,	1/6,	1/6,	0/6,	1/6,	0/6,	1/6,	1/6,	1/6,
    0/6,	0/6,	1/6,	0/6,	0/6,	1/6,	0/6,	1/6,	1/6,	2/6,
    0/6,	0/6,	1/6,	0/6,	0/6,	1/6,	0/6,	1/6,	1/6,	2/6,
    0/6,	0/6,	1/6,	0/6,	0/6,	2/6,	0/6,	1/6,	1/6,	1/6,
    0/6,	0/6,	6/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	4/6,	1/6,	1/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	5/6,	1/6,
    0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	0/6,	6/6
)
matrix2 = matrix(matrix2,sqrt(length(matrix2)),sqrt(length(matrix2)),byrow = T)
matrix2list = (c(
    1,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,
    0,0,0,0,0,1,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,1))

# takes a matrix
turnstowin <- function(n,d){
    turns=c()
    for(i in 1:n) {
        turns=append(turns,snakesandladders(d))
    }
    return(mean(turns))
}




expected <- function(d){
    count1 = 0
    count2 = 0
    count3 = 0
    count4 = 0
    count5 = 0
    count6 = 0
    
    counta = 0
    countb = 0
    countc = 0
    countd = 0
    counte = 0
    countf = 0
    
    for(i in 1:6) {
        if (checkdie(d,c(i),0)) {
            count1 = count1 + 1
        }
        if (checkdie(d,c(i),1)) {
            counta = counta + 1
        }
        for(j in 1:6) {
            if (checkdie(d,c(i,j),0)) {
                count2 = count2 + 1
            }
            if (checkdie(d,c(i,j),1)) {
                countb = countb + 1
            }
            for(w in 1:6) {
                if (checkdie(d,c(i,j,w),0)) {
                    count3 = count3 + 1
                }
                if (checkdie(d,c(i,j,w),1)) {
                    countc = countc + 1
                }
                for (z in 1:6) {
                    if (checkdie(d,c(i,j,w,z),0)) {
                        count4 = count4 + 1
                    }
                    if (checkdie(d,c(i,j,w,z),1)) {
                        countd = countd + 1
                    }
                    for(k in 1:6) {
                        if (checkdie(d,c(i,j,w,z,k),0)) {
                            count5 = count5 + 1
                        }
                        if (checkdie(d,c(i,j,w,z,k),1)) {
                            counte = counte + 1
                        }
                        for(x in 1:6) {
                            if (checkdie(d,c(i,j,w,z,k,x),0)) {
                                count6 = count6 + 1
                            }
                            if (checkdie(d,c(i,j,w,z,k,x),1)) {
                                countf = countf + 1
                            }
                        }
                    }
                }
            }
        }
    }
    return(c(count1,count2,count3,count4,count5,counta,countb,countc,countd,counte,count6,countf))
}

checkdie <- function(d,dierolls,x){
    spot=1
    boardsize=sqrt(length(d))
    for (i in dierolls) {
        roll = i
        if((spot + roll) < (boardsize +1)) {
            spot = which(d[((spot + roll -1)*boardsize+1):((spot + roll)*boardsize)] == 1)
        }
        if (x && (spot == boardsize) && (i != dierolls[length(dierolls)]))
            return(0)
    }
    return (spot == boardsize)
}
