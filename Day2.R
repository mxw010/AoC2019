#day 2
a <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0)

#R indeces starts from 1
a[1+1] <- 12
a[2+1] <- 2

intcode <- function(a) {
	i=1
	while (i <= length(a)) {
		if ( (i-1) %% 4 ==0) {
			if (a[i] == 99) {
				#print(paste("i=",i,", end the loop",sep=""))
				break
			} else if (a[i] == 1) {
				#print(paste("i=",i,", addition",sep=""))
				calc="+"
			} else if (a[i] == 2) {
				#print(paste("i=",i,", multiplication",sep=""))
				calc="*"
			}
				
		} else if ( (i-2) %%4 ==0 ) {
			#print(paste("i=",i,", first number ", a[i],sep=""))
			x1 <- a[a[i]+1]
		} else if ( (i-3) %% 4 == 0) {
			#print(paste("i=",i,", second number ", a[i],sep=""))
			x2 <- a[a[i]+1]
		} else {
			value<- eval(parse(text=paste(x1,calc,x2)))
			#print(paste("i=",i,", assign ", a[i]+1, " value ", value, sep=""))
			a[a[i]+1] <- value
		}
		i <- i+1
	}
	return(a)
}

intcode(a)[1]
#4484226


#problem 2
result <- matrix(0,ncol=100,nrow=100)

for (m in 1:100) {
	for (n in 1:100) {
		b <- a
		b[1+1] <- m-1
		b[2+1] <- n-1
		result[m,n] <- intcode(b)[1]
	}
}


#notice that the values are strictly increasing

which.row <- which(result[,1] > 19690720)[1]-1
which.col <- which(result[which.row,]==19690720)

100*(which.row-1)+(which.col-1)
