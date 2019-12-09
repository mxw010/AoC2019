input <- read.table("junk.txt")
#day 1
#problem 1
#calculating fuels needed:
#divided by 3, round down and then minus 2
fuel.output <- function(x) 
{
	z <- floor(x/3)-2
	return(z)
}
sum(fuel.output(input))

#problem 2
#Never learned how to do recursion
fuel.output <- function(x) {
	y <- 0
	z <- max(floor(x/3)-2,0)
	while (z > 0) {
		y <- y + z
		z <-  max(floor(z/3)-2,0)
	}
	return(y)	
}

#R pls
sum(apply(input,1, fuel.output))
