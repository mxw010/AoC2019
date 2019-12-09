#day 4
numbers <- 246540:787419 #all the numbers
whichones <- rep(0,length(numbers))  
for (k in 1:length(numbers)) {
	i <- numbers[k] #k-th number
	y <- rep(0,6) #create the 6 digit
	for (j in 1:6) {
		y[j] <- trunc((i - y%*%10^(5:0))/10^(6-j))
	}
	if (any(diff(y)==0) & all(diff(y) >=0) & ) { #non-decreasing and at least one repeat 
		whichones[k] <- 1
	}
}

sum(whichones)
#1063

whichones <- rep(0,length(numbers))  
for (k in 1:length(numbers)) {
	i <- numbers[k] 
	y <- rep(0,6) #create the 6 digit
	for (j in 1:6) {
		y[j] <- trunc((i - y%*%10^(5:0))/10^(6-j))
	}
  #since you can't have more than 2 repeats, table(y) must contain number 2
  #I thought 333333 was allowed at first 
	if (any(diff(y)==0) & all(diff(y) >=0) & (2 %in% table(y) ) ) { 
		whichones[k] <- 1
	}
}

sum(whichones)
#686
