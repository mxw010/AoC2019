#code for part 2 since part 2 improves on part 1

intcode <- function(a,input) {
	i=1
	while (i <= length(a)) {
		y <- rep(0,5) #create the 6 digit
		for (j in 1:5) {
			y[j] <- trunc((a[i] - y%*%10^(4:0))/10^(5-j))
	  }
	  opcode <- 10*y[4]+y[5]
		par1.mode <- y[3]
		par2.mode <- y[2]
		par3.mode <- y[1] #not needed
		#print(paste("i=",i,", first number ", a[i],sep=""))
		par1 <- ifelse(par1.mode==0, a[a[i+1]+1], a[i+1])
		#print(paste("i=",i,", second number ", a[i],sep=""))
		par2 <- ifelse(par2.mode==0, a[a[i+2]+1], a[i+2])

		if (opcode == 99) {
			print(paste("i=", i, ", program halts.", sep=""))
			break
		} else if ( opcode == 1 | opcode == 2 ) {
			#print(paste("i=",i,", addition",sep=""))
			#get what operation should be carried out
			calc=switch(opcode, "+", "*")
			#get values for both parameters
			value<- eval(parse(text=paste(par1,calc,par2)))
			#print(paste("i=",i,", assign ", a[i]+1, " value ", value, sep=""))
			a[a[i+3]+1] <- value
			#specifty next starting spot
			i <- i + 4
		} else if ( opcode == 3 ) {
			#print(paste("i=",i,", multiplication",sep=""))
			a[a[i+1]+1] <- input
			i <- i + 2
		} else if ( opcode == 4 ) {
			output.value <- par1
			print(paste("i = ", i+2, ", diagnostic code = ", output.value, ".", sep=""))
			i <- i + 2
		} else if ( opcode == 5 ) {
			if ( par1 != 0 ) {
				#gosh they moved the pointer itself
        #I am a giddy goat
				i <- par2+1
			} else {
				i <- i+3
			}
		} else if ( opcode == 6 ) {
			if ( par1 == 0 ) {
				i <- par2+1
			} else {
				i <- i+3
			}
		} else if ( opcode == 7 ) {
			a[a[i+3]+1] <- ifelse(par1 < par2, 1, 0)
			i <- i + 4
		} else if ( opcode == 8 ) {
			a[a[i+3]+1] <- ifelse(par1 == par2, 1, 0)
			i <- i + 4
		} else {
			print(paste("i = ",i, ", error opcode = ", opcode, ".", sep=""))
			break
		}
		#print(paste("opcode = ", opcode, ". par1 = ", par1, ", par2 = ", par2, ", i = ",i, ".", sep=""))
	}	
	#return(a)
}

input <- scan("D5_1.txt",sep=",")
#part 1
#[1] "i = 223diagnostic code = 15426686."
#[1] "i=223, program halts."


#part 2
intcode(input,5)
#11430197
