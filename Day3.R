path1 <- scan("paths1.txt",what="char", sep=",")
path2 <- scan("paths2.txt",what="char", sep=",")

#list all the paths the wire has been on
find.coord <- function(path) {
	path <- strsplit(path,split="")
	coord <- NULL
	cur.coord <- c(0,0,0)
	coord <- rbind(coord, cur.coord)
	for (i in 1:length(path)) {	
	  cur.steps <- cur.coord[3]
	  route <- path[[i]]
	  dir <- route[1]
	  distance <- as.numeric(paste(route[-1],collapse=""))
	  steps <- seq(cur.steps+1,cur.steps+distance,by=1)
	  if (dir == "R") {
	    new.coord <- seq(cur.coord[1]+1,cur.coord[1]+distance,by=1)
	    new.coord <- cbind(new.coord,rep(cur.coord[2],length(new.coord))) 
	  } else if (dir == "L") {
      	new.coord <- seq(cur.coord[1]-1,cur.coord[1]-distance,by=-1)
	    new.coord <- cbind(new.coord,rep(cur.coord[2],length(new.coord))) 
	  } else if (dir == "U") {
        new.coord <- seq(cur.coord[2]+1,cur.coord[2]+distance,by=1)
	    new.coord <- cbind(rep(cur.coord[1],length(new.coord)),new.coord) 
	  } else if (dir == "D") {
        new.coord <- seq(cur.coord[2]-1,cur.coord[2]-distance,by=-1)
	    new.coord <- cbind(rep(cur.coord[1],length(new.coord)),new.coord) 
	  }  
	  new.coord <- cbind(new.coord, steps)
	  cur.coord <- new.coord[nrow(new.coord),]
	  coord <- rbind(coord, new.coord)  
 	}
  return(coord)
}

coord1 <- find.coord(path1)
coord2 <- find.coord(path2)

#getting rid of the starting coord (0,0)
coord1 <- coord1[-1,]
coord2 <- coord2[-1,]

#order them numerically
#coord1 <- coord1[order(coord1[,1], coord1[,2]),]
#coord1 <- matrix(coord1,nrow=nrow(coord1)) 

#coord2 <- coord2[order(coord2[,1], coord2[,2]),]
#coord2 <- matrix(coord2,nrow=nrow(coord2)) 

#get the coordinates that exist in both paths to reduce search size
com.coord1 <- intersect(coord1[,1], coord2[,1])
com.coord2 <- intersect(coord1[,2], coord2[,2])

which.coord1 <- which((coord1[,1] %in% com.coord1) & (coord1[,2] %in% com.coord2))
which.coord2 <- which((coord2[,1] %in% com.coord1) & (coord2[,2] %in% com.coord2))

candidate.coord1 <- coord1[which.coord1,]
candidate.coord2 <- coord2[which.coord2,]

#solution to problem 1. 
#search for overlappings and map the distance to the origin
result <- NULL

for (i in 1:nrow(candidate.coord1)) {
	for (j in 1:nrow(candidate.coord2)) {
		if (sum(abs(candidate.coord1[i,1:2]-candidate.coord2[j,1:2]))==0) {
			dist <- sum(abs(candidate.coord1[i,1:2]))
			result <- rbind(result,c(i,j,dist))
		}
	}
}


#getting the answer
result[which.min(result[,3]),]
#207

#solution to problem 2
steps <- matrix(0,ncol=2,nrow=nrow(result))
for (i in 1:nrow(result)) {
	steps[i,1] <- candidate.coord1[result[i,1],3]	
	steps[i,2] <- candidate.coord2[result[i,2],3]
}

min(rowSums(steps)
#21196
