## One-Step of the binary tree algorithm ##

##  The Log-Likelhood function ####

log.like.1 <- function(alpha, data){
  n <- nrow(data)
  log.p.bar <- apply(log(data), 2, mean)
  result <- n*lgamma(sum(alpha)) - n*sum(lgamma(alpha)) + n*sum(alpha*log.p.bar)  ##Correct version
  return(result)
}

tree.finder <- function(index, dataset){
  eps <- 10^-5
  dataset <- (dataset+eps)/(1+2*eps) ## Protects against zero entries.
  dataset <- dataset/rowSums(dataset)
  orig.k <- ncol(dataset)  ##number of parts of the composition.
  index.vec <- index[index!=0]  ## removes 0 entries from index vector.
  indicator.vec <- 1:length(index.vec) ## numbers 1 through length of index vector with 0 removed
  
  ## Note:  The index vector gives us the columns to pull from the data set.  
  ## Which of the parts of the composition to use.  Or How many columns to use.
  ## A zero indicates that part of the composition has been dropped.
  
  if(length(index.vec) <= 2){
    return(matrix(rep(0, 2*length(index)), nrow=2))
  }
  
  else{
    new.data <- dataset[, index.vec]/rowSums(dataset[, index.vec])  ##Rescale to get branch proportions.
    n <- nrow(new.data)
    k <- ncol(new.data)
    j <- floor(k/2)  ## Two groups wiil be created.  k=7, (1, 6), (2, 5), (3,4)
    ## We only need the bottom half since (4,3), (5,2), (6, 1) are the same.  
    ## hence the floor of k/2
    
    combin.vec <- c()
    for(i in 1:j){
      combin.vec[i] <- choose(k, i)  ##Number of combinations for 1, 2, ....
    }
    
    total <- 1 + sum(combin.vec)  ## the extra one is for the standard DD.  
    ##Note: this double counts trees at the midway point since (1,2) (3, 4) is 
    ## the same as (3, 4) (1, 2).  Some inefficiency.
    
    criterion <- c()
    standard.alpha <- sirt::dirichlet.mle(new.data)$alpha
    criterion[1] <- log.like.1(standard.alpha, new.data)
    
    start <- 1
    
    ##The matrix labels tells which nodes are in one group.  Nodes not listed
    ## are in the other group.
    labels <-  matrix(rep(0,j), 1, j)  ##matrix ensures good formating
    for(i in 1:j){
      a <- combinations(k, i)
      labels <- rbind(labels, 
                      cbind(a, matrix(rep(0, (j-i)*nrow(a)), nrow = nrow(a))))
      
      ##Case 1: The loner node case: One internal node.
      if(i==1){
        for(m in 1:combin.vec[1]){
          level.1 <- cbind(new.data[,m], 1-new.data[,m])
          alpha.1 <- sirt::dirichlet.mle(level.1)$alpha
          level.2 <- new.data[, -m]/(1-new.data[,m])
          alpha.2 <- sirt::dirichlet.mle(level.2)$alpha
          criterion[start + m] <- log.like.1(alpha.1, level.1) + log.like.1(alpha.2, level.2)
        }
      }
      
      ## Case 2: Two internal nodes
      else{
        for(m in 1:combin.vec[i]){
          level.1 <- cbind(rowSums(new.data[, a[m,]]),  rowSums(new.data[, -a[m,]]))
          alpha.1 <- sirt::dirichlet.mle(level.1)$alpha
          level.2.left <- new.data[, a[m,]]/rowSums(new.data[, a[m,]])
          alpha.2.left <- sirt::dirichlet.mle(level.2.left)$alpha
          level.2.right <- new.data[, -a[m,]]/rowSums(new.data[, -a[m,]])
          alpha.2.right <- sirt::dirichlet.mle(level.2.right)$alpha
          criterion[start + m] <- log.like.1(alpha.1, level.1) +
            log.like.1(alpha.2.left, level.2.left) + log.like.1(alpha.2.right, level.2.right)
        }
        
      }
      start <- length(criterion)
    }
    
    bb <- c(k, rep(k+1, k), rep(k+2, nrow(labels)-k-1))
    neg2.crit <- -2*criterion
    best <- which(neg2.crit==min(neg2.crit))[1]  ##find the min, use the first if there are ties
    
    com.vec <- 1:k
    left.best <- labels[best ,]  ##nodes in left branch for the best
    right.best <- com.vec[-which(com.vec %in% left.best)]  ##Careful in the case of the non-nested design.  
    ##If no nest is the best you get left.best <- (0,0...)
    ## Right best then is integer 0.  Might need another if statement here.
    left.final <- index.vec[left.best]
    right.final <- index.vec[right.best]  ## Same problems here.
    final.matrix <- rbind( 
      c(left.final, rep(0, orig.k - length(left.final))),
      c(right.final, rep(0, orig.k - length(right.final)))
    )
    # print(list( labels = labels, criteria = neg2.crit, what=bb, loc=best, left=left.best, right=right.best, mat=final.matrix))
    return(final.matrix)
  }
}

##Complete Binary tree function ##
## Uses tree.finder to create a binary cascade tree where only one or two
## nodes are at the end of each branch.


complete.bin.tree <- function(data.set){
  eps <- 10^-5
  data.set <- (data.set+eps)/(1+2*eps)
  data.set <- data.set/rowSums(data.set)
  data <- as.matrix(data.set)  ##puts data into correct form
  
  ## Initialize vectors
  direction.mat <- c()
  splits.mat <- c()
  splits.temp <- c()
  dummy <- c()
  
  j = 1
  first.index <- 1:ncol(data)  ##for the first binary split, we are going to use all the variables.
  
  splits.mat <- rbind(splits.mat, tree.finder(first.index, data))  ##Records left and right splits
  splits.temp <- splits.mat
  total <- sum(splits.mat)  ##If total equals 0, the non-nested design is the winner
  
  ## For cases where a nested design wins.
  
  if( total != 0){
    direction.temp <- matrix( c(1,2,1,3), byrow = TRUE, nrow=2, ncol=2)  ## What does this do?
    direction.mat <- direction.temp
  }
  
  ## The iterative step.
  while ( total != 0){
    dummy <- c() 
    ## for each row of current splits.  Note there are 2*number of divisions in splits.
    ## one for the left branch and one for the right branch
    for (i in 1:nrow(splits.temp)){ 
      dummy <- rbind( dummy, tree.finder(splits.temp[i,], data))  ##does one more iteration on each split
    }
    
    total <- sum(dummy)
    splits.temp <- dummy
    splits.mat <- rbind(splits.mat, splits.temp)
    j = j+1
  }
  non.zero <- which(rowSums(splits.mat) != 0)
  splits.mat <- splits.mat[non.zero ,]
  return(splits.mat)
}
