maxfilter <- function(I, radius) {
  #
  #TODO docs
  source('padimage.r')
  source('unpadimage.r')
  
  #I <- matrix()
  
  if (length(radius) == 1) {
    stop('NOT IMPLEMENTED')
    #I <- padimage(I, radius)
    #n <- dim(I)[1]
    #m <- dim(I)[2]
    #thirdd <- dim(I)[3]
    #B <- I
    #TODO
    #for (i in (radius+1):(m-radius)) {
    #B[,i,] <- max(I(:,i-radius:i+radius,:),[],2);
    #}
    #for i = radius+1:n-radius,
    #I(i,:,:) = max(B(i-radius:i+radius,:,:),[],1);
    #end
    #I = unpadimage(I,radius);
    
  } else if (length(radius) == 4) {
    
    n <- dim(I)[1]
    m <- dim(I)[2]
    thirdd <- dim(I)[3]
    B <- I
    
    if (radius[1] > 0) {
      for (i in 1:radius[1]) {
        cols <- seq(max(1, i - radius[1]), min(m, i + radius[3]))
        t <- I[,cols]
        d <- dim(t)
        if (length(d) > 1) {
          t <- apply(t, 1, max)
        }
        B[,i] <- t
      }
    }
    
    for (i in seq(radius[1] + 1, m - radius[3])) {
      cols <- seq(i - radius[1], i+radius[3])
      t <- I[,cols]
      d <- dim(t)
      if (length(d) > 1) {
        t <- apply(t, 1, max)
      }
      B[,i] <- t
    }
    
    for (i in seq(m - radius[3] + 1, m)) {
      cols <- seq(i - radius[1], min(m, i + radius[3]))
      t <- I[,cols]
      d <- dim(t)
      if (length(d) > 1) {
        t <- apply(t, 1, max)
      }
      B[,i] <- t
    }
    
    if (radius[2] > 0) {
      for (i in 1:radius[2]) {
        rows <- seq(max(1, i - radius[2]), i + radius[4])
        t <- B[rows,]
        d <- dim(t)
        if (length(d) > 1) {
          t <- apply(t, 2, max)
        }
        I[i,] <- t
      }
    }
    
    for (i in seq(radius[2] + 1, n - radius[4])) {
      rows <- seq(max(1, i - radius[2]), min(n, i + radius[4]))
      t <- B[rows,]
      d <- dim(t)
      if (length(d) > 1) {
        t <- apply(t, 2, max)
      }
      I[i,] <- t
    }
    
    for (i in seq(n - radius[4] + 1, n)) {
      rows <- seq(i - radius[2], min(n, i + radius[4]))
      t <- B[rows,]
      d <- dim(t)
      if (length(d) > 1) {
        t <- apply(t, 2, max)
      }
      I[i,] <- t
    }
  } else {
    stop('maxfilter: poorly defined radius')
  }
  
  return(I)
}