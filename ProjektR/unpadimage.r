unpadimage <- function(i, amnt)
{
  if (length(amnt) == 1)
  {
    sx <- size(i, 2) - 2 * amnt
    sy <- size(i, 1) - 2 * amnt
    l <- amnt + 1
    r <- size(i, 2) - amnt
    t <- amnt + 1
    b <- size(i, 1) - amnt
  }
  else if (length(amnt) == 2)
  {
    sx <- size(i, 2) - 2 * amnt[1]
    sy <- size(i, 1) - 2 * amnt[2]
    l <- amnt[1] + 1
    r <- size(i, 2) - amnt[1]
    t <- amnt[2] + 1
    b <- size(i, 1) - amnt[2]
  }
  else if (length(amnt) == 4)
  {
    sx <- size(i, 2) - (amnt[1] + amnt[3])
    sy <- size(i, 1) - (amnt[2] + amnt[4])
    l <- amnt[1] + 1
    r <- size(i, 2) - amnt[3]
    t <- amnt[2] + 1
    b <- size(i, 1) - amnt[4]
  }
  else
  {
    stop('illegal unpad amount\n')
  }
  
  if ((sx < 1) || (sy < 1))
  {
    print('unpadimage newsize < 0, returning []\n')
    o <- vector[]
    return
  }
  
  if (length(dim(i)) == 2) {
    o <- i[t:b, l:r]
  } else { #is it bug?
    o <- i[t:b, l:r, ]
  }
  
  return (o)
}
