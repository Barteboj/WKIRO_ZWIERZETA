unpadimage <- function(i, amount)
{
  if (length(amount) == 1)
  {
    sizex = size(i, 2) - 2 * amount
    sizey = size(i, 1) - 2 * amount
    l = amount + 1
    r = size(i, 2) - amount
    t = amount + 1
    b = size(i, 1) - amount
  }
  else if (length(amount) == 2)
  {
    sizex = size(i, 2) - 2 * amount[1]
    sizey = size(i, 1) - 2 * amount[2]
    l = amount[1] + 1
    r = size(i, 2) - amount[1]
    t = amount[2] + 1
    b = size(i, 1) - amount[2]
  }
  else if (length(amount) == 4)
  {
    sizex = size(i, 2) - (amount[1] + amount[3])
    sizey = size(i, 1) - (amount[2] + amount[4])
    l = amount[1] + 1
    r = size(i, 2) - amount[3]
    t = amount[2] + 1
    b = size(i, 1) - amount[4]
  }
  else
  {
    stop('Wrong unpad amount, stopping...')
  }
  
  if ((sizex < 1) || (sizey < 1))
  {
    print('new size of image is less than one, returning empty vector')
    unpaddedImage = vector[]
    return (unpaddedImage)
  }
  
  if (length(dim(i)) == 2) {
    unpaddedImage = i[t:b, l:r]
  } else { 
    unpaddedImage = i[t:b, l:r, ]
  }
  
  return (unpaddedImage)
}
