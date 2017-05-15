C1 = function(stim, filters, fSiz, c1SpaceSS, c1ScaleSS, c1OL, INCLUDEBORDERS = 0) {

  library('matlab')
  library('EBImage')
  
  source('unpadimage.r')
  source('sumfilter.r')
  source('convulation.r')
  source('maxfilter.r')

  USECONV2 = 1
  
  numScaleBands = length(c1ScaleSS) - 1  #convention: last element in c1ScaleSS is max index + 1 
  numScales = tail(c1ScaleSS, n = 1) - 1
  # last index in scaleSS contains scale index where next band would start, i.e., 1 after highest scale!!
  numSimpleFilters = floor(length(fSiz) / numScales)
  ScalesInThisBand = list()
  for (iBand in 1:numScaleBands) {
    ScalesInThisBand[[iBand]] = c1ScaleSS[iBand]:(c1ScaleSS[iBand + 1] - 1)
  }
    
  ##Rebuild all filters (of all sizes)
  ##
  nFilts = length(fSiz)
  sqfilter = list()
  for (i in 1:nFilts) {
    temp = matrix(filters[1:(fSiz[i]^2), i], nrow = fSiz[i], ncol = fSiz[i])
    if (USECONV2) {
      temp = temp[fSiz[i]:1, fSiz[i]:1] #flip in order to use conv2 instead of imfilter (%bug_fix 6/28/2007);
    }
    sqfilter[[i]] = temp
  }

  ## Calculate all filter responses (s1)
  ##
  sqim = stim ^ 2
  iUFilterIndex = 0
  # precalculate the normalizations for the usable filter sizes
  
  uFiltSizes = unique(fSiz)
  s1Norm = list()
  for (i in 1:length(uFiltSizes)) {
    s1Norm[[uFiltSizes[i]]]= (sumfilter(sqim,(uFiltSizes[i]-1)/2))^0.5
    s1Norm[[uFiltSizes[i]]] = s1Norm[[uFiltSizes[i]]] + !s1Norm[[uFiltSizes[i]]]
  }
  
  s1 = list()
  for (iBand in 1:numScaleBands) {
    s1[[iBand]] = list()
    for (iScale in 1:length(ScalesInThisBand[[iBand]])) {
      s1[[iBand]][[iScale]] = list()
      for (iFilt in 1:numSimpleFilters) {
        s1[[iBand]][[iScale]][[iFilt]] = list()
        iUFilterIndex = iUFilterIndex + 1
        if (!USECONV2) {
          stim = Image(data = stim, dim(stim), colormode = 0)
          s1_temp = abs(filter2(stim, sqfilter[[iUFilterIndex]], boundary = 'replicate'))
        } else { #not 100% compatible but 20% faster at least
          s1_temp = abs(conv2same(stim, sqfilter[[iUFilterIndex]]))
        }
        if (!INCLUDEBORDERS) {
          s1_temp = removeborders(s1_temp, fSiz[iUFilterIndex])
        }
        s1[[iBand]][[iScale]][[iFilt]] = as.double(s1_temp) / s1Norm[[fSiz[iUFilterIndex]]]
      }
    }
  }
    
  ## Calculate local pooling (c1)
  ##
  c1 = list()
  for (iBand in 1:numScaleBands) {
    c1[[iBand]] = list()
    for (iFilt in 1:numSimpleFilters) {
      c1[[iBand]][[iFilt]] = zeros(size(s1[[iBand]][[1]][[iFilt]]))
      for (iScale in 1:length(ScalesInThisBand[[iBand]])) {
        c1[[iBand]][[iFilt]] = pmax(c1[[iBand]][[iFilt]], s1[[iBand]][[iScale]][[iFilt]])
      }
    }
  }
    
    
  ## 2) pool over local neighborhood
  ##
  for (iBand in 1:numScaleBands) {
    poolRange = c1SpaceSS[iBand]
    for (iFilt in 1:numSimpleFilters) {
      c1[[iBand]][[iFilt]] = maxfilter(c1[[iBand]][[iFilt]],c(0, 0, poolRange-1, poolRange-1));
    }
  }
    
  ##(3) subsample
  ##
  for (iBand in 1:numScaleBands) {
    sSS = ceil(c1SpaceSS[iBand] / c1OL)
    T = list()
    for (iFilt in 1:numSimpleFilters) {
      temp = c1[[iBand]][[iFilt]]
      dims = dim(temp)
      T[[iFilt]] = temp[seq(1, dims[1], by = sSS), seq(1, dims[2], by = sSS)]
    }
    c1[[iBand]] = T
  }

  return(list(c1, s1))
}

removeborders = function(sin, siz) {
  sin = unpadimage(sin, c((siz + 1) / 2, (siz + 1) / 2, (siz - 1) / 2, (siz -1) / 2))
  sin = padarray(sin, c((siz + 1) / 2, (siz + 1) / 2), 0, 'pre')
  sout = padarray(sin, c((siz - 1) / 2, (siz - 1) / 2), 0, 'post')
}
