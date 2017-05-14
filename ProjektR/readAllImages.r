readAllImages <- function(train_pos, train_neg, test_pos, test_neg) {
  # Reads all training and testing images into a cell of length 4
  #   cI[[1]] <- list with train positive images or cI$train_pos
  #   CI[[2]] <- list with train negative images or cI$train_neg
  #   CI[[3]] <- list with test positive images or cI$test_pos
  #   CI[[4]] <- list with test positive images or cI$test_neg
  
  library("png")
  
  paths = c(train_pos, train_neg, test_pos, test_neg)
  cI = list()
  for (j in 1:length(paths)) {
    images_names = list.files(paths[j], pattern = "png", full.names = TRUE)
    images = list()
    if (length(images_names) >= 0) {
         for (i in 1:length(images_names)) {
          images = c(images, list(readPNG(images_names[i])))
        }
    }
    cI = c(cI, list(images));
  }
  names(cI) = c("train_pos", "train_neg", "test_pos", "test_neg")
  return(cI);
}
