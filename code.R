#' 
#' Read one or more asc files into a table
#' 
#' @export
#' 
read_asc <- function(files){
  len <- length(files)
  #fl <- vector("list", len)
  fl <- lapply(files, fread)
  for(i in seq_along(files)){
    fl[[i]][, filename := files[i]]
    fl[[i]][, x := 1:2000]
  }
  data <- rbindlist(fl)
  setnames(data, "V1", "y")
  return(data)
}

#'
#' Filter the data and rescale x
#' 
pre_process <- function(data){
  ssdata <- data[x >= 1000 & x < 1500]
  # data only contains Y (raw read from file)
  ssdata[, newx := seq(-100, 100, length.out = 500)]
  return(ssdata)
}

#' Fit 3rd degree polynomials
fit_poly <- function(data){
  dat <- copy(data)
  filenames <- unique(dat[, filename])
  for(sample_i in filenames){
    fitobj <- lm(y ~ newx + I(newx^2) + I(newx^3), data = dat[filename == sample_i])
    sry   <- summary(fitobj)
    coefs <- sry$coefficients[,1]
    dat[filename == sample_i, i := coefs[1]]
    dat[filename == sample_i, a := coefs[2]]
    dat[filename == sample_i, b := coefs[3]]
    dat[filename == sample_i, c := coefs[4]]
    dat[filename == sample_i, fitted := i + newx*a + newx^2*b + newx^3*c]
    icepts <- polyroot(coefs)
    minyx <- dat[filename == sample_i][which.min(abs(fitted)), newx]
    rev_p_real <- Re(icepts[which.min(abs(Re(minyx - icepts)))])
    dat[filename == sample_i, rev_p := rev_p_real]
  }
  return(dat)
}



