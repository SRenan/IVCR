library(data.table)
library(ggplot2)

# FUNCTIONS

readfiles <- function(fileobj){
  paths <- fileobj$datapath
  samplenames <- fileobj$name
  lfr <- lapply(paths, fread)
  for(i in seq_along(samplenames)){
    lfr[[i]][, sample := samplenames[i]]
    lfr[[i]][, x := 1:2000]
    lfr[[i]][, newx := seq(-100, 100, length.out = 500)]
  }
  seldata <- rbindlist(lfr)
  setnames(seldata, "V1", "y")
  return(seldata)
}
makeTracePlot <- function(dat){
  p <- ggplot(dat, aes(x = x, y = y)) + geom_line(aes(color = sample))
  p <- p + xlab("mV") + ylab("pA/pF")
  return(p)
}
find0 <- function(dat){
  #dups <- icepts[duplicated(Re(icepts))]
  #real0 <- icepts[!Re(dups) == Re(icepts)] #We are losing a very small complex number 
  dat[which.min(abs(fitted)), newx]
  real0 <- icepts[which.min(abs(Im(icepts)))]
  real0 <- Re(real0)
  return(real0)
}
makeIVPlot <- function(dat){
  p <- ggplot(dat, aes(x = newx, y = newy)) + geom_line(size = 1)
  p <- p + geom_line(aes(x = newx, y = fitted), size = 1) + geom_vline(aes(xintercept = rev_p), color = "red")
  sample <- unique(dat$sample)
  p <- p + xlab("mV") + ylab("pA/pF") + ggtitle(sample)
  return(p)
}

# #############################
# 
# 
# quadm <- function(a, b, c){
#   num <- -b - sqrt(b^2 - 4*a*c)
#   return(num/(2*a))
# }
# 
# # ylabel: pA/pF
# # xlabel: mV
# 
# # How do they get a 0 on x axis?
# # rescale the 1001 ; 1500 to -100 ; 100
# scaledx <- seq(-100, 100, length.out = 500)
# # Maybe an hline for y=0
# 
# # divide y by the capacitance
# # 
# # Shape is due to voltage variation and chemical gradient between the inside and outside of the cell
# # outside the cell: High Ca conc, smaller current (pA)
# capacitance <- c(2.45e-11, 2.3e-11,
#                  1.8e-11, 1.57e-11,
#                  1.57e-11, 1.49e-11,
#                  1.49e-11, 2.584e-11, 1.03e-11)
# 
# setwd("~/workspace/IVCR/081117/")
 lf <- list.files(patter = ".asc")
 lf <- grep("dvf", lf, invert = T, value = T)
 lfr <- lapply(lf, fread)
 for(i in 1:length(lfr)){
   lfr[[i]][, sample := lf[i]]
   #lfr[[i]][, capacitance := capacitance[i]]
   lfr[[i]][, x := 1:2000]
   lfr[[i]] <- lfr[[i]][x >= 1000 & x < 1500]
   lfr[[i]][ , newx := seq(-100, 100, length.out = 500)]
 }
 dat <- rbindlist(lfr)
 setnames(dat, "V1", "y")
 
 for(samp in lf){
   sn <- gsub("\\..*", "", samp)
   png(filename = paste0("/media/srenan/1400-EC95/101117/plots/", sn, "_revp.png"))
   process_traces(dat[ sample == samp], plot = T)
   dev.off()
 }
# 
# p <- ggplot(dat, aes(x = x, y = y)) + geom_line(aes(color = sample))
# p
# 
# d11 <- dat[sample == "TraceBuffer_12.asc" & x > 1000 & x < 1500]
# # p1 <- lm(y ~ poly(x, 3), data = d11)
# # sp1 <- summary(p1)
# # c1 <- sp1$coefficients[, 1]
# p2 <- lm(y ~ x + I(x^2) + I(x^3), data = d11)
# sp2 <- summary(p2)
# c2 <- sp2$coefficients[, 1]
# d11[, fit := c1[1] + x*c1[4] + c1[3]*x^2 + c1[2]*x^3]
# d11[, fit := c1[1] + x*c1[2] + c1[3]*x^2 + c1[4]*x^3]
# ## 
# d11[, fit := c2[1] + x*c2[2] + c2[3]*x^2 + c2[4]*x^3]
# p <- ggplot(d11, aes(x = x, y = y)) + geom_line(aes(color = sample))+
#   geom_line(aes(x=x, y  = fit))
# p
# 
# icepts <- polyroot(sp2$coefficients[1:4,1])
# 
# p + geom_vline(xintercept = as.numeric(icepts))
# 
# 
# # Fit linear model
# # Generate fitted curves
# # solve roots for 0
# 
# fit_trace <- function(data, plot = T){
#   subdat <- data[sample == tracename & x > 1000 & x < 1500]
#   fitobj <- lm(y ~ x + I(x^2) + I(x^3), data = subdat)
#   sry <- summary(fitobj)
#   coefs <- sry$coefficients[,1]
#   subdat[, fitted := coefs[1] + x*coefs[2] + x^2*coefs[3] + x^3*coefs[4]] #Fitted values for graph
#   icepts <- polyroot(coefs[1:4])
#   rev_p <- find0(icepts)
#   print(paste("Reverse potential at", rev_p))
#   if(plot){
#     p <- ggplot(subdat, aes(x = x, y = y)) + geom_line(aes(color = sample)) +
#       geom_line(aes(x=x, y  = fitted))
#     p <- p + geom_vline(xintercept = rev_p)
#     print(p)
#   }
#   return(subdat) #temp
# }
# 
 find0 <- function(icepts){
   #dups <- icepts[duplicated(Re(icepts))]
   #real0 <- icepts[!Re(dups) == Re(icepts)] #We are losing a very small complex number 
   real0 <- icepts[which.min(abs(Im(icepts)))]
   real0 <- Re(real0)
   return(real0)
 }
 
# 
# ################################################################################
# # FUNCTIONS
# dat <- copy(dat2)
# 
 process_traces <- function(data, plot = T){
   dat <- copy(data)
   samples <- unique(dat$sample)
   
   #for(sample_i in seq_along(samples)){
   for(sample_i in samples){
     #fitobj <- lm(y ~ x + I(x^2) + I(x^3), data = dat[x > 1000 & x < 1500 & sample == sample_i])
     fitobj <- lm(y ~ newx + I(newx^2) + I(newx^3), data = dat[sample == sample_i])
     sry <- summary(fitobj)
     coefs <- sry$coefficients[,1]
     dat[sample == sample_i, i := coefs[1]]
     dat[sample == sample_i, a := coefs[2]]
     dat[sample == sample_i, b := coefs[3]]
     dat[sample == sample_i, c := coefs[4]]
     dat[sample == sample_i, fitted := i + newx*a + newx^2*b + newx^3*c]
     icepts <- polyroot(coefs)
     minyx <- dat[which.min(abs(fitted)), newx]
     rev_p <- Re(icepts[which.min(abs(Re(minyx - icepts)))])
     #dat[sample == sample_i, rev_p := find0(icepts)]
     dat[sample == sample_i, rev_p := rev_p]
   }
   
   
   if(plot){
     # p <- ggplot(dat[x > 1100 & x < 1400], aes(x = newx, y = y)) + geom_line(color = "red", size = 2)
     # p <- p + geom_line(aes(x = newx, y = fitted)) + geom_vline(aes(xintercept = rev_p))
     # p <- p + facet_wrap(~sample) 
     p <- ggplot(dat, aes(x = newx, y = y)) + geom_line(color="red", size = 1.5) + 
       geom_line(aes(x = newx, y = fitted), size = 1.5) +#, alpha = 0.7) + 
       theme(panel.grid = element_blank()) + geom_vline(xintercept = 0) + 
       geom_hline(yintercept = 0)  + xlab("mV") + ylab("pA/pF")
     p <-  p + geom_text(aes(x= min(newx), y = max(y),  label = paste("RP =", round(rev_p,2))), hjust = 0, vjust = 0)
     sample <- gsub("\\..*$", "", unique(dat$sample))
     p <- p + ggtitle(sample) + theme(plot.title = element_text(hjust = 0.5))
     #p <- p + facet_wrap(~sample) 
     print(p)
   }
   
   ret <- dat
   return(ret)
 }
 

 
