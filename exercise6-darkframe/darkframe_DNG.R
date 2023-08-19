# Building DNG RAW files from Bayer TIFF data
# Exercise 6: darkframe subtraction
# www.overfitting.net
# https://www.overfitting.net/2021/06/reduccion-de-ruido-por-sustraccion-de.html

library(tiff)

# Improve the performance of the R median function with this C++ code:
# https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative

library(Rcpp)

cppFunction('
            double cpp_med2(Rcpp::NumericVector xx) {
            Rcpp::NumericVector x = Rcpp::clone(xx);
            std::size_t n = x.size() / 2;
            std::nth_element(x.begin(), x.begin() + n, x.end());
            
            if (x.size() % 2) return x[n]; 
            return (*std::max_element(x.begin(), x.begin() + n) + x[n]) / 2.;
            }
            ')

# PARAMETERS
N=10  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename
BLACK=512  # sensor black level (Sony A7 II)


# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
img=array(0, c(nrow(img), ncol(img), N))
# Now images are read in integer mode
# "raw1.tiff" ... "raw10.tiff" are the 10 darkframes to average
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"),
                      native=FALSE, convert=FALSE, as.is=TRUE)
}

# MEAN AVERAGING 
# dark=apply(img, c(1,2), mean)  # c(1,2) means 1st and 2nd dimensions

# MEDIAN AVERAGING 
dark=apply(img, c(1,2), cpp_med2)  # c(1,2) means 1st and 2nd dimensions

hist(dark, breaks=1200, ylim=c(0,30000),
     main='darkframe', xlab='RAW level')
abline(v=BLACK, col='red', lty='dotted')

hist(dark[dark<=2000], breaks=1200, ylim=c(0,15000),
     main='darkframe', xlab='RAW level')
abline(v=BLACK, col='red', lty='dotted')


# DARKFRAME SUBTRACTION
# "raw0.tiff" is the original scene file
img=readTIFF(paste0(NAME, 0, ".tiff"), native=FALSE, convert=FALSE, as.is=TRUE)
img=img-dark  # darkframe subtraction (it cancels sensor black level at once)
img[img<0]=0
writeTIFF(img/max(img), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

dark=dark-BLACK
dark[dark<0]=0
writeTIFF((dark/max(dark))^(1/2.2), "darkframe.tif", bits.per.sample=16,
          compression="none")
