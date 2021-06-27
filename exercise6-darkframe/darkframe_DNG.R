# Building DNG RAW files from Bayer TIFF data
# Exercise 6: darkframe subtraction
# www.overfitting.net
# https://www.overfitting.net/2021/05/apilado-por-mediana-para-eliminar.html

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
img=readTIFF(paste0(NAME, 1, ".tiff"), native=F, convert=F)
img=array(0, c(nrow(img), ncol(img), N))
# Now images are read in integer mode
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F, as.is=TRUE)
}

# MEAN AVERAGING 
# imag=apply(img, c(1,2), mean)  # c(1,2) means 1st and 2nd dimensions

# MEDIAN AVERAGING 
imag=apply(img, c(1,2), cpp_med2)  # c(1,2) means 1st and 2nd dimensions

hist(imag, breaks=1200, ylim=c(0,30000),
     main='darkframe', xlab='RAW level')
abline(v=BLACK, col='red', lty='dotted')

hist(imag[imag<=2000], breaks=1200, ylim=c(0,15000),
     main='darkframe', xlab='RAW level')
abline(v=BLACK, col='red', lty='dotted')


# DARKFRAME SUBTRACTION
img=readTIFF(paste0(NAME, 0, ".tiff"), native=F, convert=F, as.is=TRUE)
img=img-imag  # darkframe subtraction (it cancels sensor black level at once)
img[img<0]=0

writeTIFF(img/max(img), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

imag2=imag-BLACK
imag2[imag2<0]=0
writeTIFF((imag2/max(imag2))^(1/2.2), paste0("darkframe.tif"),
          bits.per.sample=16, compression="none")
