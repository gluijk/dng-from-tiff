# Building DNG RAW files from Bayer TIFF data
# Exercise 3: median stacking
# www.overfitting.net
# https://www.overfitting.net/2021/05/apilado-por-mediana-para-eliminar.html

library(tiff)

# Improve the performance of the R median function with this C++ code:
# https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative

library(Rcpp)
library(microbenchmark)

cppFunction('
            double cpp_med2(Rcpp::NumericVector xx) {
            Rcpp::NumericVector x = Rcpp::clone(xx);
            std::size_t n = x.size() / 2;
            std::nth_element(x.begin(), x.begin() + n, x.end());
            
            if (x.size() % 2) return x[n]; 
            return (*std::max_element(x.begin(), x.begin() + n) + x[n]) / 2.;
            }
            ')

# This version is faster but modifies the input vector
cppFunction('
            double cpp_med3(Rcpp::NumericVector x) {
            std::size_t n = x.size() / 2;
            std::nth_element(x.begin(), x.begin() + n, x.end());
            
            if (x.size() % 2) return x[n]; 
            return (*std::max_element(x.begin(), x.begin() + n) + x[n]) / 2.;
            }
            ')

# Check equality and performance
set.seed(123)
x=rnorm(1e6)
all.equal(median(x), cpp_med2(x), cpp_med3(x))  # equality
microbenchmark::microbenchmark(median(x), cpp_med2(x), cpp_med3(x), times=200L)  # perform


# PARAMETERS
N=5  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
img=array(0, c(nrow(img), ncol(img), N))
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"), native=FALSE, convert=FALSE)
}

# MEDIAN AVERAGING 
# median: Time difference of 14.6095 mins
# imag=apply(img, c(1,2), median)  # c(1,2) means 1st and 2nd dimensions

# cpp_med2: Time difference of 1.206512 mins (~12 times faster)
imag=apply(img, c(1,2), cpp_med2)  # c(1,2) means 1st and 2nd dimensions

# cpp_med3: Time difference of 58.98304 secs (a bit faster than cpp_med2)
# imag=apply(img, c(1,2), cpp_med3)  # c(1,2) means 1st and 2nd dimensions


# BUILD OUTPUT DNG
if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

# Fusion map and RAW data files contributions (cpp_med3 cannot be used)
mapafusion=imag*0
for (i in 1:N) {
    indices=which(imag==img[,,i])
    mapafusion[indices]=i
}
writeTIFF((mapafusion-1)/(N-1), "mapafusion.tif", # grayscale fusion map
          bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
          round(length(which(mapafusion==i))/length(mapafusion)*100,2),"%"))
