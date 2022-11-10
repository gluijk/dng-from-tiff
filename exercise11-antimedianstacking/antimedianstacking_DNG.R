# Building DNG RAW files from Bayer TIFF data
# Exercise 2: mean stacking
# www.overfitting.net
# https://www.overfitting.net/2021/05/apilado-por-media-simulando-iso-ultra.html

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

antimedian = function(x) {
    desv = abs(x - cpp_med2(x))  # desv = abs(x-median(x))
    return(x[which(desv == max(desv))][1])  # [1] to avoid creating lists
}


# PARAMETERS
N=11  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename
BLACK=512  # sensor black level (Sony A7 II)
SAT=16376  # sensor sat level (Sony A7 II)


# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
img=array(0, c(nrow(img), ncol(img), N))
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"),
                      native=FALSE, convert=FALSE, as.is=TRUE)
}


# SEQUENCE AVERAGING
# c(1,2) means 1st and 2nd dimensions
# imag=apply(img, c(1,2), mean)  # mean
# imag=apply(img, c(1,2), cpp_med2)  # median (optimised)
imag=apply(img, c(1,2), antimedian)  # antimedian

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


# BUILD OUTPUT DNG

# Normalize RAW data
imag=imag-BLACK
imag[imag<0]=0
imag=imag/(SAT-BLACK)
hist(imag, breaks=1024)

if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
