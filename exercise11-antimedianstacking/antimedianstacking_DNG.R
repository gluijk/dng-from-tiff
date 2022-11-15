# Building DNG RAW files from Bayer TIFF data
# Exercise 11: antimedian stacking
# www.overfitting.net
# https://www.overfitting.net/2022/11/apilado-por-antimediana-para-replicar.html

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
    absdev = abs(x - cpp_med2(x))  # absdev = abs(x-median(x))
    x[which(absdev == max(absdev))][1]  # [1] to avoid creating lists
}

statmode = function(x) {  # statistical mode
    ux=unique(x)
    ux[which.max(tabulate(match(x, ux)))]
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

# Fusion map and RAW data files contributions
mapafusion=imag*0
for (i in 1:N) {
    indices=which(imag==img[,,i])
    mapafusion[indices]=i
}
writeTIFF((mapafusion-1)/(N-1), "mapafusion.tif", # grayscale fusion map
    bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
    round(length(which(mapafusion==i))/length(mapafusion)*100,2),"%"))


# IMPROVE FUSION MAP TO REDUCE MOVING/BAYER ARTIFACTS
DIMX=nrow(mapafusion)
DIMY=ncol(mapafusion)
mapa=array(0, c(DIMX, DIMY, 9))

# Calculate statistical mode over a 3x3 pixels matrix in fusion map
mapa[2:(DIMX-1), 2:(DIMY-1), 1] = mapafusion[(2-1):(DIMX-1-1), (2-1):(DIMY-1-1)]  # -1, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 2] = mapafusion[(2):(DIMX-1), (2-1):(DIMY-1-1)]  #  0, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 3] = mapafusion[(2+1):(DIMX-1+1), (2-1):(DIMY-1-1)]  # +1, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 4] = mapafusion[(2-1):(DIMX-1-1), (2):(DIMY-1)]  # -1,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 5] = mapafusion[(2):(DIMX-1), (2):(DIMY-1)]  #  0,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 6] = mapafusion[(2+1):(DIMX-1+1), (2):(DIMY-1)]  # +1,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 7] = mapafusion[(2-1):(DIMX-1-1), (2+1):(DIMY-1+1)]  # -1, +1
mapa[2:(DIMX-1), 2:(DIMY-1), 8] = mapafusion[(2):(DIMX-1), (2+1):(DIMY-1+1)]  #  0, +1
mapa[2:(DIMX-1), 2:(DIMY-1), 9] = mapafusion[(2+1):(DIMX-1+1), (2+1):(DIMY-1+1)]  # +1, +1

mapafusion2=apply(mapa, c(1,2), statmode)
writeTIFF((mapafusion2-1)/(N-1), "mapafusion2.tif", # grayscale fusion map
          bits.per.sample=8, compression="LZW")

# Use new fusion map
imag2=imag*0
for (i in 1:N) {
    indices=which(mapafusion2==i)
    imag2[indices]=img[,,i][indices]
}


# BUILD OUTPUT DNG
imag=imag2  # run this to use improved fusion map

# Normalize RAW data
imag=imag-BLACK  # subtract black level
imag[imag<0]=0  # clip negative values
imag=imag/(SAT-BLACK)  # normalize to 0..1
hist(imag, breaks=1024)

if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
