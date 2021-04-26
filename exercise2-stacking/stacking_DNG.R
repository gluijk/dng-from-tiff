# Building DNG RAW files from Bayer TIFF data
# Exercise 2: average stacking
# www.overfitting.net
# https://www.overfitting.net/2021/04/generando-un-raw-en-formato-dng-partir.html

library(tiff)


# PARAMETERS
N=16  # number of RAW files to merge
NAME="_DSC38"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=readTIFF(paste0(NAME, 18, ".tiff"), native=F, convert=F)
for (i in 19:(19+N-2)) {
    img=img+readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F)
}


# AVERAGING
img=img/N


# BUILD OUTPUT DNG
if (max(img)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(img),2),2), "EV"))
writeTIFF(img/max(img), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")



# Using median instead of mean

# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=readTIFF(paste0(NAME, 18, ".tiff"), native=F, convert=F)
img=array(0, c(nrow(img), ncol(img), N))
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i+17, ".tiff"), native=F, convert=F)
}

# Standard R median is VERY slow
# Improvement: https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative
imag=apply(img, c(1,2), median)  # c(1,2) means 1st and 2nd dimensions



# BUILD OUTPUT DNG
if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

