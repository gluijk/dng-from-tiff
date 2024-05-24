# Building DNG RAW files from Bayer TIFF data
# Exercise 13: min light
# www.overfitting.net
# https://www.overfitting.net/2024/05/apilado-por-minimo-para-preservar.html

library(tiff)


# PARAMETERS
N=5  # number of RAW files to merge
NAME="DSC0551"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# dcraw -v -d -r 1 1 1 1 -t 0 -k 512 -S 16383 -4 -T *.dng
# doesn't seem to work properly with these RAW files

BLACK=512/65535  # 512 obtained by visual inspection

img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
img=array(0, c(nrow(img), ncol(img), N))
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"), native=FALSE, convert=FALSE)
}

# Linearize and normalize
img=img-BLACK  # linearize
img[img<0]=0  # clip to 0 negative values
img=img/max(img)  # normalize 0..1

# MIN
imag=apply(img, c(1,2), min)  # c(1,2) means 1st and 2nd dimensions
# imag=apply(img, c(1,2), mean)  # c(1,2) means 1st and 2nd dimensions
# imag=apply(img, c(1,2), max)  # c(1,2) means 1st and 2nd dimensions


# BUILD OUTPUT DNG
if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
