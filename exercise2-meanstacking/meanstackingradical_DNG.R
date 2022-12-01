# Building DNG RAW files from Bayer TIFF data
# Exercise 2: mean stacking
# www.overfitting.net
# https://www.overfitting.net/2021/05/apilado-por-media-simulando-iso-ultra.html

library(tiff)


# PARAMETERS
N=265  # number of RAW files to merge
NAME="DSC04"  # input RAW filenames
INIT=672
OUTNAME="bayer"  # output RAW composite filename
BLACK=512  # sensor black level (Sony A7 II)
SAT=16383  # sensor sat level (Sony A7 II)


# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
img=0
for (i in INIT:(INIT+N-1)) {
    name=paste0(NAME, i, ".tiff")
    img=img+readTIFF(name, native=FALSE, convert=FALSE, as.is=TRUE)
    print(paste0(i-INIT+1, " / ", N, " added ", name))
}

# MEAN AVERAGING
img=img/N  # averaging


# BUILD OUTPUT DNG

# Normalize RAW data
img=img-BLACK  # subtract black level
img[img<0]=0  # clip negative values
img=img/(SAT-BLACK)  # normalize to 0..1
hist(img, breaks=1024)

if (max(img)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(img),2),2), "EV"))
writeTIFF(img/max(img), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
