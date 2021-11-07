# Building DNG RAW files from Bayer TIFF data
# Exercise 7: RAW decimation
# www.overfitting.net
# https://www.overfitting.net/2021/05/raw-hdr.html

library(tiff)


# PARAMETERS
N=1  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# -d -r 1 1 1 1 -t 0 -k 512 -4 -T *.dng doesn't work properly with this DNG
# clipping to 0 a lot of R and G pixels
img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)

# Linearize and normalize
BLACK=512/65535  # 512 obtained by visual inspection
img=img-BLACK  # linearize
img[img<0]=0  # clip to 0 negative values
img=img/max(img)  # normalize


# Bit decimation
for (bits in seq(8, 14)) {
    imgdec=round(img/max(img)*(2^bits-1))  # round data into 2^bits int values
    writeTIFF((imgdec/max(imgdec)),
              paste0(OUTNAME,"_",bits,"bits.tif"),
              bits.per.sample=16, compression="none")
}
