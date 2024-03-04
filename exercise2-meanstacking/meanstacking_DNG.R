# Building DNG RAW files from Bayer TIFF data
# Exercise 2: mean stacking
# www.overfitting.net
# https://www.overfitting.net/2021/05/apilado-por-media-simulando-iso-ultra.html

library(tiff)


# PARAMETERS
N=16  # number of RAW files to merge
NAME="_DSC38"  # input RAW filenames
INIT=18  # first file
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=0
for (i in 1:N) {
    name=paste0(NAME, i+INIT-1, ".tiff")
    img=img+readTIFF(name, native=FALSE, convert=FALSE)
    print(paste0(i, " / ", N, " added ", name))
}

# MEAN AVERAGING
img=img/N

# BUILD OUTPUT DNG
if (max(img)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(img),2),2), "EV"))
writeTIFF(img/max(img), paste0(OUTNAME, ".tif"), bits.per.sample=16,
          compression="none")
