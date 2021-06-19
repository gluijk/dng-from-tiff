# Building DNG RAW files from Bayer TIFF data
# Exercise 5: RAW contact sheet
# www.overfitting.net
# https://www.overfitting.net/2021/05/creando-una-hoja-de-contactos-en-raw.html

library(tiff)


# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -4 -T *.dng
filenames=dir(pattern="*.tiff")
N=length(filenames)
DIEZMADO=ceiling(N^0.5)


# PARAMETERS
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA
imag=readTIFF(filenames[1], native=F, convert=F)*0
NROW=as.integer(nrow(imag)/DIEZMADO)
NCOL=as.integer(ncol(imag)/DIEZMADO)

# Always even dimensions (Bayer multiple)
if (NROW%%2) NROW=NROW-1L
if (NCOL%%2) NCOL=NCOL-1L


imagcrop=array(0,c(NROW*DIEZMADO,NCOL*DIEZMADO))
tmp=array(0,c(NROW,NCOL))  # NROW x NCOL temporary array

# Precalculated indexes
i=which(!(row(imagcrop)-1)%%(DIEZMADO*2) & !(col(imagcrop)-1)%%(DIEZMADO*2))
j=which(row(tmp)%%2 & col(tmp)%%2)

for (k in 1:N) {
    print(paste0("Processing file '", filenames[k], "' (", k, "/", N,
                 ") ..."), quote=FALSE)
    
    img=readTIFF(filenames[k], native=F, convert=F)  # as.is=TRUE
    img=img[1:nrow(imagcrop), 1:ncol(imagcrop)]
    
    # Basic Bayer subsampling
    # tmp[j]=img[i]                         # R
    # tmp[j+NROW]=img[i+NROW*DIEZMADO]      # G1
    # tmp[j+1]=img[i+1]                     # G2
    # tmp[j+NROW+1]=img[i+NROW*DIEZMADO+1]  # B
    
    # Improved spatial Bayer subsampling (reduces pixelation)
    if (DIEZMADO%%2) {  # odd decimation
        tmp[j]=img[i]                                                   # R
        tmp[j+NROW]=img[i+(NROW*DIEZMADO)*DIEZMADO]                     # G1
        tmp[j+1]=img[i+DIEZMADO]                                        # G2
        tmp[j+NROW+1]=img[i+(NROW*DIEZMADO)*DIEZMADO+DIEZMADO]          # B
    } else {  # even decimation
        tmp[j]=img[i]                                                   # R
        tmp[j+NROW]=img[i+(NROW*DIEZMADO)*(DIEZMADO-1)]                 # G1
        tmp[j+1]=img[i+DIEZMADO-1]                                      # G2
        tmp[j+NROW+1]=img[i+(NROW*DIEZMADO)*(DIEZMADO-1)+(DIEZMADO-1)]  # B
    }

    OFFSETROW=NROW*as.integer((k-1)/DIEZMADO)
    OFFSETCOL=NCOL*(k-1)%%DIEZMADO
    imagcrop[1:NROW+OFFSETROW,1:NCOL+OFFSETCOL]=tmp
}

imag[1:nrow(imagcrop), 1:ncol(imagcrop)]=imagcrop


# BUILD OUTPUT DNG
writeTIFF(imag, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

# Set DNG effective size to 6048x4024 (same as DCRAW output):
# exiftool -overwrite_original -DefaultOrigin="0 0" rawcontactsheet.dng
# exiftool -overwrite_original -DefaultCropSize="6048 4024" rawcontactsheet.dng
