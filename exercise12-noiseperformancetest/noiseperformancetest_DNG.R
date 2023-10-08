# Building DNG RAW files from Bayer TIFF data
# Exercise 12: camera sensor noise performance test
# www.overfitting.net
# https://www.overfitting.net/2021/11/diezmado-de-bits-en-raw.html

library(tiff)


# PARAMETERS
OUTNAME="bayer"  # output RAW composite filename

# RAW extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# dcraw -v -d -r 1 1 1 1 -t 0 -k 512 -4 -T *.dng
# doesn't work properly with this DNG clipping to 0 a lot of R and G pixels

# Sony A7 II black and sat points (for all ISOs)
BLACK=512
SAT=16383

# Pixel dimensions
DIMX=666  # crop dimensions
DIMY=440
OffsetX=2955  # exact position of top left corner of crop
OffsetY=1171
DX=12  # Border of DCRAW RAW data (12px both in X and Y)
DY=12+40  # extra 40px to allow upper text

# S/N calculations
NISOS=9
S=matrix(nrow=NISOS, ncol=NISOS)
N=S


NUM=5297-1  # first RAW file (45 RAW files: 5297..5341)
k=1
imgout=readTIFF(paste0('DSC0', NUM+k, ".tiff"))*0
for (i in 1:NISOS) {  # 9 ISO values
    for (j in 1:i) {  # underexposure level
        NAME=paste0("DSC0", NUM+k)
        img=readTIFF(paste0(NAME, ".tiff"), as.is=TRUE)
        crop=img[OffsetY:(OffsetY+DIMY-1), OffsetX:(OffsetX+DIMX-1)]

        print(paste0(k, ": '", NAME,
                     "' (min=", min(img), ", max=", max(img), ")"))
        
        # Linearize and normalize
        crop=crop-BLACK  # linearize
        crop[crop<0]=0  # clip to 0 negative values
        crop=crop/(SAT-BLACK)  # normalize to 0..1 range
        crop=crop*2^(j-1)  # match RAW levels (RAW exposure)
        crop[crop>1]=1  # clip to 1 saturated values
        
        # Save crop to plot its RAW histogram
        writeTIFF(crop, paste0(NAME, "_crop.tif"),
                  bits.per.sample=16, compression="LZW")
        
        # crop[45:(45+60-1)+254, 507:(507+60-1)-166]=1
        imgout[(1+(j-1)*DIMY+DY):(j*DIMY+DY),
               (1+(i-1)*DIMX+DX):(i*DIMX+DX)]=crop

        # SNR calculations over 60x60 pixels Bayer patch (1800 samples)
        patch=crop[45:(45+60-1), 507:(507+60-1)]  # green patch
        # patch=crop[45:(45+60-1)+254, 507:(507+60-1)-166]  # black patch
        indices=which(( row(patch)%%2 & !col(patch)%%2) |
                      (!row(patch)%%2 &  col(patch)%%2))  # G1 and G2 in RG/GB
        S[j,i]=mean(patch[indices])  # S=mean
        N[j,i]=var(patch[indices])^0.5  # N=stdev
        hist(patch[indices], breaks=500, xlim=c(0,1), xlab='',
             main=paste0('ISO', 2^(i-1)*100, ' + ', j-1, 'EV'))
        
        k=k+1
    }
}

# Write labels and text
labels=readTIFF(paste0("labels.tif"))
indices=which(labels>imgout)
imgout[indices]=labels[indices]

writeTIFF(imgout, paste0(OUTNAME,".tif"),
          bits.per.sample=16, compression="none")


# SNR cuves in dB
SNR=S/N

# Plot ISO performance (first row)
plot(20*log10(SNR[1,]), ylim=c(20,45), type='o', col='red',
     main='SNR for each ISO',
     xlab='ISO', ylab='SNR (dB)', xaxt="n")
axis(1, at=1:9, cex.axis=0.8, labels=2^(0:8)*100)

# Plot ISO invariance (columns))
plot(20*log10(SNR[,NISOS]), ylim=c(15,45), type='o', col='red',
     main='SNR for each ISO invariant series',
     xlab='Exposure vs ETTR', ylab='SNR (dB)', xaxt="n")
for (i in 1:(NISOS-1)) lines(20*log10(SNR[,i]), type='o', col='red')
axis(1, at=1:9, cex.axis=0.8, labels=c('ETTR', paste0(-1:-8,'EV')))





