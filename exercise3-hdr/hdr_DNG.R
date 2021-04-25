# Building DNG RAW files from Bayer TIFF data
# Exercise 3: HDR
# www.overfitting.net
# https://www.overfitting.net/2021/04/generando-un-raw-en-formato-dng-partir.html

library(tiff)


# PARAMETERS
N=3  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename

# Linear valid exposure range
# NOTE: log2(MAX/MIN) must be >= bracketing EV intervals
MIN=2^(-5)  # from -5EV... (acceptable noise)
MAX=2^(-1/2)  # ...up to -1/2EV (avoid non-linearity)

# Output gamma
# NOTE: only gamma=1 produces correct colours but could lead to posterization
gamma=1


# READ RAW DATA

# RAW files must be named: raw1.dng, raw2.dng,... from lower to higher exposure
# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=list()
txt=list()
for (i in 1:N) {
    img[[i]]=readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F)
    txt[[i]]=paste0(NAME, i, "vs", NAME, i+1)
}


# RELATIVE EXPOSURE CALCULATIONS

# Calculate relative exposure
indices=list()
exprel=list()
f=array(-1, N-1)
for (i in 1:(N-1)) {
    indices[[i]]=which(img[[i]]>=MIN & img[[i]]<=MAX &
                       img[[i+1]]>=MIN & img[[i+1]]<=MAX)
    exprel[[i]]=img[[i+1]][indices[[i]]]/img[[i]][indices[[i]]]
    f[i]=median(exprel[[i]])  # linear exposure correction factor
}
print("Relative exposures vs lowest exposure shot (EV):")
print(round(log(cumprod(f),2),2))

# Relative exposure histograms
png("relexposure_histogram.png", width=640, height=320*(N-1))
par(mfrow=c(N-1,1))
for (i in 1:(N-1)) {
    flog=log(f[i],2)
    rflog=round(flog)
    exprelog=log(exprel[[i]],2)
    hist(exprelog[exprelog>=rflog-1/3 & exprelog<=rflog+1/3],
         main=paste0('Relative exposure histogram (', txt[[i]], ')'),
         xlab=paste0('EV (calculated: ', round(flog,2), 'EV)'),
         breaks=seq(rflog-1/3, rflog+1/3, length.out=800)
    )
    abline(v=flog, col='red')  # calculated relative exposure
    abline(v=rflog, col='gray', lty='dotted')  # closest int EV mark
}
dev.off() 

# Relative exposure calculation map
solape=array(-1, N-1)
for (i in 1:(N-1)) {
    mapacalc=img[[i]]*0  # 0=pixel did not participate in the calculation
    mapacalc[indices[[i]]]=1  # 1=pixel participated in the calculation
    writeTIFF(mapacalc, paste0("mapacalc_", txt[[i]], ".tif"),
              bits.per.sample=8, compression="LZW")
    solape[i]=length(indices[[i]])/length(img[[i]])  # % of data participating
}
print("Data participating in relative exposure calculation (%):")
print(round(solape*100,2))


# BUILD HDR COMPOSITE
hdr=img[[1]]  # start with lowest exposure data
mapafusion=hdr*0+1
for (i in 2:N) {
    indices=which(img[[i]]<=MAX)  # non-clipped highest exposure
    hdr[indices]=img[[i]][indices]/cumprod(f)[i-1]  # overwrite+exp correction
    mapafusion[indices]=i
}

if (max(hdr)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(hdr),2),2), "EV"))
writeTIFF((hdr/max(hdr))^(1/gamma), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

# Fusion map and RAW data files contributions
writeTIFF(1-(mapafusion-1)/(N-1), "mapafusion.tif", # grayscale fusion map
          bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
            round(length(which(mapafusion==i))/length(mapafusion)*100,2),"%"))



# Bit decimation (Optional)
# 10, 12, 14 bits versions -> 0..1023, 0..4095, 0..16383 levels
for (bits in seq(10,14,2)) {
    hdrdec=round(hdr/max(hdr)*(2^bits-1))  # round data into 2^bits int values
    writeTIFF((hdrdec/max(hdrdec))^(1/gamma),
              paste0(OUTNAME,"_",bits,"bits.tif"),
              bits.per.sample=16, compression="none")
}
