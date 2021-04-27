@echo off

echo.
echo ========================================================================
echo                         D N G     M A K E R                               
echo      (by Guillermo Luijk, based on Anton Wolf's DNG Stacker script)
echo.
echo   This script creates a valid Bayer DNG file from a Bayer TIFF image:
echo     Step 1: Use exiftool to convert a Bayer TIFF into a DNG
echo     Step 2: Use dng_validate to make the new DNG valid and compress it
echo ========================================================================
echo.

rem Any environment variable changes are lost at the end of the script
SETLOCAL EnableDelayedExpansion

rem Clean log and temp files from previous executions
if exist dngmaker.log del dngmaker.log
if exist temp.dng del temp.dng

rem Extract RAW data from DNG files
echo Extracting RAW data from DNG files using DCRAW...
echo.
dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng

rem Determine number of files and the first file
set numberOfFiles=0
set firstFile=
for %%i in (*.dng) do (
	set /a numberOfFiles+=1
	if "!firstFile!"=="" set firstFile=%%~ni
)
echo.
echo !numberOfFiles! DNG files found and extracted, !firstFile!.dng metadata will be used
echo Now it's time to process the resulting TIFF files and build bayer.tif
echo.
pause

if not exist bayer.tif (
	echo bayer.tif file doesn't exist >> dngmaker.log
	goto err
)

echo.
echo Creating DNG based on bayer.tif and the metadata from !firstFile!.dng using exiftool

copy bayer.tif temp.dng >NUL

exiftool -n^
 -IFD0:SubfileType#=0^
 -overwrite_original -tagsfromfile !firstFile!.dng^
 "-all:all>all:all"^
 -DNGVersion^
 -DNGBackwardVersion^
 -ColorMatrix1^
 -ColorMatrix2^
 "-IFD0:BlackLevelRepeatDim<SubIFD:BlackLevelRepeatDim"^
 "-IFD0:PhotometricInterpretation<SubIFD:PhotometricInterpretation"^
 "-IFD0:CalibrationIlluminant1<SubIFD:CalibrationIlluminant1"^
 "-IFD0:CalibrationIlluminant2<SubIFD:CalibrationIlluminant2"^
 -SamplesPerPixel^
 "-IFD0:CFARepeatPatternDim<SubIFD:CFARepeatPatternDim"^
 "-IFD0:CFAPattern2<SubIFD:CFAPattern2"^
 -AsShotNeutral^
 "-IFD0:ActiveArea<SubIFD:ActiveArea"^
 "-IFD0:DefaultScale<SubIFD:DefaultScale"^
 "-IFD0:DefaultCropOrigin<SubIFD:DefaultCropOrigin"^
 "-IFD0:DefaultCropSize<SubIFD:DefaultCropSize"^
 "-IFD0:OpcodeList1<SubIFD:OpcodeList1"^
 "-IFD0:OpcodeList2<SubIFD:OpcodeList2"^
 "-IFD0:OpcodeList3<SubIFD:OpcodeList3"^
 !exposureTag!^
 temp.dng >> dngmaker.log 2>>&1
if errorlevel 1 goto err

rem Replicate all colour-related metadata
exiftool -overwrite_original -tagsfromfile !firstFile!.dng^
 "-IFD0:AnalogBalance"^
 "-IFD0:ColorMatrix1" "-IFD0:ColorMatrix2"^
 "-IFD0:CameraCalibration1" "-IFD0:CameraCalibration2"^
 "-IFD0:AsShotNeutral" "-IFD0:BaselineExposure"^
 "-IFD0:CalibrationIlluminant1" "-IFD0:CalibrationIlluminant2"^
 "-IFD0:ForwardMatrix1" "-IFD0:ForwardMatrix2"^
 temp.dng >> dngmaker.log 2>>&1
if errorlevel 1 goto err

set resultDNG=!firstFile!-stack!numberOfFiles!
echo Writing composite DNG to !resultDNG!.dng using dng_validate
dng_validate.exe -dng !resultDNG! temp.dng >> dngmaker.log 2>>&1
if errorlevel 1 goto err

del temp.dng

echo Done
echo.
pause

GOTO:EOF

:err
echo ERROR! Please check dngmaker.log for details
pause
