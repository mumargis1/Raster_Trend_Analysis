# Raster_Trend_Analysis
Detailed description for the trend analysis of Raster dataset 


## Technologies Used:
1. GRASS GIS
2. BASH processing
3. MICROSOFT EXCEL
4. R Studio


## Complete Steps:
### 1. Import data in the GRASS GIS.
`for i in *.tif; do r.in.gdal input=$i output=$i;done`

### 2. Create list of all dataset imported in GRASS GIS using:
`for i in `g.list type=rast pat=*`; echo $i; done > filelist.txt`

### 3. Create copy of created file contained name of all imported rasters
`cp filelist.txt filelist_lagged.txt`

### 4. Go into the lagged.txt and remove the first file name and press backspace once.
`cat filelist.txt | wc -l` # it will print number of lines in the text file

###	5. Now create another files for  'r.regression.series' regression analysis (It will help in prewhitening; identifing raster cells with greated than 0.2 significant value
`for i in `g.list type=rast pat=* sep=comma`; echo $i; done > filelistx.txt`
`for i in `g.list type=rast pat=* sep=comma`; echo $i; done > filelisty.txt`
###	remove the last one from filelistx.txt
###	remove the first one from filelisty.txt

### 6. Apply regression using command 
`r.regression.series xseries=`sed -n ${i}p filelistx.txt` yseries==`sed -n ${i}p filelisty.txt` method=corcoef output=r1`

###	It will help to do the following `x2-r1x1 … xn1-r1xnx` (prewhitning)
### 7. Following are the method to do prewhiten the all raster cells; althought we need to prewhiten those time series that have higher values than significant value
`for i in `seq “mention your number of files here from 1 to as many as was printed by wc -l in line 6”` ; do x1=`sed -n ${i}p filelist.txt`; x2=`sed -n ${i}p filelist_lagged.txt`; r.mapcalc “x1.prewhitened=x2-corcoef*x1”; done`

###	7.1 Convert the output .tiff to ascii
`gdal_translate -of AAIGrid aot.tif aot.asc`

### 7.2 Copy .asc text data in excel and apply prewhitening. Select scpecific row and coulmn you need to select and prewhitened it in Excel
`for i in *.asc; do echo awk 'FNR==(row number){print $(coulmn number)}' $i; done`
#### Replace the prewhitened values over normal values

### 8. Convert back the .asc file to .tif; It will give the Prewhitened raster files.
`gawk -i inplace -v repl="(file name)" 'FNR==(row number){getline $(coulmn number) < repl}1' *asc`

