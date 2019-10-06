#====================== wczytuje biblioteki =====================================

# install.packages("dplyr")
# install.packages("rgeos")
# install.packages("viridis")
# install.packages('rgdal')
# install.packages('raster')
# install.packages('sf') 
# install.packages('tidyverse')
# install.packages('RStoolbox')
# require("rgeos")
library(rgdal)
library(RStoolbox)
library(sp)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(viridis)
library(rgeos)
library(RColorBrewer) # zaĹ‚Ä…czam biblioteke z kolorami
library(raster)

vxcvxcvxvxc
#=============================================================================
#============= Wprowadzam dane i przycinam raster ============================

# 2018.czerwiec.07
setwd("D:/Publikacja/2018.czerwiec.07") #uwstawiam folder roboczy w ktĂłrym ma mi siÄ™ wszystko zapisywaÄ‡

Granice_administracyjne <- readOGR("D:/Publikacja/Granice_adm_POZ/Poznan_granica.shp") #wczytuje shp'a dla Leborka
band_10 <- raster("LC08_L1TP_191023_20180607_20180615_01_T1_B10.TIF") #wczytuje raster

meta_dane <- stackMeta("LC08_L1TP_191023_20180607_20180615_01_T1_MTL.txt") #wczytuje metadane 

Extent <- spTransform(Granice_administracyjne, CRS(proj4string(meta_dane))) #nadaje te same wspĂłĹ‚rzÄ™dne SHP'wi co ma raster

bufor <- gBuffer(Extent, width = 5000, byid = T) # zwiekszam powierzchnie do przyciecia. gBuffer pozwala na powiekszenie pola przyciecia 
# o width gdzie width = 1000 to tj. 1km

band_10_crop <- crop(x = band_10, y = bufor)# przycinam kanal 10 do kwadratu z bufera

band_10_crop <- mask(band_10_crop, bufor)  # przycinam raster do granicy o zwiekszony bufor

plot(band_10_crop, col=grey(1:100/100))


#====================================================================================
#=================================LICZE LST==========================================

RADIANCE_MULT_BAND_10 <- 3.3420E-04 
RADIANCE_ADD_BAND_10 <- 0.10000

#Licze TOA z DN:
toa_band10 <- calc(band_10_crop, fun=function(x){RADIANCE_MULT_BAND_10 * x + RADIANCE_ADD_BAND_10})

K1_CONSTANT_BAND_10 <- 774.8853
K2_CONSTANT_BAND_10 <- 1321.0789

#Licze LST w Kelwinach dla kanalu 10 i 11
temp10_kelvin <- calc(toa_band10, fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x + 1)})

#Zamieniam Kelwiny na Celcjusze dla kanalu 10 i 11
temp10_celsius <- calc(temp10_kelvin, fun=function(x){x - 273.15})

#Zapisuje obliczone LST w nowych plikach
writeRaster(temp10_celsius, "tempC_Czerwiec_POZ.tif", overwrite = TRUE) # overwrite nadpisuje zdjecia gdy juĹĽ siÄ™ znajdujÄ… w folderze

#wczytuje zapisany raster
policzony_raster10 <- raster("tempC_Czerwiec_POZ.tif") # nie podaje pelnej sciezki bo ustawilem folder roboczy

#===================================================================================================
#=========================== Przycinanie LST do granic miasta ======================================

LST_dla_miasta <- mask(policzony_raster10, Extent)  ## przycinam LST kwadratu do granicy miasta

writeRaster(LST_dla_miasta, "LST_Czerwiec_POZ.tif", overwrite = TRUE) # zapisuje przyciety raster do .tif
LST_dla_miasta <- raster("LST_Czerwiec_POZ.tif") # wczytuje przyciety przed chwila raster dzieki temu dzialam na mniejszym rastrze


#====================================================================================================

# display.brewer.all()  # wyswietlam wszystkie dostepne kolory w gotowych paletach
# moj_kolor1 <- brewer.pal(n = 9, name = "YlOrRd") # ustawiam kolor jaki mi pasuje

graphics.off()
svg("LST_Czerwiec_POZ.svg") # otwieram svg

podzial <- c(seq(from = 19, to = 44, by=1)) # podzial dla czerwca

# moj_kolor <- viridis_pal(option = "inferno")(length(podzial))  # n = liczba poszukiwanych kolorĂłw dla paĹşdziernik 16, 10 dla lutego, 21
par(cex.axis=2)
moj_kolor <- topo.colors(length(podzial))
plot(LST_dla_miasta,  # tworze rysunek
     breaks = podzial, 
     col = moj_kolor, # rev(moj_kolor)
     xlim = c(617000,642000), # dla POZ
     ylim = c(5795000,5820000),# dla POZ
     legend.width=1, legend.shrink=0.75, # dodaje szerokosc i rozciaglasc legendy
     cex.axis=2
     
)
plot(Extent, add = TRUE) 

scalebar(type = "bar",d = 5000, divs = 2) # skala dla POZ
text(624700,5796550,"m",col="grey20",cex=1.1) # dla POZ


dev.off() # zamykam svg

# par(cex.axis, cex.lab, cex.main, cex.sub)
graphics.off()
svg("HIST_LST_Czerwiec_POZ.svg") # rysuje i zapisuje histogram
par(mar=c(5 ,5 , 2, 2))
par(cex.axis=2, cex.lab=2)
LST_dla_miasta_Hist<-hist(LST_dla_miasta, ######### histogram dla caĹ‚ego przyciÄ™rego rastra
                          breaks = podzial, 
                          main ="",
                          col=moj_kolor,  # ustawiam kolor wykresow
                          xlab= expression("Temperatura [" * degree * C *"]"),
                          ylab= "Częstość",
                          ylim=c(0,20000), # dla czerwca dla POZ
                          xaxt='n',  )# Czyszcze podzialke OX 

axis(1, at=podzial) # nadaje wlasna podzialke 
dev.off() # zamykam plik


#========================================================================================================================
#======================== Sprawdzam jaka jest miejska wyspa ciepla na podstawie dzialan na buforze ======================

Temp_Referencyjna <- mask(policzony_raster10,LST_dla_miasta, inverse = TRUE)
(SR_kwadratu <- cellStats(Temp_Referencyjna, 'mean')) # licze srednia dla calego obszaru i wyswietlam wynik
(SR_Miasta <- cellStats(LST_dla_miasta, 'mean')) # licz srednia dla granicy miasta i wyswietlam wynik
(Roznica_sl <- SR_Miasta - SR_kwadratu)

UHI <- LST_dla_miasta - 26.0087

writeRaster(UHI, "UHI_Czerwiec_POZ.tif", overwrite = TRUE) # zapisuje przyciety raster do .tif
UHI <- raster("UHI_Czerwiec_POZ.tif") # wczytuje przyciety przed chwila raster dzieki temu dzialam na mniejszym rastrze

graphics.off()
svg("UHI_Czerwiec_POZ.svg")

podzial <- c(seq(from = -7, to= 18, by =1)) # przedzial dla czerwca

colfunc <- colorRampPalette(c("blue","green" ,"red"))
moj_kolor <- colfunc(length(podzial))
par(cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2)
plot(UHI,
     breaks = podzial,
     col = moj_kolor, # rev(moj_kolor)
     xlim = c(617000,642000), # dla POZ
     ylim = c(5795000,5820000),# dla POZ
     legend.width=1, legend.shrink=0.75, # dodaje szerokosc i rozciaglasc legendy
)

plot(Extent, add = TRUE)
scalebar(type = "bar",d = 5000, divs = 2) # skala dla POZ
text(624700,5796550,"m",col="grey20",cex=1.1) # dla POZ

dev.off()

graphics.off()
svg("UHI_HIST_Czerwiec_POZ.svg")
par(mar=c(5 ,5 , 2, 2))
par(cex.axis=2, cex.lab=2)
UHI_hist<-hist(UHI, ######### histogram dla caĹ‚ego przyciÄ™rego rastra
               breaks = podzial, 
               main="",
               col=moj_kolor,  # ustawiam kolor wykresow
               xlab= expression("Temperatura [" * degree * C *"]"),  # nazwa OX
               ylab= "Częstość",
               ylim=c(0,20000), # dla czerwca dla leborka
               xaxt='n') # Czyszcze podzialke OX
axis(1, at=podzial) # nadaje wlasna podzialke 
dev.off()


#=======================================================================================================================
#======================================= Corine Land Cover dla UHI =====================================================

corine <-  readOGR("D:/Publikacja/CLC_poznan/Poznan_CLC.shp", layer = "Poznan_CLC",dropNULLGeometries=TRUE) #wczytuje shp'a corine przygotowanego wczesniej w QGIS
corine <- spTransform(corine, CRS(proj4string(meta_dane))) # nadaje ta sama projekcie co ma moj tiff z lst

corine$Grd_ranks <- rank(corine$CODE_18)
class <-unique(corine$CODE_18) # zapisuje do class to co znajduje sie w kolumnie CODE_18

i<-1

#111 112 121 122 124 132 141 142 211 231 242 243 311 312 313 321 324 411 512
# colors <- c("#e6004d"(111),"#ff0☺000"(112), "#cc4df2" (121), "#cc0000" (122), "#e6cce6
# " (124), "#a64d00" (132), "#ffa6ff" (141), "#ffe6ff" (142), "#ffffa8" (211), "#e6e64d" (231),
# , "#ffe64d" (242), "#e6cc4d" (243), "#80ff00" (311), "#00a600" (312), "#4dff00" (313), "#ccf24d" (321),
# "#a6f200" (324), "#a6a6ff" (411), "#80f2e6" (512))


#nadanie kolorow z CORINE LAND COVER 2018 do ponizdszych kodow w takiej samej kolejnosci
#111 112 121 122 124 132 141 142 211 231 242 243 311 312 313 321 324 411 512

colors <- c("#e6004d","#ff0000", "#cc4df2", "#cc0000", "#e6cce6", "#a64d00", "#ffa6ff",
            "#ffe6ff", "#ffffa8", "#e6e64d", "#ffe64d", "#e6cc4d", "#80ff00", "#00a600",
            "#4dff00", "#ccf24d","#a6f200", "#a6a6ff", "#80f2e6")

corine$value<-rank(corine$CODE_18)
# corine$value<-NA
for (i in 1:(length(corine$CODE_18))) {
  if (corine$CODE_18[i]=="111") corine$value[i]<-111 else 
    if (corine$CODE_18[i]=="112") corine$value[i]<-112 else  
      if (corine$CODE_18[i]=="121") corine$value[i]<-121 else
        if (corine$CODE_18[i]=="124") corine$value[i]<-124 else
          if (corine$CODE_18[i]=="132") corine$value[i]<-132 else
              if (corine$CODE_18[i]=="141") corine$value[i]<-141 else
                if (corine$CODE_18[i]=="142") corine$value[i]<-142 else 
                  if (corine$CODE_18[i]=="211") corine$value[i]<-211 else
                    if (corine$CODE_18[i]=="231") corine$value[i]<-231 else
                      if (corine$CODE_18[i]=="242") corine$value[i]<-242 else
                        if (corine$CODE_18[i]=="243") corine$value[i]<-243 else
                          if (corine$CODE_18[i]=="311") corine$value[i]<-311 else
                            if (corine$CODE_18[i]=="312") corine$value[i]<-312 else
                              if (corine$CODE_18[i]=="313") corine$value[i]<-313 else
                                if (corine$CODE_18[i]=="321") corine$value[i]<-321 else
                                  if (corine$CODE_18[i]=="324") corine$value[i]<-324 else
                                    if (corine$CODE_18[i]=="411") corine$value[i]<-411 else
                                      if (corine$CODE_18[i]=="512") corine$value[i]<-512
}
#=======================================================================================================================
# czy moge tak  zrobic?
corine$value
z <- which(corine$value==65)
corine$value[z] <- 124 # 124 dlatego, ze wartosci 121 jest wiecej i zalozylem ze cos z klasyfikacja 124 jest nie tak, bo 121 od poczatku sa dobrze klasyfikowane
corine$value
#=======================================================================================================================

ROI2=raster() # tworzenie pustego rastra 
extent(ROI2)<-extent(corine)
ROI2
res(ROI2) <- 10           #zmieniam rozdzielczosc do 10 m jak do 1m to dĹ‚ugo sie rysuje rasteryzacja
rasteryzacja <-rasterize(corine, ROI2, "value", fun="first")
graphics.off()


# zapisuje to co zrobilem jako tiif
writeRaster(rasteryzacja, filename="clc_czerwiec_POZ.tif",  overwrite=T)#,options="INTERLEAVE=BAND"
rr <- raster("clc_czerwiec_POZ.tif")

rr <- projectRaster(rr,UHI)
#111 112 121 122 124 132 141 142 211 231 242 243 311 312 313 321 324 411 512
kategorie <- c(111  ,112  ,112  ,112  ,121  ,121  , 121, 122, 122, 122, 124, 124, 124,
               132, 132, 132, 141, 141, 141, 142, 142 ,142  ,211  ,211  ,211  ,231  ,231 ,231  
               ,242 ,242  ,242, 243, 243, 243  ,311  ,311 , 311, 312 ,312, 312, 313, 313, 313,
               321, 321, 321, 324, 324, 324, 411, 411, 411, 512, 512) # ustawiam wartoĹ›ci dla macierzy
rclmat <- matrix(kategorie, ncol=3, byrow=TRUE)
corin_recl <- reclassify(rr, rclmat )

graphics.off()
#boxploty dla UHI
svg(filename = "BoxploT_Corine_Czerwiec_POZ.svg")
par(mar=c(5 ,5 , 2, 2))
par(cex.axis=1.5, cex.lab=1.4)
d <- boxplot(UHI, corin_recl, notch=T, na.omit=T,las=2, cex=.1, outline=F, horizontal=F,
             col=colors, main="", varwidth=F, xlab="", 
             ylab = expression("Temperatura [" * degree * C *"]"))

# liczbowe statystyki dla d
dev.off()
for (i in 1:ncol(d$stats)){
  text(i,d$stats[,i], col="black",labels = round(d$stats[,i], digits = 1),cex = 0.9,adj = c(0.5,-0.2))
  # text(i,d$stats[3,i]+1,labels = paste0("n=",d$n[i]),cex = 0.75)
  text(i-0.25,d$stats[2,i],labels = d$names[i],adj = 1,cex = 0.75)
}
