#Import Data
library(readxl)
DATA <- read_excel("I:/My Drive/KULIAH/SKRIPSI/DATA/Data Baru/2. DATA FIX.xlsx", 
                   sheet = "Data Olah", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric"))
View(DATA)


#Ringkasan Data
summary(DATA)

#Standar Deviasi
std<-function(S)
{ 
  n = length(S) 
  hasil = sqrt(sum((S-mean(S))^2)/n) 
  return (hasil) 
}

cat("Nilai Standar Deviasi Mandiri Investa Atraktif Syariah sebesar", 
    (std(DATA$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Standar Deviasi Rencana Cerdas sebesar", 
    (std(DATA$`Rencana Cerdas`)),"\n")
cat("Nilai Standar Deviasi Manulife Saham SMC Plus sebesar", 
    (std(DATA$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Standar Deviasi BNP Paribas Solaris sebesar", 
    (std(DATA$`BNP Paribas Solaris`)),"\n")

#Skewness
library(moments) 
cat("Nilai Skewness Mandiri Investa Atraktif Syariah sebesar", 
    (skewness(DATA$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Skewness Rencana Cerdas sebesar", 
    (skewness(DATA$`Rencana Cerdas`)),"\n")
cat("Nilai Skewness Manulife Saham SMC Plus sebesar", 
    (skewness(DATA$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Skewness BNP Paribas Solaris sebesar", 
    (skewness(DATA$`BNP Paribas Solaris`)),"\n")


#Kurtosis
cat("Nilai Kurtosis Mandiri Investa Atraktif Syariah sebesar", 
    (kurtosis(DATA$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Kurtosis Rencana Cerdas sebesar", 
    (kurtosis(DATA$`Rencana Cerdas`)),"\n")
cat("Nilai Kurtosis Manulife Saham SMC Plus sebesar", 
    (kurtosis(DATA$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Kurtosis BNP Paribas Solaris sebesar", 
    (kurtosis(DATA$`BNP Paribas Solaris`)),"\n")


#Plot data
plotA = plot(DATA$Tanggal, DATA$`Mandiri Investa Atraktif Syariah`, type = "l",
             col="blueviolet", main = "Plot NAB Reksa Dana Mandiri Investa Atraktif Syariah", 
             font.main=7, xlab = "", ylab = "Nilai Aktiva Bersih", font.lab=7) 
plotB = plot(DATA$Tanggal, DATA$`Rencana Cerdas`, type = "l",
             col="blueviolet", main = "Plot NAB Reksa Dana Rencana Cerdas", 
             font.main=7, xlab = "", ylab = "Nilai Aktiva Bersih", font.lab=7)
plotC = plot(DATA$Tanggal, DATA$`Manulife Saham SMC Plus`, type = "l",
             col="blueviolet", main = "Plot NAB Reksa Dana Manulife Saham SMC Plus", 
             font.main=7, xlab = "", ylab = "Nilai Aktiva Bersih", font.lab=7) 
plotD = plot(DATA$Tanggal, DATA$`BNP Paribas Solaris`, type = "l",
             col="blueviolet", main = "Plot NAB Reksa Dana BNP Paribas Solaris", 
             font.main=7, xlab = "", ylab = "Nilai Aktiva Bersih", font.lab=7)



#Mencari nilai Return Reksa Dana
library(dplyr)
data=select(DATA, -c("Tanggal"))
n=nrow(data)
c=ncol(data)
Return = data.frame(matrix(nrow=n-1, ncol = c))
for (i in 1:(n-1))
{
  Return[i,] = log(data[i+1,]/data[i,])
}
colnames(Return)=c("Mandiri Investa Atraktif Syariah","Rencana Cerdas", 
                   "Manulife Saham SMC Plus", "BNP Paribas Solaris", "IHSG")
View(Return)


#Ringkasan Return
summary(Return)

#Standar Deviasi
cat("Nilai Standar Deviasi Return Mandiri Investa Atraktif Syariah sebesar", 
    (std(Return$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Standar Deviasi Return Rencana Cerdas sebesar", 
    (std(Return$`Rencana Cerdas`)),"\n")
cat("Nilai Standar Deviasi Return Manulife Saham SMC Plus sebesar", 
    (std(Return$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Standar Deviasi Return BNP Paribas Solaris sebesar", 
    (std(Return$`BNP Paribas Solaris`)),"\n")


#Skewness
cat("Nilai Skewness Return Mandiri Investa Atraktif Syariah sebesar", 
    (skewness(Return$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Skewness Return Rencana Cerdas sebesar", 
    (skewness(Return$`Rencana Cerdas`)),"\n")
cat("Nilai Skewness Return Manulife Saham SMC Plus sebesar", 
    (skewness(Return$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Skewness Return BNP Paribas Solaris sebesar", 
    (skewness(Return$`BNP Paribas Solaris`)),"\n")


#Kurtosis
cat("Nilai Kurtosis Return Mandiri Investa Atraktif Syariah sebesar", 
    (kurtosis(Return$`Mandiri Investa Atraktif Syariah`)),"\n")
cat("Nilai Kurtosis Return Rencana Cerdas sebesar", 
    (kurtosis(Return$`Rencana Cerdas`)),"\n")
cat("Nilai Kurtosis Return Manulife Saham SMC Plus sebesar", 
    (kurtosis(Return$`Manulife Saham SMC Plus`)),"\n")
cat("Nilai Kurtosis Return BNP Paribas Solaris sebesar", 
    (kurtosis(Return$`BNP Paribas Solaris`)),"\n")


#Plot Return
n=nrow(Return)
c=1
PeriodeReturn = data.frame(matrix(nrow=n, ncol=c))
for (i in 1:n)
{
  PeriodeReturn[i,]=0+i
  colnames(PeriodeReturn)=c("Periode")
}
plotRA = plot(PeriodeReturn$Periode, Return$`Mandiri Investa Atraktif Syariah`, type = "l",
              col="blueviolet", main = "Plot Return Reksa Dana Mandiri Investa Atraktif Syariah", 
              font.main=7, xlab = "Periode", ylab = "Return", font.lab=7) 
plotRB = plot(PeriodeReturn$Periode, Return$`Rencana Cerdas`, type = "l",
              col="blueviolet", main = "Plot Return Reksa Dana Rencana Cerdas", 
              font.main=7, xlab = "Periode", ylab = "Return", font.lab=7)
plotRC = plot(PeriodeReturn$Periode, Return$`Manulife Saham SMC Plus`, type = "l",
              col="blueviolet", main = "Plot Return Reksa Dana Manulife Saham SMC Plus", 
              font.main=7, xlab = "Periode", ylab = "Return", font.lab=7) 
plotRD = plot(PeriodeReturn$Periode, Return$`BNP Paribas Solaris`, type = "l",
              col="blueviolet", main = "Plot Return Reksa Dana BNP Paribas Solaris", 
              font.main=7, xlab = "Periode", ylab = "Return", font.lab=7)


#Memisahkan Sekuritas dengan Benchmark
library(dplyr)
Sekuritas = select(Return, -c("IHSG"))
Benchmark = select(Return, c("IHSG"))
View(Sekuritas)
View(Benchmark)


#Mencari Nilai Return Harapan Masing-masing Sekuritas dan Menghapus Return Harapan Negatif
EReturn = colMeans(Sekuritas) 
cat("Nilai Return Harapan Masing-masing Sekuritas", "\n")
EReturn
Sekuritas_2 = select_if(Sekuritas, colMeans(Sekuritas)>0)
View (Sekuritas_2)


#Mencari Nilai Semivariance
n=nrow(Sekuritas_2)
c=ncol(Sekuritas_2)
Ret_B = Sekuritas_2-Benchmark$IHSG
colnames(Ret_B)=c("RD1-B","RD2-B")
View(Ret_B)

Ret_C = data.frame(matrix(nrow=n, ncol=c))
for (i in 1:n) 
{
  for (j in 1:c) 
    if (Ret_B[i,j]<0)
    { 
      Ret_C[i,j]=Ret_B[i,j] 
    }
  else
  { Ret_C[i,j]=0}
}
colnames(Ret_C)=c("Min(RD1-B,0)","Min(RD2-B,0)")
View(Ret_C)

Semivariance = data.frame(matrix(nrow=1, ncol=c))
for (i in 1:c) 
{
  Semivariance[,i] = (1/n)*sum(Ret_C[,i]^2)
}
colnames(Semivariance)= c("SV Mandiri Investa Atraktif Syariah",
                          "SV Rencana Cerdas")
View(Semivariance)


#Mencari Nilai Semicovariance
SecovAB = (1/n)*sum(Ret_C$`Min(RD1-B,0)`*Ret_C$`Min(RD2-B,0)`)

cat("Nilai Semicovariance Mandiri Investa Atraktif Syariah dengan Rencana Cerdas sebesar", SecovAB, "\n")


#Matriks SV
SV = matrix(c(Semivariance$`SV Mandiri Investa Atraktif Syariah`,SecovAB, SecovAB, 
              Semivariance$`SV Rencana Cerdas`),2,2)
View(SV)         
cat("Matriks Semivariance Semicovariance","\n","\n")
SV


#Pembobotan
Bobot = data.frame(matrix(nrow=2, ncol=1))
for (i in 1:2) 
{
  N1 = matrix(c(1,1),2,1)
  Bobot[i,] = ((solve(SV)%*%(N1))[i,])/((t(N1)%*%solve(SV))%*%(N1))
  
}
colnames(Bobot)=c("Bobot")
View(Bobot)

cat("Bobot untuk sekuritas Mandiri Investa Atraktif Syariah sebesar", Bobot [1,],"\n")
cat("Bobot untuk sekuritas Rencana Cerdas sebesar", Bobot [2,],"\n")

WTotal = sum(Bobot)
cat("Bobot untuk seluruh sekuritas sebesar", WTotal,"\n")


#Return Portofolio
n=nrow(Sekuritas_2)
c=ncol(Sekuritas_2)
Portofolio=data.frame(matrix(nrow=n,ncol=1))
for (i in 1:n) 
{
  Portofolio[i,]= (Bobot [1,]*(Sekuritas_2[i,1]))+(Bobot [2,]*(Sekuritas_2[i,2]))
}
colnames(Portofolio)=c("Return Portofolio")
View(Portofolio)

#Risiko Portofolio
SemivariancePortofolio = ((Bobot [1,]^2)*Semivariance$`SV Mandiri Investa Atraktif Syariah`)+
  ((Bobot [2,]^2)*Semivariance$`SV Rencana Cerdas`)+(2*(Bobot [1,]*Bobot [2,]*SecovAB))
cat("Nilai Semivariance Portofolio sebesar", SemivariancePortofolio,"\n")
SemideviationPortofolio = sqrt(SemivariancePortofolio)
cat("Nilai Risiko Semivariance Portofolio Sebesar", SemideviationPortofolio,"\n")


#Mencari Nilai Return Harapan Portofolio
EReturnPortofolio = colMeans(Portofolio)
cat("Nilai Return Harapan Portofolio", "\n")
EReturnPortofolio


#Kinerja Portofolo

#Memasukan BI Rate
library(readxl)
Bunga_Bebas_Risiko <- read_excel("I:/My Drive/KULIAH/SKRIPSI/DATA/Data Baru/2. DATA FIX.xlsx", 
                                 sheet = "Risk Free Rate", col_types = c("skip", 
                                                                         "numeric"))
View(Bunga_Bebas_Risiko)
Rf = colMeans(Bunga_Bebas_Risiko)
Rf
RfHarian = Rf/365
RfHarian

IndexSharpe = ((EReturnPortofolio-RfHarian)/SemideviationPortofolio)
cat("Nilai Index Portofolio", "\n")
IndexSharpe