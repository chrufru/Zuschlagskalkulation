\name{Zuschl.kalk.industrie}
\alias{
Zuschl.kalk.industrie
}
\title{
Zuschlagskalkulation mit mehreren Kostenstellen
}
\description{
Berechnet die Selbstkosten (SK) mit mehreren Kostenstellen fuer Industiebetriebe
}
\usage{
Zuschl.kalk.industrie(SK,MK,MGKZ,FLK,FGKZ,SEF,EKGK,VwGK,VwGKZ,VtGK,VtGKZ)
}
\arguments{
\item{SK}{Selbstkosten}
\item{MK}{Materialgesamtkosten}
\item{MGKZ}{Materialgemeinkostenzuschlag}
\item{FLK}{Fertigungslohnkosten}
\item{FGKZ}{Fertigungsgemeinkostenzuschlag}
\item{SEF}{Sondereinzelkosten der Fertigung}
\item{EKGK}{Energiegemeinkosten}
\item{VwGK}{Verwaltungsgemeinkosten}
\item{VwGKZ}{Verwaltungsgeimeinkostenzuschlag}
\item{VtGK}{Vertiebsgemeinkosten}
\item{VtGKZ}{Vertiebsgemeinkostenzuschlag}
}
\details{
\code{Zuschl.kalk.industrie} liest die Argumente SK, MK, MGKZ, FLK, FGKZ, SEF, EKGK, VwGK, VwGKZ, VtGK, VtGKZ der Eingabe oder einer Datei, und ermittelt die Selbstkosten (SK) für einen Auftrag. Sind Kostenstellen nicht zu ermitteln, können ähnliche Aufträge heran gezogen werden. Dabei werden die schon errechneten Selbstkosten eines Auftrages benutzt um zum Beispiel die Materialkosten zu berechen.
Dabei ist es nötig das die zu ermittelnde Kostenstelle auf NA gesetzt wird. (noch nicht bei allen Kostenstellen implementiert)
}
\value{
Ausgegegeben wird die verwendete Einheit (Euro)
}
\references{
vgl. Benford Law:
}
\author{
Christpher M. Stadin}
\examples{
# Mit vorangesetzen Argumenten
Zuschl.kalk.industrie(SK=NA,MK=0,MGKZ=120,FLK=0,FGKZ=40,SEF=0,
EKGK=20394,VwGK=2004,VwGKZ=0,VtGK=20483,VtGKZ=20)

# Ohne vorangesetzte Argumente
Zuschl.kalk.industrie(NA,0,120,0,40,0,20394,2004,0,20483,20)

### The function is currenty defined as:
Zuschl.kalk.industrie <- function(SK,MK,MGKZ,FLK,FGKZ,SEF,EKGK,VwGK,VwGKZ,VtGK,VtGKZ){
	# 1. Kostenstelle
	if(is.na(MK)==F) {
	MGK = MK/100 * MGKZ # Zuschlagskalkulation  fuer Material
	MK = MK + MGK # Materialkosten 
	cat(MK,"Euro fuer","Materialgesamtlkosten","\n")
	}else if (is.na(MK)==T){
	MK = SK-(FLK+(FLK/100 * FGKZ))-SEF-(VwGK+(VwGK/100*VwGKZ))-EKGK-(VtGK+(VtGK/100*VtGKZ))
	cat("MK = N.A.","-", "Materialgesamtkosten werden berrechnet!","\n","\n")
        cat("********************************************************************","\n")
	cat(MK,"Euro fuer","Materialeinzelkosten","\n")
	cat("********************************************************************","\n")
	}
	# 2. Kostenstelle
	if(is.na(FLK)==F) {
	FGK = FLK/100 * FGKZ # Fertigungsgemeinkostenzuschlag
        FK = FLK + FGK + SEF # Fertigungskosten 
	HK = MK + FK # Herstellkosten
	cat(HK,"Euro fuer","Herstellkosten","\n")    
	}
	# 3. Kostenstelle
	if(is.na(VwGK)==F) {
	VwK = VwGK/100 * VwGKZ # Verwaltungsgemeinkostenzuschlag
	VwG =	VwGK + VwK	# Verwlatungsgemeinkosten
	cat(VwG,"Euro fuer","Verwaltungkosten", "\n")
	}
	# 4. Kostenstelle
	if(is.na(VtGK)==F) {
	VtK = VtGK/100 * VtGKZ # Verriebgemeinkostenzuschlag
	VtG = VtGK + VtK # Vertriebsgemeinkosten
	cat(VtG,"Euro fuer","Vertriebskosten","\n","\n")
	}
	SK = HK + EKGK + VwG + VtG # Selbstkosten
        cat("Die Selbstkosten betragen", SK, "Euro", "\n")  
}
}
\keyword{
IO
}



