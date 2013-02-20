\name{Zuschl.kalk.industrie}
\alias{Zuschl.kalk.industrie}
\title{Zuschlagskalkulation mit mehreren Kostenstellen}
\description{Berechnet die Selbstkosten (SK) mit mehreren Kostenstelle fuer Industiebetriebe}
\usage{Zuschl.kalk.industrie(SK,MEK,MGKZ,FLK,FGKZ,SEKF,
EKEK,EKGKZ,VwEK,VwGKZ,VtEK,VtGKZ,plot=TRUE)}
\arguments{
\item{SK}{Selbstkosten}
\item{MEK}{Material-Einzelkosten}
\item{MGKZ}{Materialgemeinkostenzuschlag}
\item{FLK}{Fertigungslohnkosten}
\item{FGKZ}{Fertigungsgemeinkostenzuschlag}
\item{SEKF}{Sondereinzelkosten der Fertigung}
\item{EKEK}{Entwicklungs- u. Konstruktions-Einzelkosten}
\item{EKGKZ}{Entwicklungs- u. Konstruktions-Gemeinkostenzuschlag}
\item{VwEK}{Verwaltungs-Einzelkosten}
\item{VwGKZ}{Verwaltungs-Gemeinkostenzuschlag}
\item{VtEK}{Vertiebs-Einzelkosten}
\item{VtGKZ}{Vertiebs-Gemeinkostenzuschlag}
\item{plot}{Logical FALSE or TRUE}
}
\details{
\code{Zuschl.kalk.industrie} liest die Argumente SK, MEK, MGKZ, FLK, FGKZ, SEKF, EKEK, EKGKZ, VwEK, VwGKZ, VtEK, VtGKZ der Eingabe oder einer Datei, und ermittelt die Selbstkosten (SK) fuer einen Auftrag. 
Sind Kostenstellen nicht zu ermitteln, koennen eahnliche Auftraege heran gezogen werden. Dabei werden die schon errechneten Selbstkosten eines Auftrages benutzt um zum Beispiel die Materialkosten zu berechen.
Dabei ist es noetig das die zu ermittelnde Kostenstelle auf "NA" gesetzt wird. Durch das Argument plot=TRUE wird ein Barplot (Balkendiagramm) im Arbeitsverzeichnis erzeugt.
}
\value{
Ausgegegeben wird die verwendetet Einheit (Waehrung)
}
\references{
Gummersbach, Alfons -Produktionsmanagement ISBN:
}
\author{Christpher M. Stadin}
\examples{Zuschl.kalk.industrie(SK=NA,MEK=1000,MGKZ=100,FLK=1000,FGKZ=100,SEKF=1000,
EKEK=1000,EKGKZ=100,VwEK=1000,VwGKZ=100,VtEK=1000,VtGKZ=100,plot=TRUE)}

\keyword{IO}



