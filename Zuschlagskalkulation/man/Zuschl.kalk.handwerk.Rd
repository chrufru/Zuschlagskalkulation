\name{Zuschl.kalk.handwerk}
\alias{Zuschl.kalk.handwerk}
\title{Zuschlagskalkulation fuer kleine Handwerksbetriebe}
\description{Berechnet die Selbstkosten (SK) mit nur einer Kostenstelle fuer Handwerksbetriebe}
\usage{Zuschl.kalk.handwerk(MK,FLK,GEKZ,SEKF)}
\arguments{
\item{MK}{Materialgesamtkosten}
\item{FLK}{Fertigungslohnkosten}
\item{GEKZ}{Gemeinkostenzuschlag}
\item{SEKF}{Sondereinzelkosten der Fertigung}
}
\details{
\code{Zuschl.kalk.handwerk} liest die Argumente MK,FLK,GEKZ,SEKF der Eingabe oder Datei, und ermittelt die Selbstkosten (SK) fuer einen Auftrag
}
\value{
Ausgegegeben wird die verwendete Einheit (Euro)
}
\references{
Gummersbach, Alfons -Produktionsmanagement ISBN:
}
\author{Christopher M. Stadin}
\examples{
# Mit vorangesetzen Argumenten
Zuschl.kalk.handwerk(MK=300,FLK=140,GEKZ=120,SEKF=0)
#
# Ohne vorangesetzte Argumente
Zuschl.kalk.handwerk(300,140,120,0)
}
\keyword{
IO
}



