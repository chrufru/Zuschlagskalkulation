\name{Zuschl.kalk.handwerk}
\alias{
Zuschl.kalk.handwerk
}
\title{
Zuschlagskalkulation fuer kleine Handwerksbetriebe
}
\description{
Berechnet die Selbstkosten (SK) mit nur einer Kostenstelle fuer Handwerksbetriebe
}
\usage{
Zuschl.kalk.handwerk(MK,FLK,GEKZ,SEF)
}
\arguments{
\item{MK}{Materialgesamtkosten}
\item{FLK}{Fertigungslohnkosten}
\item{GEKZ}{Gemeinkostenzuschlag}
\item{SEF}{Sondereinzelkosten der Fertigung}
}
\details{
\code{Zuschl.kalk.handwerk} liest die Argumente MK,FK,GEKZ,SEF der Eingabe oder Datei, und ermittelt die Selbstkosten (SK) für einen Auftrag
}
\value{
Ausgegegeben wird die verwendete Einheit (Euro)
}
\references{
vgl. Benford Law:
}
\author{
Christopher M. Stadin}
\examples{
# Mit vorangesetzen Argumenten
Zuschl.kalk.handwerk(MK=300,FLK=140,GEKZ=120,SEF=0)

# Ohne vorangesetzte Argumente
Zuschl.kalk.handwerk(300,140,120,0)

### The function is currenty defined as:
Zuschl.kalk.handwerk <- function(MK,FLK,GEKZ,SEF){
  FK = FLK + SEF
  	HK = MK/100 *GEKZ # Gemeinkostenzuschlag in Prozent
	    SK = MK +FK +HK
	cat("Die Selbstkosten betragen", SK, "Euro", "\n")
  return(SK)
}

}
\keyword{
IO
}



