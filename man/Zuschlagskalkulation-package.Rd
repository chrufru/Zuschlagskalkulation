\name{Zuschlagskalkulation-package}
\alias{Zuschlagskalkulation-package}
\docType{package}
\title{
Berechnet die Selbstkosten anhand einer Zuschlagskalkulation  
}
\description{
Berechnet die Selbstkosten anhand einer Zuschlagskalkulation mit einer
Kostenstelle fuer kleine Handwerksbetriebe oder mehreren Kostenstellen
fuer Industiebetriebe. Die 1. Funktion ist vereinfacht und nutzt nur
einen allgemeinen Gemeinkostenzuschlag.\cr
Die 2. Funktion ist ein wenig komplexer und benutzt mehrere Gemeinkostenzuschläge. In beiden
Funktionen werden Gemeinkostenzuschläge in Prozent angegeben. Dieses
Paket ist noch im Entwickungsstadium und soll noch um weitere Funktionen
erweitert werden. Ausserdem soll eine grafische Ansicht (Plot) der
Daten, Anwendern dieses Paketes erleichtern bestimmte Kostenexplsionen
in relation zur Gesamtheit darzustellen.

Um eine grafische Darstellung der Daten zu erwirken muss, in der
geweahlten Funktion das Argument "Plot=True" gesetzt werden. Damit
erfolgt eine Darstellung der Daten im Grafikfenster.
}
\details{
\tabular{ll}{
Package: \tab Zuschlagskalkulation\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-02-14\cr
License: \tab GNU General Public License\cr
}}
\author{
Christopher M. Stadin \cr
Entwickler: Bei Fragen E-Mail an <chrufru@gmail.com>
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
# Mit einer Kostenstelle
Zuschl.kalk.handwerk(300,140,120,0)
# Mit mehreren Kostenstellen
Zuschl.kalk.industrie(NA,0,120,0,40,0,20394,2004,0,20483,20)
}
