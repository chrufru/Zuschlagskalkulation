Zuschl.kalk.handwerk <-
function(MK,FLK,GEKZ,SEKF){
  FK = FLK + SEKF
  	HK = MK *GEKZ/100 # Gemeinkostenzuschlag in Prozent
	    SK = MK +FK +HK
	cat("Die Selbstkosten betragen", SK, "Euro", "\n")
  return(SK)
}
