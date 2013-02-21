Zuschl.kalk.industrie <-
function(SK,MEK,MGKZ,FLK,FGKZ,SEKF,EKEK,EKGKZ,VwEK,VwGKZ,VtEK,VtGKZ,plot=TRUE){
	# 1. Kostenstelle -Material
	if(is.na(MEK)==F){
	MGK <- MEK * MGKZ/100 # Material-Gemeinkosten
	MK <- MEK + MGK # Materialkosten 
	cat("\n")
	cat("***********************'1. Kostenstelle *****************************","\n")
	cat(MEK,"Euro fuer","Materialeinzellkosten","\n")
	cat(MGK,"Euro fuer",MGKZ,"Prozent","Gemeinkostenzuschlag","\n")
        cat("_____","\n")
	cat(MK,"Euro feur Materialkosten","\n")
        }
        else if (is.na(MEK)==T){
    	FGK <- FLK * FGKZ/100 # Fertigungs-Gemeinkosten
    	EKGK <- EKEK * EKGKZ/100 # Entwicklungs- u. Konstuktions-Gemeinkosten
    	VwGK <- VwEK * VwGKZ/100 # Verwaltungs-Gemeinkosten
    	VtGK <- VtEK * VtGKZ/100 # Vertriebs-Gemeinkosten
    	MGK <- SK-(FLK+FGK+SEKF+EKEK+EKGK+VwEK+VwGK+VtEK+VtGK)
    	MEK <- MGK-((MGKZ/MGK) *10 *MGK)
	  cat("MEK = NA","-", "Materialeinzelkosten werden berrechnet...!","\n")
	  cat("Die Selbstkosten eines vergleichbaren Auftrages betrugen:",SK,"Euro","\n")
          cat("********************************************************************","\n")
 	  cat(MEK,"Euro fuer","Material-Einzelkosten simuliert!","\n")
          cat("angenommen wurden:", MGKZ,"Prozent Gemeinkosten des vorhergenden Auftrags","\n")
	  cat("********************************************************************","\n")
	}
  	#
	# 2. Kostenstelle -Fertigung
	if(is.na(FLK)==F){
	MGK <- MEK * MGKZ/100 # Material-Gemeinkosten
	MK <- MEK + MGK # Materialkosten 
	FGK <- FLK * FGKZ/100 # Fertigungs-Gemeinkosten
	FK <- FLK+FGK  
        HK <- MK+FK+SEKF # Hier weil, keine Kostenstelle für Sondereinzelkosten der Fertigung 
	cat("\n")
        cat("***********************'2. Kostenstelle *****************************","\n")
	cat( FLK,"Euro fuer","Fertigungslohnkosten","\n")
	cat(FGK,"Euro fuer",FGKZ,"Prozent","Gemeinkostenzuschlag","\n")
	cat("_____","\n")
	cat(FK,"Euro feur Fertigungskosten","\n")
	cat(SEKF,"Euro feur Sondereinzelkosten der Fertigung","\n")
	cat("_____","\n")
	cat(HK,"Euro feur Herstellungskosten","\n")
	}
	else if (is.na(FLK)==T){
	MGK <- MEK * MGKZ/100 # Material-Gemeinkosten
        EKGK <- EKEK * EKGKZ/100 # Entwicklungs- u. Konstuktions-Gemeinkosten
	VwGK <- VwEK * VwGKZ/100 # Verwaltungs-Gemeinkosten
	VtGK <- VtEK * VtGKZ/100 # Vertriebs-Gemeinkosten
	    FGK <- SK-(MK+SEKF+VwGKZ+EKEK+VwGK+VtGK)
            FLK <- FGK-(FGKZ/FGK *10 *FGK)
	    cat("FLK = NA","-", "Fertigungs-Lohnkosten werden berrechnet...!","\n")
	    cat("Die Selbstkosten eines vergleichbaren Auftrages betrugen:",SK,"Euro","\n")
	    cat("********************************************************************","\n")
	    cat(FLK,"Euro fuer","Fertigungs-Lohnkosten simuliert!","\n")
	    cat("angenommen wurden:",FGKZ,"Prozent Gemeinkosten des vorhergenden Auftrags","\n")
	    cat("********************************************************************","\n")
	  }
  	#
	# 3. Kostenstelle: Entwicklung- u. Konstruktion
	if(is.na(EKEK)==F){
 	EKGK <- EKEK * EKGKZ/100 # Entwicklungs- u. Konstuktions-Gemeinkosten
        EKK <- EKEK+EKGK
	cat("\n")
	cat("***********************'3. Kostenstelle *****************************","\n")
	cat(EKEK,"Euro fuer","Entwicklung- u. Konstruktions-Einzelkosten","\n")
        cat(EKGK,"Euro fuer",EKGKZ,"Prozent","Gemeinkostenzuschlag","\n")
	cat("_____","\n")
	cat(EKK,"Euro feur Entwicklung- u. Konstruktionskosten","\n")
        }
	  else if (is.na(EKEK)==T){
	    MGK <- MEK * MGKZ/100 # Material-Gemeinkosten
            FGK <- FLK * FGKZ/100 # Fertigungs-Gemeinkosten
            VwGK <- VwEK * VwGKZ/100 # Verwaltungs-Gemeinkosten
	    VtGK <- VtEK * VtGKZ/100 # Vertriebs-Gemeinkosten
	    EKGK <- SK-(HK+VwGK+VtGK)
	    EKEK <- EKGK-(EKGKZ/EKGK *10 *EKGK)
	    cat("EKEK = NA","-", "Entwicklung- u. Konstruktions-Einzelkosten werden berrechnet...!","\n")
	    cat("Die Selbstkosten eines vergleichbaren Auftrages betrugen:",SK,"Euro","\n")
	    cat("********************************************************************","\n")
	    cat(EKEK,"Euro fuer","Entwicklung- u. Konstruktions-Einzelkosten simuliert!","\n")
	    cat("angenommen wurden:", EKGKZ,"Prozent Gemeinkosten des vorhergenden Auftrags","\n")
	    cat("********************************************************************","\n")
	}
        #
	# 4. Kostenstelle: Verwaltung
	if(is.na(VwEK)==F){
	VwGK <- VwEK * VwGKZ/100 # Verwaltungs-Gemeinkosten
        VwK <- VwEK+VwGK
	cat("\n")
	cat("***********************'4. Kostenstelle *****************************","\n")
	cat(VwEK,"Euro fuer","Verwaltungs-Einzelkosten","\n")
       	cat(VwGK,"Euro fuer",VwGKZ,"Prozent","Gemeinkostenzuschlag","\n")
	cat("_____","\n")
	cat(VwK,"Euro feur Verwaltungskosten","\n")
	}
	  else if (is.na(VwEK)==T){
	    MGK <- MEK * MGKZ # Material-Gemeinkosten
	    FGK <- FLK * FGKZ # Fertigungs-Gemeinkosten
	    EKGK <- EKEK * EKGKZ # Entwicklungs- u. Konstuktions-Gemeinkosten
            VtGK <- VtEK * VtGKZ # Vertriebs-Gemeinkosten
	    VwGK<- SK-(HK+EKGK+VtGK)
	    VwEK <- VwGK-(VwGKZ/VwGK *10 *VwGK)
	    cat("MEK = NA","-", "Verwaltungs-Einzelkosten werden berrechnet...!","\n")
	    cat("Die Selbstkosten eines vergleichbaren Auftrages betrugen:",SK,"Euro","\n")
	    cat("********************************************************************","\n")
	    cat(VwGK,"Euro fuer","Verwaltungs-Einzelkosten simuliert!","\n")
	    cat("angenommen wurden:",VwGKZ,"Prozent Gemeinkosten des vorhergenden Auftrags","\n")
	    cat("********************************************************************","\n")
	}
 	#
	# 5. Kostenstelle Vertrieb
	if(is.na(VtEK)==F){
	VtGK <- VtEK * VtGKZ/100 # Vertriebs-Gemeinkosten
        VtK <- VtEK+VtGK
	cat("\n")
 	cat("***********************'5. Kostenstelle *****************************","\n")
	cat(VtEK,"Euro fuer","Vertriebs-Einzelkosten","\n")
	cat(VtGK,"Euro fuer",VtGKZ,"Prozent","Gemeinkostenzuschlag","\n")
	cat("_____","\n")
	cat(VtK,"Euro feur Vertiebskosten","\n")
	  }
	  else if (is.na(VtEK)==T){
	    MGK <- MEK * MGKZ # Material-Gemeinkosten
	    FGK <- FLK * FGKZ # Fertigungs-Gemeinkosten
	    EKGK <- EKEK * EKGKZ # Entwicklungs- u. Konstuktions-Gemeinkosten
	    VwGK <- VwEK * VwGKZ # Verwaltungs-Gemeinkosten
	    VtGK <- SK-(HK+EKEK+VwGK)
	    VtEK <- VtGK-(VtGKZ/VtGK *10 *VtGK)
	    cat("VtEK = NA","-", "Vertriebs-Einzelkosten werden berrechnet...!","\n")
	    cat("Die Selbstkosten eines vergleichbaren Auftrages betrugen:",SK,"Euro","\n")
	    cat("********************************************************************","\n")
	    cat(VtGK,"Euro fuer","Vertriebs-Einzelkosten simuliert!","\n")
	    cat("angenommen wurden:", VtGKZ,"Prozent Gemeinkosten des vorhergenden Auftrags","\n")
	    cat("********************************************************************","\n")
	}
	if(is.na(SK)==T){
        SK = HK + EKEK+EKGK + VwEK+VwGK + VtEK+VtGK # Selbstkosten
        cat("\n")
        cat("Die Selbstkosten betragen", SK, "Euro", "\n")  
        # return(SK) 
	}
	else if(is.na(SK)==F){
	cat("********************************************************************","\n")
	cat("\n")
        cat("Eine Kostenstelle wurde auf NA gesetzt")  
	}
        # Create Plot
	if((plot)==T){
	png(filename="Zuschlagskalk.png", width=1024, height=1024, pointsize=36)
	Kosten <- c(MK,FK,HK,EKK,VwK,VtK)
	labels <-list("Materialkosten","Fertigungskosten","Herstellkosten","Entwicklunkskosten","Verwaltungskosten","Vertriebskosten")
	par(mar=c(10,4,4,4))
        barplot(height=Kosten, names.arg=labels, horiz=F, las=2,col="skyblue", main="Zuschlagskakulation",cex.names=1.0)
        dev.off()
	}else{
        cat("Argument: plot=False")  
	}
}
