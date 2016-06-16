;;;================================================================
;;; Erstell ein Layerpaket für ein Planum Niveau ##
;;; von: Christoph Rinne April 2012
;;; Die Spalten der LayerListe sind auf die nachfolgende Funktion LayerNeu abgestimmt, kann aber bei Bedarf erweitert werden.
;;; Die Einträge in der Layerliste können nach Bedarf verändert oder ergänzt werden.
;;; Die Funktion fragt jeweils nach Schnitt-Nr. und Planum, der Layername lautet dann SNxxPLxx_Name.
;;; Wird Schnitt leer gelassen (""), lautet der Layername PLxx_Name  

(Defun c:LayerErstellen	( / LayerListe Praefix Schnitt Planum)

 ;Liste der zu erstellenden Layer und deren Eigenschaften
 (setq LayerListe
   '(
	 ;Name			FarbNr	Linientyp	optSpalte4
	 ("Befund" 		"36" 	"Continuous")
	 ("BefundInnen"		"36"	"Continuous")
	 ("BefundNr" 		"36" 	"Continuous")
	 ("BefundText" 		"36" 	"Continuous")
	 ("BefundKonstrukt" 	"32" 	"Continuous")
	 ("FundNr" 		"7" 	"Continuous")
	 ("Profil" 		"5" 	"Continuous")
	 ("ProfilNr" 		"5" 	"Continuous")
	 ("Schnittkante" 	"3" 	"ACAD_ISO02W100")
	 ("SchnittNr" 		"3" 	"Continuous")
	 ("Grabungsgrenze_intern" "3" 	"ACAD_ISO10W100")
	 ("Grabungsgrenze_extern" "3" 	"ACAD_ISO12W100")
	 ("Messpunkte" 		"7" 	"Continuous")
	)
 )

 ;Funktion zum erstellen der Layer
 (Defun LayerNeu (LayerProperties)
  (command "_layer"
	   "Neu"
	   (strcat Praefix (car LayerProperties))
	   "FArbe"
	   (cadr LayerProperties)
	   (strcat Praefix (car LayerProperties))
	   "Ltyp"
	   (caddr LayerProperties)
	   (strcat Praefix (car LayerProperties))
	   ;(cadddr LayerProperties) ;für die optSpalte4
	   ;(strcat Praefix (car LayerProperties))  ;für die optSpalte4
	   ;mehr als 4 Spalten geht nur mit (cadr(cadddr LayerProperties))
	   ;oder (last LayerProperties)
	   ""
  )
  (princ)
 )

 ;Praefix für die Layernamen erstellen
 (setq Schnitt(getstring "\nSchnittnummer eingeben oder leer lassen: "))
 (if (/= Schnitt "")
   (if (= (strlen Schnitt) 1)
     (setq Schnitt (strcat "0" Schnitt))
   )
 )
  
 (setq Planum (getstring "\nNummer des Planums eingeben:"))
  (if (= (strlen Planum) 1)
    (setq Planum (strcat "0" Planum))
 )
  
 (if (/= Schnitt "")
    (setq Praefix (strcat "SN" Schnitt "PL" Planum "_"))
    (setq Praefix (strcat "PL" Planum "_"))
 )
 
 ;Ruft Funktion LayerNeu auf für jedes Element der Liste.
  (mapcar 'LayerNeu LayerListe)
  
); Ende Defun Layererstellen

(prompt "\nLayerpaket pro Schnitt und Planum erstellen mit LAYERERSTELLEN")