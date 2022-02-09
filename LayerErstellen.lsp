;;;================================================================
;;; Creates series of layers for an archaeological layer (planum).
;;; The list "Layerliste" with the lists of layer names and properties can be edited.
;;; If needed a 4th property can be added to all layers in the "Layerliste"
;;; and activated in the function "LayerNeu" used to create the layers.
;;; 
;;; The function askes for a trench number (max. 2 digits) and a layer number (max. 2 digits)
;;;  and creates a prefix "SN<trech number>PL<layer number>_" for each layer name.
;;; Result e.g. SN01PL01_BefundNr. (Schnitt 01 Planum 01 = trench 01 planum 01 feature number).
;;; If the trench is ignored (nil, no value provided) the name will be PL01_BefundNr.
;;; The names, labels and prompts still preserve the German original. 
;;;
;;; author: Christoph Rinne 2012-04, 2022-02

(Defun c:LayerErstellen	( / LayerListe Praefix Schnitt Planum)

 ;List of the layers to be created with a list of the properties for each layer.
 (setq LayerListe
   '(
	 ;Name			Colour	LineType	optAttribute4
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

 ;Function to create each layer in the list of layers
 (Defun LayerNeu (LayerProperties)
  (command "_layer" ; calls the layer menu
	   "_new" ; create a new layer  
	   (strcat Praefix (car LayerProperties)) ; name of the new layer
	   "_color" ; set the color for a layer
	   (cadr LayerProperties) ; color number
	   (strcat Praefix (car LayerProperties)) ; name of the layer for the color provided
	   "_Ltype" ; set the line type for a layer
	   (caddr LayerProperties) ; name of the line type 
	   (strcat Praefix (car LayerProperties)) ; name of the layer for the line type provided
	   ;"_command" ; call a 4th property 
	   ;(cadddr LayerProperties) ; for a optional 4th attribute optAttribute4
	   ;(strcat Praefix (car LayerProperties))  
	   ""
  )
  (princ)
 )
 ; Define the prefix for the series of layers (SN<00>PL<00>_)
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
 
 ; call the function LayerNeu for each element in the list LayerListe.
  (mapcar 'LayerNeu LayerListe)
  
); Ende Defun Layererstellen

(prompt "\nLayerpaket pro Schnitt und Planum erstellen mit LAYERERSTELLEN")