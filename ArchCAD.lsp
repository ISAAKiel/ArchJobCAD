;;;== dt ================================================================================
;;; Läd Tastaturbefehle für Standardaufgaben der Archäologie bei Planarbeiten in AutoCAD
;;; Für die Befehle müssen zahlreiche Blöcke in die AutoCAD-Datei eingefügt werden.
;;; Die Funktionen sind abgestimmt auf die Layerstruktur von "LayerErstellen.lsp",
;;; können aber mit wenig Aufwand angepasst werden.
;;; Die erste Funktion Profillinie ist ausführlich kommentiert,
;;; die nachfolgenden sind ähnlich und können analog erschlossen werden.
;;; Autor: Christoph Rinne
;;; Datum: 2012-05-04, 2016-06-16
;;;
;;;== en ================================================================
;;; This application loads commands for standard documentation tasks  in AutoCAD on
;;; an archaeological excavation. The application makes use of several blocks that form 
;;; part of the project and standardised layer names using the function "layererstellen.lsp".
;;; Both can be adjusted  to personal needs in the code.  The first function "Profillinie" 
;;; is well commented and can be taken as reference for the subsequent very similar variations. 
;;;
;=====================================================================
; Definition von globalen Variablen
; Für die Zählung der Befundnummern etc.
; (en) Definition of the counted variables
(setq
  BefNr "0"
  Nagel "0"
  FundNr "0"
  blkPrefix ""
  coordorder nil
)

; Die Layernamen ohne Präfix, abgestimmt auf diejenigen in der Funktion "LayerErstellen.lsp." 
; (en) Definition of the layer names without the prefix for trench and planum 
;  as defined in the function "layererstellen" 
(setq
  layBefNr  "BefundNr"
  layProf   "Profil"
  layProfNr "ProfilNr"
  layMessP  "Messpunkte"
  layFundNr "FundNr"
)

; Spezifische Blocknamen, die ggf. vom Nutzer in die Zeichnung nachgeladen werden müssen.
; Die Blöcke liegen getrennt als dwg vor und können an persönliche Bedürfnisse angepasst werden.
; Die metrische Einheit der Blöcke ist Meter und wird bei mm von AutoCAD automatisch skalliert.
; (en) Block names used in the subsequent functions.
; !!! Metric unit is "Meter". AutoCAD will scale the blocks to corresponding inch or mm.
(setq
  blkBNR "BefNr"
  blkPRN "ProfilNr"
  blkFUN "Fund"
  blkNPT "NivPunkt"
)

; Spezifische Textstile. Die Textstile sind Teil der Blöcke und werden mit diesen geladen.
; Die Textstile können wie die Blöcke an persönliche Bedürfnisse angepasst werden.
; (en) Text stiles used and defined in the blocks.
(setq
  txtPRN100 "ProfilNr_M100"
  txtBNR100 "BefNr_M100"
  txtFUN100 "FundNr_M100"
)

;===================================================================
; Allgemeine Funktionen
; Durchsucht String nach anderem String und gibt die Pos zurück
; Search string in another, returns the position.
; (http://www.autolisp.mapcar.net/strings2.html)
(defun str-pos (str c / i l ls lc)
  (setq i 1)
  (setq ls (strlen str))
  (setq lc (strlen c))
  (setq l (1+ (- ls lc)))
  (while (and (<= i l) (/= (substr str i lc) c))
    (setq i (1+ i))
  )
  (if (<= i l)
    i
  )
)

; Letzte verwendete Texthoehe ermitteln (nach: http://www.forums.augi.com)
(defun getTextHight ()
	(if (zerop (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
	  (cdr (assoc 42 (tblsearch "style" (getvar "textstyle"))))
	  (cdr (assoc 40 (tblsearch "style" (getvar "textstyle"))))
	)
)

; Systemvariable CDate zu lesbarem Datum ändern
; (nach: http://www.afralisp.net/autolisp/tutorials/date-and-time-stamping.php)
; Für das Messprotokoll ergänzt auf Anregung von H. Frenzel
(defun TODAY ( / d yr mo day)
     (setq d (rtos (getvar "CDATE") 2 6)
          yr (substr d 1 4)
          mo (substr d 5 2)
         day (substr d 7 2)
     )
     (strcat day "/" mo "/" yr)
)

;========================================================================
; Spezifische Funktionen dieser Routine
; Funktion ermittelt das Praefix des aktuellen Layers (Schnitt und/oder Planum)
; (en) specific function: gets the prefix for trench and layer from the layer name
(defun getPrefix (layName)
  (substr layName 1 (str-pos layName "_"))
)

; Prüft ob ein Layer existiert und wechselt dann auf diesen
; cheques if layer exists and changes to it
(defun changeLayer (layName)
    (if (tblsearch "layer" layName)
	  (command "_layer" "set" layName "")
    )
)

; Wandelt Text in Ganzzahl, addiert und ändert wieder in Text, für (getstring)
; (en) switches between string and integer to add a number.
(defun add2text	(i str)
  (itoa (+ i (atoi str)))
)

; Prüft, ob der Block im zuletzt verwendeten Verzeichnis vorhanden ist,
; ansonsten muss der Nutzer den Block auswählen, der Pfad wird gespeichert
; (en) cheques if the block exists in the curent used folder and asks to select the needed block.
(defun getWBlock (blkName / blkFile)
  (setq blkFile (strcat blkPrefix blkName ".dwg"))
  (if (not (findfile blkFile))
    (progn 
      (setq blkFile (getfiled "Blockdatei auswählen" (strcat (getvar "dwgprefix") blkName) "dwg" 2))
      (setq blkPrefix (substr blkFile 1 (- (strlen blkFile) (strlen blkName) 4)))
    )
  )
  (prin1 blkFile)
)


;;;=============================================================================== 
;;; Fügt eine Profillinie mit zwei Profilnägeln (ProfilNr) ein.
;;;================================================================================
;;;(en) insert two Blocks for labeld profile nails and draws a line 
; Befundnummer setzen
; (en) set the feature number
(defun Profillinie ( / BefNr_Neu Nagel_opt Nagelnummer oldlay pt1 pt2)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  ;Bei einer neuen Befundnummer wird auch die Nagelzählung zurückgesetzt.
  ; (en) reset the nail counting if new feature 
  (if (/= BefNr_Neu "")
      (setq BefNr BefNr_Neu Nagel "0") 
  ) 

  ; Erster Nagel des Profiles
  ; (en) first nail of the profile
  (setq	Nagel_opt
	 (getstring
	   (strcat "\nErste Nagelnummer eingeben <"
		   (add2Text 1 Nagel)">:")))
  (if (= Nagel_opt "")
    (setq Nagel (add2Text 1 Nagel))
    (setq Nagel Nagel_opt)
  )
  (setq Nagelnummer
	 (strcat BefNr "/" Nagel))      
  (setq pt1
	 (getpoint "\nErsten Nagel messen: "))

  ; Den aktuellen Layer in die Variable oldlay schreiben
  ; und den passenden Layer aktivieren wenn er existiert (s. Funktion changeLayer)
  ; (en) change layer
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayProfNr))

  ; Prüft, ob der Block vorhanden ist: ja -> einfügen, nein -> mit Funktion getWBlock laden.
  ; Wenn andere Blöcke verwendet werden, müssen Attribute ab "Nagelnummer" angepasst werden.
  ; (en) cheques if the block exists and inserts its else asks user and inserts
  ; (en) inserts the label
  (if (tblsearch "block" blkPRN)
    (command "_insert" blkPRN pt1 "1" "1" "0" Nagelnummer (TODAY))
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY))
  )
  (command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer) ; Text einfügen


  ; Zweiter Nagel des Profiles
  ; (en)  second nail for profile
  (setq	Nagel_opt
	 (getstring
	   (strcat "\nZweite Nagelnummer eingeben <"
		   (add2Text 1 Nagel)">:")))
  (if (= Nagel_opt "")
    (setq Nagel (add2Text 1 Nagel))
    (setq Nagel Nagel_opt)
  )
  (setq Nagelnummer (strcat BefNr "/" Nagel))
  (setq pt2 (getpoint "\nZweiten Nagel messen: "))
  (command "_insert" blkPRN pt2 "1" "1" "0" Nagelnummer (TODAY))
  (command "_text" "Stil" txtPRN100 pt2 "0" Nagelnummer)

  (changeLayer (strcat (getPrefix oldlay) LayProf))
  (command "_line" pt1 pt2 "")

  (command "_layer" "set" oldlay "")
) ; Ende Defun Profillinie

(princ "\nProfillinie zeichnen mit PRL starten")


;;;============================================================================== 
;;;Fügt einen einzelnen ProfilNagel (ProfilNr) ein.
;;;===============================================================================
(defun ProfilNr ( / BefNr_Neu Nagel_opt oldlay Nagelnummer pt1)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  (if (/= BefNr_Neu "")
    (setq BefNr BefNr_Neu Nagel "0")
  )
  (setq	Nagel_opt
	 (getstring
	   (strcat "\nNagelnummer eingeben <"
		   (add2Text 1 Nagel)">:")))
  (if (= Nagel_opt "")
    (setq Nagel (add2Text 1 Nagel))
    (setq Nagel Nagel_opt)
  )
  (setq Nagelnummer (strcat BefNr "/" Nagel))
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayProfNr))
  (setq pt1 (getpoint "\nErsten Nagel messen: "))
  (if (tblsearch "block" blkPRN)
    (command "_insert" blkPRN pt1 "1" "1" "0" Nagelnummer (TODAY))
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY))
  )
  (command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer)
  (command "_layer" "set" oldlay "")
); Ende Defun ProfilNr

(princ "\nEinzelne Profilnummer mit PRN einfügen")


;;;==============================================================================
;;;Fügt einen Block mit Attribut (BefNr) und einen Text (BefNr) ein.
;;;===============================================================================
(defun BefundNr ( / BefNr_opt pt1)
  (setq	BefNr_opt
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" (add2Text 1 BefNr) ">:")))
  (if (= BefNr_opt "")
    (setq BefNr (add2Text 1 BefNr))
    (setq BefNr BefNr_opt)
  )
  (setq Nagel "0" FundNr "0")
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayBefNr))
  (setq pt1 (getpoint "\nBefund Mittelpunkt messen: "))
  (if (tblsearch "block" blkBNR)
    (command "_insert" blkBNR pt1 "1" "1" "0" BefNr (TODAY))
    (command "_insert" (getWBlock blkBNR) pt1 "1" "1" "0" BefNr (TODAY))
  )
  (command "_text" "Stil" txtBNR100 pt1 "0" BefNr)
  (command "_layer" "set" oldlay "")
);Ende Defun BefundNr

(princ "\nBefund-Nummer einfügen mit BNR")


;;;===============================================================================
;;;Fügt einen Fundpunkt als Block mit Attribut und als Text ein.
;;;===============================================================================
(defun Fund ( / BefNr_Neu FundNr_opt Fundnummer oldlay pt1)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  (if (/= BefNr_Neu "")
    (setq BefNr BefNr_Neu FundNr "0")
  )
  (setq	FundNr_opt
	 (getstring
	   (strcat "\nFundnummer eingeben <"(add2Text 1 FundNr)">:")))
  (if
    (= FundNr_opt "")
    (setq FundNr (add2Text 1 FundNr))
    (setq FundNr FundNr_opt)
  )
  (setq Fundnummer (strcat BefNr "-" FundNr))
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayFundNr))
  (setq pt1 (getpoint "\nFundpunkt einmessen: "))
  (if (tblsearch "block" blkFUN)
    (command "_insert" blkFUN pt1 "1" "1" "0" Fundnummer (TODAY))
    (command "_insert" (getWBlock blkFUN) pt1 "1" "1" "0" Fundnummer (TODAY))
  )
  (command "_text" "Stil" txtFUN100 pt1 "0" Fundnummer)
  (command "_layer" "set" oldlay "")
) ; Ende Defun Fund

(princ "\nFundpunkt einfügen mit FUN")


;;;===============================================================================
;;;Fügt einen Block mit Attribut (NivPunkt) ein.
;;;Die Funktion ermittelt den Z-Wert des Klickpunktes und setzt diesen als Vorgabewert.
;;;===============================================================================
(defun NivPunkt ( / pt1 Niv_opt NivWert oldlay )
  (setq pt1
	 (getpoint "\nNivPunkt messen: "))
  (setq Niv_opt
	 (rtos (last pt1) 2 2))
  (setq NivWert
	 (getstring
	   (strcat "\nNiv-Wert eingeben: <" Niv_opt ">:")))
  (if (= NivWert "")
    (setq NivWert Niv_opt)
  )
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayMessP))
  (if (tblsearch "block" blkNPT)
    (command "_insert" blkNPT pt1 "1" "1" "0" NivWert (TODAY))
    (command "_insert" (getWBlock blkNPT) pt1 "1" "1" "0" NivWert (TODAY))
  )
  (command "_layer" "set" oldlay "")
  (princ)
)

(princ "\nNiv-Punkt einfügen mit NPT")

;;;===============================================================================
;;;Fügt einen Punkt und einen Absatztext mit den Koordinaten des Klickpunktes auf dem Messpunktlayer ein.
;;;Auf Anregung von K. Reuter mit der Ergänzung coordorder auf Anregung von T. Koiki
;;;===============================================================================
(defun KoordBem ( / pt1 pt2 TextHight KoordTxt oldlay )
  ; user may change the order of the coordinates, default xyz
  (if (null coordorder)
    (progn
      (initget "XYZ,X YXZ,Y")
      (setq coordorder
	    (if (= (getkword "\nBestimmen Sie einmalig die Reihenfolge der Koordinaten [Xyz/Yxz] <XYZ>:") "YXZ")
	      "YXZ"
	      "XYZ"
	    )
      )
    )
  )
  ; get the refernce & insertation point and build the string for the mtext
  (setq pt1 (getpoint "\nKoordinatenpunkt anklicken: "))
  (if (= coordorder "YXZ")
    (setq KoordTxt
	 (strcat
	   "y: " (rtos (cadr pt1)2 2) "\\P"
	   "x: " (rtos (car pt1) 2 2) "\\P"
	   "z: " (rtos (last pt1) 2 2)))
    (setq KoordTxt
	 (strcat
	   "x: " (rtos (car pt1) 2 2) "\\P"
	   "y: " (rtos (cadr pt1)2 2) "\\P"
	   "z: " (rtos (last pt1) 2 2)))
  )
  ; if nil set ext hight to current "standard" hight
  (if (/= (> TextHight 0))
    (setq TextHight (getTextHight))
  )
  ; let the user draw a rectangle for the textbox (mtext) else pt2 will remain nil
  (setq pt2 (getcorner pt1
		(strcat "\nTextfeld markieren oder letzte Zeilenhöhe <"
			(rtos TextHight)">")))
  ; if pt2 is valid calculate TextHight for three lines else take TextHight and calculate apropriate pt2 for apropriate textbox (mText)
  (if pt2
    (setq TextHight (* (abs (- (cadr pt2) (cadr pt1))) 0.23 )) ; divide textbox hight for three lines and spacing
    ; else: build second point for textbox on basis of calculatet values for x, y and z)
    (setq pt2
	  (list
	     (+ (car pt1) (* TextHight 20)) ; calculate textbox width for 14 digits depending on TextHight
	     (+ (cadr pt1) (* TextHight 4.4)) ; calculate textbox hight for three lines and spacing
	     (caddr pt1)))
  )
  ; change layer
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayMessP))
  ; insert point, textbox and change layer
  (command "_point" pt1)
  (command "_mtext" pt1 "STil" "Standard" "H" TextHight pt2 KoordTxt "")
  (command "_layer" "set" oldlay "")
)

(princ "\nText mit Koordinaten einfügen mit KBM")

(defun c:BNR () (BefundNr))
(defun c:PRL () (Profillinie))
(defun c:PRN () (ProfilNr))
(defun c:FUN () (Fund))
(defun c:NPT () (NivPunkt))
(defun c:KBM () (KoordBem))
(princ "\nBNR für Bef.-Nr., PRL für Profillinie, PRN für Profilnagel, FUN für Fund-Nr., NPT für Niv-Punkt, KBM für x- y- z-Koordinaten")
