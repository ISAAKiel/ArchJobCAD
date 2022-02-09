;;;== dt ================================================================================
;;; Laed Tastaturbefehle fuer Standardaufgaben der Archaeologie bei Planarbeiten in AutoCAD
;;; Fuer die Befehle muessen zahlreiche Bloecke in die AutoCAD-Datei eingefuegt werden.
;;; Die Funktionen sind abgestimmt auf die Layerstruktur von "LayerErstellen.lsp",
;;; koennen aber mit wenig Aufwand angepasst werden.
;;; Die erste Funktion Profillinie ist ausfuehrlich kommentiert,
;;; die nachfolgenden sind aehnlich und koennen analog erschlossen werden.
;;; AutoLISP verwendet ausschließlich ANSI, Umlaute sind deshalb ersetzt. 
;;;
;;;== en ================================================================
;;; This application loads commands for standard documentation tasks  in AutoCAD on
;;; an archaeological excavation. The application makes use of several blocks that form 
;;; part of the project and standardised layer names using the function "layererstellen.lsp".
;;; Both can be adjusted  to personal needs in the code.  The first function "Profillinie" 
;;; is well commented and can be taken as reference for the subsequent very similar variations. 
;;; AutoLISP uses ANSI as default code page. German special characters are written ae, oe, ue and ss. 
;;;
;;;== Autoren ================================================================
;;; Christoph Rinne, Lea-Tabitha Buehrer, Robert Hoffmann, Ann-Katrin Klein, Clemens Kruckenberg, Nadine Schwarck, 
;;; Datum: 2012-05-04, 2016-06-16, 2022-02
;;;
;=====================================================================
; Definition von globalen Variablen
(setvar "attdia" 0)

; Fuer die Zaehlung der Befundnummern etc.
; (en) Definition of the counted variables
(setq
  BefNr "0"
  Nagel "0"
  FundNr "0"
  Num "0"
  blkPrefix ""
  coordorder nil
)

; Die Layernamen ohne Praefix, abgestimmt auf diejenigen in der Funktion "LayerErstellen.lsp." 
; (en) Definition of the layer names without the prefix for trench and planum 
;  as defined in the function "layererstellen" 
(setq
  layBefNr  "BefundNr"
  layProf   "Profil"
  layProfNr "ProfilNr"
  layMessP  "Messpunkte"
  layFundNr "FundNr"
)

; Spezifische Blocknamen, die ggf. vom Nutzer in die Zeichnung nachgeladen werden muessen.
; Die Bloecke liegen getrennt als dwg vor und koennen an persoenliche Beduerfnisse angepasst werden.
; Die metrische Einheit der Bloecke ist Meter und wird bei mm von AutoCAD automatisch skalliert.
; (en) Block names used in the subsequent functions.
; !!! Metric unit is "Meter". AutoCAD will scale the blocks to corresponding inch or mm.
(setq
  blkBNR "BefNr"
  blkPRN "ProfilNr"
  blkFUN "Fund"
  blkNPT "NivPunkt"
  blkNUM "NumPunkt"
)

;;; Remark: Obsolet since plain text is no longer used in the blocks
;;;
; Spezifische Textstile. Die Textstile sind Teil der Bloecke und werden mit diesen geladen.
; Die Textstile koennen wie die Bloecke an persoenliche Beduerfnisse angepasst werden.
; (en) Text stiles used and defined in the blocks.
;;;(setq
;;;  txtPRN100 "ProfilNr_M100"
;;;  txtBNR100 "BefNr_M100"
;;;  txtFUN100 "FundNr_M100"
;;;)

;;; Systemvariable(n) setzen
(SET 'ATTDIA' 0) ;Kein Fenster für die Attributwerte sondern Kommandozeile

;===================================================================
; Allgemeine Funktionen

; Returns a list of the attributes of the block provided
; Form der Liste: ("1-1" "2022-01-14" "SN01PL01_Befund" "1-2" "2022-01-14" "SN01PL...)
(defun get-blockattributes ( blockname / AllBlocks i x attributelist)
  (vl-load-com) ; comment if set in the beginning 
  (setq attributelist (list))
  (if (setq AllBlocks (ssget "X" (list (cons '0 "INSERT") (cons '2 blockname)))) ;pass blockname as dotted pair
    (repeat (setq i (sslength AllBlocks))
      (setq block	(vlax-ename->vla-object(ssname AllBlocks (setq i (1- i)))))
  	  (setq x 0)
    	  ;(setq Fundatt (list))
	  (foreach att
		(vlax-invoke block 'GetAttributes)
	        (setq attributelist (append attributelist (list (vla-get-Textstring att))))
	    (setq x (+ x 1))
          )
      )
   )
  attributelist
)

; Durchsucht String nach anderem String und gibt die Pos zurueck
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

; Systemvariable CDate zu lesbarem Datum aendern
; (nach: http://www.afralisp.net/autolisp/tutorials/date-and-time-stamping.php)
; Fuer das Messprotokoll ergaenzt auf Anregung von H. Frenzel
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

; Prueft ob ein Layer existiert und wechselt dann auf diesen
; cheques if layer exists and changes to it
(defun changeLayer (layName)
    (if
      (tblsearch "layer" layName)
      (setvar "clayer" layName)
      (alert (strcat "Der Layer mit dem Namen: " layName "\nexistiert nicht! \nKein Wechsel des Layers."))
    )
)

; Wandelt Text in Ganzzahl, addiert und aendert wieder in Text, fuer (getstring)
; (en) switches between string and integer to add a number.
(defun add2text	(i str)
  (itoa (+ i (atoi str)))
)

; Prueft, ob der Block im zuletzt verwendeten Verzeichnis vorhanden ist,
; ansonsten muss der Nutzer den Block auswaehlen, der Pfad wird gespeichert
; (en) cheques if the block exists in the curent used folder and asks to select the needed block.
(defun getWBlock (blkName / blkFile)
  (setq blkFile (strcat blkPrefix blkName ".dwg"))
  (if (not (findfile blkFile))
    (progn 
      (setq blkFile (getfiled "Blockdatei auswaehlen" (strcat (getvar "dwgprefix") blkName) "dwg" 2))
      (setq blkPrefix (substr blkFile 1 (- (strlen blkFile) (strlen blkName) 4)))
    )
  )
  (prin1 blkFile)
)

;;;=============================================================================== 
;;; Fuegt eine Profillinie mit zwei Profilnaegeln (ProfilNr) ein.
;;;================================================================================
;;;(en) insert two Blocks for labeld profile nails and draws a line 
; Befundnummer setzen
; (en) set the feature number
(defun Profillinie ( / BefNr_Neu Nagel_opt Nagelnummer oldlay pt1 pt2)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  ;Bei einer neuen Befundnummer wird auch die Nagelzaehlung zurueckgesetzt.
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

  ; Prueft, ob der Block vorhanden ist: ja -> einfuegen, nein -> mit Funktion getWBlock laden.
  ; Wenn andere Bloecke verwendet werden, muessen Attribute ab "Nagelnummer" angepasst werden.
  ; (en) cheques if the block exists and inserts its else asks user and inserts
  ; (en) inserts the label
  (if (tblsearch "block" blkPRN)
    (command "_insert" blkPRN pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
  )
  ;(command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer) ; ein zusaetzlicher Text einfuegen


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
  (command "_insert" blkPRN pt2 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
  ;(command "_text" "Stil" txtPRN100 pt2 "0" Nagelnummer) ; ein zusaetzlicher Text

  (changeLayer (strcat (getPrefix oldlay) LayProf))
  (command "_line" pt1 pt2 "")

  (setvar "clayer" oldlay)
) ; Ende Defun Profillinie

(princ "\nProfillinie zeichnen mit PRL starten")


;;;============================================================================== 
;;;Fuegt einen einzelnen ProfilNagel (ProfilNr) ein.
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
    (command "_insert" blkPRN pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
  )
  ; (command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer) ; ein zusaetzlicher Text
  (setvar "clayer" oldlay)
); Ende Defun ProfilNr

(princ "\nEinzelne Profilnummer mit PRN einfuegen")


;;;==============================================================================
;;;Fuegt einen Block mit Attribut (BefNr) und einen Text (BefNr) ein.
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

; Pruefen der Befundnummer auf Dublette
;1. Prueft ob eine Fundliste existiert und erstellt diese
(if
  (= Befundliste NIL)
   (setq Befundliste (get-blockattributes blkBNR))
)
; 2. Pruef ob die neue Befundnummer in der Liste steht und handelt angemessen)
(setq beflist (member BefNr Befundliste))
(if
  (/= beflist NIL)
   (progn
     (alert
       (strcat
	 "Befundnummer ist schon vorhanden \n"
	 (car beflist)
	 "\n"
	 "Datum: "
	 (cadr beflist)
	 "\n"
	 "Layer: "
	 (caddr beflist)
       )
     )
     (exit)
   )
)
  
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayBefNr))
  (setq pt1 (getpoint "\nBefund Mittelpunkt messen: "))
  (if (tblsearch "block" blkBNR)
    (command "_insert" blkBNR pt1 "1" "1" "0" BefNr (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkBNR) pt1 "1" "1" "0" BefNr (TODAY) (getvar "clayer"))
  )
  ;(command "_text" "Stil" txtBNR100 pt1 "0" BefNr) ;Nur Block kein zusaetzlicher Text

   ; append current values to pervious attributelist
  (setq Befundliste (append (list BefNr (getvar "clayer") (today)) Befundliste))
  
  (setvar "clayer" oldlay)
);Ende Defun BefundNr

(princ "\nBefund-Nummer einfuegen mit BNR")


;;;===============================================================================
;;;Fuegt einen Fundpunkt als Block mit Attribut und als Text ein.
;;;===============================================================================
(defun Fund ( / BefNr_Neu FundNr_opt Fundnummer flist oldlay pt1)
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

  ; Pruefen der Fundnummer auf Dublette
  ;1. Prueft ob eine Fundliste existiert und erstellt diese
  (if
    (= Fundliste NIL)
    (setq Fundliste (get-blockattributes blkFUN))
  )
  ; 2. Pruef ob die neue Fundnummer in der Liste steht und handelt angemessen)
  (setq flist (member Fundnummer Fundliste))
  (if
    (/= flist NIL)
    (progn
      (alert
	(strcat
	  "Fundnummer ist schon vorhanden \n" 
	  (car flist) "\n"
	  "Datum: " (cadr flist) "\n"
	  "Layer: " (caddr flist)))
      (exit)
    )
  )
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayFundNr))
  (setq pt1 (getpoint "\nFundpunkt einmessen: "))
  (if (tblsearch "block" blkFUN) ; checks if Block exists and if not loads it
    (command "_insert" blkFUN pt1 "1" "1" "0" Fundnummer (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkFUN) pt1 "1" "1" "0" Fundnummer (TODAY) (getvar "clayer"))
  )
  ;(command "_text" "Stil" txtFUN100 pt1 "0" Fundnummer) ; adds findnumber as text
  
  (setq Fundliste (append (list Fundnummer (getvar "clayer") (today)) Fundliste)) ; append attributelist
  
  (setvar "clayer" oldlay) ;Return to previous layer
) ; Ende Defun Fund

(princ "\nFundpunkt einfuegen mit FUN")


;;;===============================================================================
;;;Fuegt einen Block mit Attribut (NivPunkt) ein.
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
    (command "_insert" blkNPT pt1 "1" "1" "0" NivWert (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkNPT) pt1 "1" "1" "0" NivWert (TODAY) (getvar "clayer"))
  )
  (setvar "clayer" oldlay)
  (princ)
)

(princ "\nNiv-Punkt einfuegen mit NPT")

;;;===============================================================================
;;;Fuegt einen Punkt und einen Absatztext mit den Koordinaten des Klickpunktes auf dem Messpunktlayer ein.
;;;Auf Anregung von K. Reuter mit der Ergaenzung coordorder auf Anregung von T. Koiki
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
		(strcat "\nTextfeld markieren oder letzte Zeilenhoehe <"
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
  (setvar "clayer" oldlay)
)

(princ "\nText mit Koordinaten einfuegen mit KBM")

;;;===============================================================================
;;;Fuegt einen nummerierten Punkt (NumPunkt, Messbildpunkt) als Block ein.
;;;===============================================================================
(defun NummerPunkt ( / BefNr_Neu Num_opt Fundnummer flist oldlay pt1)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  (if (/= BefNr_Neu "")
    (setq BefNr BefNr_Neu Num "0")
  )
  (setq	Num_opt
	 (getstring
	   (strcat "\nNummer eingeben <"(add2Text 1 Num)">:")))
  (if
    (= Num_opt "")
    (setq Num (add2Text 1 Num))
    (setq Num Num_opt)
  )
  (setq NumPunkt (strcat BefNr "/" Num))

  ; Pruefen der Fundnummer auf Dublette
  ;1. Prueft ob eine Fundliste existiert und erstellt diese
  (if
    (= NumPunktliste NIL)
    (setq NumPunktliste (get-blockattributes blkNUM))
  )
  ; 2. Pruef ob die neue Fundnummer in der Liste steht und handelt angemessen)
  (setq flist (member NumPunkt NumPunktliste))
  (if
    (/= flist NIL)
    (progn
      (alert
	(strcat
	  "NumPunkt ist schon vorhanden \n" 
	  (car flist) "\n"
	  "Datum: " (cadr flist) "\n"
	  "Layer: " (caddr flist)))
      (exit)
    )
  )
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayMessP))
  (setq pt1 (getpoint "\nNumPunkt einmessen: "))
  (if (tblsearch "block" blkNUM) ; checks if Block exists and if not loads it
    (command "_insert" blkNUM pt1 "1" "1" "0" NumPunkt (TODAY) (getvar "clayer"))
    (command "_insert" (getWBlock blkNUM) pt1 "1" "1" "0" NumPunkt (TODAY) (getvar "clayer"))
  )
  
  (setq NumPunktliste (append (list NumPunkt (getvar "clayer") (today)) NumPunktliste)) ; append attributelist
  
  (setvar "clayer" oldlay) ;Return to previous layer
) ; Ende Defun NumPunkt

(princ "\nNummerierten Punkt einfuegen mit NummerPunkt")

;;;===============================================
;;; Regenriere Attributliste
;;;===============================================
(defun RegenAttriblist ( / blockname )
  (setq blockname (getstring (strcat "\nBlockname eingeben :")))
  (set (read(strcat blockname "liste"))  (get-blockattributes blockname))
)

(defun c:BNR () (BefundNr))
(defun c:PRL () (Profillinie))
(defun c:PRN () (ProfilNr))
(defun c:FUN () (Fund))
(defun c:NPT () (NivPunkt))
(defun c:KBM () (KoordBem))
(defun c:NUM () (NummerPunkt))
(defun c:Attributliste () (RegenAttriblist)) 
(princ "\nBNR fuer Bef.-Nr., PRL fuer Profillinie, PRN fuer Profilnagel, FUN fuer Fund-Nr., NPT fuer Niv-Punkt, NUM fuer Nummerierter Punkt \nKBM fuer x- y- z-Koordinaten, Attributliste fuer eine Liste der Attribute zu einem Block.")
