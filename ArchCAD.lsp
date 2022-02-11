;;; This application loads commands for standard documentation tasks  in AutoCAD on
;;; an archaeological excavation. The application makes use of several blocks that form 
;;; part of the project and standardised layer names using the function "layererstellen.lsp".
;;; Both can be adjusted  to personal needs in the code.  The first function "Profillinie" 
;;; is well commented and can be taken as reference for the subsequent very similar variations. 
;;;
;;; The code was originally written in German. Some variable names and the block names preserve this status.
;;; BefNr = feature number, Nagel = (profile) nail, FundNr = find number, Messpunkt = measuring or reference point
;;; AutoLISP uses ANSI as default code page. German special characters are written ae, oe, ue and ss. 
;;;
;;;== authors ================================================================
;;; Christoph Rinne, Lea-Tabitha Buehrer, Robert Hoffmann, Ann-Katrin Klein, Clemens Kruckenberg, Nadine Schwarck, 
;;; date of changes: 2012-05-04, 2016-06-16, 2022-02
;;;
;=====================================================================
; definition of global variables and setting
(setvar "attdia" 0)
(vl-load-com) ; only needed for function get-blockattributes

; Definition of the counting variables
(setq
  BefNr "0"
  Nagel "0"
  FundNr "0"
  Num "0"
  blkPrefix ""
  coordorder nil
)

; Definition of the layer names without the prefix for trench and planum 
;  as defined in the function "layererstellen" (create layers)
(setq
  layBefNr  "BefundNr"
  layProf   "Profil"
  layProfNr "ProfilNr"
  layMessP  "Messpunkte"
  layFundNr "FundNr"
)

; Block names used in the subsequent functions and provided in ./blocks/*.dwg.
; The metric unit is "meter". AutoCAD will scale the blocks to current map scale (_units).
(setq
  blkBNR "BefNr"
  blkPRN "ProfilNr"
  blkFUN "Fund"
  blkNPT "NivPunkt"
  blkNUM "NumPunkt"
)

;;; Remark: Obsolete since plain text is no longer used in the blocks
;;;
; (en) Text stiles used and defined in the blocks.
;;;(setq
;;;  txtPRN100 "ProfilNr_M100"
;;;  txtBNR100 "BefNr_M100"
;;;  txtFUN100 "FundNr_M100"
;;;)

;;; System variable(s) to be set
(SET 'ATTDIA' 0) ;no attribute window but command line

;===================================================================
; General functions

; Returns a list of the attributes of the block provided
; Form of the list: ("1-1" "2022-01-14" "SN01PL01_Befund" "1-2" "2022-01-14" "SN01PL...)
(defun get-blockattributes ( blockname / AllBlocks i x attributelist)
  ;(vl-load-com) ; comment if set in the beginning 
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

; Extract number attribute of attributelist
; Returns a list
(defun getNumbers (attributelist / )
  (setq i 0 iter 3)
  (setq ll (length attributelist))
  (setq Numbers '())
  (while (< i (+ ll))
    (setq Numbers (append Numbers (list (nth i attributelist))))
    (setq i (+ i iter))
  )
  Numbers
)

; Search string in another, returns the position.
; (source: http://www.autolisp.mapcar.net/strings2.html)
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

; get last text height (source: http://www.forums.augi.com)
(defun getTextHeight ()
	(if (zerop (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
	  (cdr (assoc 42 (tblsearch "style" (getvar "textstyle"))))
	  (cdr (assoc 40 (tblsearch "style" (getvar "textstyle"))))
	)
)

; format system date to a readable text
; (source: http://www.afralisp.net/autolisp/tutorials/date-and-time-stamping.php)
; Thanks to Hannes Frenzel who had the idea to start with a full documentation list
(defun TODAY ( / d yr mo day)
     (setq d (rtos (getvar "CDATE") 2 6)
          yr (substr d 1 4)
          mo (substr d 5 2)
         day (substr d 7 2)
     )
     (strcat day "/" mo "/" yr)
)

;========================================================================
; Specific functions for ArchCAD tasks
; Gets the prefix for trench and layer from the provided layer name
(defun getPrefix (layName)
  (substr layName 1 (str-pos layName "_"))
)

; Check if layer exists and changes to it, else gives an alert and does not change
(defun changeLayer (layName)
    (if
      (tblsearch "layer" layName)
      (setvar "clayer" layName)
      (alert (strcat "Der Layer mit dem Namen: " layName "\nexistiert nicht! \nKein Wechsel des Layers."))
    )
)

; Converts between string and integer to count numbers used in strings.
(defun add2text	(i str)
  (itoa (+ i (atoi str)))
)

; Checks if the block exists in the folder of the active drawing 
; and asks the user to select the needed block.
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

; Prueft die Versionen der uebergebenen DWG mit der aktiven DWG-Version
; TRUE wenn die Vers.-Nr. der Block-Datei groesser ist als der TAG der aktiven ACADVersion.
; Checks the the AutoCAD version of the imported wblock (dwg) 
; not to be later (greater) than current AutoCAD version
(defun checkBlockVersion (blkFile / activeVersion ACADversions activeTAG blkData blkTAG)
  ;Acad versions and file TAGs
  (setq activeVersion (atoi (substr (getvar "acadver") 1 2)))
  ;List of AutoCAD version and corresponding DWG-tag
  (setq ACADversions '(18 1024 19 1027 20 1027 22 1032 23 1032 24 1032))
  (setq activeTAG (cadr (member activeVersion ACADversions)))
  ; get block version
  (setq blkData '())
  (setq blkData (read-line (open blkFile "r")))
  (setq blkTAG (atoi (substr blkData 3 4)))
  ; compare versions
  (> blkVersion activeTAG)
)


;;;=============================================================================== 
;;; Operating part with command functions
;;;================================================================================
;;; Insert two blocks for labeled profile nails and draw a line 
;;;================================================================================
; set the feature number
(defun Profillinie ( / BefNr_Neu Nagel_opt Nagelnummer oldlay pt1 pt2)
  (setq	BefNr_Neu
	 (getstring
	   (strcat "\nBefund-Nr. eingeben <" BefNr ">:")))
  ; reset the nail counting if new feature 
  (if (/= BefNr_Neu "")
      (setq BefNr BefNr_Neu Nagel "0") 
  ) 

  ; first nail of the profile
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

  ; save the current layer and change to the appropriate layer
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayProfNr))

  ; Checks if the block exists and inserts it else asks user to selekt ist and checks the version.
  ; Adds x-, y-, rotation and needed labels.
  (if (tblsearch "block" blkPRN)
    (command "_insert" blkPRN pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
    (progn
      (if (checkBlockVersion (getWBlock blkPRN))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
    )
  )
  ;(command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer) ; ein zusaetzlicher Text einfuegen

 ; second nail for profile
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
;;; Inserts a single profile nail.
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
    (progn
      (if (checkBlockVersion (getWBlock blkPRN))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkPRN) pt1 "1" "1" "0" Nagelnummer (TODAY) (getvar "clayer"))
    )
  )
  ; (command "_text" "Stil" txtPRN100 pt1 "0" Nagelnummer) ; ein zusaetzlicher Text
  (setvar "clayer" oldlay)
); Ende Defun ProfilNr

(princ "\nEinzelne Profilnummer mit PRN einfuegen")


;;;==============================================================================
;;; Insert the block for a feature number (BefNr)
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

; Check for duplicate feature numbers in the drawing
; 1. Create the corresponding list if not exists
(if
  (= Befundliste NIL)
   (setq Befundliste (get-blockattributes blkBNR))
)
; 2. Check if new feature number is member of the list and act accordingly
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
    (progn
      (if (checkBlockVersion (getWBlock blkBNR))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkBNR) pt1 "1" "1" "0" BefNr (TODAY) (getvar "clayer"))
    )
  )
  ;(command "_text" "Stil" txtBNR100 pt1 "0" BefNr) ;Nur Block kein zusaetzlicher Text

   ; append the new values to the current list
  (setq Befundliste (append (list BefNr (getvar "clayer") (today)) Befundliste))
  
  (setvar "clayer" oldlay)
);Ende Defun BefundNr

(princ "\nBefund-Nummer einfuegen mit BNR")


;;;===============================================================================
;;; Inserts the block for a new find number.
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

  ; Check for duplicate feature numbers in the drawing
  ; 1. Create the corresponding list if not exists
  (if
    (= Fundliste NIL)
    (setq Fundliste (get-blockattributes blkFUN))
  )
  ; 2. Check if new feature number is member of the list and act accordingly
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
    (progn
      (if (checkBlockVersion (getWBlock blkFUN))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkFUN) pt1 "1" "1" "0" Fundnummer (TODAY) (getvar "clayer"))
    )
  )
  ;(command "_text" "Stil" txtFUN100 pt1 "0" Fundnummer) ; adds findnumber as text
  
  ; append the new values to the current list
  (setq Fundliste (append (list Fundnummer (getvar "clayer") (today)) Fundliste)) ; append attributelist
  
  (setvar "clayer" oldlay) ;Return to previous layer
) ; Ende Defun Fund

(princ "\nFundpunkt einfuegen mit FUN")


;;;===============================================================================
;;; Inserts the block for a levelling point (NivPunkt).
;;; The function sets the measured z-value as default.
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
    (progn
      (if (checkBlockVersion (getWBlock blkNPT))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkNPT) pt1 "1" "1" "0" NivWert (TODAY) (getvar "clayer"))
    )
  )
  (setvar "clayer" oldlay)
  (princ)
)

(princ "\nNiv-Punkt einfuegen mit NPT")

;;;===============================================================================
;;; Inserts the coordinates of the first point/click into a textbox (mText).
;;; User will be asked once to set the order xyz or yxz. 
;;; Thanks to Kay Reuter and Thomas Koiki for their ideas.
;;;===============================================================================
(defun KoordBem ( / pt1 pt2 TextHeight KoordTxt oldlay )
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
  ; get the first point providing the coordinates and build the string for the mtext
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
  ; if nil set text height to current "standard" height
  (if (/= (> TextHeight 0))
    (setq TextHeight (getTextHeight))
  )
  ; let the user draw a rectangle for the textbox (mtext) else pt2 will remain nil
  (setq pt2 (getcorner pt1
		(strcat "\nTextfeld markieren oder letzte Zeilenhoehe <"
			(rtos TextHeight)">")))
  ; if pt2 is valid calculate TextHeight for three lines else take TextHeight and calculate appropriate pt2 for apropriate textbox (mText)
  (if pt2
    ; divide textbox height for three lines and spacing
    (setq TextHeight (* (abs (- (cadr pt2) (cadr pt1))) 0.23 )) 
    ; else: build second point for textbox on basis of calculated values for x and y)
    (setq pt2
	  (list
	     (+ (car pt1) (* TextHeight 20)) ; calculate textbox width for 14 digits depending on TextHeight
	     (+ (cadr pt1) (* TextHeight 4.4)) ; calculate textbox height for three lines and spacing
	     (caddr pt1)))
  )
  ; change layer
  (setq oldlay (getvar "clayer"))
  (changeLayer (strcat (getPrefix oldlay) LayMessP))
  ; insert point, textbox and change layer
  (command "_point" pt1)
  (command "_mtext" pt1 "STil" "Standard" "H" TextHeight pt2 KoordTxt "")
  (setvar "clayer" oldlay)
)

(princ "\nText mit Koordinaten einfuegen mit KBM")

;;;===============================================================================
;;; Insert the block for a numbered point (NumPunkt) e.g. for photogrammetry.
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

  ; Check for duplicate feature numbers in the drawing
  ; 1. Create the corresponding list if not exists
  (if
    (= NumPunktliste NIL)
    (setq NumPunktliste (get-blockattributes blkNUM))
  )
  ; 2. Check if new feature number is member of the list and act accordingly
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
    (progn
      (if (checkBlockVersion (getWBlock blkNUM))
	(progn
	  (alert "Die AutoCAD-Version des Blockes ist neuer \nals die aktive AutoCAD-Version")
	  (exit)
	)
      )
    (command "_insert" (getWBlock blkNUM) pt1 "1" "1" "0" NumPunkt (TODAY) (getvar "clayer"))
    )
  )
  ; append the new values to the current list
  (setq NumPunktliste (append (list NumPunkt (getvar "clayer") (today)) NumPunktliste)) ; append attributelist
  
  (setvar "clayer" oldlay) ;Return to previous layer
) ; Ende Defun NumPunkt

(princ "\nNummerierten Punkt einfuegen mit NummerPunkt")

;;;===============================================
;;; regenerate an attribute list
;;;===============================================
(defun RegenAttriblist ( / blockname )
  (setq blockname (getstring (strcat "\nBlockname eingeben :")))
  (set (read(strcat blockname "liste"))  (get-blockattributes blockname))
)

;;;==========================================
;;; get the max. (last) feature number
;;===========================================
(defun getMaxBefNr ( / Number Numbers NumbersInt)
  (if (= Befundliste nil)
    (exit))
  (setq Numbers (getNumbers Befundliste))
  (setq NumbersInt '())
  (foreach Number Numbers 
    (setq NumbersInt (append NumbersInt (list (atoi Number)))))
  (car (vl-sort NumbersInt '>))
)

;;;==========================================
;;; get the max. (last) find number of a feature
;;===========================================
(defun getMaxFindNr ( / )
  (if (= Fundliste nil)
    (exit))
  (setq Numbers (getNumbers Fundliste))
  (setq BefNr (getstring "\Bitte Befund-Nr angeben:"))
  (setq bl (strlen BefNr))
  (setq Finds '())
  (foreach Number Numbers
    (if (= BefNr (substr Number 1 bl))
      (setq Finds (append (list (atoi (substr Number (+ bl 2))))))
      )
  )  
  (car (vl-sort Finds '>))
)


;;;================================================
;;; Set the global commands to be called in AutoCAD
;;;================================================

(defun c:BNR () (BefundNr))
(defun c:PRL () (Profillinie))
(defun c:PRN () (ProfilNr))
(defun c:FUN () (Fund))
(defun c:NPT () (NivPunkt))
(defun c:KBM () (KoordBem))
(defun c:NUM () (NummerPunkt))
(defun c:Attributliste () (RegenAttriblist))
(defun c:MaxBNR () (getMaxBefNr))
(defun c:MaxFUN () (getMaxFindNr))
(princ "\nBNR fuer Bef.-Nr., PRL fuer Profillinie, PRN fuer Profilnagel, FUN fuer Fund-Nr., NPT fuer Niv-Punkt, NUM fuer Nummerierter Punkt \nKBM fuer x- y- z-Koordinaten, Attributliste fuer eine Liste der Attribute zu einem Block.")
