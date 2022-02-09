;;;===================================================================================
;;; Claculates the slope between two points.
;;; Asks if the view is a profile (y-value) or planum (z-value).
;;; Inserts the slope, vertical and horizontal distance as text box.

;;; author Christoph Rinne 2011-11-25


(defun c:Steigung ()
  (setq Profil "")
  (setq Profil (getstring "\nPRofil oder PLanum? [PR]:"))
  (setq Startpunkt (getpoint "\nStartpunkt anklicken: "))
  (setq Endpunkt (getpoint "\nEndpunkt anklicken: "))
	(if (= Profil "")
	  (setq deltaZ (+ (car (cdr Endpunkt)) (* (car (cdr Startpunkt)) -1)))
      (setq deltaZ (+ (last Endpunkt) (* (last Startpunkt) -1)))
  	)
	(if (= Profil "")
  	  (setq deltaY (+(last Startpunkt) (* (last Endpunkt) -1)))
	  (setq deltaY (+ (car (cdr Startpunkt)) (* (car (cdr Endpunkt)) -1)))
  	)
  (setq deltaX (+ (car Startpunkt) (* (car Endpunkt) -1)))

  (setq horDist (sqrt (+ (* deltaX deltaX) (* deltaY deltaY))))
  (setq strSteig (strcat "Steigung: " (rtos (* (/ deltaZ horDist) 100) 2 1) "%"))
  (setq strdeltaH (strcat "vert. Dist.: " (rtos deltaZ 2 2)))
  (setq strhorDist (strcat "horiz. Dist.: " (rtos horDist 2 2)))
  (setq SteigTxt (strcat strSteig "\\P" strdeltaH "\\P" strhorDist))
  (setq Pkt1 (getpoint "\nEinfügepunkt Text: "))
  (setq Pkt2 (list (+ (car Pkt1) 5) (+ (car(cdr Pkt1)) -5) (last Pkt1)))
  (command "_mtext" Pkt1 Pkt2 SteigTxt "")
)
(princ "\nSteigung einfügen mit 'Steigung'")

;;; Das Ende