;|
copie multi-profile-area-2-file-v1.lsp
layer 0 trebuie sa existe si sa fie unfrozen
 C:\tmp\ trebuie sa existe, in folderul tmp va exporta fiser txt
extrage arie polilinie stg /dr
profile trebuie sa fie ordonate vertical la offset de 20m
sa aiba punct de zero de tip text 0.00 pentru a sorta stg - dr
poz km sa fie de forma  Km=41+380.00, Km nu KM sau modifici codul
|;
(vl-load-com)
(defun midpoint (p1 p2)
;;Usage (setq mid(midpoint pt1 pt2))
  (mapcar '* '(0.5 0.5 0.5) (mapcar '+ p1 p2))
  )
(defun inser_point_elem (obj)
            (vlax-invoke-method obj 'GetBoundingBox 'pt1 'pt2)
            (setq ptg (midpoint (vlax-safearray->list pt1)
                                (vlax-safearray->list pt2)
                      )
            )
            (car ptg)
  )
(defun inser_y_point_elem (obj)
            (vlax-invoke-method obj 'GetBoundingBox 'pt1 'pt2)
            (setq ptg (midpoint (vlax-safearray->list pt1)
                                (vlax-safearray->list pt2)
                      )
            )
            (cadr ptg)
  )
(defun ref_point (ss  / pt )
(setq n (1- (sslength ss)))
      (while (>= n 0)
        (setq en (ssname ss n))
        (setq obj (vlax-ename->vla-object en))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (progn
            (setq am (vla-get-textstring obj))
            (if (vl-string-search "0.00" am)
              (progn
                (setq pt (cdr (assoc 10 (entget en))))
;                (print "Am gasit zero ")
                (setq n 0);iesire din while
                (car pt) ; return
;                (setq poz_km (substr am 1 2))
              )  ;progn
            )    ;if
          )      ;progn
        )    ;if
      (setq n (1- n))
      )
(car pt) ; return
)
(defun lista_profile (ss )
;scoate lista de y din poz km
(setq n (1- (sslength ss)))
      (while (>= n 0)
        (setq en (ssname ss n))
        (setq obj (vlax-ename->vla-object en))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (progn
            (setq am (vla-get-textstring obj))
            (if (vl-string-search "Km" am)
              (progn
                (setq elem (inser_y_point_elem obj))
		        ;(print (rtos elem))
                ;(print (strcat " am_y " (rtos elem)))
                (setq lst_y (cons elem lst_y))
              )  ;progn
            )    ;if
          )      ;progn
        )    ;if
      (setq n (1- n))
      )
;(print (rtos (length lst_y) 2 2))
)
(defun poz_km (ss  / poz_km )
(setq n (1- (sslength ss)))
      (while (>= n 0)
        (setq en (ssname ss n))
        (setq obj (vlax-ename->vla-object en))
;        (print (vla-get-ObjectName obj))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (progn
            (setq am (vla-get-textstring obj))
            (if (vl-string-search "Km" am)
              (progn
                (setq poz (vl-string-search "Km" am))
                (setq n 0);iesire din while 
                (setq poz_km (substr am 5 6)) ; return
;                (print "Am poz km ")
;                (print poz_km)
                )
              )
          )      ;progn
        )    ;if
      (setq n (1- n))
      )
(setq poz_km (substr am 5 6)) ; return
)
; + + + + + ++ + + + ++ + + + ++ + + + ++ + + + ++ + + + ++ + + + ++ + + + ++ + 
(defun save_2_file (indy indx_col / elem fl i j )
  (vl-load-com)
  (setq i 1)
  (setq j 1)
  
	(setq km_start (vlax-safearray-get-element niu_matrics 0 0))
	;(print km_start)
	(setq km_end (vlax-safearray-get-element niu_matrics (- indy 1) 0))
	;(print km_end)
	(if (and (/= km_start "") (/= km_end ""))
		(progn
			  (setq nume_fisier (strcat "C:/tmp/km_" km_end "--" km_start ".txt" ))
			  (setq fl (open nume_fisier  "a"))
			  (princ (strcat " Km; " art1 "- stg; " art2 "- stg; " art1 " -dr; " art2 " - dr " " \n" )  fl)
			  (setq linie 0)
			  
			  (while (< linie indy)
			  	(setq col 0)
			  	(while (<= col 4)
				;(setq stri (vlax-safearray-get-element niu_matrics linie col))
				  	;(print stri)
				  	(princ (vlax-safearray-get-element niu_matrics linie col) fl)
				  	(if (< col 4) (princ ";" fl))
				  	(setq col (+ col 1))
			  	)
				(princ "\n" fl)
				(setq linie (+ linie 1))
			  )
			  
			   (close fl)
			   (print "salvat in ")
			   (print nume_fisier)
		); then
		
		(print " Eroare !!! Nu au fost extrase date "); else
	)
 
)
;+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
(defun C:ts (/ ss i j x_ax km_start fl lst_y)
(vl-load-com)
(command "_layer" "t" "0" "") ;thaw layer 0
(setq osm (getvar "OSMODE")) ;osnap setari
(setvar "OSMODE" 0); osnap zero ca sa nu desenze rectangle cu snap
  (setq art1 "umplutura")
  (setq art2 "frezare")
 
  ;(setq art3 "balast")
  ;(setq art4 "Nisip")
 
 (setq niu_matrics (vlax-make-safearray vlax-vbString '(0 . 50) '(0 . 4)))
 
  (setq i 1)
  (setq j 1)
  (setq indx 0)
 	 (setq ss (ssget '(
						(-4 . "<or")
						; (0 . "POLYLINE")
						; (0 . "LWPOLYLINE")
						; (0 . "HATCH")
						(0 . "Text,Mtext") ; asta are pozitia km
						; (8 . "frezare")    ;art1
						; (8 . "umplutura")  ;art2
						(-4 . "or>")
			   )
     )
   )
(setq x_ax (ref_point ss)) ; extrag ax
(lista_profile ss) ;extrag dim lista cu coord y profile
(setq ss nil)  ; sterg selection se
(setq dim_l (length lst_y))
;(print  dim_l)
;(print (strcat "nr_profile " (rtos dim_l 2 1))) ; prima cifra Mode 2 = decimal
;(print (strcat " coord Y " (rtos coord_y 2 1)))
;(print (strcat "axul " (rtos x_ax 2 1)))
(setq am_poz_km 0)
(setvar 'cmdecho 0)
(setq layer-name "0")
(setvar 'clayer "0")
(command "._-layer" "ON" layer-name "")
; + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
(while (< indx dim_l) do
	(setq coord_y (nth indx lst_y))
	(setq x1 (- x_ax 20.0))
	(setq y1 (- coord_y 1)) ; cobor 1m sub poz km
	(setq x2 (+ x_ax 35.0))
	(setq y2 (+ coord_y 15.0)) ; incadrez 18m din 20m
		
	(setq p1 (list x1 y1 0.0))
	(setq p2 (list x2 y2 0.0))	
	;(command "rectangle" p1 p2 )
	;(setq ss1 (ssget "w" p1 p2))
      (setq ss1 (ssget "w" p1 p2 '(
									(-4 . "<or")
									; (0 . "POLYLINE")
									; (0 . "LWPOLYLINE")
									; (0 . "HATCH")
									(0 . "Text,Mtext") ; asta are pozitia km
									(8 . "frezare")    ; layer art1
									(8 . "umplutura")  ; layer art2
									(-4 . "or>")
			   	                   )
                 )
      )
(if ss1
(progn
(setq km_start " ")
(setq km_start (poz_km ss1)); extrag poz km
(if (/= km_start " ")
	(command "rectangle" p1 p2 )
)
(setq arie_2_stg 0)
(setq arie_2_dr 0)
(setq arie_3_stg 0)
(setq arie_3_dr 0)
      (setq n (1- (sslength ss1)))
      (while (>= n 0)
        ;aici
        (setq en (ssname ss1 n))
        (setq obj (vlax-ename->vla-object en))
        (setq numelayer (vlax-get-property obj 'Layer))
        ;aici art1 
        (if (= numelayer art1)
          (progn
                (setq arie (vlax-get obj 'Area))
;            (print (rtos (inser_point_elem obj) 5 2))
            (if (> x_ax (inser_point_elem obj))
              (progn
;                (print "Stanga")
                (setq arie_2_stg (+ arie arie_2_stg))
                )
              (progn
;                (print "Dreapta")
                (setq arie_2_dr (+ arie arie_2_dr))
                )
              )
          )
        )
 
         ;aici art2
        (if (= numelayer art2)
          (progn
                (setq arie (vlax-get obj 'Area))
;            (print (rtos (inser_point_elem obj) 5 2))
            (if (> x_ax (inser_point_elem obj))
              (progn
;                (print "Stanga")
                (setq arie_3_stg (+ arie arie_3_stg))
                )
              (progn
;                (print "Dreapta")
                (setq arie_3_dr (+ arie arie_3_dr))
                )
              )
          )
        )
       (setq n (1- n))
      )
)
(setq ss1 nil)  ; sterg selection se
)
; populez matrice si apoi incrementez

(vlax-safearray-put-element niu_matrics (+ indx 0) 0 km_start); noua matrice

(vlax-safearray-put-element niu_matrics (+ indx 0) 1 (rtos arie_2_stg 2 2))
(vlax-safearray-put-element niu_matrics (+ indx 0) 2 (rtos arie_3_stg 2 2))
(vlax-safearray-put-element niu_matrics (+ indx 0) 3 (rtos arie_2_dr 2 2))
(vlax-safearray-put-element niu_matrics (+ indx 0) 4 (rtos arie_3_dr 2 2))
(setq indx (+ indx 1))
);while
; aici se pune index max din matrice
(save_2_file indx 4)
(print "refac setari intiale osmode: ")
(setvar "OSMODE" (+ osm 0))
) ;;defun
