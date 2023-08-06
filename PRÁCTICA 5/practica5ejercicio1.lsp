;;;;; recorrer por columnas una matriz y meterlo en otro array, para ello hacer una funcion que devuelva un vector con una columna de la matriz
;;;;; al tener todas las columnas meterla en otra cons

(defun recorreColumna (matriz fila columna dimensionFila)
	(cond
		((equal dimensionFila fila) 
			nil
		)
		(t 
			(cons (aref *matriz* fila columna) (recorreColumna matriz (+ 1 fila) columna dimensionFila))
		)
	)
)

;;;; TIENE QUE SALIR (a c)
(setf *matriz* (make-array '(2 2):initial-contents'((a b)(c d))))
(recorreColumna *matriz* 0 0 2)

;;;; TIENE QUE SALIR (b d)
(setf *matriz* (make-array '(2 2):initial-contents'((a b)(c d))))
(recorreColumna *matriz* 0 1 2)

(defun traspuestaSabiendoDimension (matriz columna dimensionFila dimensionColumna)
	(cond
		((equal dimensionColumna columna) 
			nil
		)
		(t 
			(cons (recorreColumna *matriz* 0 columna dimensionFila) (traspuestaSabiendoDimension *matriz* (+ 1 columna) dimensionFila dimensionColumna))
		)
	)
)

;;;;;TIENE QUE SALIR ((a c) (b d))
(setf *matriz* (make-array '(2 2):initial-contents'((a b)(c d))))
(traspuestaSabiendoDimension *matriz* 0 2 2)

(defun traspuesta (matriz)
	(setf *traspuesta* (make-array (list (cadr (array-dimensions *matriz*))(car (array-dimensions *matriz*))) :initial-contents(traspuestaSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;;;TIENE QUE SALIR ((a c) (b d))
(setf *matriz* (make-array '(2 2):initial-contents'((a b)(c d))))
(traspuesta *matriz*)

;;;;;TIENE QUE SALIR ((a c e) (b d f))
(setf *matriz* (make-array '(3 2):initial-contents'((a b)(c d)(e f))))
(traspuesta *matriz*)