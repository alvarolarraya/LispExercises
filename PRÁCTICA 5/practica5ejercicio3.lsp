;;;;hacer una funcion que haga la media de las matrices 2x2 dada la posicion de la primera posicion de la submatriz
;;;;en la matriz grande

(defun hazMediaDeSubmatriz (matriz fila columna)
	(/ (+ (aref *matriz* fila columna) (aref *matriz* (+ 1 fila) columna) (aref *matriz* (+ 1 fila) (+ 1 columna)) (aref *matriz* fila (+ 1 columna))) 4)
)

;;;; TIENE QUE SALIR 3/2
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(hazMediaDeSubmatriz *matriz* 0 0)

;;;; TIENE QUE SALIR 7/2
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(hazMediaDeSubmatriz *matriz* 0 2)

;;;; TIENE QUE SALIR 3/2
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(hazMediaDeSubmatriz *matriz* 2 0)

;;;; TIENE QUE SALIR 7/2
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(hazMediaDeSubmatriz *matriz* 2 2)

;;;;una funcion que vaya saltando de dos en dos los elementos en una fila y haciendo la media de las submatrices 
;;;;y metiendolas en una lista de listas

(defun recorreFila (matriz fila columna dimensionColumna)
	(cond
		((equal dimensionColumna columna) 
			nil
		)
		(t 
			(cons (hazMediaDeSubmatriz matriz fila columna) (recorreFila matriz fila (+ 2 columna) dimensionColumna))
		)
	)
)

;;;; TIENE QUE SALIR (3/2 7/2)
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(recorreFila *matriz* 0 0 4)

;;;; TIENE QUE SALIR (3/2 7/2)
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(recorreFila *matriz* 2 0 4)

(defun reduccionSabiendoDimension (matriz fila dimensionFila dimensionColumna)
	(cond
		((equal dimensionFila fila) 
			nil
		)
		(t 
			(cons (recorreFila *matriz* fila 0 dimensionColumna) (reduccionSabiendoDimension *matriz* (+ 2 fila) dimensionFila dimensionColumna))
		)
	)
)

;;;; TIENE QUE SALIR ((3/2 7/2)(3/2 7/2))
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(reduccionSabiendoDimension *matriz* 0 4 4)

(defun reduccionMatriz (matriz)
	(setf *reducida* (make-array (list (/ (car (array-dimensions *matriz*)) 2) (/ (cadr (array-dimensions *matriz*)) 2)) :initial-contents(reduccionSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;; TIENE QUE SALIR #2A((3/2 7/2)(3/2 7/2))
(setf *matriz* (make-array '(4 4):initial-contents'((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4))))
(reduccionMatriz *matriz*)

