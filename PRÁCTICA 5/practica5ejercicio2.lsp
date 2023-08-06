;;;;; recorrer por columnas una matriz y meterlo en otro array, para ello hacer una funcion que devuelva un vector con una columna de la matriz
;;;;; al tener todas las columnas meterla en otra cons

(defun recorreFila (matriz fila columna dimensionColumna)
	(cond
		((equal dimensionColumna columna) 
			nil
		)
		(t 
			(cons (* (aref *matriz* fila columna) (aref *matriz* fila columna)) (recorreFila matriz fila (+ 1 columna) dimensionColumna))
		)
	)
)

;;;; TIENE QUE SALIR (4 9)
(setf *matriz* (make-array '(2 2):initial-contents'((2 3)(4 5))))
(recorreFila *matriz* 0 0 2)

;;;; TIENE QUE SALIR (16 25)
(setf *matriz* (make-array '(2 2):initial-contents'((2 3)(4 5))))
(recorreFila *matriz* 1 0 2)

;;;;;TIENE QUE SALIR (36 49)
(setf *matriz* (make-array '(3 2):initial-contents'((2 3)(4 5) (6 7))))
(recorreFila *matriz* 2 0 2)

(defun cuadradoSabiendoDimension (matriz fila dimensionFila dimensionColumna)
	(cond
		((equal dimensionFila fila) 
			nil
		)
		(t 
			(cons (recorreFila *matriz* fila 0 dimensionColumna) (cuadradoSabiendoDimension *matriz* (+ 1 fila) dimensionFila dimensionColumna))
		)
	)
)

;;;;;TIENE QUE SALIR ((4 9) (16 25))
(setf *matriz* (make-array '(2 2):initial-contents'((2 3)(4 5))))
(cuadradoSabiendoDimension *matriz* 0 2 2)

;;;;;TIENE QUE SALIR ((4 9) (16 25) (36 49))
(setf *matriz* (make-array '(3 2):initial-contents'((2 3)(4 5) (6 7))))
(cuadradoSabiendoDimension *matriz* 0 3 2)

(defun cuadradoMatriz (matriz)
	(setf *cuadrado* (make-array (array-dimensions *matriz*) :initial-contents(cuadradoSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;;;TIENE QUE SALIR ((4 9) (16 25))
(setf *matriz* (make-array '(2 2):initial-contents'((2 3)(4 5))))
(cuadradoMatriz *matriz*)

;;;;;TIENE QUE SALIR ((4 9) (16 25) (36 49))
(setf *matriz* (make-array '(3 2):initial-contents'((2 3)(4 5) (6 7))))
(cuadradoMatriz *matriz*)