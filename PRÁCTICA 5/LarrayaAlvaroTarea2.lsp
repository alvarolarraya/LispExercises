;;;;; EJERCICIO 1

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

(defun traspuesta (matriz)
	(setf *traspuesta* (make-array (list (cadr (array-dimensions *matriz*))(car (array-dimensions *matriz*))) :initial-contents(traspuestaSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;;; EJERCICIO 2

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

(defun cuadradoMatriz (matriz)
	(setf *cuadrado* (make-array (array-dimensions *matriz*) :initial-contents(cuadradoSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;;; EJERCICIO 3

(defun hazMediaDeSubmatriz (matriz fila columna)
	(/ (+ (aref *matriz* fila columna) (aref *matriz* (+ 1 fila) columna) (aref *matriz* (+ 1 fila) (+ 1 columna)) (aref *matriz* fila (+ 1 columna))) 4)
)

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

(defun reduccionMatriz (matriz)
	(setf *reducida* (make-array (list (/ (car (array-dimensions *matriz*)) 2) (/ (cadr (array-dimensions *matriz*)) 2)) :initial-contents(reduccionSabiendoDimension *matriz* 0 (car (array-dimensions *matriz*)) (cadr (array-dimensions *matriz*)))))
)

;;;;; EJERCICIO 4

(defun cambiaFilaPrimeraUltima (matriz dimensionFila dimensionColumna)
	(let ((solucion (make-array (list dimensionFila dimensionColumna))))
		(dotimes (i dimensionFila)
			(dotimes (j dimensionColumna)
				(cond
					((= i 0) 
						(setf (aref solucion i j) (aref matriz (- dimensionFila 1) j))
					)
					((= i (- dimensionFila 1))
						(setf (aref solucion i j) (aref matriz 0 j))
					)
					(t 
						(setf (aref solucion i j) (aref matriz i j))
					)
				)
			)
		)
		solucion
	)
)

(defun cambiaColumnaPrimeraUltima (matriz dimensionFila dimensionColumna)
	(let ((solucion (make-array (list dimensionFila dimensionColumna))))
		(dotimes (i dimensionFila)
			(dotimes (j dimensionColumna)
				(cond
					((= j 0) 
						(setf (aref solucion i j) (aref matriz i (- dimensionColumna 1)))
					)
					((= j (- dimensionColumna 1))
						(setf (aref solucion i j) (aref matriz i 0))
					)
					(t 
						(setf (aref solucion i j) (aref matriz i j))
					)
				)
			)
		)
		solucion
	)
)

(defun cambiaColumnaYFilaPrimeraUltima (matriz dimensionFila dimensionColumna)
	(cambiaColumnaPrimeraUltima (cambiaFilaPrimeraUltima matriz dimensionFila dimensionColumna) dimensionFila dimensionColumna)
)

(defun operaElemento (matriz mascara dimensionFila DimensionColumna)
	(let ((resultado 0))
		(dotimes (i dimensionFila)
			(dotimes (j dimensionColumna)
				(setf resultado (+ resultado (* (aref matriz i j) (aref mascara i j))))
			)
		)
		resultado
	)
)

(defun convolucionSabiendoDimensiones (matriz mascara dimensionFila dimensionColumna)
	(let ((solucion (make-array (list dimensionFila dimensionColumna))) (auxiliar (make-array '(3 3))))
		(dotimes (i dimensionFila)
			(dotimes (j dimensionColumna)
				(cond
					((or (= j 0) (= i 0) (= j (- dimensionColumna 1)) (= i (- dimensionFila 1)))
						(setf (aref solucion i j) (aref matriz i j))
					)
					(t 
						(dotimes (k 3)
							(dotimes (l 3)
								(setf (aref auxiliar k l) (aref matriz (+ k (- i 1)) (+ l (- j 1))))
							)
						)
						(setf (aref solucion i j) (operaElemento auxiliar mascara 3 3))
					)
				)
			)
		)
		solucion
	)
)

(defun convolucion (matriz mascara)
	(convolucionSabiendoDimensiones matriz mascara (car (array-dimensions matriz)) (cadr (array-dimensions matriz)))
)