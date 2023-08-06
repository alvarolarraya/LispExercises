;;;;;; CALCULA LA SUMA DE LOS ELEMENTOS DE UN VECTOR
(defun suma (lista)
	(cond
		((null lista) 
			0
		)
		(t 
			(+ (suma (cdr lista)) (car lista))
		)
	)
)

;;;;;;;;;; CUENTA CUANTOS ELEMENTOS TIENE UN VECTOR
(defun cuentaElementos (lista)
	(cond
		((null lista) 
			0
		)
		(t 
			(+ (cuentaElementos (cdr lista)) 1)
		)
	)
)

;;;;;; HACE LA MEDIA DE LOS VALORES DE UN VECTOR
(defun avg (lista)
	(/ (suma lista) (cuentaElementos lista))
)

;;;;;;; DEVUELVE UNA CONS CON LA MEDIA DE CADA FILA DE LA MATRIZ
(defun filasMedias (matriz)
	(cond
		((null matriz) 
			nil
		)
		(t 
			(cons (avg (car matriz)) (filasMedias(cdr matriz)))
		)
	)
)

;;;;;; DEVUELVE LA FILA EN LA QUE LA MEDIA DE LOS ELEMENTOS DE ESA FILA ES IGUAL AL PARAMETRO ELEMENTO
(defun queFilaConMedia (filaActual matriz elemento)
	(cond
		((null matriz) 
			nil
		)
		((equal (avg (car matriz)) elemento)
			filaActual
		)
		(t
			(queFilaConMedia (+ 1 filaActual) (cdr matriz) elemento)
		)
	)
)

;;;;;;; DEVUELVE LA SOLUCION AL APARTADO 2
(defun solution2 (matriz)
	 (format t "La fila elegida con la MEDIA es la: ~A" (queFilaConMedia 1 matriz (apply #'max (filasMedias matriz))))
)

;;;;;; CALCULA EL MAXIMO VALOR DE UN VECTOR
(defun maximoFila (fila)
	(apply #'max fila)
)

;;;;;;; DEVUELVE UNA CONS CON EL MAXIMO DE CADA FILA DE LA MATRIZ
(defun filasMaximo (matriz)
	(cond
		((null matriz) 
			nil
		)
		(t 
			(cons (maximoFila (car matriz)) (filasMaximo(cdr matriz)))
		)
	)
)

;;;;;; DEVUELVE LA FILA EN LA QUE EL MAXIMO DE LOS ELEMENTOS DE ESA FILA ES IGUAL AL PARAMETRO ELEMENTO
(defun queFilaConMax (filaActual matriz elemento)
	(cond
		((null matriz) 
			nil
		)
		((equal (maximoFila (car matriz)) elemento)
			filaActual
		)
		(t
			(queFilaConMax (+ 1 filaActual) (cdr matriz) elemento)
		)
	)
)

;;;;;;; DEVUELVE LA SOLUCION AL APARTADO 1
(defun solution1 (matriz)
	 (format t "La fila elegida con el MAXIMO es la: ~A" (queFilaConMax 1 matriz (maximoFila (filasMaximo matriz))))
)

(defun cuantificador (a b r)
	(cond
		((< r a) 
			0
		)
		((<= r b)
			(/ (- r a) (- b a))
		)
		(t
			1
		)
	)
)

;;;;;;; AUXILIAR DE LA SIGUIENTE FUNCION
(defun pesos (dimension a b filaActual)
	(cond
		((equal filaActual (+ 1 dimension)) 
			nil
		)
		(t 
			(cons (- (cuantificador a b (/ filaActual dimension)) (cuantificador a b (/ (- filaActual 1) dimension))) (pesos dimension a b (+ filaActual 1)))
		)
	)
)

;;;;;;; CALCULA LOS PESOS DE UN VECTOR CON DIMENSION CONOCIDA
(defun owa (dimension a b)
	(pesos dimension a b 1)
)

;;;;;; CALCULA EL OWA DE UNA FILA DE DIMENSION CONOCIDA
(defun owaVerdaderoFila (fila a b filaActual dimension)
	(cond
		((equal filaActual dimension) 
			0
		)
		(t 
			(+ (* (nth filaActual fila) (nth filaActual (owa dimension a b))) (owaVerdaderoFila fila a b (+ 1 filaActual) dimension))
		)
	)
)

;;;;;;; CREA UNA CONS CON LOS OWA DE CADA UNA DE LAS FILAS DE LA MATRIZ
(defun owaVerdaderoMatriz (matriz a b)
	(cond
		((equal (car matriz) nil) 
			nil
		)
		(t 
			(cons (owaVerdaderoFila (sort (car matriz) (function >)) a b 0 (cuentaElementos (car matriz))) (owaVerdaderoMatriz (cdr matriz) a b))
		)
	)
)

;;;;;; DEVUELVE LA FILA EN LA QUE EL OWA DE LOS ELEMENTOS DE ESA FILA ES IGUAL AL PARAMETRO ELEMENTO
(defun queFilaConOwa (a b filaActual matriz elemento)
	(cond
		((null matriz) 
			nil
		)
		((equal (owaVerdaderoFila (car matriz) a b 0 (cuentaElementos (car matriz))) elemento)
			filaActual
		)
		(t
			(queFilaConOwa a b (+ 1 filaActual) (cdr matriz) elemento)
		)
	)
)

;;;;;;; DEVUELVE LA SOLUCION AL APARTADO 3
(defun solution3 (matriz)
	(format t "La fila elegida con AL MENOS LA MITAD es la: ~A" (queFilaConOwa 0 0.5 1 matriz (maximoFila (owaVerdaderoMatriz matriz 0 0.5))))
)

;;;;;;; DEVUELVE LA SOLUCION AL APARTADO 4
(defun solution4 (matriz)
	(format t "La fila elegida con a = 0.2, b = 0.6 es la: ~A" (queFilaConOwa 0.2 0.6 1 matriz (maximoFila (owaVerdaderoMatriz matriz 0.2 0.6))))
)