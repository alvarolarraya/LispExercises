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


(suma '(1 2 3 4 5 6))
(suma '(0.1 0.2 0.3))
(cuentaElementos '(0.1 0.2 0.3 0.4))
(suma '(0.1 0.2 0.3 0.4))

(defun avg (lista)
	(/ (suma lista) (cuentaElementos lista))
)

(avg '(0.1 0.2 0.3 0.4 0.2))

;;;;;funcion que crea un cons con las medias de las filas, devuelve un vector de cuatro elementos
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

(filasMedias '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)))

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

;;;;PRIMERA POSICION
(queFilaConMedia 1 '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)) 0.3)
;;;;SEGUNDA POSICION
(queFilaConMedia 1 '((0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4)) 0.3)
;;;;TERCERA POSICION
(queFilaConMedia 1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4)) 0.3)
;;;;CUARTA POSICION
(queFilaConMedia 1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5)) 0.3)

(defun solution2 (matriz)
	 (format t "La fila elegida con la MEDIA es la: ~A" (queFilaConMedia 1 matriz (apply #'max (filasMedias matriz))))
)

;;;;;ELIGE LA PRIMERA FILA
(solution2 '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)))

;;;;;ELIGE LA SEGUNDA FILA
(solution2 '((0.1 0.2 0.4) (0.1 0.3 1) (0.1 0.2 0.4) (0.1 0.2 0.4)))

;;;;;ELIGE LA TERCERA FILA
(solution2 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4)))

;;;;;ELIGE LA CUARTA FILA
(solution2 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5)))

(defun maximoFila (fila)
	(apply #'max fila)
)

;;;;;;DEVUELVE 4
(maximoFila '(1 2 3 4))

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

;;;;DEVUELVE UNA ESTRUCTURA CON EL MAXIMO DE CADA FILA
(filasMaximo '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)))

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

;;;;PRIMERA POSICION
(queFilaConMax 1 '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)) 0.5)
;;;;SEGUNDA POSICION
(queFilaConMax 1 '((0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4)) 0.5)
;;;;TERCERA POSICION
(queFilaConMax 1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4)) 0.5)
;;;;CUARTA POSICION
(queFilaConMax 1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5)) 0.5)

(defun solution1 (matriz)
	 (format t "La fila elegida con el MÁXIMO es la: ~A" (queFilaConMax 1 matriz (maximoFila (filasMaximo matriz))))
)

;;;;;ELIGE LA PRIMERA FILA
(solution1 '((0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4)))

;;;;;ELIGE LA SEGUNDA FILA
(solution1 '((0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4) (0.1 0.2 0.4)))

;;;;;ELIGE LA TERCERA FILA
(solution1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 0.5) (0.1 0.2 0.4)))

;;;;;ELIGE LA CUARTA FILA
(solution1 '((0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.2 0.4) (0.1 0.3 1)))

;;;;;OWA AL MENOS LA MITAD: a = 0 Y b = 0.5

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

;;;;;;DEVUELVE 0.4
(cuantificador 0 0.5 (/ 1.0 5))

;;;;;;DEVUELVE 0.8
(cuantificador 0 0.5 (/ 2.0 5))

;;;;;;DEVUELVE 1
(cuantificador 0 0.5 (/ 3.0 5))

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

;;;;pesos de al menos la mitad de dimension 5
(pesos 5 0 0.5 1)

(defun owa (dimension a b)
	(pesos dimension a b 1)
)

;;;;pesos de al menos la mitad de dimension 5
(owa 5 0 0.5)

;;;; nth devuelve la pos i-esima de una cons
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

;;;; TIENE QUE DEVOLVER 0.88
(owaVerdaderoFila '(1 0.8 0.8 0.5 0.5) 0 0.5 0 5)

;;;;TIENE QUE DEVOLVER 0.566
(owaVerdaderoFila '(0.9 0.4 0.1) 0.2 0.6 0 3)

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

;;;;TIENE QUE DEVOLVER (0.88 0.65 0.74 0.733 0.733 0)
(owaVerdaderoMatriz '((0.5 0.8 0.5 0.8 1) (0.2 0.4167 0.4167 0.4 1) (0.5 0.5 1 0.6 0.2) (1 0.5833 0.5 0.5 0.5) (1 0.5833 0.5 0.5 0.5) (0 0 0 0 0)) 0 0.5)

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

(defun solution3 (matriz)
	(format t "La fila elegida con AL MENOS LA MITAD es la: ~A" (queFilaConOwa 0 0.5 1 matriz (maximoFila (owaVerdaderoMatriz matriz 0 0.5))))
)

;;;;TIENE QUE DEVOLVER LA PRIMERA FILA
(solution3 '((0.5 0.8 0.5 0.8 1) (0.2 0.4167 0.4167 0.4 1) (0.5 0.5 1 0.6 0.2) (1 0.5833 0.5 0.5 0.5) (1 0.5833 0.5 0.5 0.5) (0 0 0 0 0)))

(defun solution4 (matriz)
	(format t "La fila elegida con a = 0.2, b = 0.6 es la: ~A" (queFilaConOwa 0.2 0.6 1 matriz (maximoFila (owaVerdaderoMatriz matriz 0.2 0.6))))
)

;;;;;TIENE QUE SALIR LA FILA 3
(solution4 '((0.2 0.3 0.5) (0.4 0.1 0.6) (0.4 0.1 0.9)))