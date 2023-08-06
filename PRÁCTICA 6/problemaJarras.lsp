;;;; 1)

(defun crearEstado (x y)
	(list x y)
)

;;;; 2)

(defun contenidoJarra4L (estado)
	(car estado)
)

(defun contenidoJarra3L (estado)
	(cadr estado)
)

;;;; 3)

(setf *estado-inicial* (crearEstado 0 0))

;;;; 4)

(defun es-final (estado)
	(= 2 (contenidoJarra4L estado))
)

;;; No, no hacen falta funciones auxiliares

;;;; 5)

(setf *operadores* '(llenarJarra4L llenarJarra3L vaciarJarra4L vaciarJarra3L volcarJarra4L-en-3L volcarJarra3L-en-4L))

;;;; 6)

(defun llenarJarra4L (estado)
	(if (< (contenidoJarra4L estado) 4)
		(crearEstado 4 (contenidoJarra3L estado))
		nil
	)
)

(defun llenarJarra3L (estado)
	(if (< (contenidoJarra3L estado) 3)
		(crearEstado (contenidoJarra4L estado) 3)
		nil
	)
)

(defun vaciarJarra4L (estado)
	(if (> (contenidoJarra4L estado) 0)
		(crearEstado 0 (contenidoJarra3L estado))
		nil
	)
)

(defun vaciarJarra3L (estado)
	(if (> (contenidoJarra3L estado) 0)
		(crearEstado (contenidoJarra4L estado) 0)
		nil
	)
)

(defun volcarJarra3L-en-4L (estado)
	(if (> (contenidoJarra3L estado) 0)
		(if (< (+ (contenidoJarra3L estado) (contenidoJarra4L estado)) 5)
			(crearEstado (+ (contenidoJarra3L estado) (contenidoJarra4L estado)) 0)
			(crearEstado 4 (+ (- (contenidoJarra3L estado) 4) (contenidoJarra4L estado))) 
		)
		nil
	)
)

(defun volcarJarra4L-en-3L (estado)
	(if (> (contenidoJarra4L estado) 0)
		(if (< (+ (contenidoJarra3L estado) (contenidoJarra4L estado)) 4)
			(crearEstado 0 (+ (contenidoJarra3L estado) (contenidoJarra4L estado)))
			(crearEstado (+ (- (contenidoJarra4L estado) 3) (contenidoJarra3L estado)) 3) 
		)
		nil
	)
)

;;;;; verifica

(defun aplica (op estado) (funcall (symbol-function op) estado))

(defun verifica (plan &optional (estado *estado-inicial* ))
	(cond 
		((null estado)
			(format t "~& No permitido ~&") 
			nil
		) 
		((null plan)
			(cond 
				((es-final estado)
					(format t "~& ~a estado final ~&" estado)
					t
				) 
				(t 
					(format t "~& ~a No alcanzado ~&" estado) 
					nil
				)
			)
		) 
		(t 
			(format t "~&~a ~a" estado (car plan))
			(verifica (cdr plan) (aplica (car plan) estado))
		)
	)
)

(setf *operadores* '(llenarJarra4L volcarJarra4L-en-3L vaciarJarra3L volcarJarra4L-en-3L llenarJarra4L volcarJarra4L-en-3L))
(verifica *operadores*)