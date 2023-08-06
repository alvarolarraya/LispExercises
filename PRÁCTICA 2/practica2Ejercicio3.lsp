;;;;lo que ha dicho el pavo

;;;esto no esta completo
(defun sustituye-tipo(lista)
	(cond
		((null lista) nil)
		((listp (car lista))
			...
		)
		((numberp (car lista)) (cons 'number sustituye-tipo (cdr lista))
			
		)
)

(defun sustit-elem(elemento)
	(cond
		((numberp elemento)
			'number
		)
		((stringp elemento)
			'string
		)
		((symbolp elemento)
			'symbol
		)
		(t
			(sustituye-tipo elemento)
		)
	)
)

(defun sustituye-2 (lista)
	(mapcar #'sustit-elem lista)
)

;;;en el tercero no hace falta que sea unicamente un mapcar