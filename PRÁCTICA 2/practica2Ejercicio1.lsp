(defun busca-rec (elemento lista nivel)
	(cond 
		((null lista) 0)
		((equal elemento (car lista)) nivel) 
		((listp (car lista))
			(cond
				((and (equal (busca-rec elemento (car lista) (+ nivel 1)) 0) (not (null (cdr lista))))
					(busca-rec elemento (cdr lista) nivel)
				) 
				((equal (busca-rec elemento (car lista) (+ nivel 1)) 0)
					0
				) 
				(t 
					(busca-rec elemento (car lista) (+ nivel 1))
				)
			)
		)
		(t 
			(busca-rec elemento (cdr lista) nivel)
		)
	)
)
;;;;;listp te dice si un elemento es una lista
(busca-rec ‘9 ‘((2) (3 2 5 (4 6 7)) f h (j k (j u 9) j u y) g j u ) ‘1)