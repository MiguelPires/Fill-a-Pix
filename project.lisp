;Grupo 5
;73813 Nuno Pereira
;74254 Ana Salta
;76433 Miguel Pires

(load "exemplos.fas")

;************************************************************************************
;*									estrutura restricao  						  	*
;************************************************************************************
;**** Campos ****													   				*
;* resVars - uma lista com as variaveis que participam na restricao    				*
;* resPred - a funcao de validacao usada para determinar se a restricao falha ou nao*
;* resValor - o valor associado 'a restricao no tabuleiro nao resolvido 			*
;* NOTA: o campo resValor so' e' usado para o resolve-best 							*
;************************************************************************************

(defstruct restricao
			resVars
			resPred
			resValor)
			

;************************************************************************************
;*       		          		cria-restricao (vars pred)							*
;************************************************************************************
;**** Argumentos **** 																*	
;* vars - Lista de variaveis que participam numa restricao 							*
;* pred - Predicado aplicado sobre as variaveis para determinar consistencia 		*
;**** Retorno **** 																	*
;* Restricao 																		*
;************************************************************************************
(defun cria-restricao (vars pred)
	(make-restricao :resVars vars :resPred pred))

;************************************************************************************
;*          		       restricao-variaveis (res) : vars 						*
;************************************************************************************
;**** Argumentos **** 																*
;* res - Restricao 																	*
;**** Retorno **** 																	*
;* Lista de variaveis 																*
;************************************************************************************
(defun restricao-variaveis (res)
	(restricao-resVars res))

;************************************************************************************
;*                 		restricao-funcao-validacao (res) : predicado 				*
;************************************************************************************
;**** Argumentos **** 																*
;* res - Restricao 																	*
;**** Retorno **** 																	*
;* Lista de variaveis 																*
;************************************************************************************
(defun restricao-funcao-validacao (res)
	(restricao-resPred res))

;************************************************************************************
;*									estrutura psr  					 				*
;************************************************************************************
;**** Campos **** 																	*
;* psrListaRes - uma lista todas as restricoes do PSR 								*
;* psrVars - uma hash table com as variaveis como chave e os valores como valor 	*
;* psrDoms - uma hash table com as variaveis como chave e os dominios como valor 	*
;* psrRes - uma hash table com as variaveis como chave e as restricoes como valor 	*
;************************************************************************************
(defstruct psr
			psrListaRes
			psrVars
			psrDoms
			psrRes)

;************************************************************************************
;*                 cria-psr (listaVars listaDoms listaRes) : psr 					*
;************************************************************************************
;**** Argumentos **** 																*
;* listaVars - lista de variaveis 													*
;* listaDoms - lista de dominios 													*
;* listaRes - lista de restricoes 													*
;**** Retorno **** 																	*
;* PSR 																				*
;************************************************************************************
(defun cria-psr (listaVars listaDoms listaRes)
	(let ((hashVars (make-hash-table :test 'equal))
			(hashRes (make-hash-table :test 'equal))
			(hashDom (make-hash-table :test 'equal))
			(pos 0))
			(dolist (var listaVars)
				(setf (gethash var hashVars) nil)
				(setf (gethash var hashDom) (nth pos listaDoms))
				(incf pos))
			(dolist (res listaRes)
				(dolist (var (restricao-variaveis res))
					(if (null (second (hashValues hashRes var)))
						(setf (gethash var hashRes) (list res))
						(setf (gethash var hashRes) (append (gethash var hashRes) (list res))))))		
			(make-psr :psrListaRes listaRes :psrVars hashVars :psrDoms hashDom :psrRes hashRes)))

;************************************************************************************
;*                 			psr-atribuicoes (psr) : vars 							*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;**** Retorno **** 																	*
;* Lista das atribuicoes no formato (var . val) 									*
;************************************************************************************
(defun psr-atribuicoes (psr)
	(let ((lista nil))
		(maphash 
			#'(lambda (var val)
				(if val	
					(push (cons var val) lista)))
			(psr-psrVars psr))	
		lista))

;************************************************************************************
;*    		             psr-variaveis-todas (psr) : vars 							*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;**** Retorno **** 																	*
;* Lista de todas as variaveis 														*
;************************************************************************************
(defun psr-variaveis-todas (psr)
	(let ((lista nil))
		(maphash 
			#'(lambda (var val)
				(declare (ignore val))
				(push var lista))
			(psr-psrVars psr))	
		lista))

;************************************************************************************
;*                 psr-variaveis-nao-atribuidas (psr) : vars 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;**** Retorno **** 																	*
;* Lista de variaveis nao atribuidas 												*
;************************************************************************************
(defun psr-variaveis-nao-atribuidas (psr)
	(let ((lista nil))
		(maphash 
		#'(lambda (var val)
			(if (null val)
				(push var lista))) 
		(psr-psrVars psr))	
		lista))

;************************************************************************************
;*                 		psr-variavel-valor (psr var) : valor 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* Valor da variavel (nil se nao estiver atribuida) 								*
;************************************************************************************
(defun psr-variavel-valor (psr var)
	(first (hashValues (psr-psrVars psr) var)))

;************************************************************************************
;*	                 psr-variavel-dominio (psr var) : dominio 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* Dominio da variavel (lista de valores) 											*
;************************************************************************************
(defun psr-variavel-dominio (psr var)
	(first (hashValues (psr-psrDoms psr) var)))

;************************************************************************************
;*                 psr-variavel-restricoes (psr var) : restricoes 					*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* Lista de restricoes em que a variavel participa 									*
;************************************************************************************
(defun psr-variavel-restricoes (psr var)
	(first (hashValues (psr-psrRes psr) var)))

;************************************************************************************
;*                 psr-adiciona-atribuicao! (psr var valor) : valor 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;* valor - valor a atribuir 														*
;**** Retorno **** 																	*
;* Valor que foi atribuido a variavel 												*
;************************************************************************************
(defun psr-adiciona-atribuicao! (psr var valor)
	(setf (gethash var (psr-psrVars psr)) valor))

;************************************************************************************
;*                	 psr-altera-dominio! (psr var dom) : dom 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;* dom - dominio a atribuir a variavel 												*
;**** Retorno **** 																	*
;* Dominio que foi atribuido a variavel 											*
;************************************************************************************
(defun psr-altera-dominio! (psr var dom)
	(setf (gethash var (psr-psrDoms psr)) dom))

;************************************************************************************
;*	                 psr-remove-atribuicao! (psr var) : nil 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* nil 																				*
;************************************************************************************
(defun psr-remove-atribuicao! (psr var)
	(psr-adiciona-atribuicao! psr var nil))

;************************************************************************************
;* 			                psr-completo-p (psr) : T / nil 							*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;**** Retorno **** 																	*
;* T se o psr estiver completo, nil caso contrario 									*
;************************************************************************************
(defun psr-completo-p (psr)
	(let ((lst (psr-variaveis-nao-atribuidas psr)))
		(if (null lst) 
			T NIL)))

;************************************************************************************
;*	                 psr-consistente-p (psr) : (T/nil testes) 						*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;**** Retorno **** 																	*
;* T se o psr for consistente, nil caso contrario 									*
;* Numero de testes de consistencia executados 										*
;************************************************************************************
(defun psr-consistente-p  (psr)
	(let ((testes 0)
		(resposta T)
		(listaRes (psr-psrListaRes psr)))
		(dolist (res listaRes)
			(incf testes)
			(when (not (funcall (restricao-funcao-validacao res) psr))
				(setf resposta NIL) (return)))
		(values resposta testes)))

;************************************************************************************
;*               psr-variavel-consistente-p (psr var) : (T/nil testes) 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* T se as restricoes em que a variavel participa forem consistentes, 				*
;*nil caso contrario 																*
;* Numero de testes de consistencia feitos 											*
;************************************************************************************
(defun psr-variavel-consistente-p (psr var)
	(let ((testes 0)
		(resposta T)
		(listaRes (psr-variavel-restricoes psr var)))
		(dolist (res listaRes)
			(incf testes)
			(when (not (funcall (restricao-funcao-validacao res) psr))
				(setf resposta nil)
				(return)))
		(values resposta testes)))

;************************************************************************************
;*         psr-atribuicao-consistente-p (psr var valor) : (T/nil testes) 			*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var - variavel 																	*
;* valor - valor a atribuir 														*
;**** Retorno **** 																	*
;* T se a atribuicao 'a variavel for consistente, nil caso contrario 				*
;* Numero de testes de consistencia feitos 											*
;************************************************************************************
(defun psr-atribuicao-consistente-p (psr var valor)
	(let ((oldValue (psr-variavel-valor psr var))
		(returnValues nil))
		(psr-adiciona-atribuicao! psr var valor) ;atribuir novo valor a variavel
		(setf returnValues (multiple-value-bind 
								(success tests) 
								(psr-variavel-consistente-p psr var) 
								(list success tests))) 
		(psr-adiciona-atribuicao! psr var oldValue) ;atribuir valor antigo a variavel
	(values (first returnValues) (second returnValues))))

;************************************************************************************
;*psr-atribuicoes-consistentes-arco-p (psr var1 value1 var2 value2) : (T/nil testes)*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var1 - 1a variavel 																*
;* valor1 - 1o valor 																*
;* var2 - 2a variavel 																*
;* valor2 - 2o valor 																*
;**** Retorno **** 																	*
;* T se as atribuicoes 'as variaveis forem consistentes, nil caso contrario 		*
;* Numero de testes de consistencia feitos 											*	
;************************************************************************************
(defun psr-atribuicoes-consistentes-arco-p (psr var1 valor1 var2 valor2)
	(let ((testes 0)
		  (resposta T)
		  (oldValue1 (psr-variavel-valor psr var1)) ;guardar valores antigos das variaveis
		  (oldValue2 (psr-variavel-valor psr var2)))
		(psr-adiciona-atribuicao! psr var1 valor1) ;atribuir novos valores as variaveis
		(psr-adiciona-atribuicao! psr var2 valor2)
		(dolist (res (psr-variaveis-restricoes psr var1 var2)) ;percorrer lista de restricoes que contenham ambas as variaveis
			(cond 
				((funcall (restricao-funcao-validacao res) psr) (incf testes)) ;verificar restricoes
				(T (incf testes) (setf resposta nil) (return))))

		(psr-adiciona-atribuicao! psr var1 oldValue1) ;atribuir valores antigos as variaveis
		(psr-adiciona-atribuicao! psr var2 oldValue2)
		(values resposta testes)))

;************************************************************************************
; 								funcoes auxiliares psr 								*
;************************************************************************************

;************************************************************************************
;*              psr-variaveis-restricoes (psr var1 var2) : (restricoes) 			*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* var1 - 1a variavel 																*
;* var2 - 2a variavel 																*
;**** Retorno **** 																	*
;* Lista de restricoes partilhadas por ambas as variaveis 							*
;************************************************************************************
(defun psr-variaveis-restricoes (psr var1 var2)
	(let ((lst NIL)
		(listaRes1 (psr-variavel-restricoes psr var1)) ;restricoes da 1a variavel
		(listaRes2 (psr-variavel-restricoes psr var2))) ;restricoes da 2a variavel
		(dolist (res1 listaRes1)
			(dolist (res2 listaRes2)
				(if (equal res1 res2) ;ao encontrar uma restricao onde se encontram as duas variaveis		
					(setf lst (append lst (list res1)))))) ;adiciona'-la 'a lista de restricoes partilhadas pelas variaveis
		lst))
	
;************************************************************************************
;*                 cria-predicado (valor vars) : lambda 							*
;************************************************************************************
;**** Argumentos **** 																*
;* valor - valor da restricao 														*
;* vars - variaveis da restricao 													*
;**** Retorno **** 																	*
;* Funcao de validacao da restricao 												*
;************************************************************************************
(defun cria-predicado (valor vars)
	#'(lambda (psr) 
		(let ((returnValues nil))

		(setf returnValues (multiple-value-bind
							(sum cont)
							(soma-vars psr vars)
							(list sum cont)))
		(and (>= (+ (first returnValues) (second returnValues)) valor) (<= (first returnValues) valor)))))  

;************************************************************************************
;*                 soma-vars (psr vars) : contador 									*
;************************************************************************************
;**** Argumentos **** 																*
;* psr - PSR 																		*
;* vars - variaveis de uma restricao 												*
;**** Retorno **** 																	*
;* Soma do numero de variaveis nao atribuidas 										*
;* Soma dos valores das variaveis atribuidas 										*
;************************************************************************************
(defun soma-vars (psr vars)
	(let ((cont 0)
		(sum 0))

		(dolist (var vars)
			(if (psr-variavel-valor psr var)
				(incf sum (psr-variavel-valor psr var))
				(incf cont)))

		(values sum cont)))

;************************************************************************************
; 								Funcoes	Fill a Pix 									*
;************************************************************************************

;************************************************************************************
;*		                 fill-a-pix->psr (array) : psr 								*
;************************************************************************************
;**** Argumentos **** 																*
;* array - tabuleiro fill-a-pix (array bidimensional) 								*
;**** Retorno **** 																	*
;* PSR correspondente ao tabuleiro inicial 											*
;************************************************************************************
(defun fill-a-pix->psr (array)
	(let* ((lines (array-dimension array 0))
			(columns (array-dimension array 1))
			(psr nil)
			(listaRes nil)	; lista de restricoes para o psr
			(resVars nil)	; variaveis da restricao
			(listaVars)		; lista de variaveis para o psr
			(pred nil)
			(listaDoms (make-list (* lines columns) :initial-element '(0 1)))) ;inicializacao dos dominios a (0 1)
			(dotimes (nline lines)
				(dotimes (index columns)
						(when (aref array nline index)
								(setf resVars (cria-lista array index nline)) ;procurar e adicionar as variaveis de uma restricao
								(setf pred (cria-predicado (aref array nline index) resVars))
								(setf listaRes (append listaRes (list (cria-restricao resVars pred)))))
						(setf listaVars (append listaVars (list (write-to-string (+ (* nline columns) index)))))))
			(setf psr (cria-psr listaVars listaDoms listaRes))))

;************************************************************************************
;*               	  psr->fill-a-pix (psr linhas colunas) : array 					*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* linhas 																			*
;* colunas 																			*
;**** Retorno **** 																	*
;* Tabuleiro final preenchido com 0's e 1's 										*
;************************************************************************************
(defun psr->fill-a-pix (psr linhas colunas)
	(let ((cont 0)
		(f 0) ;posicao de uma variavel no array
		(tabuleiro nil)) 

		(setf tabuleiro (make-array (list linhas colunas) :initial-element 0)) ;colocar a solucao com todas as variaveis a 0
		(dolist (element (psr-variaveis-todas psr))
			(when (eq (psr-variavel-valor psr element) 1) ;ao encontrar uma atribuicao a 1
				(setf f (multiple-value-bind (quotient remainder) (floor (/ cont colunas)) (list quotient remainder))) ;calcular a posicao da variavel no array
				(setf (aref tabuleiro (first f) (* (second f) colunas)) 1)) ;alterar o valor da variavel no array para 1
			(incf cont))
		tabuleiro))

;************************************************************************************
;* 							Funcoes auxiliares Fill a Pix 							*
;************************************************************************************

;************************************************************************************
;*		                cria-lista (array index linha) : vars 						*
;************************************************************************************
;**** Argumentos **** 																*
;* array 																			*
;* index 																			*
;* linha 																			*
;**** Retorno **** 																	*
;* Lista de variaveis presentes na restricao 										*
;************************************************************************************
(defun cria-lista (array index linha)
	(let* ((linhas (array-dimension array 0))
			(colunas (array-dimension array 1))
			(vars nil)
			(listaLinhas nil)
			(listaColunas nil))

			(cond ((equal index 0) (setf listaColunas '(0 1))) ;primeira coluna
				  ((equal index (- colunas 1)) (setf listaColunas '(-1 0))) ;ultima coluna
				  (T (setf listaColunas '(-1 0 1)))) ;coluna que nao seja nem a primeira nem a ultima

			(cond ((equal linha 0) (setf listaLinhas '(0 1))) ;primeira linha
				  ((equal linha (- linhas 1)) (setf listaLinhas '(-1 0))) ;ultima linha
				  (T (setf listaLinhas '(-1 0 1))))  ;linha que nao seja nem a primeira nem a ultima

			(dolist (i listaLinhas)
				(dolist (e listaColunas)
						(setf vars (append vars (list (write-to-string (+ (* (+ linha i) colunas) (+ index e)))))))) ;atribuir nomes 'as variaveis

		vars))

;************************************************************************************
;* 									Procuras 										*
;************************************************************************************

;************************************************************************************
;*                 -retrocesso-simples (psr) : (psr/nil testes) 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Um psr se a procura teve sucesso, nil caso contrario, e o numero de testes feitos*
;************************************************************************************
(defun procura-retrocesso-simples (psr)
	(let ((resultado nil)
	      (nao-atrib nil)
	      (testesTotais 0)
	      (valores nil))

		(if (psr-completo-p psr) 
			(setf resultado psr))

	  	(setf nao-atrib (first (psr-variaveis-nao-atribuidas psr)))
		(dolist (el (psr-variavel-dominio psr nao-atrib))
			(setf valores (multiple-value-bind (consist-p testes) (psr-atribuicao-consistente-p psr nao-atrib el) (list consist-p testes)))
			(incf testesTotais (second valores)) ;adicionar, aos testesTotais, os testes resultantes da verificacao de consistencia 

			(when (first valores) ;se a atribuicao for consistente
				(psr-adiciona-atribuicao! psr nao-atrib el)
				(setf valores (multiple-value-bind (psr testes) (procura-retrocesso-simples psr) (list psr testes)))
				(setf resultado (first valores)) ;resultado da procura-retrocesso-simples: psr ou nil
				(incf testesTotais (second valores)) ;adicionar, aos testesTotais, os testes resultantes da procura-retrocesso-simples

				(if resultado 
					(return-from procura-retrocesso-simples (values resultado testesTotais)))

				(psr-remove-atribuicao! psr nao-atrib)))

		(values resultado testesTotais)))

;************************************************************************************
;*                procura-retrocesso-grau (psr) : (psr/nil testes) 					*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Um psr se a procura teve sucesso, nil caso contrario, e o numero de testes feitos*
;************************************************************************************
(defun procura-retrocesso-grau (psr)
	(let ((resultado nil)
	      (nao-atrib nil)
	      (testesTotais 0)
	      (valores nil))

		(if (psr-completo-p psr)  
			(setf resultado psr))

		(setf nao-atrib (ordena-grau psr))
		(dolist (el (psr-variavel-dominio psr nao-atrib))
			(setf valores (multiple-value-bind (consist-p testes) (psr-atribuicao-consistente-p psr nao-atrib el) (list consist-p testes)))
			(incf testesTotais (second valores)) ;adicionar, aos testesTotais, os testes resultantes da verificacao de consistencia

			(when (first valores) ;se a atribuicao for consistente
				(psr-adiciona-atribuicao! psr nao-atrib el)
				(setf valores(multiple-value-bind (psr testes) (procura-retrocesso-grau psr) (list psr testes)))
				(setf resultado (first valores)) ;resultado da procura-retrocesso-grau: psr ou nil
				(incf testesTotais (second valores)) ;adicionar, aos testesTotais, os testes resultantes da procura-retrocesso-grau

				(when resultado 
					(return-from procura-retrocesso-grau (values resultado testesTotais)))

				(psr-remove-atribuicao! psr nao-atrib)))

		(values resultado testesTotais)))

;************************************************************************************
;*                procura-retrocesso-fc-mrv (psr) : (psr/nil testes) 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Um psr se a procura teve sucesso, nil caso contrario 							*
;* O numero de consistencia testes feitos 											*
;************************************************************************************
(defun procura-retrocesso-fc-mrv (psr)
	(let ((testesTotais 0)
		(var nil)
		(returnValues nil)
		(oldDoms (make-hash-table :test 'equal))) ;dominios anteriores a serem alterados

		(if (psr-completo-p psr)  
			(return-from procura-retrocesso-fc-mrv 
				(values psr testesTotais)))

		(setf var (MRV psr)) ;escolher a proxima variavel usando a heuristica MRV
		(dolist (val (psr-variavel-dominio psr var))
			(setf returnValues (multiple-value-bind 
								(sucesso testes)
								(psr-atribuicao-consistente-p psr var val)
								(list sucesso testes)))
			(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante da verificacao de consistencia

			(when (first returnValues) ;se a atribuicao for consistente
				(psr-adiciona-atribuicao! psr var val)
				(setf returnValues (multiple-value-bind 
									(inf testes sucesso)
									(forward-checking psr var)
									(list inf testes sucesso)))
				(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante do forward-checking

				(when (third returnValues) ;se o forward-checking teve sucesso
					(maphash #'(lambda (var dom)
						(setf (gethash var oldDoms) (psr-variavel-dominio psr var)) ;guardar os dominios actuais
						(psr-altera-dominio! psr var dom)) (first returnValues)) ;alterar os dominios para os retornados pelo forward-checking

					(setf returnValues (multiple-value-bind 
								(resultado testes)
								(procura-retrocesso-fc-mrv psr)
								(list resultado testes)))
					(incf testesTotais (second returnValues)) ;adicionar aos testesTotais o numero de testes resultante da procura-retrocesso-fc-mrv

					(when (first returnValues) ;se for retornado um psr
						(return-from procura-retrocesso-fc-mrv (values (first returnValues) testesTotais))) ;retornar da funcao com o psr e os testesTotais
					
					(maphash #'(lambda (var dom)
						(psr-altera-dominio! psr var dom)) oldDoms) ;voltar a atribuir os dominios guardados as variaveis
					(setf oldDoms (make-hash-table :test 'equal)))

				(psr-remove-atribuicao! psr var)))
			(values nil testesTotais)))

;************************************************************************************
;*                procura-retrocesso-MAC-MRV (psr) : (psr/nil testes) 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Um psr se a procura encontrou uma solucao, nil caso contrario 					*
;* O numero de testes de consistencia feitos 										*
;************************************************************************************
(defun procura-retrocesso-MAC-mrv (psr)
	(let ((testesTotais 0)
		(var nil)
		(returnValues nil)
		(oldDoms (make-hash-table :test 'equal))) ;dominios anteriores a serem alterados

		(if (psr-completo-p psr)  
			(return-from procura-retrocesso-MAC-mrv (values psr testesTotais)))

		(setf var (MRV psr)) ;escolher a proxima variavel usando a heuristica MRV
		(dolist (val (psr-variavel-dominio psr var))
			(setf returnValues (multiple-value-bind 
								(sucesso testes)
								(psr-atribuicao-consistente-p psr var val)
								(list sucesso testes)))
			(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante da verificacao de consistencia

			(when (first returnValues) ;se a atribuicao for consistente
				(psr-adiciona-atribuicao! psr var val)
				(setf returnValues (multiple-value-bind 
									(inf testes sucesso)
									(MAC psr var)
									(list inf testes sucesso)))
				(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante do MAC

				(when (third returnValues) ;se o MAC tiver sucesso
					(maphash #'(lambda (var dom)
						(setf (gethash var oldDoms) (psr-variavel-dominio psr var)) ;guardar os dominios actuais
						(psr-altera-dominio! psr var dom)) (first returnValues)) ;alterar os dominios para os retornados pelo MAC

					(setf returnValues (multiple-value-bind 
								(resultado testes)
								(procura-retrocesso-MAC-mrv psr)
								(list resultado testes)))
					(incf testesTotais (second returnValues)) ;adicionar aos testesTotais o numero de testes resultante da procura-retrocesso-MAC-mrv

					(if (first returnValues) 
						(return-from procura-retrocesso-MAC-mrv (values (first returnValues) testesTotais))) ;retornar da funcao com o psr e os testesTotais

					(maphash #'(lambda (var dom)
						(psr-altera-dominio! psr var dom)) oldDoms)  ;voltar a atribuir os dominios guardados as variaveis
					(setf oldDoms (make-hash-table :test 'equal)))

				(psr-remove-atribuicao! psr var)))
			(values nil testesTotais)))

;************************************************************************************
;* 							funcoes auxiliares procuras 							*
;************************************************************************************

;************************************************************************************
;*                    revise (psr varX varY inf) : (T/nil testes hashTables) 		*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* T se a variavel varX foi revista e o seu dominio foi alterado, nil caso contrario*
;* O numero de testes de consistencia feitos 										*
;* Uma hashTable com o novo dominio inferido para a varX 							*
;************************************************************************************
(defun revise (psr varX varY inf)
	(let ((testesTotais 0)
		(revised nil)
		(dom-x nil)
		(novo-dom-x nil)
		(dom-y nil)
		(returnValues nil)
		(foundConsistentValue nil))

		(setf returnValues (hashValues inf varX))

		(if (first returnValues) ;se a variavel x tiver um dominio nas inferencias
			(setf dom-x (first returnValues)) ;o dominio de x passa a ser o que se encontra nas inferencias
			(setf dom-x (psr-variavel-dominio psr varX))) ;caso contrario, passa a ser o que se encontra no psr

		(setf novo-dom-x dom-x)
		(setf returnValues (hashValues inf varY))

		(cond ((psr-variavel-valor psr varY) (setf dom-y (list (psr-variavel-valor psr varY))))
			((first returnValues) (setf dom-y (first returnValues))) ;o dominio da variavel y passaa ser o que se encontra nas inferencias se tal existir 
			(T (setf dom-y (psr-variavel-dominio psr varY))))

		(dolist (valorX dom-x)
			(setf foundConsistentValue nil)
			(dolist (valorY dom-y)
				(setf returnValues (multiple-value-bind 
										(consistente testes) 
										(psr-atribuicoes-consistentes-arco-p psr varX valorX varY valorY) 
										(list consistente testes)))
				(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultantes da verificacao de consistencia

				(when (first returnValues) ;se o teste de consistencia tiver sucesso
					(setf foundConsistentValue T)
					(return)))

			(when (null foundConsistentValue) ;nenhum valor de y e' consistente com a atribuicao de x
				(setf revised T)
				(setf novo-dom-x (remove valorX novo-dom-x))))

		(if revised
			(setf (gethash varX inf) novo-dom-x))

		(values revised testesTotais inf)))

;************************************************************************************
;*                forward-checking (psr var) : (hashTable testes T/nil) 			*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var - Variavel 																	*
;**** Retorno **** 																	*
;* Uma hash table com as inferencias, nil se a variavel nao e' consistente 			*
;* O numero de testes de consistencia feitos 										*
;* T se a atribuicao para a variavel e' consistente, nil caso contrario 			*
;************************************************************************************
(defun forward-checking (psr var)
	(let ((testesTotais 0)
		(inf (make-hash-table :test 'equal)) ;hash-table para guardar as inferencias
		(listaArcos (arcos-vizinhos-nao-atribuidos psr var))
		(returnValues nil))

		(dolist (arco listaArcos)
			(setf returnValues (multiple-value-bind 
				(revised testes inf) 
				(revise psr (first arco) (second arco) inf) 
				(list revised testes inf)))
			(setf inf (third returnValues)) ;alterar as inferencias para as devolvidas pelo revise
			(incf testesTotais (second returnValues)) ;adicionar aos testesTotais o numero de testes resultante do revise

			(if (and (first returnValues) (equal 0 (length (first (hashValues inf (first arco)))))) ;se foi revised e o dominio da primeira variavel do arco estiver vazio na hash-table das inferencias
				(return-from forward-checking (values nil testesTotais nil)))) ;o forward-checking falhar

		(values inf testesTotais T)))

;************************************************************************************
;*                MAC (psr var) : (inferencias/nil testes sucesso/nil) 				*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var -variavel 																	*
;**** Retorno **** 																	*
;* Uma hash table com inferencias, nil caso a variavel nao seja consistente 		*
;* O numero de testes de consistencia feitos 										*
;* T se a variavel e' consistente, nil caso contrario 								*
;************************************************************************************
(defun MAC (psr var)
	(let* ((testesTotais 0)
		(inf (make-hash-table :test 'equal))  ;hash-table para guardar as inferencias
		(listaArcos (arcos-vizinhos-nao-atribuidos psr var))
		(returnValues nil)
		(novosArcos nil)
		(cont 0)
		(max (length listaArcos))
		(arco nil))

		(loop 
			(if (eq cont max) ;se ja viu todos os arcos da listaArcos
				(return)) ;retorna da funcao

			(setf arco (nth cont listaArcos))
			(setf returnValues (multiple-value-bind 
				(revised testes inf) 
				(revise psr (first arco) (second arco) inf) 
				(list revised testes inf)))
			(setf inf (third returnValues)) ;alterar as inferencias para as devolvidas pelo revise
			(incf testesTotais (second returnValues)) ;adicionar aos testesTotais o numero de testes resultante do revise
			(when (first returnValues) 
				(if (equal 0 (length (first (hashValues inf (first arco))))) ;se foi revised e o dominio da primeira variavel do arco estiver vazio na hash-table das inferencias
					(return-from MAC (values nil testesTotais nil))) ;o MAC falha

				(setf novosArcos (arcos-vizinhos-nao-atribuidos psr (first arco))) 
				(setf novosArcos (remove (list (second arco) (first arco)) novosArcos :test #'equal))
				(setf listaArcos (remove-duplicates (append listaArcos novosArcos) :test #'equal))) ;adicionar 'a lista de arcos, os arcos-vizinhos-nao-atribuidos da primeira variavel do arco

			(incf cont)
			(setf max (length listaArcos)))

		(values inf testesTotais T)))

;************************************************************************************
;*                arcos-vizinhos-nao-atribuidos (psr var) : arcos 					*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var - Variavel 																	*
;**** Retorno **** 																	*
;* Uma lista sem duplicados de arcos (var1 var2) 									*
;************************************************************************************
(defun arcos-vizinhos-nao-atribuidos (psr var)
	(let ((arcos ()))

		(dolist (nao-atribuida (psr-variaveis-nao-atribuidas psr)) ;para cada variavel nao atribuida do psr
			(if (not (equal var nao-atribuida)) ;se a variavel nao for a variavel var
				(when (not (equal (length (psr-variaveis-restricoes psr var nao-atribuida)) 0)) ;se as duas variaveis tiverem restricoes em comum
					(setf arcos (append arcos (list (list nao-atribuida var))))))) ;adiciona o arco (nao-atribuida var) 'a lista de arcos
		(remove-duplicates arcos :test #'equal))) ;garantir que a lista de arcos nao tem duplicados


;************************************************************************************
;*							Heuristicas para variaveis 								*
;************************************************************************************

;************************************************************************************
;*			                    MRV (psr) : variavel 								*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Variavel nao atribuida com menor valores no dominio 								*
;************************************************************************************
(defun MRV (psr)
	(let* ((lista (psr-variaveis-nao-atribuidas psr))
		(minVar (first lista)))

		(dolist (var lista) ;alterar a minVar para var, caso o dominio da var seja menor que o dominio da minVar
			(if (< (length (psr-variavel-dominio psr var)) (length (psr-variavel-dominio psr minVar)))
				(setf minVar var)))
		minVar))

;************************************************************************************
;*		                    ordena-grau (psr) : variavel 							*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Variavel nao atribuida envolvida no maior numero de restricoes 					*
;************************************************************************************
(defun ordena-grau (psr)
	(let* ((lista (psr-variaveis-nao-atribuidas psr))
		(maxVar (first lista)))

		(dolist (var lista) ;alterar a maxVar para var, caso o numero de restricoes nao atribuidas da var seja maior que o da maxVar
			(if (> (restricoes-nao-atribuidas psr var) (restricoes-nao-atribuidas psr maxVar))
				(setf maxVar var)))
		maxVar))

;************************************************************************
;*                 MRV-com-grau (psr) : variavel
;************************************************************************
;**** Argumentos ****
;* psr
;**** Retorno ****
;* Variavel nao atribuida com menor dominio
;* NOTA: Entre duas variaveis com dominios do mesmo tamanho sera escolhida 
;* a que tiver o maior numero de restricoes
;************************************************************************
(defun MRV-com-grau (psr)
	(let* ((lista (psr-variaveis-nao-atribuidas psr))
		(minVar (first lista)))

		(dolist (var lista)
			(cond ((< (length (psr-variavel-dominio psr var)) (length (psr-variavel-dominio psr minVar))) (setf minVar var)) ;alterar a minVar para var, caso o dominio da var seja menor que o dominio da minVar
					((and (= (length (psr-variavel-dominio psr var)) (length (psr-variavel-dominio psr minVar))) ;em caso de empate
					(> (restricoes-nao-atribuidas psr var) (restricoes-nao-atribuidas psr minVar))) (setf minVar var)))) ;;alterar a minVar para var, caso o numero de restricoes nao atribuidas da var seja maior que o da minVar
		minVar))


;************************************************************************************
;*							Heuristicas para valores 								*
;************************************************************************************

;************************************************************************************
;*			                    LCV (psr var) : valor 								*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* Valor do dominio da variavel que elimina menos valores do dominio das outras 	*
;* variaveis 																		*
;************************************************************************************
(defun LCV (psr var)
	(let ((bestVal nil) ;valor que elimina menor valores do dominio de outras variaveis
		(varDom (psr-variavel-dominio psr var)) ;dominio da variavel
		(bestCont -1) ;numero de dominios alterados do melhor valor
		(cont 0)
		(testes 0)
		(returnValues nil))

		(dolist (val varDom) ;para cada valor do dominio da variavel
			(psr-adiciona-atribuicao! psr var val)
			(setf returnValues (multiple-value-bind ;fazer inferencia sobre os dominios das outras variaveis
						(inf testes success) 
						(MAC psr var) 
						(list inf testes success)))
			(incf testes (second returnValues)) ;somar os testes totais com os testes resultantes da inferencia

			(when (third returnValues) ;se houve novas inferencias
				(maphash #'(lambda (var dom)
						(declare (ignore var dom))
						(incf cont)) ;incrementar o numero de dominios alterados ao atribuir o valor
					(first returnValues))

				(when (or (< cont bestCont) (eq bestCont -1)) ;se o numero de dominios alterados for menor que o do melhor valor ou ainda nao existir um melhor valor
					(setf bestVal val) ;alterar o melhor valor para o actual
					(setf bestCont cont))) ;alterar o numero de dominios alterados do melhor valor para o actual

			(setf cont 0))
		(psr-remove-atribuicao! psr var)
		(values bestVal testes)))

;************************************************************************************
;* 							funcoes auxiliares heuristicas 							*
;************************************************************************************

;************************************************************************************
;*                restricoes-nao-atribuidas (psr var1) : contador 					*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;* var - variavel 																	*
;**** Retorno **** 																	*
;* Numero de restricoes com variaveis nao atribuidas 								*
;************************************************************************************
(defun restricoes-nao-atribuidas (psr var1)
	(let ((cont 0)
		(listaRes (psr-variavel-restricoes psr var1))
		(resVars nil))

		(dolist (res listaRes)
			(setf resVars (restricao-variaveis res))
			(dolist (var resVars)
				(if (not (equal var1 var)) ;caso a variavel var nao seja a var1
					(when (null (psr-variavel-valor psr var)) ;se a variavel var nao estiver atribuida
						(incf cont) ;incrementar o numero de restricoes
						(return)))))
		cont))

;************************************************************************************
;* 									funcoes resolve 								*
;************************************************************************************

;************************************************************************************
;*		               resolve-simples (array) : (array) 							*
;************************************************************************************
;**** Argumentos **** 																*
;* array 																			*
;**** Retorno **** 																	*
;* Um array com o problema fill-a-pix resolvido 									*
;************************************************************************************
(defun resolve-simples (array)
	(let ((psr (fill-a-pix->psr array))
			(linhas (array-dimension array 0))
			(colunas (array-dimension array 1)))
			(procura-retrocesso-simples psr)
			(psr->fill-a-pix psr linhas colunas)))

;************************************************************************************
;* 			               resolve-best (array) : (array) 							*
;************************************************************************************
;**** Argumentos **** 																*
;* array 																			*
;**** Retorno **** 																	*
;* Um array com o problema fill-a-pix resolvido 									*
;************************************************************************************
(defun resolve-best (array)
	(let ((psr (best-fill-a-pix->psr array))
			(linhas (array-dimension array 0))
			(colunas (array-dimension array 1)))
			(procura-teste psr)
			(psr->fill-a-pix psr linhas colunas)))

;************************************************************************************
;*							FUNCOES AUXILIARES BEST 								*
;************************************************************************************

;************************************************************************************
;*	                 best-cria-restricao (vars pred valor) 							*
;************************************************************************************
;**** Argumentos **** 																*
;* vars - Lista de variaveis que participam numa restricao 							*
;* pred - Predicado aplicado sobre as variaveis para determinar consistencia 		*
;* valor - Valor da restricao 														*
;**** Retorno **** 																	*
;* Restricao 																		*
;************************************************************************************
(defun best-cria-restricao(vars pred valor)
	(make-restricao :resVars vars :resPred pred :resValor valor))

;************************************************************************************
;* 			                best-restricao-valor (res) 								*
;************************************************************************************
;**** Argumentos **** 																*
;* res - restricao 																	*
;**** Retorno **** 																	*
;* Valor da restricao 																*
;************************************************************************************
(defun best-restricao-valor (res)
	(restricao-resValor res))

;************************************************************************************
;*              	 procura-best (psr) : (psr/nil testes) 							*
;************************************************************************************
;**** Argumentos **** 																*
;* psr 																				*
;**** Retorno **** 																	*
;* Um psr se a procura teve sucesso, nil caso contrario, e o numero de testes feitos*
;************************************************************************************
(defun procura-best (psr)
	(let ((var nil)
		(returnValues nil)
		(oldDoms (make-hash-table :test 'equal))) ;dominios anteriores a serem alterados

		(if (psr-completo-p psr)  
			(return-from procura-best psr))
		(setf var (MRV psr)) ;escolher a proxima variavel usando a heuristica MRV + Grau para desempate
		
		(dolist (val (psr-variavel-dominio psr var))
			(when (psr-atribuicao-consistente-p psr var val)
				(psr-adiciona-atribuicao! psr var val)
				(setf returnValues (multiple-value-bind 
									(inf testes successo)
									(forward-checking psr var)
									(list inf testes successo)))

				(when (third returnValues) ;se o forward-checking tiver sucesso
					(maphash #'(lambda (var dom)
						(setf (gethash var oldDoms) (psr-variavel-dominio psr var)) ;guardar os dominios actuais
						(psr-altera-dominio! psr var dom)) (first returnValues)) ;alterar os dominios para os retornados pelo forward-checking

					(setf returnValues (multiple-value-bind 
								(resultado testes)
								(procura-best psr)
								(list resultado testes)))

					(when (first returnValues) ;se for retornado um psr
						(return-from procura-best (first returnValues))) ;retornar da funcao com o psr 
					
					(maphash #'(lambda (var dom)
						(psr-altera-dominio! psr var dom)) oldDoms) ;voltar a atribuir os dominios guardados as variaveis
					(setf oldDoms (make-hash-table :test 'equal)))

				(psr-remove-atribuicao! psr var)))
		nil))

;************************************************************************************
;*	                 best-fill-a-pix->psr (array) : psr/nil 						*
;************************************************************************************
;**** Argumentos **** 																*
;* array - tabuleiro fill-a-pix (array bidimensional) 								*
;**** Retorno **** 																	*
;* PSR correspondente ao tabuleiro com as restricoes triviais ja' resolvidas ou nil	*
;* se for possivel determinar que nao tem solucao									*
;************************************************************************************
(defun best-fill-a-pix->psr (array)
	(let* ((linhas (array-dimension array 0))
			(colunas (array-dimension array 1))
			(psr nil)
			(listaRes nil)	; lista de restricoes para o psr
			(resVars nil)	; variaveis da restricao
			(listaVars)		; lista de variaveis para o psr
			(pred nil)
			(listaDoms (make-list (* linhas colunas) :initial-element '(0 1)))) ;inicializacao dos dominios a (0 1)
			
			(dotimes (nlinha linhas)
				(dotimes (index colunas)
					(when (aref array nlinha index)
						(setf resVars (cria-lista array index nlinha)) ;procurar e adicionar as variaveis de uma restricao
						(setf pred (cria-predicado (aref array nlinha index) resVars))
						(setf listaRes (append listaRes (list (best-cria-restricao resVars pred (aref array nlinha index))))))
						(setf listaVars (append listaVars (list (write-to-string (+ (* nlinha colunas) index))))) ))
			(setf psr (cria-psr listaVars listaDoms listaRes))
			
			(dolist (res1 (psr-psrListaRes psr)) ;por cada restricao do psr
				(cond 
					((eq (length (restricao-variaveis res1)) (best-restricao-valor res1)) ;se o numero de variaveis da restricao for igual ao valor desta 
						(if (null (populateVars psr res1 1)) ;se o resultado de atribuir o valor 1 as variaveis da restricao for null
							(return-from best-fill-a-pix->psr nil))) ;entao o problema nao tem solucao
					((zerop (best-restricao-valor res1))  ;se o valor da restricao for 0
						(if (null (populateVars psr res1 0)) ;se o resultado de atribuir o valor 0 as variaveis da restricao for null
							(return-from best-fill-a-pix->psr nil))))) ;entao o problema nao tem solucao
			psr))

;************************************************************************
;*                 populateVars (psr res valor) : T/nil
;************************************************************************
;**** Argumentos ****
;* psr
;* res - restricao
;* valor - Valor a atribuir as variaveis da restricao res
;**** Retorno ****
;* nil se as atribuicoes nao forem consistentes, T caso contrario
;************************************************************************			
(defun populateVars (psr res valor)
	(let ((resVars (restricao-variaveis res))
		(success nil))

		(dolist (var resVars)
			(psr-adiciona-atribuicao! psr var valor)) ;adiciona o valor a cada variavel var da restricao res

		(dolist (var resVars)
			(setf success (initial-forward-checking psr var))	 ;faz inferencia usando o forward-checking

			(if (null success) 			;se o initial-forward-checking falhou
				(return-from populateVars nil))) 		;entao o problema nao tem solucao
		 T))

;************************************************************************************
;*	                 initial-forward-checking (psr var) : T/nil						*
;************************************************************************************
;**** Argumentos **** 																*
;* array - tabuleiro fill-a-pix (array bidimensional) 								*
;**** Retorno **** 																	*
;* PSR correspondente ao tabuleiro com as restricoes triviais ja' resolvidas 		*
;************************************************************************************
(defun initial-forward-checking (psr var)
	(let ((inf (make-hash-table :test 'equal)) ;hash-table para guardar as inferencias
		(listaArcos (arcos-vizinhos-nao-atribuidos psr var)) 
		(returnValues nil))

		(dolist (arco listaArcos)
			(setf returnValues (multiple-value-bind 
				(revised testes inf) 
				(revise psr (first arco) (second arco) inf) 
				(list revised testes inf)))
			(setf inf (third returnValues)) ;alterar as inferencias para as devolvidas pelo revise

			(cond ((and (first returnValues) (zerop (length (first (hashValues inf (first arco)))))) ;se foi revised e o dominio da primeira variavel do arco estiver vazio na hash-table das inferencias
								(return-from initial-forward-checking nil)) ;o initial-forward-checking falha
			((and (first returnValues) (equal 1 (length (first (hashValues inf (first arco))))) ;se foi revised e o numero de valores do dominio da primeira variavel do arco for igual a 1
								(psr-adiciona-atribuicao! psr (first arco) (first (first (hashValues inf (first arco))))))))) ;atribui o valor do dominio da primeira variavel do arco 'a propria variavel
		T))

;************************************************************************************
;*	                 initial-MAC (psr var) : T/nil									*
;************************************************************************************
;**** Argumentos **** 																*
;* array - tabuleiro fill-a-pix (array bidimensional) 								*
;**** Retorno **** 																	*
;* PSR correspondente ao tabuleiro com as restricoes triviais ja' resolvidas 		*
;* NOTA: nao e' usado na versao final do projecto
;************************************************************************************
(defun initial-MAC (psr var)
	(let* ((inf (make-hash-table :test 'equal)) ;hash-table para guardar as inferencias
		(listaArcos (arcos-vizinhos-nao-atribuidos psr var))
		(returnValues nil)
		(novosArcos nil)
		(cont 0)
		(max (length listaArcos))
		(arco nil))

		(loop 
			(if (eq cont max) ; termina se ja viu todos os arcos da listaArcos
				(return)) 

			(setf arco (nth cont listaArcos))
			(setf returnValues (multiple-value-bind 
				(revised testes inf) 
				(revise psr (first arco) (second arco) inf) 
				(list revised testes inf)))
			(setf inf (third returnValues)) ;alterar as inferencias para as devolvidas pelo revise

			(when (first returnValues)

				(cond ((zerop (length (first (hashValues inf (first arco))))) ;se foi revised e o dominio da primeira variavel do arco estiver vazio na hash-table das inferencias
							(return-from initial-MAC nil)) ;o initial-MAC falha
						((equal 1 (length (first (hashValues inf (first arco))))) ;se foi revised e o numero de valores do dominio da primeira variavel do arco for igual a 1
							(psr-adiciona-atribuicao! psr (first arco) (first (first (hashValues inf (first arco))))))) ;atribui o valor do dominio da primeira variavel do arco 'a propria variavel

				(setf novosArcos (arcos-vizinhos-nao-atribuidos psr (first arco)))
				(setf novosArcos (remove (list (second arco) (first arco)) novosArcos :test #'equal))
				(setf listaArcos (remove-duplicates (append listaArcos novosArcos) :test #'equal))) ;adicionar 'a lista de arcos, os arcos-vizinhos-nao-atribuidos da primeira variavel do arco

			(incf cont)
			(setf max (length listaArcos)))

		T))

;************************************************************************************
;*		               hashValues (ht var) : (valor presenca) 						*
;************************************************************************************
;**** Argumentos **** 																*
;* ht - hashtable 																	*
;* var -variavel 																	*
;**** Retorno **** 																	*
;* O valor da variavel var na hashtable e o valor logico indicando se a variavel var* 
;* esta ou nao presente na hashtable 												*
;************************************************************************************
(defun hashValues (ht var)
	(multiple-value-bind 
		(valor presenca) 
		(gethash var ht) 
		(list valor presenca)))

;************************************************************************************
;* 							funcoes auxiliares gerais 								*
;************************************************************************************
(defun retroc-simples (array)
	(let ((psr (fill-a-pix->psr array))
		(returnValues nil)
		(tabuleiro nil)
		(linhas (array-dimension array 0))
		(colunas (array-dimension array 1)))

		(setf returnValues (multiple-value-bind
							(resultado testes)
							(procura-retrocesso-simples psr)
							(list resultado testes)))
		(setf tabuleiro (psr->fill-a-pix psr linhas colunas))
		(values (second returnValues) tabuleiro)))

(defun retroc-grau (a)
	(let ((p (fill-a-pix->psr a))
		(returnValues nil)
		(board nil)
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))

		(setf returnValues (multiple-value-bind
							(psr testes)
							(procura-retrocesso-grau p)
							(list psr testes)))
		(setf board (psr->fill-a-pix p linhas colunas))
		(values (second returnValues) board)))

(defun resolve-grau (a)
	(let ((p (fill-a-pix->psr a))
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))
		
		(procura-retrocesso-grau p)
		(psr->fill-a-pix p linhas colunas)))

(defun retroc-fc (a)
	(let ((p (fill-a-pix->psr a))
		(returnValues nil)
		(board nil)
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))

		(setf returnValues (multiple-value-bind
							(psr testes)
							(procura-retrocesso-fc-mrv p)
							(list psr testes)))
		(setf board (psr->fill-a-pix p linhas colunas))
		(values (second returnValues) board)))

(defun resolve-fc-mrv (a)
	(let ((p (fill-a-pix->psr a))
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))
		
		(procura-retrocesso-fc-mrv p)
		(psr->fill-a-pix p linhas colunas)))

(defun retroc-mac (a)
	(let ((p (fill-a-pix->psr a))
		(returnValues nil)
		(board nil)
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))
		
		(setf returnValues (multiple-value-bind
							(psr testes)
							(procura-retrocesso-MAC-mrv p)
							(list psr testes)))
		(setf board (psr->fill-a-pix p linhas colunas))
		(values (second returnValues) board)))

(defun resolve-MAC-mrv (a)
	(let ((p (fill-a-pix->psr a))
		(linhas (array-dimension a 0))
		(colunas (array-dimension a 1)))
		
		(procura-retrocesso-MAC-mrv p)
		(psr->fill-a-pix p linhas colunas)))

(defun procura-teste (psr)
(let ((testesTotais 0)
		(var nil)
		(returnValues nil)
		(oldDoms (make-hash-table :test 'equal))) ;dominios anteriores a serem alterados

		(if (psr-completo-p psr)  
			(return-from procura-teste (values psr testesTotais)))

		(setf var (MRV-com-grau psr)) ;escolher a proxima variavel usando a heuristica MRV
		(dolist (val (psr-variavel-dominio psr var))
			(setf returnValues (multiple-value-bind 
								(sucesso testes)
								(psr-atribuicao-consistente-p psr var val)
								(list sucesso testes)))
			(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante da verificacao de consistencia

			(when (first returnValues) ;se a atribuicao for consistente
				(psr-adiciona-atribuicao! psr var val)
				(setf returnValues (multiple-value-bind 
									(inf testes sucesso)
									(MAC psr var)
									(list inf testes sucesso)))
				(incf testesTotais (second returnValues)) ;adicionar aos testesTotais, o numero de testes resultante do forward-checking

				(when (third returnValues) ;se o forward-checking teve sucesso
					(maphash #'(lambda (var dom)
						(setf (gethash var oldDoms) (psr-variavel-dominio psr var)) ;guardar os dominios actuais
						(psr-altera-dominio! psr var dom)) (first returnValues)) ;alterar os dominios para os retornados pelo forward-checking

					(setf returnValues (multiple-value-bind 
								(resultado testes)
								(procura-teste psr)
								(list resultado testes)))
					(incf testesTotais (second returnValues)) ;adicionar aos testesTotais o numero de testes resultante da procura-retrocesso-fc-mrv

					(when (first returnValues) ;se for retornado um psr
						(return-from procura-teste (values (first returnValues) testesTotais))) ;retornar da funcao com o psr e os testesTotais
					
					(maphash #'(lambda (var dom)
						(psr-altera-dominio! psr var dom)) oldDoms) ;voltar a atribuir os dominios guardados as variaveis
					(setf oldDoms (make-hash-table :test 'equal)))

				(psr-remove-atribuicao! psr var)))
			(values nil testesTotais)))