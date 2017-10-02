#lang br/quicklang
;------------------importar archivos---------------------------------------------------------
(require graph)
(require racket/string)
;------------------reader del archivo--------------------------------------------------------
;transforma las líneas en objetos de sintaxis entendibles por racket. (hanble "linea capturada")
(define (read-syntax path port)
  (define args (port->lines port))
  (define handle-datums (format-datums '(handle \"~a\") args))
  (define module-datum `(module stacker-mod "ldp.rkt"
                          ,@handle-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)
;------------------expansor del archivo------------------------------------------------------
;construye el módulo
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...))
(provide (rename-out [stacker-module-begin #%module-begin]))
;------------------declaracion de grafo------------------------------------------------------
;define un grafo ponderado y dirigido como base para todos los grafos.
(define G (weighted-graph/directed '()))
;------new-----------------------------------------------------------------------------------
;diferencia si es un grafo, nodo o arista nuevo.
(define (new arg)
  (cond
    [(equal? (substring arg 0 4) "graf") (graf (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "nodo") (nodo (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "aris") (aris (string-split (substring arg 5 (string-length arg))))]
    [else (error "error en new")])
  )
;------------------graf----------------------------------------------------------------------
;deja el grafo en blanco y luego si es que requiere que sea completo llama a grafcompleto
(define (graf listaarg)
  (set! G (weighted-graph/directed '()))
  (cond
    [(and (= (length listaarg) 2) (equal? (list-ref listaarg 0) "completo") (integer? (string->number (list-ref listaarg 1))))
     (grafcompleto (string->number (list-ref listaarg 1)))]
    )
  )
;------------------grafcompleto--------------------------------------------------------------
;crea el grupo de nodos y todos los enlaces posibles entre los nodos.
(define (grafcompleto arg)
  (for ([i arg])
    (add-vertex! G (number->string i))
    )
  (for ([i arg])
    (for ([j arg])
      (cond
        [(not (= i j)) (add-edge! G (number->string i) (number->string j))])
      )
    )
  )
;------------------nodo----------------------------------------------------------------------
;crea un nodo
(define (nodo listaarg)
  (cond
    [(and (= (length listaarg) 1)) (add-vertex! G (list-ref listaarg 0))])
  )
;------------------aris----------------------------------------------------------------------
;crea una arista segun los parametros de entrada diferencia entre si es unidireccional o bidireccional y el peso de la arista
(define (aris listaarg)
  (cond
    [(and (= (length listaarg) 4) (equal? (list-ref listaarg 0) "!")) (add-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2) (list-ref listaarg 3))]
    [(and (= (length listaarg) 4) (equal? (list-ref listaarg 0) "#")) (add-edge! G (list-ref listaarg 1) (list-ref listaarg 2) (list-ref listaarg 3))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "!")) (add-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "#")) (add-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    )
  )
;------del-----------------------------------------------------------------------------------
;diferencia que es lo que se desea eliminar si es un nodo o arista
(define (del arg)
  (cond
    [(equal? (substring arg 0 4) "nodo") (del_nodo (string-split (substring arg 5 (string-length arg))))]
    [(equal? (substring arg 0 4) "aris") (del_aris (string-split (substring arg 5 (string-length arg))))]
    [else (error "error en new")])
  )
;------------------nodo----------------------------------------------------------------------
;elimina un nodo
(define (del_nodo listaarg)
  (cond
    [(and (= (length listaarg) 1)) (remove-vertex! G (list-ref listaarg 0))])
  )
;------------------aris----------------------------------------------------------------------
;elimina una arista segun el parametro de direccion
(define (del_aris listaarg)
  (cond
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "!")) (remove-directed-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    [(and (= (length listaarg) 3) (equal? (list-ref listaarg 0) "#")) (remove-edge! G (list-ref listaarg 1) (list-ref listaarg 2))]
    )
  )
;------handle--------------------------------------------------------------------------------
;es la primera función llamada en el módulo la cual recibe un string el cual desloza en las 3 operaciones principales.
(define (handle [arg #f])
  (cond
    [(equal? arg "") (display "")]
    [(equal? (substring arg 0 3) "new") (new (substring arg 4 (string-length arg)))]
    [(equal? (substring arg 0 3) "del") (del (substring arg 4 (string-length arg)))]
    [(equal? arg "show graf") (get-vertices G)]
    [(equal? arg "show aris") (get-edges G)]
    [else (displayln "error")]))
(provide handle)
