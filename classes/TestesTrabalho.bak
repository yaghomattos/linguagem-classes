#lang racket

(require dcc019/classes/ast)

;class teste extends object
;   field y
;   field x
;
;   method initialize (v)
;      begin
;        set x = v
;      end
;
;   method a(z)
;      begin
;         set y = -(x, z)
;      end
;
;   method gety() y

; cria uma lista de declarações com a classe descrita acima
(define decl-list (list
       (ast:decl (ast:var "teste")
                 (ast:var "object")
                 (list (ast:var "y") (ast:var "x"))
                 (list
                      (ast:method (ast:var "initialize") (list (ast:var "v")) (ast:begin (list (ast:assign (ast:var "x") (ast:var "v")))))
                      (ast:method (ast:var "a") (list (ast:var "z")) (ast:begin (list (ast:assign (ast:var "y") (ast:dif (ast:var "x") (ast:var "z"))))))
                      (ast:method (ast:var "gety") '() (ast:var "y"))
                 )
       )
)) 


; ################## Exercicio 4 ##################

(struct object (classname fields))
; criar o objeto a partir do descritor da classe, recebe uma lista de campos da classe (teste e x,y)

(define (create-object classname fields)
  (object classname fields)
)

(struct method (vars body super-names field-names))
; todos os campos vem da assinatura do metodo e da classe

(define (create-method vars body super-names field-names)
  (method vars body super-names field-names)
)

(struct class (super-names method-env))
; ambiente de metodos a -> (method (z) (- x z) object (x y)), b -> ...

(define (create-class superclass-name method-env)
   (class superclass-name method-env)
)

; #############################################

(define (teste-ex4 decl-list)
  ; exibe a cabeça da lista
 (display (car decl-list))
)

(define (get-declarations decl-list)
  (if (null? decl-list)
      empty
      (match (car decl-list)
        [(ast:decl name super fields methods) (display name) (display super) (display fields) ]
  ))  
)

;(define (get-class-from-delc decl)
;  (display (ast:decl name super fields methods))
;)

(get-declarations decl-list)