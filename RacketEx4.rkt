#lang racket

(require dcc019/classes/ast)

;class teste extends object
;   field y
;
;   method initialize (v)
;      begin
;        set x = v
;      end
;
;   method gety() y

; cria uma função identidade
(define (decl-list) decl-list)

; preenche a função identidade com uma ast manual da classe acima
(set! decl-list (list (ast:decl (ast:var "teste") (ast:var "object") (list (ast:var "y")) (list (ast:method (ast:var "initialize") (list (ast:var "v")) (ast:begin (list (ast:assign (ast:var "x") (ast:var "v"))))) (ast:method (ast:var "gety") '() (ast:var "y"))))) )


; ######### Exercicio 4 #########

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


(define (teste-ex4 decl-list)
  ; exibe a cabeça da lista
 (display (car decl-list))
)

(teste-ex4 decl-list)