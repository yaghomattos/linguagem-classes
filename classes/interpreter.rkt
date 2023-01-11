#lang racket

;Trabalho Prático 3 - Linguagens de Programação

;Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC 
;         Thiago de Almeida Lopes - Matrícula: 201765556AC 

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

(provide value-of-program)

; Representação de procedimentos para escopo estático

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ) ; call by value
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; Criação de ambiente estendido com procedimento recursivo
(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int n) n]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:proc (ast:var v) e) (proc-val v e Δ)]
    [(ast:call e1 e2) (apply-proc (value-of e1 Δ) (value-of e2 Δ))] ; call by value
    [(ast:letrec (ast:var f) (ast:var v) e1 e2) (value-of e2 (extend-env-rec f v e1 Δ))]
    [(ast:begin es) (foldl (lambda (e v) (value-of e Δ)) (value-of (first es) Δ) (rest es))]
    [(ast:assign (ast:var x) e) (begin
                                  (setref! (apply-env Δ x) (value-of e Δ)) ;set the value in the store
                                  42)] ; return the 42 value
    
    ;[(ast:self) (apply-env Δ '%self)]
    ;[(ast:send obj-exp method-name rands)]
    ;[(ast:super name args)]
    ;[(ast:new class-name args)]
    
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  ; you must collect all the classes declared and building its respectively environment
  (get-declarations (ast:prog-decls prog))
  ;(display (ast:prog-decls prog))
  ; execute the prog expression in the correct environment
  (value-of (ast:prog-exp prog) init-env))

; Recebe uma lista de declarações (classes)
(define (get-declarations decl-list)
  (if (null? decl-list)
      ; then
      null
      ; else
      (begin ; esse begin é pra fazer grupos de procedimentos
       (match (car decl-list)
          ; usa o match pra separar os argumentos e poder usar eles na função de criar a classe
          [(ast:decl name super fields methods) (create-class name super fields methods) ]
        )
        ; recursão com a cauda da lista de declarações
        (get-declarations (cdr decl-list))
      )
  )        
)

; Cria as structs
(struct object (classname fields))
;(struct method (vars body super-names field-names))
(struct method (vars body))
(struct class (super-names method-env))

; criar o objeto a partir do descritor da classe, recebe uma lista de campos da classe (teste e x,y)
(define (create-object classname fields)
  (object
     classname
     ; cria uma lista de referências na mesma ordem dos campos (variaveis) da classe
    (map (lambda field-name (newref 0)) fields)
  )
)

; Aqui cria a classe na linguagem
; ambiente de metodos a -> (method (z) (- x z) object (x y)), b -> ...
(define (create-class name super fields methods)
  ; Cria o objeto com os campos locais
  (define obj (create-object name fields))
  (define method-env (map (lambda item (create-method item)) methods))
  (display (car method-env))
)

; todos os campos vem da assinatura do metodo e da classe
(define (create-method method-data)
  (match (car method-data)
    ; metodo com parâmetro
    [(ast:method (ast:var name) (list (ast:var params)) body) '(name (method params body))]
    ; metodo sem parametros
    [(ast:method (ast:var name) '() body) '(name (method params body))]
  )
  ;(method vars body super-names field-names)
)