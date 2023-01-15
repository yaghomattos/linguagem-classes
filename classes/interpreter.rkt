#lang racket

;Trabalho Prático 3 - Linguagens de Programação

;Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC 
;         Thiago de Almeida Lopes - Matrícula: 201765556AC 

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

(provide value-of-program)

; Cria um print pra debug
(define (print value)
  (display value)
  (display "\n")
)

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
    
    ; ----------- Trabalho -------------
    
    [(ast:self) (apply-env Δ '%self)]

    [(ast:send obj-exp method-name args) (apply-method (value-of obj-exp Δ) method-name args)]
    ; [(ast:send obj-exp method-name args) (begin (display "send: ") (display obj-exp) (display " = ") (display (value-of obj-exp Δ)) (display method-name) (print args))]

    ;[(ast:super name args)]

     [(ast:new class-name args) (begin 
        (define obj (create-object class-name args))
        ; Chama o initialize da classe
        (apply-method obj (ast:var "initialize") args)
        obj
     )]
   ; [(ast:new class-name args) (begin (display "ast:new ") (print (create-object class-name args)))]
    
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  ; you must collect all the classes declared and building its respectively environment
  (get-declarations (ast:prog-decls prog))
  ;(display (ast:prog-decls prog))
  ;(display class-env)
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
(struct objeto (classname fields) #:transparent)
(struct metodo (method-name vars body) #:transparent)
(struct classe (superclass fields method-env) #:transparent)

; inicializa a lista de classes
(define class-env '())

; Aqui cria uma struct de classe da linguagem e adiciona no class-env
(define (create-class name super fields methods)
  ; lista de nomes dos campos da classe
  (define fieldnames (map (lambda fd (ast:var-name (first fd))) fields))

  ; ambiente de metodos a -> (method (z) (- x z) object (x y)), b -> ...
  (define method-env 
    (lambda (methods objeto)
      (map (lambda (methods)
        (methods metodo-body metodo-vars objeto-classname objeto-fields)))
      method-env))
  
  ; Cria uma lista do nome da classe + struct de classe e adiciona ao fim da lista de ambientes de classes
  (set! class-env (append class-env (list (list (ast:var-name name) (classe super fieldnames methods)))))

  ; debug
  ;(display "create-class: ")
  ;(print (car (car class-env)))
)

; criar o objeto quando for executado um 'new' no código.
; Recebe o nome da classe e a lista de argumentos passados para o inicializador
(define (create-object classname args)
  ; Procura a classe no env de classes
  (define classitem (get-class class-env (ast:var-name classname)))

  ; debug
 ; (display "class-env: ") (print class-env)
 ; (display "new: ") (display classname) (display ", ") (print args)
 ; (display "find-class: ") (print classitem)
  
  ; Cria o objeto
  (objeto
     classname
     ; cria uma lista de referências na mesma ordem dos campos (variaveis) da classe
    (map (lambda _ (newref 0)) (classe-fields classitem))
  )
)

; Busca uma classe na lista de classes (env da classe)
; Recebe a variavel da lista de classes e o nome da classe a procurar.
; Retorna o objeto da classe encontrada
(define (get-class classenv classname)
  ; Testa se o nome fornecido é o mesmo do elemento da cabeça da lista
   (if (equal? (car (car classenv)) classname)
      ; Se o nome for o mesmo, retorna a classe
      (car (cdr (car classenv)))
      ; Se o nome for diferente, chama a recursão na cauda do classenv
      (get-class (cdr classenv) classname)
   )
)

; Busca um metodo no method-env da classe
; Recebe a variavel do method-env e o nome do metodo a procurar.
; Retorna o objeto do metodo encontrado
(define (get-method method-env methodname)
  ; Testa se o nome fornecido é o mesmo do elemento da cabeça da lista
  ;(display "get-method: ")(display methodname) (print (ast:var-name (ast:method-name (car method-env))))
   (if (equal? (ast:var-name (ast:method-name (car method-env))) methodname)
      ; Se o nome for o mesmo, retorna o metodo
      (car method-env)
      ; Se o nome for diferente, chama a recursão na cauda do method-env
      (get-method (cdr method-env) methodname)
   )
)

; todos os campos vem da assinatura do metodo e da classe
;(define (create-method method-data)
;  (match (car method-data)
;    ; metodo com parâmetro
;    [(ast:method (ast:var name) (list (ast:var params)) body) (metodo params body)]
;    ; metodo sem parametros
;    [(ast:method (ast:var name) '() body) '(name (metodo params body))]
;  )
;  ;(method vars body super-names field-names)
;)

; Avalia o resultado de um metodo
; Recebe por parâmetro o objeto da classe, o nome do metodo e os argumentos passados
(define (apply-method object method args)
  ;(display "apply-method: ") (display object) (display method) (print args)
  ; Procura a classe no env de classes
  (define classitem (get-class class-env (ast:var-name (objeto-classname object))))
  ; Extrai o method-env
  (define method-env (classe-method-env classitem))
  ; Procura o metodo no method-env
  (define method-struct (get-method method-env (ast:var-name method)))
  ; Extrai os campos da classe
  (define class-fields (classe-fields classitem))
  ; Extrai os locations dos campos da classe vindos do objeto
  (define fields-locations (objeto-fields object))
  ; Extrai a lista de argumentos
  (define arguments (map (lambda m (ast:int-value (car m))) args))
  ; Extrai a lista de parametros
  (define params (map (lambda m (ast:var-name (car m))) (ast:method-params method-struct)))
  ; Monta o env da classe
  (define Δ2 (build-class-env class-fields fields-locations empty-env))
  ; Monta o env da função com os campos e o env da classe
  (define Δ3 (build-class-env params (map newref arguments) Δ2))

  ;(display "class-fields: ") (print class-fields)
  ;(display "fields-locations: ") (print fields-locations)
  ;(display "method-env: ") (print method-struct)
  ;(display "build-class-env: ") (print Δ2)
  ;(display "build-method-env: ") (print Δ3)
  ;(display "arguments: ") (print arguments)
  ;(display "params: ") (print params)
  ;(display "body: ") (print (ast:method-body method-struct))

  (value-of (ast:method-body method-struct) Δ3)
)

(define (build-class-env class-fields fields-locations env)
    (if (equal? class-fields null) 
      ; then
      env
      ; else
      ;(begin
      ;(display "class-fields: ")
      ;(display (car class-fields))
      ;(display ", fields-locations: ")
      ;(print (car fields-locations))
      (build-class-env
          (cdr class-fields) 
          (cdr fields-locations)
          (extend-env (car class-fields) (car fields-locations) env)
      )
      ;)
    )
)

; ################### Testes ####################

;(define funcao
;  (ast:method (ast:var "a") (list (ast:var "z")) (ast:begin (list (ast:assign (ast:var "y") (ast:dif (ast:var "x") (ast:var "z"))))))
;)

;(define objfuncao
;   (metodo (list "z") (ast:begin (list (ast:assign (ast:var "y") (ast:dif (ast:var "x") (ast:var "z"))))) )
;)

;(define objstruct
;  (objeto "teste" (list (newref 0) (newref 0) (newref 0))) ; Cria referencias de teste para x y z de teste
;)

;(define (apply-method m)
  ; o env da classe
 ; (define Δ2 (extend-env "y" 0 (extend-env "x" 0 empty-env)))

  ; o env da função
 ; (define Δ3 (extend-env "z" 3 Δ2))
  
 ; (display (value-of (metodo-body m) Δ3))
  ;(display (metodo-vars m))
;)
;(apply-method objfuncao)


