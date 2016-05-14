(define-library (owl xml)

  (import
    (owl base))

  (export
    xml-render)

  (begin

    ;; placeholders, fixme

    ;; string withing exml, tag quoted
    (define render-quoted render)

    ;; value within string, " quoted
    (define render-string-quoted render)

    ;; X = (ns? tag ((ns? attr value) ...)? . X*)
    ;;   | number
    ;;   | string
    ;;   | #false = standalone tag sans close

    (define (get-tag exp)
      (let loop ((exp exp) (tag #false))
        (cond
          ((not (pair? exp))
            (error "xml render: no tag: " exp))
          ((symbol? (car exp))
            (loop (cdr exp) (if tag (str tag ":" (car exp)) (str (car exp)))))
          (tag (values tag exp))
          (else
            (error "xml render: no tag: " exp)))))
    
    (define (get-atts exp)
      (if (and (pair? exp) (pair? (car exp)) (pair? (caar exp)))
        (values (car exp) (cdr exp))
        (values null exp)))

    (define (xren-tag tag open? tl)
      (let ((tl (render tag (if open? tl (cons #\> tl)))))
        (if open?
          (cons #\< tl)
          (ilist #\< #\/ tl))))
       
    (define (xren-atts atts tl)
      (foldr
        (λ (att tl)
          (lets ((att-name rest (get-tag att)))
            (if (and att-name (= (length rest) 1))
              (cons #\space
                (render att-name
                  (ilist #\= #\"
                    (render-string-quoted (car rest)
                      (cons #\" tl)))))
              (error "bad attribute: " att))))
        tl atts))
        
    (define (xren exp tl)
      (cond
        ((null? exp)
          tl)
        ((string? exp)
          (render-quoted exp tl))
        ((number? exp)
          (render-quoted exp tl))
        ((list? exp)
          (lets
            ((tag exp (get-tag exp))
             (atts exp (get-atts exp)))
            (xren-tag tag #true
              (xren-atts atts
                (cons #\> 
                  (foldr
                    (λ (exp tl)
                      (xren exp tl))
                    (xren-tag tag #false tl)
                    exp))))))
        (else
          (error "xml render: bad node " exp))))

    (define (xml-render exp . args)
      (list->string
        (append
          (string->list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
          (xren exp null))))))

