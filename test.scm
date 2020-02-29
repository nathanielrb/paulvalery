(use srfi-1)

(define (list->alist lst)
  (let rec ((lst lst)
            (groups '()))
    (if (null? lst) (reverse groups)
        (let-values (((group rest) (split-at lst 2)))
          (rec rest
               (cons (cons (substring (car group) 2)
                           (cadr group))
                     groups))))))
    

(print (list->alist (command-line-arguments)))
