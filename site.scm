(use sxml-serializer posix srfi-1)

(define src-directory (make-parameter "src"))

(define current-page (make-parameter #f))

(define ($ var #!optional page)
  (let ((page (or page (current-page))))
    (or (alist-ref var (page-vars page)) "")))

(define prev-pages (make-parameter #f))

(define next-pages (make-parameter #f))

(define-record page name vars content)

(define (render-page content)
  `((xhtml-1.0-strict)
    (html
     (head
      (title ,($ 'title)))
     (body
      (h1 ,($ 'title))
      ,content))))

(define (render-single-page)
  (render-page (render-page-content (page-content (current-page)))))

(define (render-page-content content)
  `(div
    ,@(if (null? (prev-pages)) '()
          `("Prev: " 
            (a (@ (href ,(url (car (prev-pages)))))
               ,($ 'title (car (prev-pages))))))
    ,@(if (null? (next-pages)) '()
          `("Next: "
            (a (@ (href ,(url (car (next-pages)))))
               ,($ 'title (car (next-pages))))))
    (p ,content)))

(define (render-list-page pages)
  (render-page (render-list pages)))

(define (render-list pages)
  `(ul
    ,@(map render-list-item pages)))

(define (render-list-item page)
  (parameterize ((current-page page))
                `(li ,($ 'title))))

(define (load-page path)
  (with-input-from-file path
    (lambda ()
      (let-values (((_ name ext) (decompose-pathname path)))
      (make-page name (read) (read-string))))))

(define (file-path page)
  (make-pathname "out" (page-name page) "html"))

(define (url page)
  (make-pathname "out" (page-name page) "html"))

(define (load-pages)
  (sort (map (lambda (path) (load-page path))
             (glob "./src/*.nb"))
        (lambda (a b) (string>?
                       (alist-ref 'date (page-vars a))
                       (alist-ref 'date (page-vars b))))))

(let ((pages (load-pages)))
  ;; Index
  (parameterize ((current-page (car pages))
                 (next-pages '())
                 (prev-pages (cdr pages)))
                (print "\n**INDEX**\n" )
                (print
                 (serialize-sxml
                  (render-single-page))))

  ;; All Pages
  (do ((n 0 (+ n 1))
       (pages pages (cdr pages))
       (rev-pages '() (cons (car pages) rev-pages)))
      ((null? pages))
    (let ((page (car pages)))
      (parameterize ((current-page page)
                     (next-pages (cdr pages))
                     (prev-pages rev-pages))
                    (print "\n**FILE: " (file-path page) "**\n")
                    (print
                     (serialize-sxml
                      (render-single-page))))))

  ;; Tags List
  (let ((tags (delete-duplicates
               (apply append
                      (map (cut $ 'tags <>) pages)))))
    (do ((n 0 (+ n 1))
         (tags tags (cdr tags)))
        ((null? tags))
      (let ((tag (car tags)))
        (parameterize ((current-page (make-page tag
                                                `((title . ,tag))
                                                "none")))
                      (print
                       (serialize-sxml
                        (render-list-page
                         (filter (lambda (page)
                                   (member tag ($ 'tags page)))
                                 pages))))))))
  )


