(use sxml-serializer posix srfi-1)

(define src-directory (make-parameter "src"))

(define page-size (make-parameter 3))

;; Pages

(define-record page type name vars content)

(define (page-source-directory page)
  (make-pathname "src" (->string (page-type page))))

(define (page-out-directory page)
  (make-pathname "out" (->string (page-type page))))

(define (page-out-path page)
  (make-pathname "out" (make-pathname (->string (page-type page)) (page-name page) "html")))

(define (page-url page)
  (make-pathname "/"
     (make-pathname (->string (page-type page)) (page-name page) "html")))

;; Dynamic parameters

(define current-page (make-parameter #f))

(define list-pages (make-parameter '()))

(define prev-page (make-parameter #f))

(define next-page (make-parameter #f))

(define ($content #!optional page)
  (let ((page (or page (current-page))))
    (page-content (current-page))))

(define ($ var #!optional page)
  (let ((page (or page (current-page))))
    (or (alist-ref var (page-vars page)) "")))

(define list-type (make-parameter #f))

(define list-tag (make-parameter #f))

(define list-page-number (make-parameter #f))

(define list-total-pages (make-parameter #f))

;; Links

(define (url #!optional page)
  (let ((page (or page (current-page))))
    (page-url page)))

(define (prev-page-link)
  (let ((page (prev-page)))
    (if page
        `(a (@ (href ,(url page))) ,($ 'title page))
        "")))

(define (next-page-link)
  (let ((page (next-page)))
    (if page
        `(a (@ (href ,(url page))) ,($ 'title page))
        "")))

(define (list-page-url)
  (make-pathname "/"
   (make-pathname (->string  (list-type))
     (make-pathname (conc "_" (list-tag)) (->string (list-page-number))))))

;; abstract these
(define (next-list-page-url)
  (make-pathname "/"
   (make-pathname (->string  (list-type))
     (make-pathname (conc "_" (list-tag))
                    (->string (+ (list-page-number) 1))))))

(define (prev-list-page-url)
  (make-pathname "/"
   (make-pathname (->string (list-type))
     (make-pathname (conc "_" (list-tag))
                    (->string (- (list-page-number) 1))))))

(define (prev-list-page-link)
  `(span
    (a (@ (href ,(prev-list-page-url)))
       "<<" ,(- (list-page-number) 1))
    "/" ,(->string (list-total-pages))))
  
(define (next-list-page-link)
  `(a (@ (href ,(next-list-page-url)))
     ">>" ,(->string (+ (list-page-number) 1))))

(define (tag-url tag)
  (make-pathname "/"
   (make-pathname (->string (page-type (current-page))) tag)))

;; Templates

(define (define-template level type template)
  (put! level type template))

(define (get-template level type)
  (get level type))

(define-template '/ 'base
   (lambda ()
     `((xhtml-1.0-strict)
       (html
        (head
         (title ,($ 'title))
         (link (@ (rel "stylesheet") (type "text/css")
                  (href "site.css"))))
        (body ,(page))))))

(define (base)
  (let ((type (or (list-type) (page-type (current-page)))))
    ((or (get-template type 'base) (get-template '/ 'base)))))

(define-template '/ 'page
  (lambda ()
    `(div
      (div
       (h1 ,($ 'title)))
      ,(content))))

(define (page)
  (let ((type (or (list-type) (page-type (current-page)))))
    ((or (get-template type 'page)  (get-template '/ 'page)))))
  
(define-template '/ '_index
  (lambda ()
    `(div
      (div
       (h1 "Index")
       (h1 ,($ 'title)))
      ,($ 'content)
      ,((content-template)))))
        
(define content-template
   (lambda ()
     `(div
        (p ,($content)))))

(define (content)
  (if (list-type)
      (let ((type (list-type)))
        ((or (get-template type 'list) (get-template '/ 'list))))
      (let ((type (page-type (current-page))))
        ((or (get-template type 'content) content-template)))))

(define-template 'post 'content
  (lambda ()
    `(div
      (p ,($content)))))

(define-template '/ 'list
  (lambda ()
    `(div
      (div
       (h1 ,($ 'title))
       (h2 ,(list-page-url)))
      (ul ,(map-list-template))
      ,(prev-list-page-link)
      ,(next-list-page-link)
      )))

(define (map-list-template)
  (map (lambda (page)
         (parameterize ((current-page page))
          (list-item)))
       (list-pages)))

(define list-item-template
  (lambda ()
    `(li (a (@ (href ,(url)))
            ,($ 'title)))));)

(define (list-item)
  (let* ((type (list-type)))
    ((or (get-template type 'list-item) list-item-template))))

;; File loading

(define (load-page type path)
  (with-input-from-file path
    (lambda ()
      (let-values (((_ name ext) (decompose-pathname path)))
        (make-page type name (read) (read-string))))))

;; Needs real date parsing and handling
(define (date>? a b)
  (let ((date-a (alist-ref 'date (page-vars a)))
        (date-b (alist-ref 'date (page-vars b))))
    (and date-a date-b
         (string>? date-a date-b))))

(define page-sorter
  (make-parameter date>?))

(define (load-files dir)
  (sort (map (lambda (path) (load-page (string->symbol dir) path))
             (glob (conc "./src/" dir "/*.nb")))
        (page-sorter)))

;; Load

 (include "templates.scm")
               
(define (load-pages type pages)
  
  (when (not (directory-exists? (conc "out/" type))) ; abstract this
        (create-directory (conc "out/" type)))

  (do ((pages pages (cdr pages))
       (rev-pages '() (cons (car pages) rev-pages)))
      ((null? pages))
    (let ((page (car pages)))
    (parameterize ((current-page page)
                     (prev-page (and (not (null? (cdr pages))) (cadr pages)))
                     (next-page (and (not (null? rev-pages)) (car rev-pages))))
        (with-output-to-file (page-out-path page)
          (lambda ()
            (print
             (serialize-sxml 
              (base)))))))))
  
(define (load-lists dir pages)
  (let ((tags (extract-tags pages))
        (type (string->symbol dir)))
    (do ((tags tags (cdr tags)))
        ((null? tags))
      (let* ((tag (car tags))
             (file-dir (make-pathname (make-pathname "out" dir) (conc "_" tag))) ;; abstract this
             (page-set '())
             (pages (filter-tag tag pages))
             (total-pages (length pages)))
        (let rec ((n 0) (p 1)
                  (pages pages)
                  (page-set '()))
          (cond ((null? pages) 
                 (run-list type tag file-dir p (reverse page-set) total-pages))
                ((= n (page-size))
                 (run-list type tag file-dir p (reverse page-set) total-pages)
                 (rec 0 (+ p 1) pages '()))
                (else (rec (+ n 1) p (cdr pages) 
                           (cons (car pages) page-set)))))))))

(define (extract-tags pages)
  (delete-duplicates
   (apply append (map (cut $ 'tags <>) pages))))

(define (filter-tag tag pages)
  (filter (lambda (page)
            (member tag ($ 'tags page)))
          pages))

(define (run-list type tag dir p page-set total-pages)
  (parameterize ((current-page (make-page #f tag `((title . ,tag)) "none"))
                 (list-page-number p)
                 (list-total-pages total-pages)
                 (list-tag tag)
                 (list-type type) 
                 (list-pages page-set))
   (when (not (directory-exists? dir))
         (create-directory dir))
   (with-output-to-file (make-pathname dir (->string p) "html")
     (lambda ()
       (print
        (serialize-sxml
         (base)))))  ))

(define pages (load-files ""))

(define posts (load-files "posts"))

(with-output-to-file (make-pathname "out" "index" "html")
  (lambda ()
    (print 
     (serialize-sxml
      (parameterize ((current-page (load-page '_index "src/_index.site"))
                     (list-pages pages))
       (base))))))

(load-pages "/" pages)

(load-pages "posts" posts)

(load-lists "posts" posts)

(quit)
