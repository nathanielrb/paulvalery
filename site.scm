(use sxml-serializer posix srfi-1)

(define src-directory (make-parameter "src"))

(define page-size (make-parameter 3))

(define-record page type name vars content)

;; Dynamic parameters

(define current-page (make-parameter #f))

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

(define (url #!optional page)
  (let ((page (or page (current-page))))
    (make-pathname "/"
     (make-pathname (page-type page) (page-name page) "html"))))

(define (list-page-url)
  (make-pathname "/"
   (make-pathname (list-type)
     (make-pathname (conc "_" (list-tag)) (->string (list-page-number))))))

(define (prev-list-page-link)
  `(span
    (a (@ (href ,(list-page-number)))
       "<<" ,(- (list-page-number) 1))
    "/" ,(list-total-pages)))
  
(define (next-list-page-link)
  `(a (@ (href ,(list-page-number)))
     ">>" ,(+ (list-page-number) 1)))

(define (tag-url tag)
  (make-pathname "/"
   (make-pathname (page-type (current-page)) tag)))

;; Templates

(define base-template 
  (make-parameter
   (lambda (content)
     `((xhtml-1.0-strict)
       (html
        (head
         (title ,($ 'title))
         (link (@ (rel "stylesheet") (type "text/css")
                  (href "site.css"))))
        (body ,content))))))

(define page-template
  (make-parameter
   (lambda (content)
     `(div
       (div
        (h1 ,($ 'title)))
       ,content))))
  
(define index-template
  (make-parameter
   (lambda (content)
     `(div
       (div
        (h1 ,($ 'title)))
       ,($ 'content)))))
        
(define page-content-template
  (make-parameter
   (lambda ()
     `(div
        (p ,($content))))))

(define post-content-template
  (make-parameter
   (lambda ()
     `(div
        (p ,($content))))))

(define list-template
  (make-parameter
   (lambda (content)
     `(div
       (div
        (h1 ,($ 'title))
        (h2 ,(list-page-url)))
       (ul ,content)
       ,(prev-list-page-link)
       ,(next-list-page-link)
       ))))       

(define list-item-template 
  (make-parameter
   (lambda ()
     `(li (a (@ (href ,(url)))
             ,($ 'title))))))

;; File loading

(define (file-path page)
  (make-pathname (make-pathname "out" (page-type page))
                 (page-name page) "html"))

(define (load-page dir path)
  (with-input-from-file path
    (lambda ()
      (let-values (((_ name ext) (decompose-pathname path)))
      (make-page dir name (read) (read-string))))))

;; Needs real date parsing and handling
(define (date>? a b)
  (let ((date-a (alist-ref 'date (page-vars a)))
        (date-b (alist-ref 'date (page-vars b))))
    (and date-a date-b
         (string>? date-a date-b))))

(define page-sorter
  (make-parameter date>?))

(define (load-pages dir)
  (sort (map (lambda (path) (load-page dir path))
             (glob (conc "./src/" dir "*.nb")))
        (page-sorter)))

;; Load

(include "templates.scm")
               
(define (load-pages-or-posts dir pages #!optional (template page-content-template))
  (when (not (directory-exists? (conc "out/" dir)))
        (create-directory (conc "out/" dir)))
  
  (do ((pages pages (cdr pages))
       (rev-pages '() (cons (car pages) rev-pages)))
      ((null? pages))
    (let ((page (car pages)))
      (parameterize ((current-page page)
                     (prev-page (and (not (null? (cdr pages)))
                                     (cadr pages)))
                     (next-page (and (not (null? rev-pages))
                                     (car rev-pages))))
        (with-output-to-file (file-path page)
          (lambda ()
            (print
             (serialize-sxml 
              ((base-template)
               ((page-template) 
                ((template))))))))))))

(define (load-lists dir pages)
  (let ((tags (extract-tags pages)))
    (do ((tags tags (cdr tags)))
        ((null? tags))
      (let* ((tag (car tags))
             (file-dir (make-pathname (make-pathname "out/" dir) (conc "_" tag))) ;; abstract this
             (page-set '())
             (pages (filter-tag tag pages))
             (total-pages (length pages)))
        (let rec ((n 0) (p 1)
                  (pages pages)
                  (page-set '()))
          (cond ((null? pages) 
                 (run-list dir tag file-dir p (reverse page-set) total-pages))
                ((= n (page-size))
                 (run-list dir tag file-dir p (reverse page-set) total-pages)
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
                 (list-type type))
   (when (not (directory-exists? dir))
         (create-directory dir))
   (with-output-to-file
       (make-pathname dir (->string p) "html")
     (lambda ()
       (print
        (serialize-sxml
         ((base-template) 
          ((page-template)
           ((list-template) 
            (map (lambda (page)
                   (parameterize ((current-page page))
                     ((list-item-template))))
                 page-set))))))))))

(define pages (load-pages ""))

(define posts (load-pages "posts/"))

(with-output-to-file (make-pathname "out" "index" "html")
  (lambda ()
    (print 
     (serialize-sxml
      (parameterize ((current-page (load-page "/" "src/_index.site")))
       ((base-template)
        ((index-template))))))))

(load-pages-or-posts "" pages page-content-template)

(load-pages-or-posts "posts/" posts post-content-template)

(load-lists "posts/" posts)

(quit)
