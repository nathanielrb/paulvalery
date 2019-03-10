(use sxml-serializer posix srfi-1)

(define src-directory (make-parameter "src"))

(define page-size (make-parameter 3))

(define types (make-parameter '("notebook")))

(define-record page type name vars content)

(define current-page (make-parameter #f))

(define ($content #!optional page)
  (let ((page (or page (current-page))))
    (page-content (current-page))))

(define ($ var #!optional page)
  (let ((page (or page (current-page))))
    (or (alist-ref var (page-vars page)) "")))

(define prev-pages (make-parameter #f))

(define next-pages (make-parameter #f))

(define page-number (make-parameter #f))

;; Templates

(define (prev-page-link)
  (if (null? (prev-pages)) ""
  (let ((page (car (prev-pages))))
   `(a (@ (href ,(url page))) ,($ 'title page)))))

(define (next-page-link)
  (if (null? (next-pages)) ""
      (let ((page (car (next-pages))))
        `(a (@ (href ,(url page))) ,($ 'title page)))))

(define (url #!optional page)
  (let ((page (or page (current-page))))
    (make-pathname (page-type page) (page-name page) "html")))

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
        
(define page-content-template
  (make-parameter
   (lambda ()
     `(div
        (p ,($content))))))

(define post-template
  (make-parameter
   (lambda (content)
     `(div
       (div
        (h1 ,($ 'title)))
       ,content))))
        
(define post-content-template
  (make-parameter
   (lambda ()
     `(div
        (p ,($content))))))

(define list-page-template
  (make-parameter
   (lambda (content)
     `(div
       (div
        (h1 ,($ 'title)))
       (ul ,content)
       ,(prev-list-page-link)
       ,(next-list-page-link)
       ))))       

(define list-item-template 
  (make-parameter
   (lambda ()
     `(li (a (@ (href ,(url)))
             ,($ 'title))))))

(define (render-list-page pages)
  ((base-template) 
   ((page-template)
    ((list-page-template) 
     (render-list-items pages)))))

(define (render-list-items pages)
  (map (lambda (page)
           (parameterize ((current-page page))
            ((list-item-template))))
         pages))

;; File loading

(define (file-path page)
  (let ((page (or page (current-page))))
    (make-pathname (make-pathname "out" (page-type page))
                   (page-name page) "html")))

(define (load-page dir path)
  (with-input-from-file path
    (lambda ()
      (let-values (((_ name ext) (decompose-pathname path)))
      (make-page dir name (read) (read-string))))))

;; Needs real date parsing and handling
(define (date>? a b)
  (let ((date-a (alist-ref 'date (page-vars a)))
        (date-b (alist-ref 'date (page-vars b))))
    (print "Sorting on " date-a " >? " date-b)
    (and date-a date-b
         (string>? date-a date-b))))

(define page-sorter
  (make-parameter date>?))

(define (load-pages dir)
  (print "Doing " (glob (conc "./src/" dir "*.nb")))
  (sort (map (lambda (path) (load-page dir path))
             (glob (conc "./src/" dir "*.nb")))
        (page-sorter)))

;; Load

(include "templates.scm")

(define (load-directory dir #!optional
                        (page-content-template page-content-template)
                        (lists? #t))
  (when (not (directory-exists? (conc "out/" dir)))
        (create-directory (conc "out/" dir)))

  (let ((pages (load-pages dir)))
    ;; Index - needs to be generalized to template(s)
    (parameterize ((current-page (car pages))
                   (next-pages '())
                   (prev-pages (cdr pages)))
                  (with-output-to-file (conc "out/" dir "index.html")
                    (lambda ()
                      (print
                       (serialize-sxml
                        ((base-template)
                         ((page-template) 
                          ((page-content-template)))))))))

    ;; All Pages
    (do ((n 0 (+ n 1))
         (pages pages (cdr pages))
         (rev-pages '() (cons (car pages) rev-pages)))
        ((null? pages))
      (let ((page (car pages)))
        (parameterize ((current-page page)
                       (prev-pages (cdr pages))
                       (next-pages rev-pages))
                      (with-output-to-file (file-path page)
                        (lambda ()
                          (print
                           (serialize-sxml
                            ((base-template)
                             ((page-template)
                              ((page-content-template)))))))))))

    ;; Tags List
    (when lists?
          (let ((tags (delete-duplicates
                       (apply append (map (cut $ 'tags <>) pages)))))
            (do ((tags tags (cdr tags)))
                ((null? tags))
              (let* ((tag (car tags))
                     (dir (make-pathname (make-pathname "out/" dir) tag))
                     (page-set '())
                     (pages (filter (lambda (page)
                                      (member tag ($ 'tags page)))
                                    pages))
                     (total-pages-n (length pages)))
                (let rec ((n 0)
                          (p 1)
                          (pages pages)
                          (page-set '()))
                  (cond ((null? pages) 
                         (run-list tag dir p (reverse page-set) total-pages-n))
                        ((= n (page-size))
                         (run-list tag dir p (reverse page-set) total-pages-n)
                         (rec 0 (+ p 1) pages '()))
                        (else (rec (+ n 1) p (cdr pages) 
                                   (cons (car pages) page-set)))))))))))

(define (prev-list-page-link)
  `(span
    (a (@ (href ,(page-number)))
       "<<" ,(- (page-number) 1))
    "/" ,(total-pages)))
  
(define total-pages (make-parameter #f))

(define (next-list-page-link)
  `(a (@ (href ,(page-number)))
     ">>" ,(+ (page-number) 1)))

(define (run-list tag dir p page-set total-pages-n)
  (parameterize ((current-page (make-page #f tag `((title . ,tag)) "none"))
                 (page-number p)
                 (total-pages total-pages-n))
   (when (not (directory-exists? dir))
         (create-directory dir))
   (with-output-to-file
       (make-pathname dir (->string p) "html")
     (lambda ()
       (print
        (serialize-sxml
         (render-list-page page-set)))))))

(load-directory "" page-content-template #f)

(load-directory "posts/"  post-content-template #t)

(quit)
