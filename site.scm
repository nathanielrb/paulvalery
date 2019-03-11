(use sxml-serializer posix srfi-1)

(define src-directory (make-parameter "src"))

(define templates-file (make-parameter "templates.scm"))

(define page-size (make-parameter 3))

;; Pages

(define-record page type name vars content)

(define (page-source-directory page)
  (make-pathname "src" (->string (page-type page))))

(define (type-source-directory type)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (make-pathname "src" dir) "*.nb")))

(define (page-out-directory page)
  (make-pathname "out" (->string (page-type page))))

(define (type-out-directory type)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname "out" dir)))

(define (type-tag-list-out-directory type tag)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (make-pathname "out" dir)
                   (conc "_" tag))))

;; fix this
(define (type-tag-list-path type tag)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (make-pathname "out" dir)
                   (conc "_" tag))))

(define (page-out-path page)
  (make-pathname "out" 
   (make-pathname (->string (page-type page))
                  (page-name page) "html")))

(define (list-page-out-directory type)
  (make-pathname "out" (make-pathname (->string type) "list")))

(define (list-page-out-path type p)
  (make-pathname "out" (make-pathname (->string type) "list")
                 (->string p) "html"))

(define (list-page-filename p)
  (conc (->string p) ".html"))

(define (page-url page)
  (make-pathname "/"
   (make-pathname (->string (page-type page)) 
                  (page-name page) "html")))

;; Dynamic parameters

(define index? (make-parameter #f))

(define current-page (make-parameter #f))

(define list-pages (make-parameter '()))

(define prev-pages (make-parameter #f))

(define (prev-page)
  (let ((pp (prev-pages)))
    (and pp (not (null? pp))
         (car pp))))

(define next-pages (make-parameter #f))

(define (next-page)
  (let ((np (next-pages)))
    (and np (not (null? np))
         (car np))))

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

;; Templating

(define (define-template level type template)
  (put! level type template))

(define (get-template level type)
  (get level type))

(define (base)
  (let ((type (or (list-type) (page-type (current-page)))))
    ((or (get-template type 'base) (get-template '/ 'base)))))

(define (page)
  (let ((type (or (list-type) (page-type (current-page)))))
    ((or (get-template type 'page)  (get-template '/ 'page)))))
  
(define (content)
  (cond ((index?) (index))
        ((list-type) (page-list))
        (else (p-content))))

(define (p-content) ; naming
  (let ((type (page-type (current-page))))
    ((or (get-template type 'content) (get-template '/ 'content) ))))

(define (index) ; naming
  (let ((type (page-type (current-page))))
    ((or (get-template type '_index) (get-template '/ '_index)))))

(define (page-list) ; naming
  (let ((type (list-type)))
    ((or (get-template type 'list) (get-template '/ 'list)))))

(define (list-items)
  (map (lambda (page)
         (parameterize ((current-page page))
          (list-item)))
       (list-pages)))

(define (list-items-by-type type)
  (let ((docs (documents type)))
    (parameterize ((list-page-number 1)
                   (list-total-pages (length docs))
                   (list-type type))
   (map (lambda (page)
          (parameterize ((current-page page))
            (list-item)))
        docs))))

(define (list-item)
  (let ((type (list-type)))
    ((or (get-template type 'list-item) (get-template '/ 'list-item)))))

;; Default templates

(define-template '/ 'base
   (lambda ()
     `((xhtml-1.0-strict)
       (html
        (head
         (title ,($ 'title))
         (link (@ (rel "stylesheet") (type "text/css")
                  (href "site.css"))))
        (body ,(page))))))

(define-template '/ 'page
  (lambda ()
    `(div
      (div
       (h1 ,($ 'title)))
      ,(content))))

(define-template '/ '_index
  (lambda ()
    `(div
      (div
       (h1 ,($ 'title)))
      ,($content) 
      (ul ,(list-items)))))
        
(define-template '/ 'content
   (lambda ()
     `(div
        (p ,($content)))))

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
      (ul ,(list-items))
      ,(prev-list-page-link)
      ,(next-list-page-link)
      )))

(define-template '/ 'list-item
  (lambda ()
    `(li (a (@ (href ,(url)))
            ,($ 'title)))))


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

(define (load-files type)
  (sort (map (lambda (path) (load-page type path))
             (glob (type-source-directory type)))
        (page-sorter)))

;; Load

(load (templates-file))
               
(define (render-pages type)
  (let ((dir (type-out-directory type))
        (pages (documents type)))
    (when (not (directory-exists? dir)) 
          (create-directory dir))

  (do ((pages pages (cdr pages))
       (rev-pages '() (cons (car pages) rev-pages)))
      ((null? pages))
    (let ((page (car pages)))
    (parameterize ((current-page page)
                   (prev-pages (cdr pages))
                   (next-pages rev-pages))
        (with-output-to-file (page-out-path page)
          (lambda ()
            (print
             (serialize-sxml 
              (base))))))))))
  

(define (extract-tags pages)
  (delete-duplicates
   (apply append (map (cut $ 'tags <>) pages))))

(define (filter-tag tag pages)
  (filter (lambda (page)
            (member tag ($ 'tags page)))
          pages))

(define (render-lists type )
  (parameterize ((current-page (make-page type (->string type) `((title . ,type)) "none")))
   (paginate-list type (documents type) (list-page-out-directory type))))
  
(define (render-tags-lists type)
  (let* ((pages (documents type))
         (tags (extract-tags pages)))
    (do ((tags tags (cdr tags)))
        ((null? tags))
      (let* ((tag (car tags))
             (file-dir (type-tag-list-out-directory type tag))
             (page-set '())
             (pages (filter-tag tag pages)))
        (parameterize ((current-page (make-page #f tag `((title . ,tag)) "none"))
                       (list-tag tag))
          (paginate-list type pages file-dir))))))

(define (paginate-list type pages file-dir)
  (when (not (directory-exists? file-dir))
        (create-directory file-dir))
  
  (let ((total-pages (length pages)))
    (let rec ((n 0) (p 1)
              (pages pages)
              (page-set '()))

      (cond ((null? pages) 
             (render-list-page type file-dir p (reverse page-set) total-pages))
            ((= n (page-size))
             (render-list-page type file-dir p (reverse page-set) total-pages)
             (rec 0 (+ p 1) pages '()))
            (else (rec (+ n 1) p (cdr pages) 
                       (cons (car pages) page-set)))))))
               
(define (render-list-page type dir p page-set total-pages)
  (print "type " type ", page " p "/" total-pages ", dir " dir ": " page-set)
  (parameterize ((list-page-number p)
                 (list-total-pages total-pages)
                 (list-type type) 
                 (list-pages page-set))
 (print "here")
   (with-output-to-file (make-pathname dir (list-page-filename p))
     (lambda ()
       (print
        (serialize-sxml (base)))))  ))

(define (save-documents type documents)
  (put! type '_documents documents))

(define (documents type)
  (get type '_documents))

(save-documents '/ (load-files '/))

(save-documents 'posts (load-files 'posts))

(with-output-to-file (make-pathname "out" "index" "html")
  (lambda ()
    (print 
     (serialize-sxml
      (parameterize ((current-page (load-page '_index "src/_index.site"))
                     (list-pages (documents '/))
                     (list-page-number 1)
                     (list-total-pages (length (documents '/)))
                     (list-type '/)
                     (index? #t))
       (base))))))

(render-pages '/)

(render-pages 'posts)

(render-tags-lists 'posts)

(render-lists 'posts)

(quit)
