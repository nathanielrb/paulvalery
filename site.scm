(use sxml-serializer posix srfi-1)

(define source-directory (make-parameter "src"))

(define settings-path (make-parameter "site.nb"))

(define out-directory (make-parameter "out"))

(define templates-file (make-parameter "templates.scm"))

(define page-size (make-parameter 3))

(define html-extension? (make-parameter #t))

(define local-paths? (make-parameter #t))

;; Pages

(define-record page type name vars content)

;; File Paths

(define (page-source-directory page)
  (make-pathname (source-directory)
                 (->string (page-type page))))

(define (type-source-directory type)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (source-directory) dir)))

(define (page-out-directory page)
  (make-pathname (out-directory) 
                 (->string (page-type page))))

(define (page-filename page)
  (make-pathname "" (page-name page) "html"))

(define (page-out-path page)
  (make-pathname (page-out-directory page) 
                 (page-filename page)))

(define (type-out-directory type)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (out-directory) dir)))

(define (type-list-out-directory type)
  (make-pathname (type-out-directory type) "_list"))

(define (type-list-filename type p)
  (make-pathname "" (->string p) "html"))

(define (type-list-out-path type p)
  (make-pathname (type-list-out-directory type)
                 (type-list-filename p)))

(define (type-tag-list-out-directory type tag)
  (make-pathname (type-out-directory type) (conc "_" tag)))

(define (type-tag-list-out-path type tag p)
  (make-pathname (type-tag-list-out-directory type)
                   (type-list-filename p)))

;; URL Paths

(define (number-of-pages n)
  (ceiling (/ n (page-size))))

(define (html-extension)
  (if (html-extension?) "html" ""))

;; Buggy ... need to keep better track of place in hierarchy
(define (base-path)
  (if (local-paths?)
      (if (current-page)
          (if (equal? (type) '/')
              "."
              (if (list-tag)
                  "../../"
                  "..")))
    "/"))

(define (page-path page)
  (make-pathname (base-path)
   (make-pathname (->string (page-type page)) 
                  (page-name page) 
                  (html-extension))))

(define (type-path type)
  (make-pathname (base-path) (->string type)))

(define (index-path type)
  (make-pathname (type-path type) 
                 "index" 
                 (html-extension)))

(define (type-list-path type #!optional page-number)
  (make-pathname (type-path type)
   (make-pathname "_list"
                  (if page-number (->string page-number) "1")
                  (html-extension))))

(define (type-tag-list-path type tag #!optional page-number)
  (make-pathname (type-path type)
    (make-pathname (conc "_" tag)
                   (if page-number (->string page-number) "1")
                   (html-extension))))

(define (list-page-path)
  (let ((tag (list-tag)))
    (if tag
        (type-tag-list-path (list-type) (list-tag) (list-page-number))
        (type-list-path (list-type) (list-page-number)))))

(define (next-list-page-path)
  (let ((tag (list-tag))
        (page-number (+ (list-page-number) 1)))
    (and (<= page-number (list-total-pages))
        (if tag
            (type-tag-list-path (list-type) (list-tag) page-number)
            (type-list-path (list-type) page-number)))))        

(define (prev-list-page-path)
  (let ((tag (list-tag))
        (page-number (- (list-page-number) 1)))
    (and (> page-number 0)
         (if tag
             (type-tag-list-path (list-type) (list-tag) page-number)
             (type-list-path (list-type) page-number)))))

;; Dynamic parameters

(define index? (make-parameter #f))

(define current-page (make-parameter #f))

(define (path)
  (cond ((index?) (index-path (list-type)))
        ((current-page) (page-path (current-page)))
        (else
         (type-tag-list-path (list-type) (list-tag) (list-page-number))
         (type-list-path (list-type) (list-page-number)))))

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
    (alist-ref var (page-vars page))))

(define list-type (make-parameter #f))

(define list-tag (make-parameter #f))

(define list-page-number (make-parameter #f))

(define list-total-pages (make-parameter #f))

(define (type)
  (if (current-page)
      (page-type (current-page))
      (list-type)))

;; Links

(define (prev-page-link)
  (let ((page (prev-page)))
    (if page
        `(a (@ (href ,(page-path page))) ,($ 'title page))
        "")))

(define (next-page-link)
  (let ((page (next-page)))
    (if page
        `(a (@ (href ,(page-path page))) ,($ 'title page))
        "")))

(define (prev-list-page-link)
  (let ((path (prev-list-page-path)))
    (if path
        `(a (@ (href ,path))
            "<<" ,(- (list-page-number) 1))
        "")))
  
(define (next-list-page-link)
  (let ((path (next-list-page-path)))
    (if path
        `(a (@ (href ,path))
            ">>" ,(->string (+ (list-page-number) 1)))
        "")))

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
                   (list-total-pages (number-of-pages (length docs)))
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
       (h2 ,(list-page-path)))
      (ul ,(list-items))
      ,(prev-list-page-link)
      ,(next-list-page-link)
      )))

(define-template '/ 'list-item
  (lambda ()
    `(li (a (@ (href ,(path)))
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
             (glob (make-pathname (type-source-directory type)
                                  "*" "nb")))
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
  

(define (render-index type)
  (let ((index-page (load-page '_index (make-pathname (type-source-directory type) "_index.site"))))
    (with-output-to-file (make-pathname (type-out-directory type) "index" "html")
      (lambda ()
        (print 
         (serialize-sxml
          (parameterize ((current-page index-page)                      
                         (list-pages (documents type))
                         (list-page-number 1)
                         (list-total-pages (number-of-pages (length (documents type))))
                         (list-type type)
                         (index? #t))
                        (base))))))))

(define (extract-tags pages)
  (delete-duplicates
   (apply append  (filter values (map (cut $ 'tags <>) pages)))))

(define (filter-tag tag pages)
  (filter (lambda (page)
            (member tag ($ 'tags page)))
          pages))

(define (render-lists type )
  (parameterize ((current-page (make-page type (->string type) `((title . ,type)) "none")))
   (paginate-list type (documents type) (type-list-out-directory type))))
  
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
               
(define (render-list-page type out-directory p page-set total-pages)
  (parameterize ((list-page-number p)
                 (list-total-pages (number-of-pages total-pages))
                 (list-type type) 
                 (list-pages page-set))
   (with-output-to-file (make-pathname out-directory (type-list-filename type p))
     (lambda ()
       (print
        (serialize-sxml (base)))))  ))

(define (save-documents type documents)
  (put! type '_documents documents))

(define (documents type)
  (get type '_documents))

(define settings (with-input-from-file (settings-path) read))

(define types (alist-ref 'types settings))

(map (lambda (type)
       (save-documents type (load-files type)))
  
     types)

(map (lambda (type)
       (render-pages type)
       (render-index type)
       (render-lists type)
       (render-tags-lists type))
     types)

;; (render-index 'posts)
(quit)
