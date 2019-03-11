(use sxml-serializer posix srfi-1)

(define source-directory (make-parameter "src"))

(define settings-path (make-parameter "site.nb"))

(define out-directory (make-parameter "out"))

(define templates-file (make-parameter "templates.scm"))

(define list-page-size (make-parameter 3))

(define html-extension? (make-parameter #t))

(define local-paths? (make-parameter #t))

;; Pages

(define-record document type name vars content)

;; File Paths

(define (document-source-directory page)
  (make-pathname (source-directory)
                 (->string (document-type page))))

(define (type-source-directory type)
  (let ((dir (if (eq? type '/) "." (->string type))))
    (make-pathname (source-directory) dir)))

(define (type-index-source type)
  (make-pathname (type-source-directory type) "_index.site"))

(define (type-index-out-path type)
  (make-pathname (type-out-directory type) "index" "html"))

(define (document-out-directory page)
  (make-pathname (out-directory) 
                 (->string (document-type page))))

(define (document-filename page)
  (make-pathname "" (document-name page) "html"))

(define (document-out-path page)
  (make-pathname (document-out-directory page) 
                 (document-filename page)))

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

(define (list-count-pages n)
  (inexact->exact (ceiling (/ n (list-page-size)))))

(define (html-extension)
  (if (html-extension?) "html" ""))

(define base-path (make-parameter "/"))

(define (document-path page)
  (make-pathname (base-path)
   (make-pathname (->string (document-type page)) 
                  (document-name page) 
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

(define current-document (make-parameter #f))

(define (path)
  (cond ((current-document) (document-path (current-document)))
        ((index?) (index-path (list-type)))
        (else
         (type-tag-list-path (list-type) (list-tag) (list-page-number))
         (type-list-path (list-type) (list-page-number)))))

(define list-documents (make-parameter '()))

(define prev-documents (make-parameter #f))

(define (prev-document)
  (let ((pp (prev-documents)))
    (and pp (not (null? pp))
         (car pp))))

(define next-documents (make-parameter #f))

(define (next-document)
  (let ((np (next-documents)))
    (and np (not (null? np))
         (car np))))

(define ($content #!optional page)
  (document-content (current-document)))

(define ($ var #!optional page)
  (let ((page (or page (current-document))))
    (alist-ref var (document-vars page))))

(define list-type (make-parameter #f))

(define list-tag (make-parameter #f))

(define list-page-number (make-parameter #f))

(define list-total-pages (make-parameter #f))

(define list-length (make-parameter #f))

(define (type)
  (if (current-document)
      (document-type (current-document))
      (list-type)))

;; Links

(define (prev-document-link #!optional text)
  (let ((page (prev-document))
        (text (or text ($ 'title page))))
    (if page
        `(a (@ (href ,(document-path page))) 
            ,text)
        "")))

(define (next-document-link #!optional text)
  (let ((page (next-document))
        (text (or text ($ 'title page))))
    (if page
        `(a (@ (href ,(document-path page))) 
            ,text)
        "")))

(define (prev-list-page-link #!optional (text "<"))
  (let ((path (prev-list-page-path)))
    (if path
        `(a (@ (href ,path)) ,text)
        "")))
  
(define (next-list-page-link #!optional (text ">"))
  (let ((path (next-list-page-path)))
    (if path
        `(a (@ (href ,path)) ,text)
        "")))

;; Templating

(define (define-template level type template)
  (put! level type template))

(define (get-template level type)
  (get level type))

(define (base)
  (let ((type (or (list-type) (document-type (current-document)))))
    ((or (get-template type 'base) (get-template '/ 'base)))))

(define (page)
  (let ((type (or (list-type) (document-type (current-document)))))
    ((or (get-template type 'page)  (get-template '/ 'page)))))
  
(define (content)
  (cond ((index?) (index))
        ((list-type) (document-list))
        (else (page-content))))

(define (index)
  (let ((type (document-type (current-document))))
    ((or (get-template type '_index) (get-template '/ '_index)))))

(define (page-content)
  (let ((type (document-type (current-document))))
    ((or (get-template type 'content) (get-template '/ 'content) ))))

(define (document-list)
  (let ((type (list-type)))
    ((or (get-template type 'list) (get-template '/ 'list)))))

;; TO DO
;; Macro for calling `list-items` in templates with parameterization 
;; of current-document

;; (define-syntax map-documents
;;   (syntax-rules ()
;;     ((_ (var) documents body)
;;      (map (lambda (var)
;;             (parameterize ((current-document var))
;;              body))
;;           documents))))

(define (list-items)
  (map (lambda (page)
         (parameterize ((current-document page))
          (list-item)))
       (list-documents)))

(define (list-items-by-type type)
  (let ((docs (documents type)))
    (parameterize ((list-documents docs)
                   (list-page-number 1)
                   (list-total-pages (list-count-pages (length docs)))
                   (list-type type))
     (list-items))))

(define (list-item)
  (let ((type (list-type)))
    ((or (get-template type 'list-item) (get-template '/ 'list-item)))))

(define (list-title)
  (let ((type (or (list-type) (document-type (current-document)))))
    ((or (get-template type 'list-title) (get-template '/ 'list-title)))))

(define (page-title)
  (let ((type (or (list-type) (document-type (current-document)))))
    ((or (get-template type 'page-title) (get-template '/ 'page-title)))))

;; Default templates

(define-template '/ 'base
   (lambda ()
     `((xhtml-1.0-strict)
       (html
        (head
         (title ,(page-title))
         (link (@ (rel "stylesheet") (type "text/css")
                  (href ,(make-pathname (base-path) "static/site" "css")))))
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
       (h1 ,(settings 'title)))
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
       (h1 ,($ 'title)))
      (ul ,(list-items))
      ,(prev-list-page-link)
      " " ,(list-page-number) / ,(list-total-pages) " "
      ,(next-list-page-link)
      )))

(define-template '/ 'list-item
  (lambda ()
    `(li (a (@ (href ,(path)))
            ,(list-title)))))

(define-template '/ 'list-title
  (lambda ()
    ($ 'title)))

(define-template '/ 'page-title
  (lambda ()
    (conc
     (let ((title ($ 'title)))
       (if title
           (conc title " | ")
           ""))
     (settings 'title))))

;; File loading

(define (load-document type path)
  (with-input-from-file path
    (lambda ()
      (let-values (((_ name ext) (decompose-pathname path)))
        (make-document type name (read) (read-string))))))

;; Needs real date parsing and handling
(define (date>? a b)
  (let ((date-a (alist-ref 'date (document-vars a)))
        (date-b (alist-ref 'date (document-vars b))))
    (and date-a date-b
         (string>? date-a date-b))))

(define document-sorter
  (make-parameter date>?))

(define (load-files type)
  (sort (map (lambda (path) (load-document type path))
             (glob (make-pathname (type-source-directory type)
                                  "*" "nb")))
        (document-sorter)))

;; Load

(load (templates-file))
               
(define (render-pages type)
  (let ((dir (type-out-directory type))
        (pages (documents type)))
    (when (not (directory-exists? dir)) 
          (create-directory dir))

  (do ((pages pages (cdr pages))
       (rev-documents '() (cons (car pages) rev-documents)))
      ((null? pages))
    (let ((page (car pages)))
    (parameterize ((current-document page)
                   (prev-documents (cdr pages))
                   (next-documents rev-documents))
        (with-output-to-file (document-out-path page)
          (lambda ()
            (print
             (serialize-sxml 
              (base))))))))))
  

(define (render-index type)
  (let* ((index-src (type-index-source type))
         (index-document (if (file-exists? index-src) (load-document '_index index-src)
                         (make-document type (->string type) `((title . ,type)) #f))))
    (with-output-to-file (type-index-out-path type)
      (lambda ()
        (print 
         (serialize-sxml
          (parameterize ((current-document index-document)                      
                         (list-documents (documents type))
                         (list-page-number 1)
                         (list-total-pages (list-count-pages (length (documents type))))
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
  (parameterize ((current-document (make-document type (->string type) `((title . ,type)) "none")))
   (paginate-list type (documents type) (type-list-out-directory type))))
  
(define (render-tags-lists type)
  (let* ((pages (documents type))
         (tags (extract-tags pages)))
    (do ((tags tags (cdr tags)))
        ((null? tags))
      (let* ((tag (car tags))
             (file-dir (type-tag-list-out-directory type tag))
             (document-set '())
             (pages (filter-tag tag pages)))
        (parameterize ((current-document (make-document #f tag `((title . ,tag)) "none"))
                       (list-tag tag))
          (paginate-list type pages file-dir))))))

(define (paginate-list type pages file-dir)
  (when (not (directory-exists? file-dir))
        (create-directory file-dir))
  
  (let ((total-documents (length pages)))
    (let rec ((n 0) (p 1)
              (pages pages)
              (document-set '()))
      (cond ((null? pages) 
             (render-list-page type file-dir p (reverse document-set) total-documents))
            ((= n (list-page-size))
             (render-list-page type file-dir p (reverse document-set) total-documents)
             (rec 0 (+ p 1) pages '()))
            (else (rec (+ n 1) p (cdr pages) 
                       (cons (car pages) document-set)))))))
               
(define (render-list-page type out-directory p document-set total-documents)
  (parameterize ((list-page-number p)
                 (list-total-pages (list-count-pages total-documents))
                 (list-type type) 
                 (list-documents document-set))
   (with-output-to-file (make-pathname out-directory (type-list-filename type p))
     (lambda ()
       (print
        (serialize-sxml (base)))))  ))

(define (save-documents type documents)
  (put! type '_documents documents))

(define (documents type)
  (get type '_documents))

(define *settings* (with-input-from-file (settings-path) read))

(define (settings var)
  (alist-ref var *settings*))
    
(when (settings 'base-path)
      (base-path (settings 'base-path)))

(define types (settings 'types))

(map (lambda (type)
       (save-documents type (load-files type)))
     types)

(map (lambda (type)
       (render-pages type)
       (render-index type)
       (render-lists type)
       (render-tags-lists type))
     types)

(quit)
