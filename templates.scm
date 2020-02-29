(define-template '/ 'page
 (lambda ()
   `(div (@ (class "wrap"))
     (div (@ (class "columns"))
          (div (@ (class "column left"))
               (h1
                (@ (class "site-title"))
                (a (@ (class "")
                      (href ,(make-path "index")))

                   (span (@ (class "title"))
                         "Reading")
                   (br)
                   (span (@ (class "name"))
                         "Paul Valery"))
                   )

               (ul (@ (class "menu"))
                   (li (@ (class "menu-item"))
                       (a (@ (href ,(make-path "index")))
                          (span (@ (class "from-script"))
                                "from the ")
                          "Notebooks"))
                   (li (@ (class "menu-item"))
                       (small "Essays")
                       (ul
                        (li (@ (class "sub-item"))
                            (a (@ (href ,(make-path "essays/liberty")))
                               "Fluctuations on Liberty"))
                        (li (@ (class "sub-item"))
                            (a (@ (href ,(make-path "essays/dictatorship")))
                               "The Idea of Dictatorship")))
                       )

               (li (@ (class "menu-item book"))
                   (ul
                    (li (@ (class "label"))
                       (small "Poetry"))
                    (li (@ (class "book-title"))
                       (a (@ (href ,(make-path "book")))
                          "The Idea of Perfection: "
                          (span (@ (class "book-subtitle"))
                                "The Poetry and Prose of Paul Valery")
                          (span (@ (class "forthcoming"))
                                " (FSG)")))))                      

                    (li (@ (class "menu-item notices"))
                        (ul
                         (li
                          (a (@ (href ,(make-path "about")))
                             "About Paul Valery"))
                         (li
                            (a (@ (href "http://nathanielrb.org"))
                               "NRB"))

                         (li
                            (a (@ (href ,(make-path "sources")))
                               "©"))
                         ))



               ))
               
          (div (@ (class "column right"))
               (div (@ (class "content"))
                    ,(content)
                    
                    ))
          ))))



(define-template '/ 'content
 (lambda ()
   `(div (@ (class "article"))
         (h2 ,($ 'title))
         (p  ,($content)))))

(define (tag-link tag)
  `(li (@ (class "tag"))
       (a (@ (href ,(type-tag-list-path (type) tag)))
          ,tag)))

(define (tag-links)
  `(ul (@ (class "tags"))
       ,@(map tag-link ($ 'tags))))

(define-template 'posts 'content
  (lambda ()
    `(div (@ (class "notebooks entry"))
          (div (@ (class "entry"))
               ,($content))
          (div (@ (class "info"))
               (p (@ (class "attribution"))
                  ,($ 'notebook) " ("
                  ,($ 'year) ")")
               ,(tag-links)
               (ul (@ (class "nav"))
                   ,(if (prev-document)
                        `(li (@ (class "left")) ,(prev-document-link "<"))
                        "")
                   ,(if (next-document) 
                        `(li  (@ (class "right")) ,(next-document-link ">"))
                        "")
                   )))))

(define-template 'posts 'list-title
  (lambda ()
    ($ 'first-line)))

(define-template '/ '_index
 (lambda ()
   (parameterize ((current-document (car (documents 'posts)))
                  (prev-documents (cdr (documents 'posts))))
     `(div ,(page-content)))))

(define-template 'posts '_index
 (lambda ()
   `(div (@ (class "notebooks categories"))
         (h2 "Notebook Categories")
           (p "From Paul Valery's notebooks, as edited by Judith Robinson.")
           ,(map tag-link (extract-tags (documents 'posts))))))

(define-template 'essays '_index
 (lambda ()
   (parameterize ((current-document (car (documents 'posts)))
                  (prev-documents (cdr (documents 'posts))))
     `(div (@ (class "essays"))
           (h2 "Essays")
           (ul (@ (class "list essays"))
               ,(list-items))))))
