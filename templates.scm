(define-template '/ 'page
 (lambda ()
   `(div (@ (class "wrap"))
     (div (@ (class "columns"))
          (div (@ (class "column left"))
               (h1 (@ (class "site-title"))
                   (span (@ (class "title"))
                         "Reading")
                   (br)
                   (span (@ (class "name"))
                         "Paul Valery")
                   (br)
                   ;; (span (@ (class "text"))
                   ;;       "in Translation"))
                   )

               (ul (@ (class "menu"))
                   (li (@ (class "menu-item"))
                       (a (@ (href "/about.html"))
                          "About the Notebooks"))
                   (li (@ (class "menu-item"))
                       (a (@ (href "/posts/index.html"))
                          "Browse by Category"))
                   (li (@ (class "menu-item"))
                       (a (@ (href "/essays/index.html"))
                          "Three Essays")))

               (div (@ (class "book"))
                    (p (@ (class "forthcoming"))
                       "Forthcoming February, 2020")
                    (p (@ (class "book-title"))
                       (a (@ (href "/book.html"))
                          "The Idea of Perfection"))
                    (p (@ (class "book-subtitle"))
                       (a (@ (href "/book.html"))
                          "The Poetry and Prose of Paul Valery"))
                    (p (@ (class "book-cover"))
                       (img (@ (src "http://marginalia.be/valery.jpg")))))

               (div (@ (class "notices"))
                  (p (@ (class "copyright"))
                     "Translations Copyright 2019 by " (br)
                     (a (@ (href "http://nathanielrb.com"))
                        "Nathaniel Rudavsky-Brody"))

                  (p (@ (class "sources"))
                     (a (@ (href "/sources.html"))
                        "Sources and Acknowledements"))))
               
          (div (@ (class "column right"))
               (div (@ (class "content"))
                    ,(content)))))))


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
         (p  ,($content))
         (h3 -- ,($ 'title))
         ,(tag-links)
         (ul (@ (class "nav"))
             ,(if (prev-document)
                  `(li (@ (class "left")) ,(prev-document-link "<"))
                  "")
             ,(if (next-document) 
                  `(li  (@ (class "right")) ,(next-document-link ">"))
                  "")))))


(define-template 'post 'list-item
   (lambda ()
     `(li (a (@ (href ,(path)))
             ,($ 'first-line))
          " " (small ,($ 'title)))))

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
           (h2 "Three Essays")
           (p "Excerpted from " (i  "Regards sur le monde actuel") ".")
           (ul ,(list-items))))))
