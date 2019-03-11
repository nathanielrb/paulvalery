(define (tag-links)
  `(ul (@ (class "tags"))
       ,@(map (lambda (tag)
                `(li (@ (class "tag"))
                     (a (@ (href ,(type-tag-list-path (type) tag)))
                        ,tag)))
              ($ 'tags))))

(define-template '/ 'page
 (lambda ()
   `(div
     (div
      (h1 "The notebooks of" (br) "Paul Valery"))
     ,(content)
     (ul ,(list-items-by-type '/))
     (h2 (small "Coming soon:") "The Idea of Perfection")
     (p (@ (class "copyright"))
        "Translations Copyright 2019 Nathaniel RB"))))

(define-template '/ 'content
 (lambda ()
   `(div (@ (class "columns"))
         (div (@ (class "column left"))
              (ul (@ (class "nav"))
                  )
         (div (@ (class "column right"))
              (h3 ,($ 'title))
              (p  ,($content))
              )))))

(define-template 'posts 'content
 (lambda ()
   `(div (@ (class "columns"))
         (p "POSTS")
         (div (@ (class "column left"))
              (ul (@ (class "nav"))
                  ,(if (prev-document)
                       `(li "<< " ,(prev-document-link))
                       "")
                  ,(if (next-document) 
                       `(li ">> " ,(next-document-link))
                       "")
                  (li (@ (class "facebook"))
                      "FB: "
                      ,(path))))
         (div (@ (class "column right"))
              (p  ,($content))
              (h3 -- ,($ 'title))
              ,(tag-links)))))

(define-template 'post 'list-item
   (lambda ()
     `(li (a (@ (href ,(path)))
             ,($ 'first-line))
          " " (small ,($ 'title)))))

(define-template '/ '_index
 (lambda ()
   (parameterize ((current-document (car (documents 'posts)))
                  (prev-documents (cdr (documents 'posts))))
     `(div (p "INDEX")
           ,($content)))))

(define-template 'posts '_index
 (lambda ()
   `(div (p "INDEX")
         (ul ,(list-items)))))
