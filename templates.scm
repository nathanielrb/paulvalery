(define (tag-links)
  `(ul (@ (class "tags"))
       ,@(map (lambda (tag)
                `(li (@ (class "tag"))
                     (a (@ (href ,(tag-url tag)))
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
                  ,(if (prev-page)
                       `(li "<< " ,(prev-page-link))
                       "")
                  ,(if (next-page) 
                       `(li ">> " ,(next-page-link))
                       "")
                  (li (@ (class "facebook"))
                      "FB: "
                      ,(url))))
         (div (@ (class "column right"))
              (p  ,($content))
              (h3 -- ,($ 'title))
              ,(tag-links)))))

(define-template 'post 'list-item
   (lambda ()
     `(li (a (@ (href ,(url)))
             ,($ 'first-line))
          " " (small ,($ 'title)))))

(define-template '/ '_index
 (lambda ()
   (parameterize ((current-page (car (documents 'posts)))
                  (prev-pages (cdr (documents 'posts))))
     `(div (p "INDEX")
           ,($content)))))
