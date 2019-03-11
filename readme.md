# Static Site Generator

## Overview

## Templating

Example:

```
(define (truncate len str) ...)

(define-template 'posts 'list-title
  (lambda ()
    (truncate 20 ($ 'title))))
```

## Site Settings

## API

### Parameters

(define source-directory (make-parameter "src"))

(define settings-path (make-parameter "site.nb"))

(define out-directory (make-parameter "out"))

(define templates-file (make-parameter "templates.scm"))

(define list-page-size (make-parameter 3))

(define html-extension? (make-parameter #t))

(define local-paths? (make-parameter #t))

#### Accessors

`(path)`

`(type)`

`(current-documents)`

`(list-documents)`

`($content)`

`($ VAR)`

`(list-type)` `-tag`, `-page-number`, `-total-pages`, `-length`


### Templating


`(base)`

`(page)`

`(content)`

`(index)`

`(document-list)`

`(list-items)`

`(list-items-by-type TYPE)`

`(list-item)`

`(page-title)`

`(list-title)`