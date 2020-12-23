#lang web-server/insta

;;; servlet
(define (start request)
    (render-blog-page request))


;;; xexpr helpers

; render-as-itemized-list: (listof xexpr) -> xexpr
; Consumes a list of items, and produces a rendering
; as an unordered list.
(define (render-as-itemized-list fragments)
    `(ul ,@(map render-as-item fragments)))

; render-as-item: xexpr -> xexpr
; Consumes an xexpr, and produces a rendering
; as a list item.
(define (render-as-item a-fragment)
    `(li ,a-fragment))


;;; posts
(struct post (title body comments) #:mutable)

(define (post-append-comment! a-post a-comment)
    (set-post-comments!
        a-post
        (append (post-comments a-post) `(,a-comment))))

(define (render-post embed/url a-post)
    (define (view-post-handler request)
        (render-post-detail-page a-post request))
    `(div ((class "post"))
        (h2 (a ((href ,(embed/url view-post-handler))) ,(post-title a-post)))
        (p ,(post-body a-post))
        (p ,(string-append 
            "Comments: "
            (number->string (length (post-comments a-post)))))))

(define (render-posts embed/url)
    (define (render-helper a-post)
        (render-post embed/url a-post))
    `(div ((class "posts"))
        ,@(map render-helper (blog-posts BLOG))))

(define (can-parse-post? bindings)
    (and (exists-binding? 'title bindings)
         (exists-binding? 'body bindings)))

(define (parse-post bindings)
    (post (extract-binding/single 'title bindings)
          (extract-binding/single 'body bindings)
          '()))

(define (render-post-detail-page a-post request)
    ; TODO
    '())

;;; blog
(struct blog (posts) #:mutable)

(define (blog-insert-post! a-blog a-post)
    (set-blog-posts! a-blog (cons a-post (blog-posts a-blog))))
    
(define (render-blog-page request)
    (define (response-generator embed/url)
        (response/xexpr
            `(html
                (head (title "Guthrie's Blog"))
                (body (h1 "Guthrie's Blog")
                    ,(render-posts embed/url)
                    (form ((action ,(embed/url insert-post-handler)))
                        (input ((name "title")))
                        (input ((name "body")))
                        (input ((type "submit"))))))))

    (define (insert-post-handler request)
        (when (can-parse-post? (request-bindings request))
              (blog-insert-post! BLOG (parse-post (request-bindings request))))
        (render-blog-page request))

    (send/suspend/dispatch response-generator))


;;; static data
(define ex-post-1 (post "Example post #1" 
                        "This is the body of post #1"
                        '("Post #1 w/ Comment #1")))
(define ex-post-2 (post "Example post #2"
                        "This is the body of post #2"
                        '("Post #2 w/ Comment #1" "Post #2 w/ Comment #2")))
(define BLOG (blog `(,ex-post-1 ,ex-post-2)))