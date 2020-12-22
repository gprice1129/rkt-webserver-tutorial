#lang web-server/insta

;;; servlet
(define (start request)
    (render-blog-page BLOG request))


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
(struct post (title body))

(define (render-post a-post)
    `(div ((class "post"))
        (h2 ,(post-title a-post))
        (p ,(post-body a-post))))

(define (render-posts posts)
    `(div ((class "posts"))
        ,@(map render-post posts)))

(define (can-parse-post? bindings)
    (and (exists-binding? 'title bindings)
         (exists-binding? 'body bindings)))

(define (parse-post bindings)
    (post (extract-binding/single 'title bindings)
          (extract-binding/single 'body bindings)))


;;; blog
(define (render-blog-page a-blog request)
    (define (response-generator embed/url)
        (response/xexpr
            `(html
                (head (title "Guthrie's Blog"))
                (body (h1 "Guthrie's Blog")
                    ,(render-posts a-blog)
                    (form ((action ,(embed/url insert-post-handler)))
                        (input ((name "title")))
                        (input ((name "body")))
                        (input ((type "submit"))))))))

    (define (insert-post-handler request)
        (render-blog-page
            (cond ((can-parse-post? (request-bindings request))
                   (cons (parse-post (request-bindings request)) a-blog))
              (else a-blog))
            request))

    (send/suspend/dispatch response-generator))


;;; static data
(define ex-post-1 (post "Example post #1" "This is the body of post #1"))
(define ex-post-2 (post "Example post #2" "This is the body of post #2"))
(define BLOG (list ex-post-1 ex-post-2))