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


;;; comments
(define (can-parse-comment? bindings)
    (exists-binding? 'comment bindings))

(define (parse-comment bindings)
    (extract-binding/single 'comment bindings))

(define (render-confirm-add-comment-page a-comment a-post request)
    (define (response-generator embed/url)
        (response/xexpr
            `(html (head (title "Add a Comment"))
                (body
                    (h1 "Add a Comment")
                    "The comment: " (div (p ,a-comment))
                    "will be added to "
                    (div ,(post-title a-post))
                    
                    (p (a ((href ,(embed/url yes-handler)))
                        "Yes, add the comment."))
                    (p (a ((href ,(embed/url cancel-handler)))
                        "No, I changed my mind!"))))))

    (define (yes-handler request)
        (post-append-comment! a-post a-comment)
        (render-post-detail-page a-post request))

    (define (cancel-handler request)
        (render-post-detail-page a-post request))
    (send/suspend/dispatch response-generator))

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
    (define p-title (post-title a-post))
    (define (response-generator embed/url)
        (response/xexpr
            `(html
                (head (title ,p-title))
                (body
                    (h2 ,p-title))
                    (p ,(post-body a-post))
                    (a ((href ,(embed/url back-handler))) "Back to Blog")
                    ,(render-as-itemized-list (post-comments a-post))
                    (form ((action ,(embed/url insert-comment-handler)))
                        (input ((name "comment")))
                        (input ((type "submit")))))))

    (define (insert-comment-handler request)
        (define bindings (request-bindings request))
        (if (can-parse-comment? bindings)
            (render-confirm-add-comment-page
                (parse-comment bindings) a-post request)
            (render-post-detail-page a-post request)))

    (define (back-handler request)
        (render-blog-page request))
    (send/suspend/dispatch response-generator))

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
        (define bindings (request-bindings request))
        (when (can-parse-post? bindings)
              (blog-insert-post! BLOG (parse-post bindings)))
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