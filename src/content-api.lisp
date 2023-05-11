(defpackage #:cl-ghost/content
  (:use #:cl-ghost #:cl)
  (:nicknames #:content))

(in-package #:content)


(defclass content-api (endpoint get-r)
  ())

(defmethod pick-base ((endpoint content-api))
  (admin-url *ghost*))

(defclass browse-api (content-api)
  ())

(defclass read-api (content-api)
  ())

(defmacro defendpoint (name supers url &body body)
  `(progn (defclass ,name ,supers
            ((url
              :initform ,url)))
          (export (list ',name))))


(defendpoint posts-browse (browse-api)
    "/posts/"
  ())

(defendpoint posts-by-id (read-api has-id)
    "/posts/"
  ())

(defendpoint posts-by-slug (read-api has-slug)
    "/posts/slug/"
  ())

(defendpoint authors-browse (browse-api)
    "/authors/"
  ())

(defendpoint authors-by-id (read-api has-id)
    "/authors/"
  ())

(defendpoint authors-by-slug (read-api has-slug)
    "/authors/slug/"
  ())

(defendpoint tags-browse (browse-api)
    "/tags/"
  ())

(defendpoint tags-by-id (read-api has-id)
    "/tags/"
  ())

(defendpoint tags-by-slug (read-api has-slug)
    "/tags/slug/"
  ())

(defendpoint pages-browse (browse-api)
    "/pages/"
  ())

(defendpoint pages-by-id (read-api has-id)
    "/pages/"
  ())

(defendpoint pages-by-slug (read-api has-slug)
    "/pages/slug/"
  ())

(defendpoint tiers-browser (browse-api)
    "/tiers/"
  ())

(defendpoint settings (browse-api)
    "/settings/"
  ())

(defmethod call-api :before ((endpoint content-api))
  (unless (slot-boundp *ghost* 'content-api-key)
    (signal-poorly-configured "Please set the slot 'content-api-key in *ghost*"))
  (unless (slot-boundp *ghost* 'api-url)
    (signal-poorly-configured "Please set the slot 'api-url in *ghost*")))

(defmethod call-api ((endpoint content-api))
  (with-accessors ((query query)
                   (url url))
      endpoint
    (with-accessors ((content-api-key content-api-key))
        *ghost*
      (let ((query-url (generate-query (cons (cons "key" content-api-key) query))))
        (apply (dex-fun endpoint)
               (build-url endpoint query-url)
               (build-headers endpoint))))))
        
        
        
  
  

