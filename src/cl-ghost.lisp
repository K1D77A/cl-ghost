;;;; cl-ghost.lisp
(in-package #:cl-ghost)


(defparameter *api*)

(defun split-api (api)
  (destructuring-bind (id secret)
      (str:split #\: api)
    (list :id id :secret secret)))

(defun jwt-header (id)
  `(("alg" . "HS256")
    ("kid" . ,id)
    ("typ" . "JWT")))

(defun jwt-payload (amount unit)
  (let* ((now (local-time:now))
         (future (local-time:timestamp+ now amount unit)))
    (values `(("iat" . ,now)
              ("exp" . ,(local-time:timestamp-to-unix future))
              ("aud" . "/admin"))
            now future)))

(defclass ghost ()
  ((api-url
    :accessor api-url
    :initarg :api-url
    :type string)
   (content-api-key
    :accessor content-api-key
    :initarg :content-api-key
    :type string)))

(defclass ghost-admin (ghost)
  ((admin-api-key
    :accessor admin-api-key
    :initarg :admin-api-key
    :type string)
   (jwt
    :accessor jwt
    :initarg :jwt
    :type jwt)))

(defclass jwt ()
  ((creation-time
    :accessor creation-time
    :initarg :creation-time
    :type local-time:timestamp)
   (token
    :accessor token
    :initarg :token
    :type string)
   (expire-time
    :accessor expire-time
    :initarg :expire-time
    :type local-time:timestamp)))

;;only admin actually has to login

(defgeneric login (ghost expire-amount expire-unit)
  (:documentation "Generates JWT for instance of GHOST that lasts until local-time:now +
EXPIRE-AMOUNT of EXPIRE-UNIT.
Expire unit one of: (:NSEC :SEC :MINUTE :HOUR :DAY :DAY-OF-WEEK :MONTH :YEAR)."))

(defmethod login ((ghost ghost-admin) (expire-amount fixnum) (expire-unit symbol))
  (check-type expire-unit keyword)
  (destructuring-bind (&key id secret &allow-other-keys)
      (split-api (admin-api-key ghost))
    (let ((secret-as-bytes (ironclad:hex-string-to-byte-array secret)))
      (multiple-value-bind (payload now future)
          (jwt-payload expire-amount expire-unit)
        (let ((token (jose/jwt:encode :hs256 secret-as-bytes payload
                                      :headers (jwt-header id))))
          (setf (jwt ghost)
                (make-instance 'jwt :expire-time future
                                    :creation-time now
                                    :token token)))))))
      
(defparameter *query*
  ("fields" "include" "formats" "filter" "limit" "page" "order"))

(defparameter *version* "v5.0")

(defparameter *ghost* nil
  "Top level instance of GHOST class.")

(defclass endpoint ()
  ((url
    :accessor url
    :initarg :url
    :type (or string function))
   (accept-version
    :accessor accept-version
    :initarg :accept-version
    :initform *version*
    :type string)
   (query
    :accessor query
    :initarg :query
    :type (or null list))))

(defclass content-api (endpoint)
  ())

(defclass browse-api (content-api)
  ())

(defclass read-api (content-api)
  ())

(defclass has-id ()
  ((id
    :accessor id
    :initarg :id
    :type string)))

(defclass has-slug ()
  ((slug
    :accessor slug
    :initarg :slug
    :type string)))

(defmacro defendpoint (name supers url &body body)
  `(defclass ,name ,supers
      ((url
        :initform ,url))))


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


(defun generate-query (alist)
  (format nil "?~{~A~^&~}"
          (loop :for (key . val) :in alist
                :collect (format nil "~A=~A" key val))))

(defgeneric build-url (endpoint query)
  (:method ((endpoint has-id) query)
    (format nil "~A~A~A/~A"
            (url *ghost*)
            (url endpoint)
            (id endpoint)
            query))
  (:method ((endpoint has-slug) query)
    (format nil "~A~A~A/~A"
            (url *ghost*)
            (url endpoint)
            (slug endpoint)
            query)))
            

(defgeneric call-api (endpoint))

(defmethod call-api ((endpoint content-api))
  (with-accessors ((query query)
                   (url url)
                   (accept-version accept-version))
      endpoint
    (with-accessors ((content-api-key content-api-key))
        *ghost*
      (let ((query-url (generate-query (cons (cons "key" content-api-key) query))))
        (dex:get (build-url endpoint query-url) :headers `(("Accept-Version" . *version*)))))))
        
        
        
  
  


