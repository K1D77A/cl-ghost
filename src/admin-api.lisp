(defpackage #:cl-ghost/admin
  (:use #:cl-ghost #:cl)
  (:nicknames #:admin)
  (:export #:expired-jwt
           #:jwt
           #:ghost-admin
           #:endpoint
           #:ghost
           #:ghost-admin
           #:admin-api-key
           #:admin-url
           #:creation-time
           #:token
           #:expire-time
           #:login))

(in-package #:admin)

(define-condition expired-jwt (ghost-condition)
  ((jwt
    :accessor jwt
    :initarg :jwt
    :type jwt)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :type endpoint)
   (ghost
    :accessor ghost
    :initarg :ghost
    :type ghost-admin)))

(defclass ghost-admin (ghost)
  ((admin-api-key
    :accessor admin-api-key
    :initarg :admin-api-key
    :type string)
   (admin-url
    :accessor admin-url
    :initarg :admin-url
    :type string
    :documentation "Admin can be a different URL than normal Ghost instance.")
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

(defun expired-jwt-p (jwt)
  (with-accessors ((expire-time expire-time))
      jwt
    (local-time:timestamp>= (local-time:now) expire-time)))

(defclass admin-api (endpoint)
  ())

(defmethod pick-base ((endpoint admin-api))
  (admin-url *ghost*))

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
      
(defmethod call-api :before ((endpoint admin-api))
  (unless (slot-boundp *ghost* 'jwt)
    (signal-poorly-configured "Please eval (login *ghost* <amount> <unit>)"))
  (unless (slot-boundp *ghost* 'admin-api-key)
    (signal-poorly-configured "Please set the slot 'admin-api-key in *ghost*"))
  (unless (slot-boundp *ghost* 'admin-url)
    (signal-poorly-configured "Please set the slot 'admin-url in *ghost*"))
  (when (expired-jwt-p (jwt *ghost*))
    (restart-case 
        (error 'expired-jwt :jwt (jwt *ghost*)
                            :ghost *ghost*
                            :endpoint endpoint)
      (renew ()
        :report "Renew JWT?"
        (declare (special *amount* *unit*))
        (let ((amount (if (boundp '*amount*)
                          *amount*
                          30))
              (unit (if (boundp '*unit*)
                        *unit*
                        :minute)))
          (login *ghost* amount unit))))))

(defmethod call-api ((endpoint admin-api))
  (with-accessors ((query query)
                   (url url))
      endpoint
    (let ((query-url (generate-query query)))
      (apply (dex-fun endpoint)
             (build-url endpoint query-url)
             (append (build-headers endpoint)
                     (build-body endpoint))))))

(defmacro defendpoint (name supers url &body body)
  `(progn (defclass ,name ,supers
            ((url
              :initform ,(format nil "/admin~A" url))))
          (export (list ',name))))


(defendpoint posts-browse (admin-api get-r)
    "/posts/"
  ())

(defendpoint posts-by-id (admin-api get-r has-id)
    "/posts/"
  ())

(defendpoint posts-by-slug (admin-api get-r has-slug)
    "/posts/slug/"
  ())

(defendpoint posts-new (admin-api post-r)
    "/posts/"
  ())

(defendpoint posts-update (admin-api put-r has-id)
    "/posts/"
  ())

(defendpoint posts-delete (admin-api delete-r has-id)
    "/posts/"
  ())





(defendpoint pages-browse (admin-api get-r)
    "/pages/"
  ())

(defendpoint pages-by-id (admin-api get-r has-id)
    "/pages/"
  ())

(defendpoint pages-by-slug (admin-api get-r has-slug)
    "/pages/slug/"
  ())

(defendpoint pages-new (admin-api post-r)
    "/pages/"
  ())

(defendpoint pages-update (admin-api put-r has-id)
    "/pages/"
  ())

(defendpoint pages-delete (admin-api delete-r has-id)
    "/pages/"
  ())



(defendpoint tiers-browse (admin-api get-r)
    "/tiers/"
  ())

(defendpoint tiers-by-id (admin-api get-r has-id)
    "/tiers/"
  ())

(defendpoint tiers-new (admin-api post-r)
    "/tiers/"
  ())

(defendpoint tiers-update (admin-api put-r has-id)
    "/tiers/"
  ())






(defendpoint offers-browse (admin-api get-r)
    "/offers/"
  ())

(defendpoint offers-by-id (admin-api get-r has-id)
    "/offers/"
  ())

(defendpoint offers-new (admin-api post-r)
    "/offers/"
  ())

(defendpoint offers-update (admin-api put-r has-id)
    "/offers/"
  ())


(defendpoint roles-browse (admin-api get-r)
    "/roles/"
  ())

(defendpoint invites-new (admin-api post-r)
    "/invites/"
  ())



;;;users are staff
(defendpoint users-browse (admin-api get-r)
    "/users/"
  ())

(defendpoint users-update (admin-api put-r has-id)
    "/users/"
  ())

(defendpoint users-delete (admin-api delete-r has-id)
    "/users/"
  ())



(defclass multipart-form (post-r)
  ((content-type
    :initform "multipart/form-data")))

(defendpoint images-new (admin-api multipart-form)
    "/images/upload/"
  ())
;;need a special fun for this as its multipart form rather than json.

(defendpoint themes-new (admin-api multipart-form)
    "/themes/upload/"
  ())


(defendpoint themes-activate (admin-api put-r has-id)
    "/themes/";;fuck sake
  ())

;;this has special encoding for the URL.. have to take the ID and put in the right place

(defmethod build-url ((endpoint themes-activate) query)
  (format nil "~A~A/~A/activate/~A"
          (admin-url *ghost*)
          (url endpoint)
          (id endpoint)
          (or query
              "")))



(defendpoint site-browse (admin-api get-r)
    "/site/"
  ())




(defendpoint webhooks-new (admin-api post-r has-id)
    "/webhooks/"
  ())

(defendpoint webhooks-update (admin-api put-r has-id)
    "/webhooks/"
  ())

(defendpoint webhooks-delete (admin-api delete-r has-id)
    "/webhooks/"
  ())

