;;;; cl-ghost.lisp
(in-package #:cl-ghost)


(defclass ghost ()
  ((api-url
    :accessor api-url
    :initarg :api-url
    :type string)
   (content-api-key
    :accessor content-api-key
    :initarg :content-api-key
    :type string)))

(defmethod print-object ((o ghost) stream)
  (print-unreadable-object (o stream) :type t
    (format stream "URL: ~S" (api-url o))))

;;only admin actually has to login

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
  '("fields" "include" "formats" "filter" "limit" "page" "order"))

(defparameter *version* "v5.0")

(defparameter *ghost* nil
  "Top level instance of GHOST class.")


