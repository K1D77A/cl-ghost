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

(defclass endpoint ()
  ((url
    :accessor url
    :initarg :url
    :type (or string function))
   (dex-fun
    :accessor dex-fun
    :initarg :dex-fun
    :type function)
   (accept-version
    :accessor accept-version
    :initarg :accept-version
    :initform *version*
    :type string)
   (query
    :accessor query
    :initarg :query
    :initform nil
    :type (or null list))))

(defclass with-body ()
  ((body
    :accessor body
    :initarg :body
    :initform nil)
   (content-type
    :accessor content-type
    :initarg :content-type
    :initform "application/json"
    :type string)))


(defclass dex-fun ()
  ((dex-fun :accessor dex-fun)))

(defclass get-r (dex-fun)
  ((dex-fun
    :initform #'dex:get)))

(defclass put-r (dex-fun with-body)
  ((dex-fun
    :initform #'dex:put)))

(defclass delete-r (dex-fun)
  ((dex-fun
    :initform #'dex:delete)))

(defclass post-r (dex-fun with-body)
  ((dex-fun
    :initform #'dex:post)))

