(in-package #:cl-ghost)

(define-condition ghost-condition (serious-condition)
  ())

(define-condition poorly-configured (ghost-condition)
  ((message
    :accessor message
    :initarg :message
    :type string))
  (:report
   (lambda (obj stream)
     (format stream "Poorly configured.~%Message: ~A."
             (message obj)))))

(defun signal-poorly-configured (message)
  (error 'poorly-configured :message message))

(define-condition api-condition (ghost-condition)
  ((http-status
    :accessor http-status
    :initarg :http-status
    :type fixnum)
   (endpoint
    :accessor endpoint
    :initarg :endpoint 
    :type endpoint)
   (errors
    :accessor errors
    :initarg :errors)
   (dex-response 
    :accessor dex-response 
    :initarg :dex-response))
  (:report
   (lambda (obj stream)
     (let ((errors (gethash "errors" (errors obj))))
       (format stream "Status: ~D~%Errors:~%"
               (http-status obj))
       (map nil (lambda (obj)
                  (format stream " Code: ~S~% Message: ~S~%"
                          (gethash "code" obj)
                          (gethash "message" obj)))                          
            errors)))))

(define-condition badly-formed-query (api-condition)
  ((http-status
    :initform 400))
  (:documentation "Status 400: Badly formed queries e.g. filter parameters that are not correctly encoded"))

(define-condition authentication-failure (api-condition)
  ((http-status
    :initform 401))
  (:documentation "Status 401: Authentication failures e.g. unrecognised keys"))

(define-condition unknown-resource (api-condition)
  ((http-status
    :initform 404))
  (:documentation "Status 404: Unknown resources e.g. data which is not public"))

(define-condition server-error (api-condition)
  ((http-status
    :initform 500))
  (:documentation "Status 500: Server errors e.g. where something has gone"))

(defun condition->status (status)
  (case status
    (400 'badly-formed-query)
    (401 'authentication-failure)
    (404 'unknown-resource)
    (500 'server-error)
    (otherwise 'api-condition)))

(defun construct-condition (endpoint condition)
  (with-accessors ((body dexador.error:response-body)
                   (status dexador.error:response-status))
      condition 
    (let ((new-con (condition->status status))
          (parsed (parse body)))
      (make-condition new-con :endpoint endpoint
                              :errors parsed
                              :http-status status
                              :dex-response condition))))
