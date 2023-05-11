(in-package #:cl-ghost)

(defparameter *json-parser* #'com.inuoe.jzon:parse)            

(defun parse (json)
  (funcall *json-parser* json))

(defgeneric login (ghost expire-amount expire-unit)
  (:documentation "Generates JWT for instance of GHOST that lasts until local-time:now +
EXPIRE-AMOUNT of EXPIRE-UNIT.
Expire unit one of: (:NSEC :SEC :MINUTE :HOUR :DAY :DAY-OF-WEEK :MONTH :YEAR)."))

(defgeneric pick-base (endpoint)
  (:documentation "Depending on the class of *ghost* return the correct base URL."))

(defgeneric build-url (endpoint query)
  (:method ((endpoint endpoint) query)
    (format nil "~A~A~A"
            (pick-base endpoint)
            (url endpoint)
            (or query "")))
  (:method ((endpoint has-id) query)
    (format nil "~A~A~A/~A"
            (pick-base endpoint)
            (url endpoint)
            (id endpoint)
            (or query "")))
  (:method ((endpoint has-slug) query)
    (format nil "~A~A~A/~A"
            (pick-base endpoint)
            (url endpoint)
            (slug endpoint)
            (or query ""))))

(defgeneric build-headers (endpoint)
  (:method-combination append :most-specific-last)
  (:method append (endpoint)
    `(("Accept-Version" . ,(accept-version endpoint))))
  (:method append ((endpoint with-body))
    `(("Content-Type" . ,(content-type endpoint)))))

(defgeneric build-body (endpoint)
  (:method (endpoint)
    nil)
  (:method ((endpoint with-body))
    `(:content ,(body endpoint))))
    

(defun generate-query (alist)
  (format nil "?~{~A~^&~}"
          (loop :for (key . val) :in alist
                :collect (format nil "~A=~A" key val))))

(defgeneric call-api (endpoint)
  (:documentation "Make a call to ENDPOINT. Make sure you have bound *ghost* to an instance
of ghost. Attempts to correct wrap and parse conditions caught using #'construct-condition.")
  (:method :around (endpoint)
    (handler-case (call-next-method)
      (dexador.error:http-request-failed (c)
        (let ((con (construct-condition endpoint c)))
          (error con))))))

