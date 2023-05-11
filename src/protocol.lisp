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
  (:documentation "Depending on the class of endpoint attempt to construct the request URL.")
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
  (:documentation "Uses APPEND. Construct the ALIST passed to dex to generate the headers")
  (:method-combination append :most-specific-last)
  (:method :around (endpoint)
    `(:headers ,(call-next-method)))
  (:method append (endpoint)
    `(("Accept-Version" . ,(accept-version endpoint))))
  (:method append ((endpoint with-body))
    `(("Content-Type" . ,(content-type endpoint)))))

(defgeneric build-body (endpoint)
  (:documentation "When there is a body in endpoint generate the plist.")
  (:method (endpoint)
    nil)
  (:method ((endpoint with-body))
    `(:content ,(body endpoint))))
    
(defun generate-query (alist)
  (format nil "?~A" (quri:url-encode-params alist)))

(defgeneric call-api (endpoint)
  (:documentation "Make a call to ENDPOINT. Make sure you have bound *ghost* to an instance
of ghost. Attempts to correct wrap and parse conditions caught using #'construct-condition.")
  (:method :around (endpoint)
    (unless *ghost*
      (signal-poorly-configured
       "Please bind *ghost* to an instance of 'ghost or 'ghost-admin"))
    (handler-case (call-next-method)
      (dexador.error:http-request-failed (c)
        (let ((con (construct-condition endpoint c)))
          (error con))))))


