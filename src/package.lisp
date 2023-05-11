;;;; package.lisp

(defpackage #:cl-ghost
  (:use #:cl)
  (:nicknames #:ghost)
  ;;protocol
  (:export #:*json-parser*
           #:parse 
           #:login
           #:build-url
           #:pick-base
           #:build-headers
           #:build-body
           #:generate-query
           #:call-api)
  ;;conditions
  (:export #:ghost-condition
           #:poorly-configured
           #:message
           #:signal-poorly-configured
           #:api-condition
           #:http-status
           #:endpoint
           #:errors
           #:dex-response
           #:badly-formed-query
           #:authentication-failure
           #:unknown-resource
           #:server-error)
  ;;classes
  (:export #:ghost
           #:api-url
           #:content-api-key
           #:has-id
           #:id
           #:has-slug
           #:slug
           ;;#:endpoint
           #:url
           #:dex-fun
           #:accept-version
           #:query
           #:with-body
           #:body
           #:content-type
           ;;#:dex-fun
           #:get-r
           #:put-r
           #:delete-r
           #:post-r)
  ;;cl-ghost
  (:export #:*version*
           #:*ghost*))
  

