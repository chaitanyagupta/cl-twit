;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; classes.lisp

;;; Copyright (c) 2009, Chaitanya Gupta.
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:in-package #:cl-twit)

;;; Our objects

(defstruct status
  created-at
  id
  text
  source
  truncated
  in-reply-to-status-id
  in-reply-to-user-id
  favorited
  user)

(defmethod print-object ((x status) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A (~A) "
            (status-id x)
            (and (status-user x)
                 (user-screen-name (status-user x))))
    (let ((text (status-text x)))
      (format stream "~S"
              (if (< (length text) 10)
                  text
                  (concatenate 'string (subseq text 0 10) " ..."))))))

(defstruct user
  id
  name
  screen-name
  location
  description
  profile-image-url
  url
  protected
  followers-count)

(defmethod print-object ((x user) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A (~A)" (user-name x) (user-screen-name x))))

(defstruct (extended-user
             (:include user)
             (:conc-name "USER-"))
  profile-background-color
  profile-text-color
  profile-link-color
  profile-sidebar-fill-color
  profile-sidebar-border-color
  friends-count
  created-at
  favourites-count
  utc-offset
  time-zone
  profile-background-image-url
  profile-background-tile
  following
  notifications
  statuses-count)

(defstruct message
  id
  sender-id
  text
  recipient-id
  created-at
  sender-screen-name
  recipient-screen-name
  sender
  recipient)

(defmethod print-object ((x message) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~A-->~A "
            (message-sender-screen-name x)
            (message-recipient-screen-name x))
    (let ((text (message-text x)))
      (format stream "~S"
              (if (< (length text) 10)
                  text
                  (concatenate 'string (subseq text 0 10) " ..."))))))

(defstruct rate-limit
  reset-time
  reset-time-seconds
  remaining-hits
  hourly-limit)

;;; Conditions

(define-condition twitter-error (error)
  ())

(define-condition twitter-simple-error (twitter-error simple-error)
  ())

(define-condition http-error (twitter-error)
  ((status-code :reader http-status-code
                :initarg :status-code)
   (url :reader http-url
        :initarg :url)
   (body :reader http-body
         :initarg :body)))

(defmethod print-object ((x http-error) stream)
  (if (null *print-escape*)
      (format stream "~A returned by ~A~%Body: ~A"
              (http-status-code x)
              (http-url x)
              (http-body x))
      (call-next-method)))

(define-condition xml-error (twitter-error)
  ((reason :reader xml-error-reason
           :initarg :reason)
   (deadline :reader xml-error-deadline
             :initarg :deadline)
   (text :reader xml-error-text
         :initarg :text)))

(defmethod print-object ((x xml-error) stream)
  (if (null *print-escape*)
      (format stream "Reason: ~A~%Deadline: ~A~%Text: ~A"
              (xml-error-reason x)
              (xml-error-deadline x)
              (xml-error-text x))
      (call-next-method)))

;;; An 'id' method for our objects

(defgeneric id (x)
  (:method ((x status)) (status-id x))
  (:method ((x user)) (user-id x))
  (:method ((x message)) (message-id x)))

