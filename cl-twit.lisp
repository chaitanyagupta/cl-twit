;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; cl-twit.lisp

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

;;; TODOs:
;;;; TODO: Convenience fn for sent messages
;;;; TODO: Documentation
;;;; TODO: Add favorites, etc.
;;;; TODO: Should we parse-status for the user (?)
;;;; TODO: Epoch for twitter's Unix time (?)
;;;; TODO: Better return type handling for twitter-methods (?)

;;; Utils

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defun compose (fn1 &rest fns)
  (if fns
      (lambda (x)
        (funcall fn1 (funcall (apply #'compose fns) x)))
      fn1))

;;; Parse XML

(defun xml-root (raw-xml)
  (stp:document-element (cxml:parse raw-xml (stp:make-builder))))

(defun child-value (node child-name)
  (stp:string-value (stp:find-child-if (stp:of-name child-name) node)))

(defun boolify (string)
  (string-equal string "true"))

(defun parse-xml-error (node)
  (make-condition 'xml-error
                  :reason (stp:attribute-value node "reason")
                  :deadline (stp:attribute-value node "deadline")
                  :text (stp:string-value node)))

(defun safe-xml-root (raw-xml)
  (let ((node (xml-root raw-xml)))
    (if (string-equal (stp:local-name node) "error")
        (error (parse-xml-error node))
        node)))

(defun parse-status (status-node)
  (flet ((!child-value (name)
           (child-value status-node name)))
    (make-status :created-at (!child-value "created_at")
                 :id (!child-value "id")
                 :text (!child-value "text")
                 :source (!child-value "source")
                 :truncated (boolify (!child-value "source"))
                 :in-reply-to-status-id (!child-value "in_reply_to_status_id")
                 :in-reply-to-user-id (!child-value "in_reply_to_user_id")
                 :favorited (boolify (!child-value "favorited"))
                 :user (when-let (user-node (stp:find-child-if (stp:of-name "user") status-node))
                         (parse-user user-node)))))

(defun parse-statuses (node)
  (mapcar #'parse-status (stp:filter-children (stp:of-name "status") node)))

(defun parse-user (user-node)
  (flet ((!child-value (name)
           (child-value user-node name)))
    (make-user :id (!child-value "id")
               :name (!child-value "name")
               :screen-name (!child-value "screen_name")
               :location (!child-value "location")
               :description (!child-value "description")
               :profile-image-url (!child-value "profile_image_url")
               :url (!child-value "url")
               :protected (boolify (!child-value "protected"))
               :followers-count (parse-integer (!child-value "followers_count")))))

(defun parse-users (node)
  (mapcar #'parse-user (stp:filter-children (stp:of-name "user") node)))

(defun parse-extended-user (user-node)
  (flet ((!child-value (name)
           (child-value user-node name)))
    (make-extended-user
     :id (!child-value "id")
     :name (!child-value "name")
     :screen-name (!child-value "screen_name")
     :location (!child-value "location")
     :description (!child-value "description")
     :profile-image-url (!child-value "profile_image_url")
     :url (!child-value "url")
     :protected (boolify (!child-value "protected"))
     :followers-count (parse-integer (!child-value "followers_count"))
     :profile-background-color (!child-value "profile_background_color")
     :profile-text-color (!child-value "profile_text_color")
     :profile-link-color (!child-value "profile_link_color")
     :profile-sidebar-fill-color (!child-value "profile_sidebar_fill_color")
     :profile-sidebar-border-color (!child-value "profile_sidebar_border_color")
     :friends-count (parse-integer (!child-value "friends_count"))
     :created-at (!child-value "created_at")
     :favourites-count (parse-integer (!child-value "favourites_count"))
     :utc-offset (parse-integer (!child-value "utc_offset") :junk-allowed t)
     :time-zone (!child-value "time_zone")
     :profile-background-image-url (!child-value "profile_background_image_url")
     :profile-background-tile (boolify (!child-value "profile_background_tile"))
     :following (boolify (!child-value "following"))
     :notifications (boolify (!child-value "notifications"))
     :statuses-count (parse-integer (!child-value "statuses_count")))))

(defun parse-message (message-node)
  (flet ((!child-value (name)
           (child-value message-node name)))
    (make-message
     :id (!child-value "id")
     :sender-id (!child-value "sender_id")
     :text (!child-value "text")
     :recipient-id (!child-value "recipient_id")
     :created-at (!child-value "created_at")
     :sender-screen-name (!child-value "sender_screen_name")
     :recipient-screen-name (!child-value "recipient_screen_name")
     :sender (when-let (user-node (stp:find-child-if (stp:of-name "sender") message-node))
               (parse-user user-node))
     :recipient (when-let (user-node (stp:find-child-if (stp:of-name "recipient") message-node))
                  (parse-user user-node)))))

(defun parse-messages (node)
  (mapcar #'parse-message (stp:filter-children (stp:of-name "direct_message") node)))

(defun parse-ids (node)
  (mapcar #'stp:string-value (stp:filter-children (stp:of-name "id") node)))

(defun parse-rate-limit (node)
  (flet ((!child-value (name)
           (child-value node name)))
    (make-rate-limit
     :reset-time (!child-value "reset-time")
     :reset-time-seconds (parse-integer (!child-value "reset-time-in-seconds"))
     :remaining-hits (parse-integer (!child-value "remaining-hits"))
     :hourly-limit (parse-integer (!child-value "hourly-limit")))))

;;; Methods base

(defparameter *base-url* "http://www.twitter.com")

(defvar *username* nil)
(defvar *password* nil)

(defconstant +http-ok+ 200)

(defvar *twitter-parameters* nil)

(defun twitter-request (path &rest args)
  (let ((url (concatenate 'string *base-url* path)))
    (multiple-value-bind (body status-code)
        (apply #'drakma:http-request
               url
               (append (and *username* *password*
                            (list :basic-authorization (list *username* *password*)))
                       (unless (getf args :parameters)
                         (list :parameters *twitter-parameters*))
                       args))
      (if (= status-code +http-ok+)
          body
          (error 'http-error
                 :status-code status-code
                 :url url
                 :body body)))))

;;; Methods

(defun params-list (&rest pairs)
  (remove nil pairs))

(defun pair-when (name value)
  (and value
       (cons name (format nil "~A" value))))

(defun optional-id-path (id prefix)
  (format nil "~A~@[/~A~].xml" prefix id))

;;; Macro for defining twitter methods

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parameter-string (symbol)
    (let ((string (string-downcase (string symbol))))
      (substitute #\_ #\- string))))

(defmacro def-twitter-method (name args &body body)
  (let ((arglist nil)
        (params nil)
        (doc-string nil))
    (dolist (arg args)
      (cond
        ((member arg '(&optional &key &rest &aux))
         (push arg arglist))
        ((atom arg)
         (push arg arglist)
         (push `(pair-when ,(parameter-string arg) ,arg) params))
        ((listp arg)
         (destructuring-bind (arg-name &key default (parameter nil parameterp))
             arg
           (if default
               (push (list (first arg) default) arglist)
               (push (first arg) arglist))
           (cond ((null parameterp)
                  (push `(pair-when ,(parameter-string parameter) ,arg-name) params))
                 ((null parameter)
                  t)
                 ((stringp parameter)
                  (push `(pair-when ,parameter ,arg-name) params))
                 (t
                  (error "DEF-TWITTER-METHOD arg parser: bad user of :PARAMETER.~%~A" args)))))
        (t (error "DEF-TWITTER-METHOD arg parser: Can't handle this case.~%~A" args))))
    (setf arglist (nreverse arglist)
          params (cons 'params-list params))
    (when (stringp (first body))
      (setf doc-string (first body)
            body (rest body)))
    `(defun ,name ,arglist
       ,@(when doc-string (list doc-string))
       (let ((*twitter-parameters* ,params))
         ,@body))))

;;; Status methods

(def-twitter-method m-public-timeline ()
  (let* ((body (twitter-request "/statuses/public_timeline.xml")))
    (parse-statuses (safe-xml-root body))))

(def-twitter-method m-friends-timeline
    (&key
     since-id
     count page)
  (let* ((body (twitter-request "/statuses/friends_timeline.xml")))
    (parse-statuses (safe-xml-root body))))

(def-twitter-method m-user-timeline
    (&key
     (id :parameter nil)
     since-id
     count page)
  (let* ((body (twitter-request (optional-id-path id "/statuses/user_timeline"))))
    (parse-statuses (safe-xml-root body))))

(def-twitter-method m-show
    ((id :parameter nil))
  (let ((body (twitter-request (format nil "/statuses/show/~A.xml" id))))
    (parse-status (safe-xml-root body))))

(def-twitter-method m-update
    (status
     &key
     in-reply-to-status-id)
  (assert (not (null status))
          (status)
          "Give a non-null STATUS.")
  (let* ((body (twitter-request "/statuses/update.xml"
                             :method :post)))
    (parse-status (safe-xml-root body))))

(def-twitter-method m-replies
    (&key
     since-id
     page)
  (let* ((body (twitter-request "/statuses/replies.xml")))
    (parse-statuses (safe-xml-root body))))

(def-twitter-method m-destroy
    ((id :parameter nil))
  (let ((body (twitter-request (format nil "/statuses/destroy/~A.xml" id)
                            :method :delete)))
    (parse-status (safe-xml-root body))))

;;; User methods

(def-twitter-method m-friends
    (&key
     (id :parameter nil)
     page)
  (let ((body (twitter-request (optional-id-path id "/statuses/friends"))))
    (parse-users (safe-xml-root body))))

(def-twitter-method m-followers
    (&key
     (id :parameter nil)
     page)
  (parse-users
   (safe-xml-root (twitter-request (optional-id-path id "/statuses/followers")))))
(def-twitter-method m-user-show
    (&key
     (id :parameter nil)
     email)
  (assert (or id email)
          (id email)
          "Provide atleast one of id or email.")
  (parse-extended-user
   (safe-xml-root (twitter-request (optional-id-path id "/users/show")))))

;;; Direct message methods

(def-twitter-method m-messages
    (&key
     since-id
     page)
  (parse-messages
   (safe-xml-root (twitter-request "/direct_messages.xml"))))

(def-twitter-method m-messages-sent
    (&key
     since-id
     page)
  (parse-messages
   (safe-xml-root (twitter-request "/direct_messages/sent.xml"))))

(def-twitter-method m-messages-new
    (user
     text)
  (parse-message
   (safe-xml-root (twitter-request "/direct_messages/new.xml"
                                :method :post))))
(def-twitter-method m-messages-destroy
    ((id :parameter nil))
  (parse-message
   (safe-xml-root (twitter-request (format nil "/direct_messages/destroy/~A.xml" id)
                                :method :delete))))

;;; Friendship methods

(def-twitter-method m-friendship-create
    ((id :parameter nil)
     &key
     follow)
  (parse-user
   (safe-xml-root (twitter-request (format nil "/friendships/create/~A.xml" id)
                                :method :post))))

(def-twitter-method m-friendship-destroy
    ((id :parameter nil))
  (parse-user
   (safe-xml-root (twitter-request (format nil "/friendships/destroy/~A.xml" id)
                                :method :delete))))

(def-twitter-method m-friendship-exists
    (user-a
     user-b)
  (stp:string-value
   (safe-xml-root (twitter-request "/friendships/exists.xml"))))

;;; Social graph methods

(def-twitter-method m-graph-friends
    (&key
     (id :parameter nil))
  (parse-ids
   (safe-xml-root (twitter-request (optional-id-path id "/friends/ids")))))

(def-twitter-method m-graph-followers
    (&key
     (id :parameter nil))
  (parse-ids
   (safe-xml-root (twitter-request (optional-id-path id "/followers/ids")))))

;;; Account methods

(def-twitter-method m-verify-credentials ()
  (parse-user
   (safe-xml-root (twitter-request "/account/verify_credentials.xml"))))

(def-twitter-method m-rate-limit-status ()
  (parse-rate-limit
   (safe-xml-root (twitter-request "/account/rate_limit_status.xml"))))

(def-twitter-method m-update-profile
    (&key
     name
     email
     url
     location
     description)
  (parse-extended-user
   (safe-xml-root (twitter-request "/account/update_profile.xml"
                                   :method :post))))

;;; Help methods

(def-twitter-method m-test ()
  (twitter-request "/help/test.xml")
  t)

;;; Account and session helpers

(defvar *state*)

(defun login (username password)
  (prog1
      (let ((*username* username)
            (*password* password))
        (m-verify-credentials))
    (setf *username* username
          *password* password
          *state* (make-session-state))))

(defun logout ()
  (setf *username* nil
        *password* nil
        *state* nil))

(defstruct (session-state
             (:conc-name nil))
  (friends-timeline-id nil)
  (user-timeline-ids (make-hash-table :test #'equalp))
  (replies-id nil)
  (messages-id nil)
  (session-statuses (make-hash-table :test #'equalp))
  (session-messages (make-hash-table :test #'equalp)))

(defun forget-state ()
  (setf *state* (make-session-state)))

(defmacro with-session ((username password &key (login t)) &body body)
  `(let ((*username* ,username)
         (*password* ,password)
         (*state* (make-session-state)))
     , (when login
         `(login *username* *password*))
     ,@body))

(defun store-status (status)
  (setf (gethash (status-id status) (session-statuses *state*)) status))

(defun store-statuses (statuses)
  (dolist (status statuses statuses)
    (store-status status)))

(defun store-message (message)
  (setf (gethash (message-id message) (session-messages *state*)) message))

(defun store-messages (messages)
  (dolist (message messages messages)
    (store-message message)))

;;; Status management helpers

(defun get-newest-id (items)
  (when items
    (apply #'max (mapcar (compose #'parse-integer #'id) items))))

(defgeneric display-item (x stream n initialp finalp)
  (:documentation "Generic display function"))

(defmethod display-item ((x null) stream n initialp finalp)
  (format stream "~&No items to display.~%"))

(defmethod display-item ((status status) stream n initialp finalp)
  (when initialp
    (format stream "~&Status stream starts: ~%~%"))
  (format stream "~&~A (~A): ~A~%"
          (user-name (status-user status))
          (user-screen-name (status-user status))
          (status-text status))
  (format stream "~&~A~:[~; (truncated)~] at ~A from ~A~%~%"
          (status-id status)
          (status-truncated status)
          (status-created-at status)
          (status-source status))
  (when finalp
    (format stream "~&Status stream ends.~%")))

(defmethod display-item ((message message) stream n initialp finalp)
  (when initialp
    (format stream "~&Message stream starts: ~%~%"))
  (format stream "~&~A to ~A: ~A~%"
          (message-sender-screen-name message)
          (message-recipient-screen-name message)
          (message-text message))
  (format stream "~&~A at ~A~%~%"
          (message-id message)
          (message-created-at message))
  (when finalp
    (format stream "~&Message stream ends.~%")))

(defun display-items (items stream &aux (n -1))
  (labels ((!display-items (items &optional (initialp t))
             (cond
               ((null items)
                (display-item nil stream 0 t t))
               ((rest items)
                (display-item (first items) stream (incf n) initialp nil)
                (!display-items (rest items) nil))
               (t
                (display-item (first items) stream (incf n) initialp t)))))
    (!display-items items)
    items))

(defun update (fmt &rest args)
  (store-status (m-update (apply #'format nil fmt args))))

(defun find-status (id)
  (or (gethash id (session-statuses *state*))
      (store-status (ignore-errors (m-show id)))))

(defun reply-to (status-id fmt &rest args)
  (store-status
   (m-update (apply #'format nil fmt args)
             :in-reply-to-status-id status-id)))

(defun @reply-to (status-id fmt &rest args)
  (let ((fmt (format nil "@~A ~A"
                     (user-screen-name (status-user (find-status status-id)))
                     fmt)))
    (reply-to status-id (apply #'format nil fmt args))))

(defmacro update-newest-id (statuses place)
  ;; FIXME: We are broke if the ids don't remain numeric.
  ;; Shouldn't we use the created_at date instead?
  (let ((request-newest-id (gensym))
        (place-id (gensym)))
    `(let* ((,request-newest-id (get-newest-id ,statuses))
            (,place-id ,place))
       (when (and ,request-newest-id
                  (or (null ,place-id)
                      (> ,request-newest-id ,place-id)))
         (setf ,place ,request-newest-id)))))

(defun timeline (&key
                 (since-id (friends-timeline-id *state*) since-id-p)
                 count
                 page
                 (stream *standard-output*))
  (let ((statuses (m-friends-timeline
                   :since-id (unless page since-id)
                   :count count
                   :page page)))
    (when (or since-id (not since-id-p))
      (update-newest-id statuses (friends-timeline-id *state*)))
    (display-items statuses stream)
    (store-statuses statuses)))

(defun user-timeline (user
                      &key
                      (since-id (gethash user (user-timeline-ids *state*)) since-id-p)
                      count
                      page
                      (stream *standard-output*))
  (let ((statuses (m-user-timeline
                   :id (if (eql user :me)
                           *username*
                           user)
                   :since-id (unless page since-id)
                   :count count
                   :page page)))
    (when (or since-id (not since-id-p))
      (update-newest-id statuses (gethash user (user-timeline-ids *state*))))
    (display-items statuses stream)
    (store-statuses statuses)))

(defun @replies (&key
                 (since-id (replies-id *state*) since-id-p)
                 page
                 (stream *standard-output*))
  (let ((statuses (m-replies :since-id (unless page since-id)
                             :page page)))
    (when (or since-id (not since-id-p))
      (update-newest-id statuses (replies-id *state*)))
    (display-items statuses stream)
    (store-statuses statuses)))

(defun messages (&key
                 (since-id (messages-id *state*) since-id-p)
                 page
                 (stream *standard-output*))
  (let ((messages (m-messages :since-id (unless page since-id)
                              :page page)))
    (when (or since-id (not since-id-p))
      (update-newest-id messages (messages-id *state*)))
    (display-items messages stream)
    (store-messages messages)))

;;; TinyURL-ize
;;; Using the very simple TinyURL API

(defparameter *tinyurl-url* "http://tinyurl.com/api-create.php")

(defun get-tinyurl (url)
  (multiple-value-bind (body status-code)
      (drakma:http-request *tinyurl-url*
                           :parameters `(("url" . ,url)))
    (if (= status-code +http-ok+)
        body
        (error 'http-error
               :status-code status-code
               :url url
               :body body))))



