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
;;;; TODO: Methods to be tested: update- (?)
;;;; TODO: Easily switch context between profiles (?)
;;;; TODO: Should we parse-status for the user (?)
;;;; TODO: Epoch for twitter's Unix time (?)

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

(defvar *username* nil
  "The user's account username")

(defvar *password* nil
  "The user's account password")

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

(defparameter *max-text-length* 140)

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
  (assert (and (not (null status)) (<= (length status) *max-text-length*))
          nil
          "STATUS must be non-NIL and its length must not be greater than ~A. Current length: ~A"
          *max-text-length*
          (length status))
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
          nil
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
  (assert (and (not (null text)) (<= (length text) *max-text-length*))
          nil
          "TEXT must be non-NIL and its length must not be greater than ~A. Current length: ~A"
          *max-text-length*
          (length text))
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
  (boolify
   (stp:string-value
    (safe-xml-root (twitter-request "/friendships/exists.xml")))))

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

(def-twitter-method m-end-session ()
  (twitter-request "/account/end_session.xml"
                   :method :post))

(def-twitter-method m-update-delivery-device
    (device)
  (assert (member device (list "sms" "im" "none") :test #'equal))
  (parse-user
   (safe-xml-root (twitter-request "/account/update_delivery_device.xml"
                                   :method :post))))

(def-twitter-method m-update-profile-colors
    (&key
     profile-background-color
     profile-text-color
     profile-link-color
     profile-sidebar-fill-color
     profile-sidebar-border-color)
  (parse-extended-user
   (safe-xml-root (twitter-request "/account/update_profile_colors.xml"
                                   :method :post))))

(def-twitter-method m-update-profile-image
    (image)
  (parse-extended-user
   (safe-xml-root (twitter-request "/account/update_profile_image.xml"
                                   :method :post))))

(def-twitter-method m-update-profile-background-image
    (image)
  (parse-extended-user
   (safe-xml-root (twitter-request "/account/update_profile_background_image.xml"
                                   :method :post))))

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

;;; Favorite methods

(def-twitter-method m-favorites
    (&key
     (id :parameter nil)
     page)
  (parse-statuses
   (safe-xml-root (twitter-request (optional-id-path id "/favorites")))))

(def-twitter-method m-favorites-create
    ((id :parameter nil))
  (parse-status
   (safe-xml-root (twitter-request (format nil "/favorites/create/~A.xml" id)
                                   :method :post))))

(def-twitter-method m-favorites-destroy
    ((id :parameter nil))
  (parse-status
   (safe-xml-root (twitter-request (format nil "/favorites/destroy/~A.xml" id)
                                   :method :delete))))

;;; Notification methods

(def-twitter-method m-follow
    ((id :parameter nil))
  (parse-user
   (safe-xml-root (twitter-request (format nil "/notifications/follow/~A.xml" id)
                                   :method :post))))

(def-twitter-method m-leave
    ((id :parameter nil))
  (parse-user
   (safe-xml-root (twitter-request (format nil "/notifications/leave/~A.xml" id)
                                   :method :post))))

;;; Block methods

(def-twitter-method m-block-create
    ((id :parameter nil))
  (parse-user
   (safe-xml-root (twitter-request (format nil "/blocks/create/~A.xml" id)
                                   :method :post))))

(def-twitter-method m-block-destroy
    ((id :parameter nil))
  (parse-user
   (safe-xml-root (twitter-request (format nil "/blocks/destroy/~A.xml" id)
                                   :method :post))))

;;; Help methods

(def-twitter-method m-test ()
  (twitter-request "/help/test.xml")
  t)

;;; Account and session helpers

(defvar *state*)

(defstruct (session-state
             (:conc-name nil))
  (friends-timeline-id nil)
  (user-timeline-ids (make-hash-table :test #'equalp))
  (replies-id nil)
  (messages-id nil)
  (sent-messages-id nil)
  (session-statuses (make-hash-table :test #'equalp))
  (session-messages (make-hash-table :test #'equalp))
  (session-last-displayed-statuses nil))

(defun login (authenticatep &optional (username *username*) (password *password*))
  "Sets the *USERNAME* and *PASSWORD* to USERNAME and PASSWORD
respectively. Also authenticate the given USERNAME and PASSWORD using
the verify_credentials service if AUTHENTICATEP is non-NIL. Also
clears any existing session state."
  (setf *username* username
        *password* password
        *state* (make-session-state))
  (when authenticatep
    (m-verify-credentials)))

(defun logout ()
  "Clear user information and session state."
  (setf *username* nil
        *password* nil
        *state* nil))

(defun forget-state ()
  "Just clear the existing session state. Don't clear user
authentication information."
  (setf *state* (make-session-state)))

(defmacro with-session ((username password &key (authenticatep t)) &body body)
  "Authenticate the USERNAME and PASSWORD, and set up a fresh session
state for this user. If AUTHENTICATEP is NIL, then don't automatically
authenticate the credentials on twitter."
  `(let ((*username*)
         (*password*)
         (*state*))
     , `(login ,authenticatep ,username ,password)
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
  (:documentation "Generic display function for cl-twit objects.

X is the object to be displayed.
STREAM is the output stream.
N is the zero-indexed number of the item a list, if any.
INITIALP indicates that X is the first item of the list.
FINALP indicates that X is the last item of the list.

Must be specialized for NULL, STATUS and MESSAGE."))

(defmethod display-item ((x null) stream n initialp finalp)
  (format stream "~&No items to display.~%"))

(defmethod display-item ((status status) stream n initialp finalp)
  (when initialp
    (format stream "~&Status stream starts: ~%~%"))
  (format stream "~&~A (~A): ~A~%"
          (user-name (status-user status))
          (user-screen-name (status-user status))
          (status-text status))
  (format stream "~&#~A (id ~A)~:[~; (truncated)~] at ~A from ~A~%~%"
          n
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
  "Displays a list of ITEMS using DISPLAY-ITEM to output STREAM."
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

(defun display-statuses (statuses stream)
  (display-items statuses stream)
  (setf (session-last-displayed-statuses *state*) statuses))

(defun last-displayed-statuses ()
  "Return the last status items displayed in this session."
  (session-last-displayed-statuses *state*))

(defun update (fmt &rest args)
  "Update your status. FMT is a FORMAT string, and ARGS are its
corresponding arguments."
  (store-status (m-update (apply #'format nil fmt args))))

(defun find-status (id)
  "Find a particular status.

If ID is a NUMBER, the (zero-based) index with for this position is
checked for in the last displayed statuses, and if present, that
status is returned. If no status is found at this position, an error
is signalled.

If ID is a string, a status with the same status-id as ID is searcehd
for in the local cache. If it is not present locally, a CERROR is
signalled. If the user chooses to continue, get the status from
twitter."
  (etypecase id
    (number (or (nth id (session-last-displayed-statuses *state*))
                (error 'twitter-simple-error
                       :format-control "Can't find status #~A in last displayed statuses."
                       :format-arguments (list id))))
    (string
     (or (gethash id (session-statuses *state*))
         (progn
           (cerror "Couldn't find status with ID locally. Ask twitter?"
                   'twitter-error)
           (store-status (ignore-errors (m-show id))))))))

(defun reply-to (id fmt &rest args)
  "Send a reply to a particular status with STATUS-ID. FMT and ARGS
are the format-control string and args."
  (store-status
   (m-update (apply #'format nil fmt args)
             :in-reply-to-status-id (etypecase id
                                      (number (status-id (find-status id)))
                                      (string id)))))

(defun @reply-to (id fmt &rest args)
  "Send a reply to a particular status with STATUS-ID. FMT and ARGS
are the format-control string and args.

This function prepends the @username of the status's sender to the
final text."
  (let ((fmt (format nil "@~A ~A"
                     (user-screen-name (status-user (find-status id)))
                     fmt)))
    (reply-to id (apply #'format nil fmt args))))

(defun send-message (user fmt &rest args)
  "Send a direct message to USER (screenname or id) with
format-controlled string FMT and ARGS."
  (m-messages-new user (apply #'format nil fmt args)))

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
                 (since-id (friends-timeline-id *state*))
                 count
                 page
                 (stream *standard-output*))
  "Retrieve the authenticating user's home timeline. Equivalent of /home on twitter.

SINCE-ID should be the id of a status.
COUNT (must be < 200) is the number of statuses to retrieve.
PAGE is the page number to retrieve.
STREAM should be the output stream (default *STANDARD-OUTPUT*).

SINCE-ID is not checked when PAGE is non-NIL."
  (let ((statuses (m-friends-timeline
                   :since-id (unless page since-id)
                   :count count
                   :page page)))
    (update-newest-id statuses (friends-timeline-id *state*))
    (display-statuses statuses stream)
    (store-statuses statuses)))

(defun user-timeline (user &key
                      (since-id (gethash user (user-timeline-ids *state*)))
                      count
                      page
                      (stream *standard-output*))
  "Retrieve the USER's timeline. Equivalent to /username on
  twitter. If USER eql's :ME, get the timeline for authenticating
  user.

SINCE-ID should be the id of a status.
COUNT (must be < 200) is the number of statuses to retrieve.
PAGE is the page number to retrieve.
STREAM should be the output stream (default *STANDARD-OUTPUT*).

SINCE-ID is not checked when PAGE is non-NIL."
  (let ((statuses (m-user-timeline
                   :id (if (eql user :me)
                           *username*
                           user)
                   :since-id (unless page since-id)
                   :count count
                   :page page)))
    (update-newest-id statuses (gethash user (user-timeline-ids *state*)))
    (display-statuses statuses stream)
    (store-statuses statuses)))

(defun @replies (&key
                 (since-id (replies-id *state*))
                 page
                 (stream *standard-output*))
  "Retrieve any @REPLIES for the authenticating user.

SINCE-ID should be the id of a status.
COUNT (must be < 200) is the number of statuses to retrieve.
STREAM should be the output stream (default *STANDARD-OUTPUT*).

SINCE-ID is not checked when PAGE is non-NIL."
  (let ((statuses (m-replies :since-id (unless page since-id)
                             :page page)))
    (update-newest-id statuses (replies-id *state*))
    (display-statuses statuses stream)
    (store-statuses statuses)))

(defun messages (&key
                 (since-id (messages-id *state*))
                 page
                 (stream *standard-output*))
  "Retrieve direct messages sent to the authenticating user.

SINCE-ID should be the id of a status.
PAGE is the page number to retrieve.
STREAM should be the output stream (default *STANDARD-OUTPUT*).

SINCE-ID is not checked when PAGE is non-NIL."
  (let ((messages (m-messages :since-id (unless page since-id)
                              :page page)))
    (update-newest-id messages (messages-id *state*))
    (display-items messages stream)
    (store-messages messages)))

(defun sent-messages (&key
                      (since-id (sent-messages-id *state*))
                      page
                      (stream *standard-output*))
  "Retrieves the direct messages sent by the authenticating user.

SINCE-ID should be the id of a status.
PAGE is the page number to retrieve.
STREAM should be the output stream (default *STANDARD-OUTPUT*).

SINCE-ID is not checked when PAGE is non-NIL."
  (let ((messages (m-messages-sent :since-id (unless page since-id)
                                   :page page)))
    (update-newest-id messages (sent-messages-id *state*))
    (display-items messages stream)
    (store-messages messages)))

;;; TinyURL-ize
;;; Using the very simple TinyURL API

(defparameter *tinyurl-url* "http://tinyurl.com/api-create.php")

(defun get-tinyurl (url)
  "Get a TinyURL for the given URL. Uses the TinyURL API service."
  (multiple-value-bind (body status-code)
      (drakma:http-request *tinyurl-url*
                           :parameters `(("url" . ,url)))
    (if (= status-code +http-ok+)
        body
        (error 'http-error
               :status-code status-code
               :url url
               :body body))))

;;; Load customizations, if any
;;; Code thanks to asdf-install

(eval-when (:load-toplevel :execute)
  (let* ((file (probe-file (merge-pathnames
			    (make-pathname :name ".cl-twit.lisp")
			    (truename (user-homedir-pathname))))))
    (when file (load file))))



