;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; packages.lisp

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

(cl:defpackage #:cl-twit
  (:use #:cl)
  (:nicknames #:twit)
  (:export
   ;; STATUS
   #:status
   #:status-created-at
   #:status-id
   #:status-text
   #:status-source
   #:status-truncated
   #:status-in-reply-to-status-id
   #:status-in-reply-to-user-id
   #:status-favorited
   #:status-user
   ;; USER
   #:user
   #:user-id
   #:user-name
   #:user-screen-name
   #:user-location
   #:user-description
   #:user-profile-image-url
   #:user-url
   #:user-protected
   #:user-followers-count
   ;; EXTENDED-USER
   #:extended-user
   #:user-profile-background-color
   #:user-profile-text-color
   #:user-profile-link-color
   #:user-profile-sidebar-fill-color
   #:user-profile-sidebar-border-color
   #:user-friends-count
   #:user-created-at
   #:user-favourites-count
   #:user-utc-offset
   #:user-time-zone
   #:user-profile-background-image-url
   #:user-profile-background-tile
   #:user-following
   #:user-notifications
   #:user-statuses-count
   ;; (Direct) MESSAGE
   #:message
   #:message-id
   #:message-sender-id
   #:message-text
   #:message-recipient-id
   #:message-created-at
   #:message-sender-screen-name
   #:message-recipient-screen-name
   #:message-sender
   #:message-recipient
   ;; RATE-LIMIT
   #:rate-limit
   #:rate-limit-reset-time
   #:rate-limit-reset-time-seconds
   #:rate-limit-remaining-hits
   #:rate-limit-hourly-limit
   ;; Errors
   #:twitter-error
   #:twitter-simple-error
   #:http-error
   #:http-status-code
   #:http-url
   #:http-body
   #:xml-error
   #:xml-error-reason
   #:xml-error-deadline
   #:xml-error-text
   ;; Twitter API methods
   #:m-public-timeline
   #:m-friends-timeline
   #:m-user-timeline
   #:m-show
   #:m-update
   #:m-replies
   #:m-destroy
   #:m-friends
   #:m-followers
   #:m-user-show
   #:m-messages
   #:m-messages-sent
   #:m-messages-new
   #:m-messages-destroy
   #:m-friendship-create
   #:m-friendship-destroy
   #:m-friendship-exists
   #:m-graph-friends
   #:m-graph-followers
   #:m-verify-credentials
   #:m-end-session
   #:m-update-delivery-device
   #:m-update-profile-colors
   #:m-update-profile-image
   #:m-update-profile-background-image
   #:m-rate-limit-status
   #:m-update-profile
   #:m-favorites
   #:m-favorites-create
   #:m-favorites-destroy
   #:m-follow
   #:m-leave
   #:m-block-create
   #:m-block-destroy
   #:m-test
   ;; Our convenience operators for interactive use
   #:*username*
   #:*password*
   #:login
   #:logout
   #:forget-state
   #:with-session
   #:update
   #:find-status
   #:reply-to
   #:@reply-to
   #:send-message
   #:*default-page-size*
   #:*reverse-display-items-p*
   #:display-item
   #:display-items
   #:last-displayed-statuses
   #:timeline
   #:user-timeline
   #:@replies
   #:messages
   #:sent-messages
   ;; TinyURL API client
   #:get-tinyurl))

