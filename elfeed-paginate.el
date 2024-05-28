;;; elfeed-paginate.el --- Paginate Elfeed feeds -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jim Porter

;; Author: Jim Porter
;; Version: 0.0.1-pre
;; Keywords: feed, rss
;; Package-Requires: ((emacs "29.1") (elfeed))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; elfeed-paginate adds the ability to retrieve multiple pages of results for
;; a web feed.  Currently, it only supports pagination of WordPress feeds by
;; default.

;;; Code:

(require 'elfeed)

(defgroup elfeed-paginate nil
  "Add support for fetching multiple pages of feeds with Elfeed."
  :group 'comm)

(defcustom elfeed-paginate-max-pages 5
  "The maximum number of feed pages to fetch when updating."
  :type 'natnum)

(defcustom elfeed-paginate-next-page-url-hook
  '(elfeed-paginate-next-page-url-wordpress)
  "A list of functions to get the \"next\" page for a feed URL.
Each function should take the current page's URL and its XML.
Elfeed will use the first non-nil result. If the result is a
symbol, this means that there is no next page."
  :type 'hook)

(defun elfeed-paginate-next-page-url-wordpress (url xml _feed)
  (when-let ((generator (xml-query* (rss channel generator *) xml))
             (genurl (url-generic-parse-url generator))
             ((string= (url-host genurl) "wordpress.org")))
    (let* ((urlobj (url-generic-parse-url url))
           (path-and-query (url-path-and-query urlobj))
           (query (when (cdr path-and-query)
                    (url-parse-query-string (cdr path-and-query))))
           (page (string-to-number (car (alist-get "paged" query '("1")
                                                   nil #'equal)))))
      (setf (alist-get "paged" query nil nil #'equal)
            (list (number-to-string (1+ page))))
      (setf (url-filename urlobj) (concat (car path-and-query) "?"
                                          (url-build-query-string query)))
      (url-recreate-url urlobj))))

(defun elfeed-paginate-next-page-url (url xml feed)
  "Return the next page of the feed for URL.
XML is the current page's XML as an S-expr.  FEED is the Elfeed
feed object."
  (let ((url (run-hook-with-args-until-success
              'elfeed-paginate-next-page-url-hook url xml feed)))
    (when (stringp url) url)))

(defmacro elfeed-paginate-with-fetch (url last-modified etag &rest body)
  "Asynchronously run BODY in a buffer with the contents from URL.
This macro is anaphoric, with STATUS referring to the status from
`url-retrieve'/cURL and USE-CURL being the original invoked-value
of `elfeed-use-curl'."
  (declare (indent 3))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (cb (lambda (status) ,@body))
          (last-modified ,last-modified)
          (etag ,etag))
     (if elfeed-use-curl
         (let ((headers `(("User-Agent" . ,elfeed-user-agent))))
           (when etag
             (push `("If-None-Match" . ,etag) headers))
           (when last-modified
             (push `("If-Modified-Since" . ,last-modified) headers))
           (elfeed-curl-enqueue ,url cb :headers headers))
       (url-queue-retrieve ,url cb () t t))))

(defun elfeed-paginate--update-feed (feed url &optional since depth)
  "Update a specific FEED.
URL is the URL to fetch (possibly a subsequent page for the
feed).  If non-nil, SINCE should be the `:last-modified' value for
the feed, in string form.  DEPTH increases by one for each nested
call to this function; it will continue calling itself for the
next page until it finds a post older than SINCE, runs out of
posts, or DEPTH reaches `elfeed-paginate-max-pages'."
  (setq depth (or depth 1))
  (elfeed-paginate-with-fetch url since
                              (when (= depth 1) (elfeed-meta feed :etag))
    (if (elfeed-is-status-error status use-curl)
        (let ((print-escape-newlines t))
          (elfeed-handle-http-error
           url (if use-curl elfeed-curl-error-message status)))
      (condition-case error
          (progn
            (unless use-curl
              (elfeed-move-to-first-empty-line)
              (set-buffer-multibyte t))
            (unless (eql elfeed-curl-status-code 304)
              (when (= depth 1)
                ;; Update Last-Modified and Etag
                (setf (elfeed-meta feed :last-modified)
                      (cdr (assoc "last-modified" elfeed-curl-headers))
                      (elfeed-meta feed :etag)
                      (cdr (assoc "etag" elfeed-curl-headers)))
                (if (equal url elfeed-curl-location)
                    (setf (elfeed-meta feed :canonical-url) nil)
                  (setf (elfeed-meta feed :canonical-url)
                        elfeed-curl-location)))
              (let* ((feed-id (elfeed-feed-id feed))
                     (xml (elfeed-xml-parse-region (point) (point-max)))
                     (entries
                      (cl-case (elfeed-feed-type xml)
                        (:atom (elfeed-entries-from-atom feed-id xml))
                        (:rss (elfeed-entries-from-rss feed-id xml))
                        (:rss1.0 (elfeed-entries-from-rss1.0 feed-id xml))
                        (otherwise (error (elfeed-handle-parse-error
                                           url "Unknown feed type."))))))
                (elfeed-db-add entries)
                (if-let (entries
                           ((< depth elfeed-paginate-max-pages))
                           ((or (null since)
                                (< (elfeed-float-time since)
                                   (elfeed-entry-date (car (last entries))))))
                           (next-url (elfeed-paginate-next-page-url
                                      url xml feed)))
                    (elfeed-paginate--update-feed
                     feed next-url since (1+ depth))
                  (run-hook-with-args 'elfeed-update-hooks url)))))
        (error (elfeed-handle-parse-error url error))))
    (unless use-curl
      (kill-buffer))))

(defun elfeed-paginate-update-feed (url)
  "Update a specific feed."
  (interactive (list (completing-read "Feed: " (elfeed-feed-list))))
  (unless elfeed--inhibit-update-init-hooks
    (run-hooks 'elfeed-update-init-hooks))
  (let ((feed (elfeed-db-get-feed url)))
    (elfeed-paginate--update-feed feed url (elfeed-meta feed :last-modified))))

;;;###autoload
(defun elfeed-paginate ()
  "Initialize Elfeed pagination."
  (elfeed-log 'info "elfeed-paginate enabled")
  (advice-add 'elfeed-update-feed :override #'elfeed-paginate-update-feed))

(defun elfeed-paginate-unload-function ()
  (advice-remove 'elfeed-update-feed #'elfeed-paginate-update-feed))

(provide 'elfeed-paginate)
;;; elfeed-paginate.el ends here
