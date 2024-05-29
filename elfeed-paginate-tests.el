;;; elfeed-paginate-tests.el --- Tests for elfeed-paginate -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Jim Porter
;; URL: https://github.com/jimporter/elfeed-paginate
;; Keywords: tests

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

;; Tests for elfeed-paginate

;;; Code:

(require 'elfeed-paginate)
(require 'ert)

(defmacro elpag-tests-without-save (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'elfeed-db-save) #'ignore))
     ,@body))

(defconst elpag-test-atom-link-rel-contents
  (concat "<feed>"
          "  <link rel=\"self\" href=\"https://nowhere.invalid/feed/\"/>"
          "  <link rel=\"next\" href=\"https://nowhere.invalid/feed/?page=2\"/>"
          "  <entry><title>Post</title></entry>"
          "</feed>"))

(defconst elpag-test-wordpress-contents
  (concat "<rss>"
          "  <channel>"
          "    <generator>https://wordpress.org/?v=6.2.2</generator>"
          "    <item><title>Post</title></item>"
          "  </channel>"
          "</rss>"))

(defconst elpag-test-rss-generic-contents
  (concat "<rss>"
          "  <channel>"
          "    <item><title>Post</title></item>"
          "  </channel>"
          "</rss>"))

;;; Tests:

(ert-deftest elpag-tests/next-page-url/link-rel ()
  "Check that pagination for Wordpress returns the right URL."
  (elpag-tests-without-save
    (with-temp-buffer
      (insert elpag-test-atom-link-rel-contents)
      (let* ((url "https://nowhere.invalid/feed/")
             (xml (elfeed-xml-parse-region (point-min) (point-max)))
             (feed (elfeed-db-get-feed url)))
        (should (equal (elfeed-paginate-next-page-url url xml feed)
                       "https://nowhere.invalid/feed/?page=2"))))))

(ert-deftest elpag-tests/next-page-url/wordpress ()
  "Check that pagination for Wordpress returns the right URL."
  (elpag-tests-without-save
    (with-temp-buffer
      (insert elpag-test-wordpress-contents)
      (let* ((url "https://nowhere.invalid/feed/")
             (xml (elfeed-xml-parse-region (point-min) (point-max)))
             (feed (elfeed-db-get-feed url)))
        (setq url (elfeed-paginate-next-page-url url xml feed))
        (should (equal url "https://nowhere.invalid/feed/?paged=2"))
        (setq url (elfeed-paginate-next-page-url url xml feed))
        (should (equal url "https://nowhere.invalid/feed/?paged=3"))))))

(ert-deftest elpag-tests/next-page-url/maybe-wordpress ()
  "Check that pagination for Wordpress returns the right URL.
This relies on the feed being appropriately tagged locally, since
the <generator> is missing."
  (elpag-tests-without-save
    (with-temp-buffer
      (insert elpag-test-rss-generic-contents)
      (let* ((url "https://nowhere.invalid/feed/")
             (xml (elfeed-xml-parse-region (point-min) (point-max)))
             (feed (elfeed-db-get-feed url)))
        (should-not (elfeed-paginate-next-page-url url xml feed))
        (setf (elfeed-meta feed :generator) 'wordpress)
        (setq url (elfeed-paginate-next-page-url url xml feed))
        (should (equal url "https://nowhere.invalid/feed/?paged=2"))
        (setq url (elfeed-paginate-next-page-url url xml feed))
        (should (equal url "https://nowhere.invalid/feed/?paged=3"))))))

;;; elfeed-paginate-tests.el ends here
