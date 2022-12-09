;;; kiwix.el --- Searching offline Wikipedia through Kiwix.  -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; Homepage: https://repo.or.cz/kiwix.el.git
;; Created: 23th July 2016
;; Version: 1.1.5
;; Package-Requires: ((emacs "25.1") (request "0.3.0"))

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This currently only works for GNU/Linux, not tested for Mac OS X and Windows.

;;;; Kiwix installation
;;
;; https://repo.or.cz/kiwix.el.git/README.org#install

;;;; Config:
;;
;; (use-package kiwix
;;   :ensure t
;;   :after org
;;   :commands (kiwix-launch-server kiwix-at-point kiwix-search-at-library kiwix-search-full-context)
;;   :bind (:map document-prefix ("w" . kiwix-at-point))
;;   :custom ((kiwix-server-type 'docker-remote)
;;            (kiwix-server-url "http://192.168.31.251")
;;            (kiwix-server-port 8089))
;;   :hook (org-load . org-kiwix-setup-link)
;;   :init (require 'org-kiwix)
;;   :config (add-hook 'org-load-hook #'org-kiwix-setup-link))

;;;; Usage:
;;
;; 1. [M-x kiwix-launch-server] to launch Kiwix server.
;; 2. [M-x kiwix-at-point] to search the word under point or the region selected string.

;;; Code:


(require 'cl-lib)
(require 'url)
(require 'request)
(require 'subr-x)
(require 'thingatpt)
(require 'json)
(require 'dom)

(declare-function helm "helm")
(declare-function helm-build-async-source "helm")
(declare-function ivy-read "ivy")


(defgroup kiwix nil
  "Kiwix customization options."
  :group 'kiwix)

(defcustom kiwix-zim-dir
  (when (file-readable-p (expand-file-name "~/.www.kiwix.org/kiwix"))
    (file-name-directory
     (concat
      (expand-file-name "~/.www.kiwix.org/kiwix")
      (car (directory-files (expand-file-name "~/.www.kiwix.org/kiwix") nil ".*\\.default\\'")) ; profile folder name
      "/data/library/*.zim")))
  "The kiwix ZIM files directory."
  :type 'string
  :safe #'stringp)

(defcustom kiwix-server-type 'docker-local
  "Specify the kiwix-serve type.
- 'docker-remote :: remote docker service
- 'docker-local  :: local docker service
- 'kiwix-serve-local :: local kiwix-serve service"
  :type '(choice
          (const :tag "Remote Docker Service" docker-remote)
          (const :tag "Local Docker Service" docker-local)
          (const :tag "Local kiwix-serve Service" kiwix-serve-local)))

(defcustom kiwix-server-api-version "v2"
  "The kiwix-serve homepage API version.
- 3.1.0 :: v1
- 3.2.0 :: v2"
  :type 'string
  :safe #'stringp)

(defcustom kiwix-server-url "http://127.0.0.1"
  "Specify Kiwix server URL."
  :type 'string
  :safe #'stringp)

(defcustom kiwix-server-port 8000
  "Specify default kiwix-serve server port."
  :type 'number
  :safe #'numberp)

(defcustom kiwix-server-command
  (when (eq kiwix-server-type 'kiwix-serve-local)
    (cond
     ((file-executable-p "/usr/bin/kiwix-serve") "/usr/bin/kiwix-serve")
     ((eq system-type 'gnu/linux) "/usr/lib/kiwix/bin/kiwix-serve")
     ((eq system-type 'darwin)
      (warn "You need to specify Mac OS X Kiwix path. And send a PR to my repo."))
     ((eq system-type 'windows-nt)
      (warn "You need to specify Windows Kiwix path. And send a PR to my repo."))))
  "Specify kiwix server command."
  :type 'string)

(defcustom kiwix-default-completing-read (cond
                                          ((fboundp 'vertico--all-completions) 'vertico)
                                          ((fboundp 'consult--read) 'selectrum)
                                          ((fboundp 'ivy-read) 'ivy)
                                          ((fboundp 'helm) 'helm)
                                          (t t))
  "Kiwix default completion frontend.
Currently `selectrum', Ivy (`ivy') and Helm (`helm') all supported.
Set it to ‘t’ will use Emacs built-in ‘completing-read’."
  :type 'symbol
  :safe #'symbolp)

(defcustom kiwix-default-browser-function browse-url-browser-function
  "Set default browser for open kiwix query result URL."
  :type '(choice
          (const :tag "browse-url default function" browse-url-default-browser)
          (const :tag "EWW" eww-browse-url)
          (const :tag "EAF web browser" eaf-open-browser)
          (const :tag "Firefox web browser" browse-url-firefox)
          (const :tag "Google Chrome web browser" browse-url-chrome)
          (const :tag "Conkeror web browser" browse-url-conkeror)
          (const :tag "xwidget browser" xwidget-webkit-browse-url))
  :safe #'symbolp)


(defvar kiwix-libraries ()
  "A list of Kiwix libraries.")

(defun kiwix--get-library-name (file)
  "Extract library name from library file."
  (replace-regexp-in-string "\\.zim\\'" "" file))

(defun kiwix-get-libraries ()
  "Check out all available Kiwix libraries."
  (cond
   ;; http://192.168.31.251:8580/catalog/search?start=0&count=
   ((and (eq kiwix-server-type 'docker-remote) (string-equal kiwix-server-api-version "v2"))
    (let ((url (format "%s:%s/catalog/search?start=0&count=" kiwix-server-url kiwix-server-port)))
      (request url
        :type "GET"
        :sync t
        :parser (lambda ()
                  (if (libxml-available-p)
                      (let ((xml-data (libxml-parse-xml-region (point-min) (point-max))))
                        (setq kiwix-libraries
                              (remove-if 'null
                                         (mapcar
                                          (lambda (cons)
                                            (when (and (listp cons) (eq (car cons) 'entry))
                                              (let* ((entry-xml cons)
                                                     (title (caddr (assq 'title entry-xml))) ; "title"
                                                     (thumbnail-url (format "%s:%s/%s"
                                                                            kiwix-server-url kiwix-server-port
                                                                            (cdr (assq 'href (cadr (assq 'link entry-xml))))))
                                                     (library-link-path (cdr
                                                                         (assq 'href
                                                                               (cadr
                                                                                (seq-find
                                                                                 (lambda (element)
                                                                                   (if (and (listp element) (eq (car element) 'link))
                                                                                       (if (string-equal (cdr (assq 'type (cadr element))) "text/html")
                                                                                           element)))
                                                                                 entry-xml)))))
                                                     (library-filename (string-trim-left library-link-path "/")))
                                                (propertize library-filename
                                                            'display (concat
                                                                      (when-let ((return-buffer (url-retrieve-synchronously thumbnail-url :silent)))
                                                                        (unwind-protect
                                                                            (let ((image-data (with-current-buffer return-buffer
                                                                                                (goto-char (point-min))
                                                                                                (search-forward "\n\n") ; skip HTTP response headers.
                                                                                                (buffer-substring-no-properties (point) (point-max)))))
                                                                              (kill-buffer return-buffer)
                                                                              (propertize " "
                                                                                          'display (create-image image-data nil t
                                                                                                                 :ascent 'center
                                                                                                                 :max-height (default-font-height))
                                                                                          'read-only t))))
                                                                      " "
                                                                      (format "%s (%s)" title library-filename))
                                                            'read-only t))))
                                          xml-data))))))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Function kiwix-get-libraries error.")))
        :success (cl-function
                  (lambda (&key _data &allow-other-keys)
                    _data))
        :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                       (500 . (lambda (&rest _) (message (format "Error from %s." url))))))))

   ;; ZIM library files on remote Docker server, parse index HTML page.
   ((and (eq kiwix-server-type 'docker-remote) (string-equal kiwix-server-api-version "v1"))
    ;; http://192.168.31.251:8580
    (let ((url (format "%s:%s" kiwix-server-url kiwix-server-port)))
      (request url
        :type "GET"
        :sync t
        :parser (lambda ()
                  (cond
                   ((libxml-available-p) ; (and (not (featurep 'elquery)) (libxml-available-p))
                    (let ((document (libxml-parse-html-region (point-min) (point-max))))
                      (setq kiwix-libraries
                            (mapcar
                             ;; remove "/" from "/<zim_library_name>"
                             (lambda (slash_library)
                               (substring slash_library 1 nil))
                             ;; list of "/<zim_library_name>"
                             (mapcar
                              (lambda (a)
                                (dom-attr a 'href))
                              (dom-by-tag
                               (dom-by-class
                                (dom-by-class document "kiwix") ; <div class="kiwix">
                                "book__list")                   ; <div class="book__list">
                               'a)      ; <a href="/wikipedia_zh_all_maxi_2021-03">
                              )))))
                   ((featurep 'elquery)
                    (require 'elquery)
                    (let ((html (elquery-read-string
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                      (setq kiwix-libraries
                            (mapcar
                             ;; remove "/" from "/<zim_library_name>"
                             (lambda (slash_library)
                               (substring slash_library 1 nil))
                             ;; extract plist values. list of "/<zim_library_name>"
                             (mapcar 'cadr
                                     ;; extract nodes properties in plist
                                     (mapcar #'elquery-props
                                             ;; return a list of elquery nodes
                                             (elquery-children
                                              ;; return the <div class="book__list">
                                              (car (elquery-$ ".book__list" html)))))))
                      (elquery-children (first (elquery-$ ".book__list" html)))))))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Function kiwix-get-libraries error.")))
        :success (cl-function
                  (lambda (&key _data &allow-other-keys)
                    _data))
        :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                       (500 . (lambda (&rest _) (message (format "Error from %s." url))))))))
   ;; ZIM library files on local host, parse directory files.
   ((or (eq kiwix-server-type 'kiwix-serve-local)
        (eq kiwix-server-type 'docker-local))
    (when (and (file-directory-p kiwix-zim-dir) (file-readable-p kiwix-zim-dir))
      (setq kiwix-libraries
            (mapcar #'kiwix--get-library-name
                    (directory-files kiwix-zim-dir nil ".*\\.zim\\'")))))))

(defun kiwix-select-library (&optional filter)
  "Select Kiwix library name."
  (kiwix-get-libraries)
  (completing-read "Kiwix library: " kiwix-libraries nil t filter))

(defcustom kiwix-mode-prefix nil
  "Specify kiwix-mode keybinding prefix before loading."
  :type 'kbd)

(defvar kiwix--server-process nil
  "Local server process launched by ‘kiwix-launch-server’.")

;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  (let ((library-path kiwix-zim-dir))
    (cl-case kiwix-server-type
      ('docker-remote
       (message "kiwix-serve service is started by user manually at other place."))
      ('docker-local (start-process
                      "kiwix-server"
                      " *kiwix server*"
                      "docker"
                      "container" "run" "-d"
                      "--name" "kiwix-serve"
                      "-v" (concat (file-name-directory library-path) ":" "/data")
                      "-p" (format "%s:80" kiwix-server-port)
                      "kiwix/kiwix-serve"
                      "--library" "library.xml"))
      ('kiwix-serve-local
       (setq kiwix--server-process
             (start-process
              "kiwix-server"
              " *kiwix server*"
              kiwix-server-command
              "--port" (number-to-string kiwix-server-port)
              "--library" (concat kiwix-zim-dir "/library.xml")))))))

(defun kiwix-stop-local-server ()
  "Stops a Kiwix server started by ‘kiwix-launch-server’."
  (interactive)
  (when kiwix--server-process
    (kill-process kiwix--server-process)
    (setq kiwix--server-process nil)))

(defun kiwix-capitalize-first (string)
  "Only capitalize the first word of STRING."
  (concat (string (upcase (aref string 0))) (substring string 1)))

(defun kiwix-query (query &optional selected-library)
  "Search `QUERY' in `LIBRARY' with Kiwix."
  (cond
   ;; http://192.168.31.251:8580/wikipedia_en_all_maxi_2021-03/A/Linux_kernel
   ((and (eq kiwix-server-type 'docker-remote) (string-equal kiwix-server-api-version "v2"))
    (let* ((library (or selected-library (kiwix--get-library-name selected-library)))
           (url (concat (format "%s:%s" kiwix-server-url kiwix-server-port)
                        "/" library "/A/" (replace-regexp-in-string " " "_" query)))
           (browse-url-browser-function kiwix-default-browser-function))
      (browse-url url)))
   ;; http://192.168.31.251:8580/search?content=wikipedia_en_all_maxi_2021-03&pattern=Linux%20kernel
   ((and (eq kiwix-server-type 'docker-remote) (string-equal kiwix-server-api-version "v1"))
    (let* ((library (or selected-library (kiwix--get-library-name selected-library)))
           (url (concat (format "%s:%s" kiwix-server-url kiwix-server-port)
                        "/search?content=" library "&pattern=" (url-hexify-string query)))
           (browse-url-browser-function kiwix-default-browser-function))
      (browse-url url)))
   ;; http://192.168.31.251:8580/search?content=wikipedia_en_all_maxi_2021-03&pattern=Linux%20kernel
   (t
    (let* ((library (or selected-library (kiwix--get-library-name selected-library)))
           (url (concat (format "%s:%s" kiwix-server-url kiwix-server-port)
                        "/search?content=" library "&pattern=" (url-hexify-string query)))
           (browse-url-browser-function kiwix-default-browser-function))
      (browse-url url)))))

(defun kiwix-docker-check ()
  "Make sure Docker image 'kiwix/kiwix-server' is available."
  (let ((docker-image (replace-regexp-in-string
                       "\n" ""
                       (shell-command-to-string
                        "docker image ls kiwix/kiwix-serve | sed -n '2p' | cut -d ' ' -f 1"))))
    (string-equal docker-image "kiwix/kiwix-serve")))

(defvar kiwix-server-available? nil
  "The kiwix-server current available?")

;;;###autoload
(defun kiwix-ping-server ()
  "Ping Kiwix server to set `kiwix-server-available?' global state variable."
  (and (eq kiwix-server-type 'docker-local)
       (or (kiwix-docker-check)
           (async-shell-command "docker pull kiwix/kiwix-serve")))
  (let ((inhibit-message t))
    ;; http://192.168.31.251:8580
    (request (format "%s:%s" kiwix-server-url kiwix-server-port)
      :type "GET"
      :sync t
      :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (setq kiwix-server-available? nil)
                (when (string-equal (cdr error-thrown) "exited abnormally with code 7\n")
                  (warn "kiwix.el failed to connect to host. exited abnormally with status code: 7."))))
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (setq kiwix-server-available? t)))
      :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                     (500 . (lambda (&rest _) (message (format "Error from  %s." url))))))))

(defun kiwix--ajax-search-hints (input &optional selected-library)
  "Instantly AJAX request to get available Kiwix entry keywords
list and return a list result."
  (kiwix-ping-server)
  (when (and input kiwix-server-available?)
    (let* ((library (or selected-library
                        (kiwix--get-library-name selected-library)))
           ;; http://192.168.31.251:8580/suggest?content=wikipedia_en_all_maxi_2021-03&term=linux
           (ajax-api (format "%s:%s/suggest?content=%s&term="
                             kiwix-server-url kiwix-server-port
                             library))
           (ajax-url (concat ajax-api input))
           (data (request-response-data
                  (let ((inhibit-message t))
                    (request ajax-url
                      :type "GET"
                      :sync t
                      :headers '(("Content-Type" . "application/json"))
                      :parser #'json-read
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  data)))))))
      (if (vectorp data) (mapcar #'cdar data)))))

(defun kiwix--get-thing-at-point ()
  "Get region select text or symbol at point."
  (if (use-region-p)
      (buffer-substring
       (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defun kiwix--ajax-select-available-hints (zim-library)
  "AJAX search hints on the selected library and select one term from available hints."
  (pcase kiwix-default-completing-read
    ('vertico
     (require 'vertico)
     (require 'consult)
     (consult--read
      (lambda (input)
        (apply #'kiwix--ajax-search-hints
               input `(,zim-library)))
      :prompt "Kiwix related entries: "
      :require-match nil))
    ('selectrum
     (require 'selectrum)
     (require 'consult)
     (consult--read
      (lambda (input)
        (apply #'kiwix--ajax-search-hints
               input `(,zim-library)))
      :prompt "Kiwix related entries: "
      :require-match nil))
    ('ivy
     (require 'ivy)
     (ivy-read
      "Kiwix related entries: "
      (lambda (input)
        (apply #'kiwix--ajax-search-hints
               input `(,zim-library)))
      :predicate nil
      :require-match nil
      :initial-input (kiwix--get-thing-at-point)
      :preselect nil
      :def nil
      :history nil
      :keymap nil
      :update-fn 'auto
      :sort t
      :dynamic-collection t
      :caller 'ivy-done))
    ('helm
     (require 'helm)
     (helm
      :source (helm-build-async-source
               "kiwix-helm-search-hints"
               :candidates-process
               (lambda (input)
                 (apply #'kiwix--ajax-search-hints
                        input `(,zim-library))))
      :input (kiwix--get-thing-at-point)
      :buffer "*helm kiwix completion candidates*"))
    (_
     (completing-read
      "Kiwix related entries: "
      ;; FIXME: This needs work!
      (completion-table-dynamic
       (lambda (input)
         (apply #'kiwix--ajax-search-hints
                input `(,zim-library))))
      nil nil
      (kiwix--get-thing-at-point)))))

;;;###autoload
(defun kiwix-search-at-library (zim-library query)
  "Search QUERY in selected ZIM library."
  (interactive (let ((zim-library (kiwix-select-library)))
                 (list zim-library (kiwix--ajax-select-available-hints zim-library))))
  (message (format "library: %s, query: %s" zim-library query))
  (if (or (null zim-library)
          (string-empty-p zim-library)
          (null query)
          (string-empty-p query))
      (error "Your query is invalid")
    (kiwix-query query zim-library)))

;;;###autoload
(defun kiwix-search-full-context (query)
  "Full context search QUERY in all kiwix ZIM libraries. It's very slow."
  (interactive
   (list (read-string "kiwix full context search in all libraries: ")))
  ;; http://192.168.31.251:8580/search?content=wikipedia_en_all_maxi_2021-03&pattern=Linux%20kernel
  (browse-url (format "%s:%s/search?pattern=%s" kiwix-server-url kiwix-server-port query)))

;;;###autoload
(defun kiwix-at-point ()
  "Search for the symbol at point with `kiwix-search-at-library'."
  (interactive)
  (unless (kiwix-ping-server)
    (kiwix-launch-server))
  (if kiwix-server-available?
      (call-interactively 'kiwix-search-at-library)
    (warn "kiwix-serve is not available, please start it at first."))
  (setq kiwix-server-available? nil))

;;===============================================================================

(defun kiwix-mode-enable ()
  "Enable kiwix-mode."
  )

(defun kiwix-mode-disable ()
  "Disable kiwix-mode."
  )

(defvar kiwix-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "kiwix-mode map.")

;;;###autoload
(define-minor-mode kiwix-mode
  "Kiwix global minor mode for searching Kiwix serve."
  :global t
  :lighter " Kiwix"
  (if kiwix-mode (kiwix-mode-enable) (kiwix-mode-disable)))


(provide 'kiwix)

;;; kiwix.el ends here
