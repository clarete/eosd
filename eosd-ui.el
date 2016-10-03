;;; eosd-ui.el --- UI code to display notifications already received
;;
;;; Commentary:
;;
;; Copyright (C) 2016  Lincoln Clarete <lincoln@clarete.li>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'eosd-cache)

(defface eosd-action-link-face '((t (:foreground "yellow")))
  "Face used for links to notification actions."
  :group 'eosd)

(defface eosd-delete-link-face '((t (:foreground "red")))
  "Face used for links to delete notifications."
  :group 'eosd)

(defvar eosd-ui-map (make-sparse-keymap)
  "The keymap to use with eosd.")

(defun eosd-ui-link (text face key func arg)
  "Insert link showing TEXT with FACE, activated by KEY and execute FUNC passing ARG."
  (lexical-let ((map (make-sparse-keymap))
                (key key)
                (func func)
                (arg arg))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive key) (apply func arg)))
    (insert
     (propertize text 'face face 'keymap map 'mouse-face 'highlight))))

(defun eosd-ui-delete-notification (notification-id)
  "Delete a notification identified by NOTIFICATION-ID."
  (message "notification deleted"))

(defun eosd-ui-render-app-icon (notification)
  "Render application info inside of NOTIFICATION."
  (let ((icon (cdr (assoc 'app-icon notification))))
    (if (file-regular-p icon)
        (insert-image (create-image icon))
      nil)))

(defun eosd-ui-render-body (notification)
  "Render body field of NOTIFICATION."
  (push-mark)
  (region-beginning)
  (insert (format " %s" (cdr (assoc 'body notification))))
  (region-end)
  (fill-paragraph))

(defun eosd-ui-render-actions (notification)
  "Render NOTIFICATION actions."
  (dolist (action (cdr (assoc 'actions notification)))
    (eosd-ui-link
     (format " %s," action)
     'eosd-action-link-face
     "d" 'message notification))
  (eosd-ui-link
   " [d]elete" 'eosd-delete-link-face "d"
   'eosd-ui-delete-notification notification))

(defun eosd-ui-render-notification (notification)
  "Render a single NOTIFICATION item."
  (eosd-ui-render-app-icon notification)
  (insert (format " %s\n\n" (cdr (assoc 'summary notification))))
  (eosd-ui-render-body notification)
  (eosd-ui-render-actions notification)
  (insert "\n -----------------------------------------------------------------\n\n"))

(defun eosd-ui-render-notification-list (notifications)
  "Render all NOTIFICATIONS each one in a different line."
  (dolist (notification notifications)
    (eosd-ui-render-notification notification)))

(defun eosd-ui-render-to-new-buffer (notifications buf-name)
  "Render NOTIFICATIONS in newly created buffer called BUF-NAME."
  (with-output-to-temp-buffer buf-name
    (switch-to-buffer buf-name)
    (setq font-lock-mode nil)
    (use-local-map eosd-ui-map)
    (eosd-ui-render-notification-list notifications)))

(defun eosd-ui-render-to-existing-buffer (notifications buffer)
  "Render NOTIFICATIONS in existing BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (switch-to-buffer buffer))))

(defun eosd-ui-buffer (notifications)
  "Get buffer to render NOTIFICATIONS into."
  (let* ((buf-name "*EOSD*")
         (buf (get-buffer buf-name)))
    (if buf
        (eosd-ui-render-to-existing-buffer notifications buf)
      (eosd-ui-render-to-new-buffer notifications buf-name))))

(defun eosd-ui ()
  "Build or Update eosd UI on a configurable buffer."
  (interactive)
  (if eosd-ui-map
    (progn
      (define-key eosd-ui-map (kbd "q") 'bury-buffer)))
  (eosd-ui-buffer (eosd-cache-list)))

;(eosd-ui)

(provide 'eosd-ui)
;;; eosd-ui.el ends here
