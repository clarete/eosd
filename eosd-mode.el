;;; eosd-mode.el --- UI code to display notifications received ; -*- lexical-binding: t; -*-
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
(require 'shr)

(defface eosd-heading-face
  '((((class color) (background dark)) :foreground "dim gray")
    (t (:foreground "gray")))
  "Face for section headings."
  :group 'eosd-faces)

(defface eosd-title-face
  '((((class color) (background dark)) :foreground "#5180b3")
    (t (:foreground "blue")))
  "Face for notification titles."
  :group 'eosd-faces)

(defface eosd-datetime-face
  '((((class color) (background dark)) :foreground "#5180b3")
    (t (:foreground "blue")))
  "Face for notification date-time."
  :group 'eosd-faces)

(defface eosd-action-link-face
  '((((class color) (background dark)) :foreground "cornsilk4")
    (t (:foreground "yellow")))
  "Face used for links to notification actions."
  :group 'eosd-faces)

(defface eosd-delete-link-face
  '((((class color) (background dark)) :foreground "IndianRed")
    (t (:foreground "red")))
  "Face used for links to delete notifications."
  :group 'eosd-faces)

(defvar eosd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "g") 'eosd-mode-create-or-update-buffer)
    map)
  "The keymap to use with `eosd-mode'.")

(defgroup eosd-mode nil
  "Emacs Desktop Notifications."
  :group 'eosd-mode)

(defcustom eosd-mode-enable-icon nil
  "EOSD will render notification icons if this value is not nil."
  :group 'eosd-mode
  :type 'boolean)

(defcustom eosd-mode-datetime-format nil
  "Date-Time format for rendering dates.

If set to nil, it will show approximate time."
  :group 'eosd-mode
  :type 'string)

(defcustom eosd-mode-notification-indent 4
  "How spaces should be used to indent a notification message."
  :group 'eosd-mode
  :type 'integer)

(defcustom eosd-mode-hook nil
  "Hook run after entering eosd-mode."
  :group 'eosd-mode
  :type 'hook)

(defcustom eosd-mode-section-hook
  '(eosd-mode-section-header
    eosd-mode-section-notifications)
  "Hooks for building the UI in eosd-mode."
  :group 'eosd-mode
  :type 'hook)

(defun eosd-mode-link (text face &rest key func arg)
  "Insert link showing TEXT with FACE, activated by KEY and execute FUNC passing ARG."
  (let ((map (make-sparse-keymap))
        (key key)
        (func func)
        (arg arg))
    (unless (not (and key func))
      (define-key map (kbd "<RET>")
        #'(lambda (e) (interactive key) (apply func arg))))
    (insert (propertize text 'face face 'keymap map))))

(defun eosd-mode-parse-icon (icon)
  "Parse ICON from libnotify."
  (let ((data (string (nth 6 (nth 0 (nth 0 icon))))))
    (create-image (mapcar #'byte-to-string data) 'xbm t)))

(defun eosd-mode-render-app-icon (notification)
  "Render application info inside of NOTIFICATION."
  (let ((icon (cdr (assoc 'app-icon notification))))
    (if (file-regular-p icon)
        (insert-image (create-image icon))
      (let ((hints (cdr (assoc 'hints notification))))
        (dolist (h hints)
          (pcase (car h)
            (`"icon_data" (insert "   "))  ; TODO: Parse icon data in (cdr h)
            (`"image-data" (insert "   ")) ; TODO: Parse icon data in (cdr h)
            ))))
    (insert " ")))

(defun eosd-mode-render-actions (notification)
  "Render NOTIFICATION actions."
  (dolist (action (cdr (assoc 'actions notification)))
    (eosd-mode-link
     (downcase action)
     'eosd-action-link-face "d"
     'message notification)
    (insert " ⋅ "))
  (eosd-mode-link
   "delete" 'eosd-delete-link-face "d"
   'eosd-mode-delete-notification notification)
  (insert ?\n))

(defun eosd-mode-delete-notification (notification-id)
  "Delete a notification identified by NOTIFICATION-ID."
  (message "notification deleted"))

(defun eosd-mode-render-body (notification)
  "Render `body' field of NOTIFICATION.

The text will be rendered along with the actions a notification
may present to the user.  Check `eosd-mode-render-actions' to see
which callbacks are associated with printable text.

The `shr' library will be used to render simple HTML in order to
support `body-hyperlinks'.  Blank lines are trimmed from text
after rendered as HTML."
  (let ((start (point))
	(_ (insert (cdr (assoc 'body notification)))))
    (shr-render-region start (point))
    (while (eq (preceding-char) ?\n)
      (goto-char (- (point) 1)))
    (insert ?\n)
    (eosd-mode-render-actions notification)
    (save-excursion
      (save-restriction
        (widen)
        (indent-region start (point) eosd-mode-notification-indent)
        (goto-char start)
        (delete-blank-lines)))))

(defun eosd-mode-find-second-format (s)
  "Find good format for S."
  (cond ((zerop s) "just now")
        ((<= s 19) (format "%ds" s))
        ((and (> s 19) (< s 40)) "half a minute")
        ((< s 59) "~1m")
        (t "1m")))

(defun eosd-mode-distance-from-current-time (timestamp)
  "TIMESTAMP."
  (let* ((current (float-time))
         (minutes (fround (/ (- current timestamp) 60.0)))
         (seconds (fround (- current timestamp))))
    (cond ((<= minutes 1)
           (eosd-mode-find-second-format seconds))
          ((and (> minutes 1) (< minutes 45))
           (format "%dm" minutes))
          ((and (> minutes 90) (< minutes 14401))
           (format "%dh" (fround (/ minutes 60))))
          (t "a while ago"))))

(defun eosd-mode-render-title (notification)
  "Render the title of NOTIFICATION.

Customize `eosd-title-face' and `eosd-datetime-face' to change
the font configuration for the title and date-time respectively.

Edit `eosd-mode-datetime-format' for customizing the date-time
format."
  (eosd-mode-link
   (cdr (assoc 'summary notification))
   'eosd-title-face)
  (let* ((timestamp (cdr (assoc 'timestamp notification)))
         (printable (if eosd-mode-datetime-format
                        (format-time-string eosd-mode-datetime-format timestamp)
                      (eosd-mode-distance-from-current-time timestamp))))
    (eosd-mode-link (format " ⋅ %s" printable) 'eosd-datetime-face)))

(defun eosd-mode-render-notification (notification)
  "Render a single NOTIFICATION item.

A NOTIFICATION is rendered into three different parts: An icon, a
title, and some content.  The title is usually the only item that
is always present.  The application may not send any icon
information for example.

The rendering of the icon can be enabled or disabled through the
variable `eosd-mode-enable-icon'."
  (when eosd-mode-enable-icon
    (eosd-mode-render-app-icon notification))
  (eosd-mode-render-title notification)
  (eosd-mode-render-body notification)
  (insert ?\n))

(defun eosd-mode-render-notification-list (notifications)
  "Render each item in the NOTIFICATIONS list."
  (dolist (notification notifications)
    (eosd-mode-render-notification notification))
  (goto-char 0))

(defun eosd-mode-setup ()
  "Prepare ground for the `EOSD' buffer."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  ;; disable yasnippet & linum-mode
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode -1))
  (when (fboundp 'linum-mode)
    (linum-mode -1)))

(defun eosd-mode-heading-string ()
  "Generate the text for the header of the EOSD buffer."
  (let ((l (length (eosd-cache-list))))
    (format "%d Notification%s\n\n" l (if (eq l 1) "" "s"))))

(defun eosd-mode-section-header ()
  "Insert the header of the EOSD buffer.

Customize the variable `eosd-heading-face' to change font
settings of the header."
  (insert
   (propertize
    (eosd-mode-heading-string) 'face 'eosd-heading-face)))

(defun eosd-mode-section-notifications ()
  "Insert notifications from the cache in the EOSD buffer."
  (eosd-mode-render-notification-list (eosd-cache-list)))

(define-derived-mode eosd-mode special-mode "Desktop Notifications"
  "Major mode for displaying Desktop Notifications."
  :group 'eosd-mode
  (eosd-mode-setup)
  (run-hooks 'eosd-mode-section-hook))

(defun eosd-mode-create-or-update-buffer ()
  "Update or Create special EOSD buffer."
  (interactive)
  (let* ((buf-name "*notifications*")
         (buf (get-buffer buf-name)))
    (if buf
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (eosd-mode)
            (switch-to-buffer buf)))
      (with-output-to-temp-buffer buf-name
        (switch-to-buffer buf-name)
        (setq font-lock-mode nil)
        (use-local-map eosd-mode-map)
        (let ((inhibit-read-only t))
          (eosd-mode))))))

(provide 'eosd-mode)
;;; eosd-mode.el ends here

;;  LocalWords:  TIMESTAMP
