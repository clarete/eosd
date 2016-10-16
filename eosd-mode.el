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

(defface eosd-text-mark-face
  '((t (:height 1)))
  "Face for section headings."
  :group 'eosd-faces)

(defvar eosd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "g") 'eosd-mode-create-or-update-buffer)
    (define-key map (kbd "n") 'eosd-mode-next-notification)
    (define-key map (kbd "p") 'eosd-mode-previous-notification)
    (define-key map (kbd "i") 'eosd-mode-notification-body-toggle)
    map)
  "The keymap to use with `eosd-mode'.")

(defvar eosd-buffer-name "*notifications*"
  "Name of notifications buffer.")

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

(defcustom eosd-mode-notification-mark "¶"
  "Character used as a text mark for each notification.

The default font configuration makes this char invisible, but
it can be changed through the variable `eosd-text-mark-face'"
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

(defun eosd-mode-link (text face &optional key func arg)
  "Insert actionable link with custom face.

TEXT will be inserted.  FACE will be used to configure all the
visual settings for the link.

The action will be triggered when keyboard shortcut defined by
`KEY'.  Action means the execution of `FUNC' passing `ARG' as
arguments."
  (let ((map (make-sparse-keymap)))
    (when (and key func)
      (define-key map (kbd key)
        #'(lambda (e) (interactive "p") (apply func arg))))
    (insert (propertize text 'face face 'keymap map))))

(defmacro eosd-mode-notification-find-boundaries (body)
  "Capture notification under cursor and pass it to BODY.

Any function that needs to use this functionality can start with
something like this:

  (eosd-mode-notification-find-boundaries
   #'(lambda (notification-id begin end)
      (message \"N:%d B:%s E:%s\" notification-id begin end)))

The notification ID is retrieved from a hidden text that is
inserted right before the text marker.

The visibility of the text marker is configurable whereas the id
visibility isn't."

  `(save-excursion
     (end-of-line)
     (when (re-search-backward (eosd-mode-mark "^[0-9]+%s.*") nil t 1)
       ;; Retrieve notification id hidden in text before the text
       ;; mark.  It will also be fed into the BODY function.
       (beginning-of-line)
       (mark-word)
       (let ((notification-id (string-to-number
                               (buffer-substring (mark) (point)))))
         (forward-line)
         (let* ((begin (point))
                (_ (forward-line))
                (end-point (re-search-forward
                            (eosd-mode-mark "^[0-9]+%s") nil t 1))
                (_ (beginning-of-line))
                (end (if end-point (point) (point-max)))
                (inhibit-read-only t))
           (funcall ,body notification-id begin end))))))

(defun eosd-mode-delete-notification-under-cursor ()
  "Delete notification under cursor."
  (interactive)
  (eosd-mode-notification-find-boundaries
   #'(lambda (notification-id begin end)
       (re-search-backward (eosd-mode-mark "^[0-9]+%s.*$") nil t 1)
       (beginning-of-line)
       (delete-region (point) end)
       (eosd-cache-delete-notification notification-id)))
  (eosd-mode-next-notification))

(defun eosd-mode-mark (fmt)
  "Return `FMT' filled in with configured text mark.

To change the configured text mark, refer to the variable
`eosd-mode-text-mark'."
  (format fmt eosd-mode-text-mark))

(defun eosd-mode-notification-body-toggle ()
  "Toggle visibility of the current notification's body."
  (interactive)
  (eosd-mode-notification-find-boundaries
   #'(lambda (notification-id begin end)
       (if (get-text-property begin 'invisible)
           (facemenu-remove-special begin end)
         (facemenu-set-invisible begin end)))))

(defun eosd-mode-next-notification ()
  "Go to the next notification."
  (interactive)
  (re-search-forward (eosd-mode-mark "^[0-9]+%s") nil t 1))

(defun eosd-mode-previous-notification ()
  "Go to the previous notification."
  (interactive)
  (forward-line -1)
  (re-search-backward (eosd-mode-mark "^[0-9]+%s") nil t 1)
  (eosd-mode-next-notification))

(defun eosd-mode-notification-mark (mark)
  "Insert invisible MARK."
  (insert (propertize (eosd-mode-mark "%s") 'face 'eosd-text-mark-face)))

(defun eosd-mode-parse-icon (icon)
  "Parse ICON from `libnotify'."
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
     (downcase action) 'eosd-action-link-face "<RET>"
     #'(lambda () (message "%s" notification)))
    (insert " ⋅ "))
  (eosd-mode-link
   "delete" 'eosd-delete-link-face "<RET>"
   'eosd-mode-delete-notification-under-cursor)
  (insert ?\n))

(defun eosd-mode-render-body (notification)
  "Render `body' field of NOTIFICATION.

The text will be rendered along with the actions a notification
may present to the user.  Check `eosd-mode-render-actions' to see
which callbacks are associated with printable text.

The `shr' library will be used to render simple HTML in order to
support `body-hyperlinks'.  Blank lines are trimmed from text
after rendered as HTML."
  (insert ?\n)
  (let ((start (point)))
    (insert (cdr (assoc 'body notification)) ?\n)
    (fill-region start (point))
    (eosd-mode-render-actions notification)
    (indent-region
     start (point)
     eosd-mode-notification-indent)))

(defun eosd-mode-find-second-format (s)
  "Find good format for S."
  (cond ((zerop s) "just now")
        ((<= s 19) (format "%ds" s))
        ((and (> s 19) (< s 40)) "half a minute")
        ((< s 59) "~1m")
        (t "1m")))

(defun eosd-mode-distance-from-current-time (timestamp)
  "How much time past since TIMESTAMP in text.

 *   1m <= X: calls `eosd-mode-find-second-format'
 *   1m >  X > 45m:  Xm
 *  45m >= X > 90m:  about 1h
 *  90m >= X > 240h: Xh
 * 240h >= X:        a while ago"
  (let* ((current (float-time))
         (minutes (fround (/ (- current timestamp) 60.0)))
         (seconds (fround (- current timestamp))))
    (cond ((<= minutes 1)
           (eosd-mode-find-second-format seconds))
          ((and (> minutes 1) (< minutes 45))
           (format "%dm" minutes))
          ((and (>= minutes 45) (< minutes 90))
           "about 1h")
          ((and (>= minutes 90) (< minutes 14401))
           (format "%dh" (fround (/ minutes 60))))
          (t "a while ago"))))

(defun eosd-mode-notification-id (notification)
  "Insert id of NOTIFICATION as an invisible text."
  (let* ((begin (point))
         (id (cdr (assoc 'id notification))))
    (insert (number-to-string id))
    (facemenu-set-invisible begin (point))))

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
  (eosd-mode-notification-id notification)
  (eosd-mode-notification-mark "begin")
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

(defun eosd-mode-notification-insert-if-buffer (notification)
  "Insert NOTIFICATION if *notifications* buffer exists."
  (eosd-mode-with-buffer
   #'(lambda (b)
       (goto-char 0)
       (forward-line 2)
       (eosd-mode-render-notification notification))))

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

(defmacro eosd-mode-with-buffer (body)
  "BODY."
  `(let ((buf (get-buffer eosd-buffer-name)))
     (when buf
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (funcall ,body buf)))
       buf)))

(defun eosd-mode-create-buffer ()
  "Create the *notifications* buffer."
  (with-output-to-temp-buffer eosd-buffer-name
    (switch-to-buffer eosd-buffer-name)
    (setq font-lock-mode nil)
    (use-local-map eosd-mode-map)
    (let ((inhibit-read-only t))
      (eosd-mode))))

(defun eosd-mode-create-or-update-buffer ()
  "Update or Create special *notifications* buffer."
  (interactive)
  (if (not (eosd-mode-with-buffer
            #'(lambda (b)
                (erase-buffer)
                (eosd-mode)
                (switch-to-buffer b))))
      (eosd-mode-create-buffer)))

(provide 'eosd-mode)
;;; eosd-mode.el ends here
