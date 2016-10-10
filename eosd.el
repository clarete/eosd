;;; eosd.el --- Emacs Desktop Notification Daemon
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;; URL: https://github.com/clarete/eosd.el
;; Keywords: desktop, notification, osd
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (magit-popup "2.6.0") (json-mode "1.7.0"))
;;
;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
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

;(add-to-list 'load-path "~/src/github.com/clarete/eosd/")

(require 'eosd-dbus)
(require 'eosd-mode)

(defun eosd-start ()
  "Register EOSD service."
  (eosd-dbus-start))

(defun eosd-stop ()
  "Unregister EOSD service."
  (eosd-dbus-stop))

(defun eosd ()
  "Create or Open + Refresh special *EOSD* buffer."
  (interactive)
  (eosd-mode-create-or-update-buffer))

(provide 'eosd)
;;; eosd.el ends here
