;;; eosd-cache.el --- Caching layer for EOSD
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

(defvar eosd-notification-list nil
  "The list of notifications to display.")

(defun eosd-cache-entry-id ()
  "Generate a unique id for entries."
  1);(random (expt 16 4)))

(defun eosd-cache-entry (entry)
  "Transforms ENTRY from list to an assoc list."
  (pairlis
   '(id app-name replaces-id app-icon summary body actions hints expire-timeout)
   entry))

(defun eosd-cache-list ()
  "List all notifications saved."
  eosd-notification-list)

(defun eosd-cache-new-notification (fields)
  "Transform FIELDS into a notification and save it into cache."
  (let ((entry-id (eosd-cache-entry-id)))
    (push (eosd-cache-entry (cons entry-id fields))
          eosd-notification-list)
    entry-id))

(provide 'eosd-cache)
;;; eosd-cache.el ends here
