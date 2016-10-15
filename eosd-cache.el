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

(eval-when-compile (require 'cl))

(defvar eosd-notification-list nil
  "The list of notifications to display.")

(defvar eosd-notification-fields
  '(id timestamp app-name
       replaces-id app-icon summary body actions hints
       expire-timeout)
  "Field names of a notification entry.")

(defun eosd-cache-entry-id ()
  "Generate a unique id for entries."
  (random (expt 16 4)))

(defun eosd-cache-list ()
  "List all notifications saved."
  eosd-notification-list)

(defun eosd-cache-new-notification (fields)
  "Transform FIELDS into a notification and save it into cache."
  (let ((entry-id (eosd-cache-entry-id))
        (time (float-time)))
    (push (cl-pairlis eosd-notification-fields
                      (cons entry-id (cons time fields)))
          eosd-notification-list)
    entry-id))

(defun eosd-cache-delete-notification (notification-id)
  "Delete NOTIFICATION-ID from the cache."
  (setq eosd-notification-list
        (delq (assoc (cons 'id notification-id)
                     eosd-notification-list)
              eosd-notification-list)))

(provide 'eosd-cache)
;;; eosd-cache.el ends here
