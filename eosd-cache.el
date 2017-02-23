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

(defvar eosd-notification-filter nil
  "Expression applied to filter which notifications to show.")

(defvar eosd-notification-fields '(id timestamp app-name
  replaces-id app-icon summary body actions hints expire-timeout)
  "Field names of a notification entry.")

(defun eosd-cache-entry-id ()
  "Generate a unique id for entries."
  (random (expt 16 4)))

(defun eosd-cache-apply-filter (notification filter)
  "Test if NOTIFICATION match with FILTER."
  (let* ((operator (car filter))
         (field (cadr filter))
         (needed (caddr filter))
         (content (cdr (assoc field notification))))
    (if (apply (subst content field filter))
        notification
      nil)))

(defun eosd-cache-apply-filters (notification)
  "Apply all filters in NOTIFICATION."
  (if (null eosd-notification-filter)
      notification
    (if (and (delq nil
              (mapcar (lambda (filter)
                        (eosd-cache-apply-filter notification filter))
                      eosd-notification-filter)))
        notification
      nil)))

(defun eosd-cache-filter (notifications)
  "Filter NOTIFICATIONS list."
  (delq nil
   (mapcar 'eosd-cache-apply-filters notifications)))

(defun eosd-cache-list ()
  "Notifications filtered with expression in `eosd-notification-filter'."
  (eosd-cache-filter eosd-notification-list))

(defun eosd-cache-new-notification (fields)
  "Transform FIELDS into a notification and save it into cache.

The return value will be the newly created notification."
  (let* ((entry-id (eosd-cache-entry-id))
         (time (float-time))
         (notification (cl-pairlis
                        eosd-notification-fields
                        (cons entry-id (cons time fields)))))
    (push notification eosd-notification-list)
    notification))

(defun eosd-cache-delete-notification (notification-id)
  "Delete NOTIFICATION-ID from the cache."
  (setq eosd-notification-list
        (delq (assoc (cons 'id notification-id)
                     eosd-notification-list)
              eosd-notification-list)))

(provide 'eosd-cache)
;;; eosd-cache.el ends here
