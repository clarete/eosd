;;; eosd-test.el --- Server interface to desktop notifications
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

(require 'ert)
(require 'cl)

(when (require 'undercover nil t)
  (undercover "eosd*.el"))

(require 'eosd)
(require 'eosd-cache)

(ert-deftest eosd-new-notification ()
  "Convert entries received from D-Bus to internal format."

  (should
   (equal
    (eosd-cache-entry '(1234             ; id
                        "my application" ; app-name
                        0                ; replaces-id
                        "icon"           ; icon
                        "summary"        ; summary
                        "body"           ; body
                        '("a1" "a2")     ; actions
                        nil              ; hints
                        0))              ; expire-timeout
    '((id             . 1234)
      (app-name       . "my application")
      (replaces-id    . 0)
      (app-icon       . "icon")
      (summary        . "summary")
      (body           . "body")
      (actions        . '("a1" "a2"))
      (hints          . nil)
      (expire-timeout . 0)))))

(ert-deftest eosd-receive-notify ()
  "D-Bus client feeds the cache with incoming notifications."

  ;; When a new notification is fired
  (eosd-dbus-notify
   "my application" 0 "icon" "summary"
   "body" "actions" nil 0)
  (eosd-dbus-notify
   "another app" 1 "another icon" "another summary"
   "another body" "other actions" "other hints" 2)

  ;; Then it should appear in the notification list
  (should
   (equal (eosd-cache-list)
          (list
           (eosd-cache-entry '(1 "another app" 1 "another icon"
                                 "another summary" "another body"
                                 "other actions" "other hints" 2))
           (eosd-cache-entry '(1 "my application" 0 "icon" "summary"
                                 "body" "actions" nil 0))))))

;;; eosd-test.el ends here
