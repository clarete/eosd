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

(when (require 'undercover nil t)
  (undercover "eosd*.el"))

(require 'ert)
(require 'eosd)


(ert-deftest eosd-receive-notify ()
  "The D-Bus client should feed the Cache with incoming notifications."

  ;; When a new notification is fired
  (eosd-dbus-notify
   "my application" 0 "icon" "summary"
   "body" "actions" "hints" 0)

  (eosd-dbus-notify
   "another app" 1 "another icon" "another summary"
   "another body" "other actions" "other hints" 2)

  ;; Then it should appear in the notification list
  (should
   (equal (eosd-cache-list)
          '(("another app" 1 "another icon" "another summary" "another body"
             "other actions" "other hints" 2)
            ("my application" 0 "icon" "summary" "body" "actions" "hints" 0)))))

;;; eosd-test.el ends here
