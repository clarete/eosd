;;; eosd-dbus.el --- D-Bus service that receives desktop notifications
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

(require 'dbus)
(require 'eosd-cache)


(defun eosd-dbus-get-server-information ()
  "Return server info to client."
  '("eosd" "https://clarete.li/eosd" "0.0.1" "1"))


(defun eosd-dbus-get-capabilities ()
  "Return server capabilities to the client."
  '("body" "body-hyperlinks" "body-images"))


(defun eosd-dbus-notify (app-name replaces-id app-icon summary
                         body actions hints expire-timeout)
  "Handle the notifications from external services.

 * APP-NAME: Name of the application that requested the
     notification
 * REPLACES-ID: ID of the message being replaced by the
     notification being received.  Defaults to 0.
 * APP-ICON: Application icon (Format?)
 * SUMMARY: Small description of the notification
 * BODY: Notification content
 * ACTIONS: Actions the user can take when the notification is
     received.
 * HINTS: ?
 * EXPIRE-TIMEOUT: How long the message should be available for
     the user in the main view."
  (eosd-cache-new-notification
   (list app-name replaces-id app-icon summary body actions hints
         expire-timeout)))


(defun eosd-dbus-close-notification (id)
  "Close a notification identified by ID."
  (message "CloseNotification: %s" id))


(defun eosd-dbus-notification-closed (id reason)
  "Handle the signal `NotificationClosed'.

ID: Identification of the notification that has being closed
REASON: Reason for closing the notification client"
  (message "NotificationClosed: %s" id))


(defun eosd-dbus-action-invoked (id action-key)
  "Handle the signal `ActionInvoked'.

ID: Identification of the notification emitting the action
ACTION-KEY: The Key of the action invoked"
  (message "ActionInvoked: %s %s" id action-key))


(defun eosd-dbus-start ()
  "Register Eosd service under `org.freedesktop.Notifications'."
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "Notify" 'eosd-dbus-notify)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "GetServerInformation" 'eosd-dbus-get-server-information)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "GetCapabilities" 'eosd-dbus-get-capabilities)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "CloseNotification" 'eosd-dbus-close-notification)

  (dbus-register-signal
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "NotificationClosed" 'eosd-dbus-notification-closed)
  (dbus-register-signal
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "ActionInvoked" 'eosd-dbus-action-invoked))


(defun eosd-dbus-stop ()
  "Unregister Eosd D-Bus service."
  (dbus-unregister-service
   :session "org.freedesktop.Notifications"))

(provide 'eosd-dbus)
;;; eosd-dbus.el ends here
