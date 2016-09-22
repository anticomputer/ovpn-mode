;;; ovpn-mode.el --- an openvpn management mode for emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Bas Alberts

;; Author: Bas Alberts <bas@collarchoke.org>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage: M-x ovpn to enter main mode, M-x ovpn-mode-dir-set to switch to
;; a new conf dir base.
;;
;; Note that ovpn-mode will maintain state, so you can switch between
;; conf dir bases and it will display the correct states for any confs
;; in use as you swap back and forth between conf listings
;;
;; The only _hard_ requirement is that your openvpn configurations are
;; named according to the bla.ovpn convention.
;;

;;; Code:
(require 'cl)
(require 'netrc)

;;; sudo auth convenience functions
(defvar ovpn-mode-authinfo (expand-file-name "~/.authinfo.gpg"))
(defvar ovpn-mode-authinfo-token "ovpn-mode-sudo")
(defvar ovpn-mode-use-authinfo t) ; set to nil if you prefer to be prompted

;; add "machine ovpn-mode-sudo login root password yoursudopassword" to .authinfo
;; if you don't want to be prompted for your sudo password all the time
(defun ovpn-mode-pull-authinfo ()
  "Pulls an .authinfo password for machine `ovpn-mode-authinfo-token'"
  (let* ((netrc (netrc-parse ovpn-mode-authinfo))
         (hostentry (netrc-machine netrc ovpn-mode-authinfo-token)))
    (when hostentry (netrc-get hostentry "password"))))

;;; this is where all your openvpn confs live, if not using absolute certificate paths
;;; in your .ovpn's then we assume the certificates are in the same directory as the conf
(defvar ovpn-mode-base-directory "~/vpn/default")

(defvar ovpn-mode-hook nil
  "Hook being run after `ovpn-mode' has completely set up the buffer.")

;;; major mode for future buffer and keymap enhancements
(defvar ovpn-mode-keywords '("ovpn"))
(defvar ovpn-mode-keywords-regexp (regexp-opt ovpn-mode-keywords 'words))
(defvar ovpn-mode-font-lock-keywords `((,ovpn-mode-keywords-regexp . font-lock-keyword-face)))

;;; this is used for platform specific quirks
(defvar ovpn-mode-current-platform 'linux)

;;; struct so that we can expand this easily
(cl-defstruct struct-ovpn-mode-platform-specific
  ipv6-toggle
  ipv6-status
  netns-create)

(defvar ovpn-mode-platform-specific nil)

(cond ((equal ovpn-mode-current-platform 'linux)
       (setq ovpn-mode-platform-specific
             (make-struct-ovpn-mode-platform-specific
              :ipv6-toggle 'ovpn-mode-ipv6-linux-toggle
              :ipv6-status 'ovpn-mode-ipv6-linux-status
              :netns-create 'ovpn-mode-netns-linux-create)))
      (t
       ;; default to 'ignore non-specifics platform
       (setq ovpn-mode-platform-specific
             (make-struct-ovpn-mode-platform-specific
              :ipv6-toggle 'ignore
              :ipv6-status 'ignore
              :netns-create 'ignore))))

(defvar ovpn-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from fundamental and do mode-global keybindings here
    (define-key map "s" 'ovpn-mode-start-vpn)
    (define-key map "r" 'ovpn-mode-restart-vpn)
    (define-key map "q" 'ovpn-mode-stop-vpn)
    (define-key map "i" 'ovpn-mode-info-vpn)
    (define-key map "b" 'ovpn-mode-buffer-vpn)
    (define-key map "e" 'ovpn-mode-edit-vpn)
    (define-key map "d" 'ovpn-mode-dir-set)
    (define-key map "n" 'ovpn-mode-start-vpn-with-namespace)
    (define-key map "x" 'ovpn-mode-async-shell-command-in-namespace)
    (define-key map "6" (struct-ovpn-mode-platform-specific-ipv6-toggle
                         ovpn-mode-platform-specific))
    (define-key map "h" 'describe-mode)
    map)
  "The keyboard map for ovpn-mode.")

(define-derived-mode ovpn-mode special-mode
  "ovpn-mode"
  "Management mode for interacting with openvpn configurations"
  (setq font-lock-defaults '((ovpn-mode-font-lock-keywords)))
  (use-local-map (copy-keymap ovpn-mode-map))
  ;; define-derive-mode automatically installs ovpn-mode-hook
  ;; to run through run-mode-hooks as the last thing it does
  )

(setq ovpn-mode-keywords nil)
(setq ovpn-mode-keywords-regexp nil)
;; end of major mode code

(defvar ovpn-mode-configurations nil)
(defvar ovpn-mode-buffer-name "*ovpn-mode*")
(defvar ovpn-mode-buffer nil)


;;; sudo process handling

(defun ovpn-mode-send-sudo-password (proc prompt)
  (let ((password (or (when ovpn-mode-use-authinfo
                        (ovpn-mode-pull-authinfo))
                      (read-passwd prompt))))
    (process-send-string proc (concat password "\n"))))

(defun ovpn-mode-sudo-filter (proc string)
  (when (process-live-p proc)
    (if (string-match tramp-password-prompt-regexp string)
        (ovpn-mode-send-sudo-password proc string)
      (mapcar 'message (split-string string "\n")))))

(defmacro ovpn-mode-sudo (name buffer &rest args)
  "sudo exec a command ARGS with name PROC-NAME and output to BUFFER"
  `(let ((process (start-process ,name ,buffer "sudo" ,@args)))
     (when (process-live-p process)
       (set-process-filter process 'ovpn-mode-sudo-filter))
     process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Linux specifics

;; namespace management ... inspired by https://github.com/crasm/vpnshift.sh
;; in contrast, this implementation lets you juggle multiple namespaces at once

(defvar ovpn-mode-netns-base 0)
(defvar ovpn-mode-netns-free-base '()) ; :P

(defvar ovpn-mode-netns-ns0 "8.8.8.8") ; ns1 to use in namespace
(defvar ovpn-mode-netns-ns1 "8.8.4.4") ; ns2 to use in namespace

(defun ovpn-mode-netns-linux-create ()
  "Return a prop list containing an active namespace description"

  ;; we only increment the base-id counter if we can't recycle an old id
  ;; current logic hard limits us to 256-10 concurrent namespaces in use
  ;; because we also use the base-id to increment our network ranges in use
  ;; this should be moved to some proper CIDR logic

  ;; make an id available if we need one
  (when (= (length ovpn-mode-netns-free-base) 0)
    (message "Peeling off a network id for new namespace")
    (push ovpn-mode-netns-base ovpn-mode-netns-free-base)
    (setq ovpn-mode-netns-base (1+ ovpn-mode-netns-base)))

  (let* ((base-id (pop ovpn-mode-netns-free-base)) ;; recycle old base-id's
         (netns (format "ovpnmode_ns%d" base-id))
         (netns-buffer (get-buffer-create netns))
         (veth-default (format "veth_default%d" base-id))
         (veth-vpn (format "veth_vpn%d" base-id))

         ;; this is a little icky, need some CIDR templating and upper bound checking
         (netns-range-default
          (format "%s/31"
                  (mapconcat
                   #'(lambda (x) (format "%d" x))
                   `[10 10 ,(+ 10 base-id) 10] ; 10.10.10.10/31 base
                   ".")))
         (netns-range-vpn
          (format "%s/31"
                  (mapconcat
                   #'(lambda (x) (format "%d" x))
                   `[10 10 ,(+ 10 base-id) 11] ; 10.10.10.11/31 base
                   ".")))

         ;; the device set for openvpn inside the namespace
         (netns-tunvpn (format "tunvpn%d" base-id))

         ;; XXX: TODO add error checking ... a failure in any of these is a fail for *

         ;; initialize namespace ... not shell escaping any args because we
         ;; explicitly control the content of the variables here (see above)
         ;; in the case of potentially destructive commands, we will escape

         ;; use old ip netns syntax for wider compatibility base ... e.g. Ubuntu 14.04.5 LTS
         ;; does not support the newer -n/-netns switch

         (setup-cmds
          `(
            ;; setup the basic namespace
            ("ip" "netns" "add" ,netns)
            ("mkdir" "--parents" ,(format "/etc/netns/%s" netns))
            ("ip" "netns" "exec" ,netns "ip" "address" "add" "127.0.0.1/8" "dev" "lo")
            ("ip" "netns" "exec" ,netns "ip" "address" "add" "::1/128" "dev" "lo")
            ("ip" "netns" "exec" ,netns "ip" "link" "set" "lo" "up")

            ;; build a veth tunnel
            ("ip" "link" "add" ,veth-vpn "type" "veth" "peer" "name" ,veth-default)
            ("ip" "link" "set" ,veth-vpn "netns" ,netns)
            ("ip" "link" "set" ,veth-default "up")
            ("ip" "netns" "exec" ,netns "ip" "link" "set" ,veth-vpn "up")
            ("ip" "address" "add" ,netns-range-default "dev" ,veth-default)
            ("ip" "netns" "exec" ,netns "ip" "address" "add" ,netns-range-vpn "dev" ,veth-vpn)
            ("ip" "netns" "exec" ,netns "ip" "route" "add" "default" "via" ,(car (split-string netns-range-default "/")) "dev" ,veth-vpn)

            ;; enable IP forwarding ... we just keep this blanket enabled after set initially
            ("sysctl" "net.ipv4.ip_forward=1")

            ;; set up the dns for the namespace
            ("echo" "-e" ,(format "\"nameserver %s\\nnameserver %s\\n\""
                                  ovpn-mode-netns-ns0
                                  ovpn-mode-netns-ns1)
             ">" ,(format "/etc/netns/%s/resolv.conf" netns))
            ))

         ;; enable masquerading
         (masquerade-cmds-iptables
          `(
            ("iptables" "--table" "nat" "--append" "POSTROUTING" "--jump" "MASQUERADE" "--source" ,netns-range-default)
            ))
         (masquerade-cmds-firewalld
          `(
            ;; strictly speaking we only need to turn on the masquerading and we don't need to
            ;; set a dedicated zone, but I like to have them for more granular port filtering
            ;; control on the namespace through firewalld ... commented out for casual user sake

            ;; ("firewall-cmd" "-q" "--permanent" ,(format "--new-zone=%s" netns))
            ;; ("firewall-cmd" "-q" "--permanent" ,(format "--zone=%s" netns) "--set-target=default")
            ;; ("firewall-cmd" "-q" ,(format "--zone=%s" netns) ,(format "--change-interface=%s" veth-default))
            ;; ("firewall-cmd" "-q" ,(format "--zone=%s" netns) ,(format "--add-source=%s" netns-range-default))
            ;; enable masquerading on default zone
            ("firewall-cmd" "-q"  "--add-masquerade")
            ("firewall-cmd" "-q" ,(format "--add-rich-rule=\'rule family=\"ipv4\" source address=\"%s\" masquerade\'" netns-range-default))

            ))
         )

    ;; cycle through the setup commands synchronously as root
    (with-current-buffer netns-buffer
      (cd "/sudo::/tmp")
      ;; init namespace
      (dolist (cmd (append setup-cmds
                           ;; check if we're on a firewalld enabled system per chance
                           (if (equal (shell-command-to-string "pgrep firewalld") "")
                               masquerade-cmds-iptables
                             (progn
                               (message "Firewalld is running on this system, diverting masquerade setup")
                               masquerade-cmds-firewalld))))
        (let ((cmd (mapconcat #'(lambda (x) (format "%s" x)) cmd " ")))
          (message "ovpn-mode sudo executing: %s" cmd)
          (shell-command cmd))))

    ;; return the relevant property list for this namespace
    `(:netns ,netns
             :netns-buffer ,netns-buffer
             :veth-default ,veth-default
             :veth-vpn ,veth-vpn
             :netns-range-default ,netns-range-default
             :netns-range-vpn ,netns-range-vpn
             :netns-tunvpn ,netns-tunvpn
             :base-id ,base-id)
    ))

(defun ovpn-mode-netns-linux-delete-default-route (netns)
  "Remove the default route for a given network namespace property list NETNS"
  (let* ((netns-buffer (plist-get netns :netns-buffer))
         (netns-range-default (plist-get netns :netns-range-default))
         (veth-vpn (plist-get netns :veth-vpn))
         (namespace (plist-get netns :netns)))

    ;; XXX: TODO error checking
    (with-current-buffer netns-buffer
      (cd "/sudo::/tmp")
      ;; wait for the link to actually be up
      (shell-command (format "ip netns exec %s ip route delete default via \"%s\" dev %s"
                             namespace
                             (car (split-string netns-range-default "/"))
                             veth-vpn)))
    ))

(defun ovpn-mode-netns-linux-delete (netns)
  "Delete a given network namespace based on property list NETNS."
  (let ((netns-buffer (plist-get netns :netns-buffer))
        (namespace (plist-get netns :netns))
        (veth-default (plist-get netns :veth-default))
        (veth-vpn (plist-get netns :veth-vpn))
        (base-id (plist-get netns :base-id)))

    ;; XXX: TODO error checking
    ;; XXX: TODO state restore for iptables mode through iptables-save/iptables-restore

    (with-current-buffer netns-buffer
      (cd "/sudo::/tmp")
      (shell-command (format "ip netns delete %s" namespace))
      (shell-command (format "rm -rf /etc/netns/%s" (shell-quote-argument namespace))) ; better safe than sorry, since this is running as root
      (shell-command (format "ip link delete %s" veth-default))
      (shell-command (format "ip link delete %s" veth-vpn))
      (unless (equal (shell-command-to-string "pgrep firewalld") "")
        ;; allow someone to reset firewalld completely if they want to ... not needed though
        (when (yes-or-no-p "Reset firewalld? (answer no if other ovpn-mode namespaces exist): ")
          (shell-command "systemctl restart firewalld"))))

    ;; release the base-id for re-use
    (push base-id ovpn-mode-netns-free-base)

    ))

;; ipv6 support

(defun ovpn-mode-ipv6-linux-status ()
  "message status of IPv6 support"
  (let ((status_all (shell-command-to-string "sysctl net.ipv6.conf.all.disable_ipv6"))
        (status_def (shell-command-to-string "sysctl net.ipv6.conf.default.disable_ipv6"))
        (status_loc (shell-command-to-string "sysctl net.ipv6.conf.lo.disable_ipv6")))
    (if (and (string-match "^.*= 1" status_all)
             (string-match "^.*= 1" status_def)
             (string-match "^.*= 1" status_loc))
        ;; disabled
        nil
      ;; enabled
      t)))

(defun ovpn-mode-ipv6-linux-toggle ()
  "toggle ipv6 state (enabled/disabled)"
  (interactive)
  (let* ((on-or-off '((t . 1) (nil . 0))) ; t means current val is 0 ...
         (sysctl-arg (cdr (assoc (ovpn-mode-ipv6-linux-status) on-or-off))))
    (when (y-or-n-p (format "ipv6 support is %s, toggle to %s?"
                            (nth sysctl-arg '("off" "on")) ; :P
                            (nth sysctl-arg '("on" "off"))))
      (ovpn-mode-ipv6-linux-sysctl-disable sysctl-arg))))

(defun ovpn-mode-ipv6-linux-sysctl-disable (on-or-off)
  "disable ipv6 support via sysctl to value of ON-OR-OFF"
  (unless (ovpn-mode-sudo
           "ipv6-linux-sysctl-disable"
           (get-buffer-create "*Messages*")
           "sysctl"
           "-w" (format "net.ipv6.conf.all.disable_ipv6=%d" on-or-off)
           "-w" (format "net.ipv6.conf.default.disable_ipv6=%d" on-or-off)
           "-w" (format "net.ipv6.conf.lo.disable_ipv6=%d" on-or-off))
    (message "Could not disable ipv6 support")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of Linux specifics

;;; this lets you juggle multiple dirs of confs and maintain state between them
(defun ovpn-mode-dir-set (dir)
  "set new base DIR for ovpn confs and redisplay"
  (interactive "fPath to .ovpn configurations: ")
  (setq ovpn-mode-base-directory dir)
  (setq ovpn-mode-configurations nil)
  (ovpn))

(defun ovpn-mode-pull-configurations (dir)
  "pull .ovpn configs from directory DIR"
  (setq ovpn-mode-configurations (directory-files dir t ".*\\.ovpn")))

(defun ovpn-mode-link-status (status &optional clear)
  "Update the ovpn-mode current link status with STATUS"
  (save-excursion
    (with-current-buffer ovpn-mode-buffer
      (setq buffer-read-only nil)
      (goto-char (point-max))
      ;; delete any existing status line
      (delete-region
       (progn (forward-visible-line 0) (point))
       (progn (forward-visible-line 1) (point)))
      (setq buffer-read-only t)
      (unless clear
        (ovpn-mode-insert-line (format "Link remote status: %s" status) t)))))

(defun ovpn-mode-insert-line (line &optional no-newline)
  "insert a LINE into the main ovpn-mode interface buffer"
  (with-current-buffer ovpn-mode-buffer
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (let ((fmt (if no-newline "%s" "%s\n")))
      (insert (format fmt line)))
    (let* ((ovpn-process (gethash line ovpn-mode-process-map)))
      (cond ((and ovpn-process (process-live-p (struct-ovpn-process-process ovpn-process)))
             ;; highlight blue if we're running inside of a namespace
             (if (struct-ovpn-process-netns ovpn-process)
                 (ovpn-mode-highlight-conf line 'hi-blue)
               (ovpn-mode-highlight-conf line 'hi-green)))
            ;; if it's in the list but the process is dead, it's waiting for purging by q
            (ovpn-process
             (ovpn-mode-highlight-conf line 'hi-red-b))
            ))
    (setq buffer-read-only t)))

(defvar ovpn-mode-process-map (make-hash-table :test 'equal))
(cl-defstruct struct-ovpn-process buffer buffer-name process conf pid link-remote netns)

;;; we use tramp's password prompt matcher
(require 'tramp)

(defun ovpn-process-filter (proc string)
  (when (process-live-p proc)
    (let ((prompts
           ;; deal with openvpn auth and 2fa challenge response prompts as well
           (format "\\(%s\\)\\|\\(^.*\\(Enter Auth Username\\|Enter Auth Password\\|Response\\).*: *\\)\\|\\(^.*Enter Google Authenticator Code*\\)"
                   tramp-password-prompt-regexp)))
      (save-match-data
        (if (string-match prompts string)
            ;; intercept any sudo prompts with our sudo auth wrapper
            (if (string-match ".*sudo.*password.*" string)
                (ovpn-mode-send-sudo-password proc string)
              ;; deal with any ovpn password prompts
              (process-send-string proc (concat (read-passwd
                                                 ;; strip any color control codes
                                                 (replace-regexp-in-string "\e\\[[0-9;]*m" "" string))
                                                "\n")))
          (progn
            ;; Thu Aug  6 16:11:03 2015 UDPv4 link remote: [AF_INET]111.111.111.111:1194
            (save-match-data
              (when (string-match "link remote: \\(.*\\)" string)
                (let* ((ovpn-process (gethash proc ovpn-mode-process-map))
                       (link-remote (match-string 1 string)))
                  (when ovpn-process
                    (setf (struct-ovpn-process-link-remote ovpn-process) link-remote)
                    ;; update status first time we see link remote, or on ovpn buffer redraws
                    (let ((conf (struct-ovpn-process-conf ovpn-process)))
                      (ovpn-mode-link-status
                       (format "%s (%s)" link-remote (file-name-nondirectory conf))))
                    ))))
            ;; drop default routes in namespaces when openvpn is up and running
            (when (string-match "Initialization Sequence Completed" string)
              (let* ((ovpn-process (gethash proc ovpn-mode-process-map))
                     (netns (if ovpn-process (struct-ovpn-process-netns ovpn-process) nil)))
                (when netns
                  (message "Dropping default route in namespace %s" (plist-get netns :netns))
                  (ovpn-mode-netns-linux-delete-default-route netns))))
            (princ (format "%s" string) (process-buffer proc))))))))

(defun ovpn-process-sentinel (proc string)
  (let* ((ovpn-process (gethash proc ovpn-mode-process-map))
         (conf nil))
    (cond ((and ovpn-process
                (memq (process-status proc) '(exit signal)))
           (setq conf  (struct-ovpn-process-conf ovpn-process))
           (ovpn-mode-unhighlight-conf conf)
           (ovpn-mode-highlight-conf conf 'hi-red-b)
           (message (format "Manually q conf \"%s\" to reset state (%s)"
                            (file-name-nondirectory conf)
                            (replace-regexp-in-string "\n$" "" string))))
          (t
           (princ (format "ovpn-process-sentinel: %s"
                          (replace-regexp-in-string "\n$" "" string)) (process-buffer proc))))))

(defun ovpn-mode-purge-process-map ()
  (setq ovpn-mode-process-map (make-hash-table :test 'equal)))

(defun ovpn-mode-highlight-conf (conf face)
  (with-current-buffer ovpn-mode-buffer
    (setq buffer-read-only nil)
    (highlight-regexp conf face)
    (setq buffer-read-only t)))

(defun ovpn-mode-unhighlight-conf (conf)
  (with-current-buffer ovpn-mode-buffer
    (setq buffer-read-only nil)
    (unhighlight-regexp conf)
    (setq buffer-read-only t)))

(defun ovpn-mode-start-vpn-with-namespace ()
  "Sarts openvpn conf at point with namespace."
  (interactive)
  (ovpn-mode-start-vpn t))

(defun ovpn-mode-start-vpn (&optional with-namespace)
  "Starts openvpn conf at point.
This assumes any associated certificates live in the same directory as the conf."
  (interactive)
  ;; disable ipv6 (if so desired, and supported on the current platform)
  (when (funcall (struct-ovpn-mode-platform-specific-ipv6-status
                  ovpn-mode-platform-specific))
    (funcall (struct-ovpn-mode-platform-specific-ipv6-toggle
              ovpn-mode-platform-specific)))
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line))))
    (when (string-match ".*\\.ovpn" conf)
      (if (not (gethash conf ovpn-mode-process-map))
          (progn
            (let* ((process nil)
                   (default-directory (file-name-directory conf))
                   (buffer-name (file-name-nondirectory conf))
                   (buffer (generate-new-buffer buffer-name))
                   ;; init a namespace if needed (and supported on platform)
                   (netns (if with-namespace
                              (funcall (struct-ovpn-mode-platform-specific-netns-create
                                        ovpn-mode-platform-specific))
                            nil)))
              (message (format "%s" conf))
              (setq process
                    (apply 'start-process
                           buffer-name
                           buffer
                           (if with-namespace
                               ;; set up an openvpn instance for conf inside a given namespace
                               (if netns
                                   (progn
                                     (message "Starting %s with namespace %s"
                                              (file-name-nondirectory conf)
                                              (plist-get netns :netns))
                                     (list "sudo" "ip" "netns" "exec"
                                           (format "%s" (plist-get netns :netns))
                                           "openvpn"
                                           "--cd" (file-name-directory conf)
                                           "--config" conf
                                           "--dev" (plist-get netns :netns-tunvpn)))
                                 (error "Failed to initialize namespace, aborting!"))
                             ;; just start normally for the main system route
                             (list "sudo" "openvpn"
                                   "--cd" (file-name-directory conf)
                                   "--config" conf))))
              (if (process-live-p process)
                  (progn
                    (set-process-filter process 'ovpn-process-filter)
                    (set-process-sentinel process 'ovpn-process-sentinel)
                    (setq ovpn-process (make-struct-ovpn-process
                                        :buffer buffer
                                        :buffer-name buffer-name
                                        :process process
                                        :conf conf
                                        :netns netns))
                    ;; so we can look up by both conf name as well as process
                    (puthash conf ovpn-process ovpn-mode-process-map)
                    (puthash process ovpn-process ovpn-mode-process-map)
                    ;; highlight the active conf ... blue implies inside a namespace
                    (if netns
                        (ovpn-mode-highlight-conf conf 'hi-blue)
                      (ovpn-mode-highlight-conf conf 'hi-green)))
                (message (format "Could not start openvpn for %s"
                                 (file-name-nondirectory conf))))))
        (message (format "Already started %s (q to purge)" (file-name-nondirectory conf)))))))

;;; as root through kill since we don't know about priv-drops and can't use signal-process
(defun ovpn-mode-signal-process (sig ovpn-process)
  "sends SIG to OVPN-PROCESS->process (sudo) child directly"
  (when ovpn-process
    (progn
      (let* ((process (struct-ovpn-process-process ovpn-process))
             (buffer (struct-ovpn-process-buffer ovpn-process))
             (conf (struct-ovpn-process-conf ovpn-process))
             (buffer-name (struct-ovpn-process-buffer-name ovpn-process)))
        ;; sudo doesn't relay all signals (e.g. SIGKILL), so to ensure
        ;; that ANY signal we send ends up at the right process, we send
        ;; it directly to the sudo child (which is our actual openvpn proc)
        (if (process-live-p process)
            (let* ((cpid (string-to-number (shell-command-to-string
                                            (format "pgrep -P %d"
                                                    (process-id process))))))
              (progn
                (setq process (start-process
                               (format "ovpn-mode-stop: %s (pid: %d)"
                                       buffer-name (process-id process))
                               buffer
                               "sudo" "kill"
                               (format "-%d" sig)
                               (format "%d" cpid)))
                (set-process-filter process 'ovpn-process-filter)
                (set-process-sentinel process 'ovpn-process-sentinel)))
          (message "Target openvpn process no longer alive"))))))

;; so you can control the window switch on stop
(defvar ovpn-mode-switch-to-buffer-on-stop nil)

(defun ovpn-mode-stop-vpn ()
  "stops openvpn conf through SIGTERM"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map))
         (netns (if ovpn-process (struct-ovpn-process-netns ovpn-process) nil)))
    (when ovpn-process
      (ovpn-mode-signal-process 15 ovpn-process)
      (ovpn-mode-unhighlight-conf conf)
      ;; pull the hash table entry for this instance
      (remhash conf ovpn-mode-process-map)
      (remhash (struct-ovpn-process-process ovpn-process) ovpn-mode-process-map)
      ;; clear the status line from the mode buffer
      (ovpn-mode-link-status nil t)
      ;; clear out any associated namespace if needed
      (when netns
        (ovpn-mode-netns-linux-delete netns))
      ;; swap to the associated buffer for convenient killing if desired
      (when ovpn-mode-switch-to-buffer-on-stop
        (message (format "Swapping to associated output buffer for %s (kill if you want)"
                         (file-name-nondirectory conf)))
        (switch-to-buffer (struct-ovpn-process-buffer ovpn-process))))))

(defun ovpn-mode-restart-vpn ()
  "restarts openvpn conf through SIGHUP"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    ;; relay SIGHUP through sudo
    (when ovpn-process
      (ovpn-mode-signal-process 1 ovpn-process))))

(defun ovpn-mode-info-vpn ()
  "dumps info stats on selected ovpn conf"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    (when ovpn-process
      (let ((link-remote (struct-ovpn-process-link-remote ovpn-process))
            (netns (struct-ovpn-process-netns ovpn-process))
            (conf (file-name-nondirectory conf)))
        (cond ((and link-remote netns)
               (message "%s: %s (namespace: %s)"
                        conf link-remote (plist-get netns :netns)))
              (link-remote
               (message "%s: %s (no namespace)" conf link-remote))
              (t
               (message "%s: no info available" conf)))))))

(defun ovpn-mode-buffer-vpn ()
  "switches to the associated ovpn conf output buffer"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    (when ovpn-process
      (switch-to-buffer (struct-ovpn-process-buffer ovpn-process)))))

(defun ovpn-mode-async-shell-command-in-namespace (cmd user)
  "Executes CMD as USER in the conf associated namespace."
  (interactive "sCmd: \nsUser (default current user): \n")
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map))
         (netns (if ovpn-process (struct-ovpn-process-netns ovpn-process) nil))
         (user (if (equal user "") (user-real-login-name) user)))
    (if netns
        (progn
          (message "Attempting to execute \"%s\" in namespace %s as user %s"
                   cmd
                   (plist-get netns :netns)
                   user)
          ;; you can do stuff like xterm -e "sudo -u targetuser bash" here
          ;; to deal with e.g. Xserver annoyances (as user root obviously)
          (ovpn-mode-sudo "ovpn-mode-sudo-exec"
                          (plist-get netns :netns-buffer)
                          "sh" "-c"
                          (format "ip netns exec %s sudo -u %s %s"
                                  (plist-get netns :netns)
                                  user
                                  cmd)))
      (message "No associated namespace for this conf"))))

(defun ovpn-mode-edit-vpn ()
  "opens the selected ovpn conf for editing"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line))))
    (when (string-match ".*\\.ovpn" conf)
      (find-file conf))))

(defun ovpn ()
  "main entry point for ovpn-mode interface"
  (interactive)
  (cond ((not ovpn-mode-buffer)
         (setq ovpn-mode-buffer (get-buffer-create ovpn-mode-buffer-name))
         (switch-to-buffer ovpn-mode-buffer)
         (ovpn-mode))
        (t
         (switch-to-buffer ovpn-mode-buffer)))
  ;; populate with confs if needed
  (unless ovpn-mode-configurations
    ;; we clear ovpn-mode-configurations on ovpn-mode-dir-set thus triggering
    ;; a redisplay ... ovpn-mode-insert-line will check for any active processes
    ;; associated with a displayed config and highlight accordingly
    (with-current-buffer ovpn-mode-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t))
    (ovpn-mode-insert-line
     "s start, n start with namespace, r restart, q stop/purge\n\nAvailable openvpn configurations:\n")
    (cond ((file-exists-p ovpn-mode-base-directory)
           (ovpn-mode-pull-configurations ovpn-mode-base-directory)
           (mapc #'(lambda (config) (ovpn-mode-insert-line config)) ovpn-mode-configurations))
          (t
           (ovpn-mode-insert-line "Please set a valid base directory (d)")))
    (ovpn-mode-insert-line "\n") ; space for link status
    ;; put any active link status
    (dolist (conf ovpn-mode-configurations)
      (let* ((ovpn-process (gethash conf ovpn-mode-process-map))
             (link-remote (if ovpn-process
                              (struct-ovpn-process-link-remote ovpn-process)
                            nil)))
        (when link-remote
          (ovpn-mode-link-status (format "%s (%s)" link-remote (file-name-nondirectory conf))))))
    (goto-char (point-min))))

;; reset default port and process var on buffer kill
(add-hook 'kill-buffer-hook '(lambda ()
                               (when (string= (buffer-name) ovpn-mode-buffer-name)
                                 (setq ovpn-mode-configurations nil
                                       ovpn-mode-buffer nil))))

(provide 'ovpn-mode)
;;; ovpn-mode.el ends here
