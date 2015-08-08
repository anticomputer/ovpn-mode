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

;; this is where all your openvpn confs live, if not using absolute certificate paths
;; in your .ovpn's then we assume the certificates are in the same directory as the conf
(defvar ovpn-mode-base-directory "~/VPN/PIA")

(defvar ovpn-mode-hook nil
  "Hook being run after `ovpn-mode' has completely set up the buffer.")

;; major mode for future buffer and keymap enhancements
(defvar ovpn-mode-keywords '("ovpn"))
(defvar ovpn-mode-keywords-regexp (regexp-opt ovpn-mode-keywords 'words))
(defvar ovpn-mode-font-lock-keywords `((,ovpn-mode-keywords-regexp . font-lock-keyword-face)))

(defvar ovpn-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from fundamental and do mode-global keybindings here
    (define-key map "s" 'ovpn-mode-start-vpn)
    (define-key map "r" 'ovpn-mode-restart-vpn)
    (define-key map "q" 'ovpn-mode-stop-vpn)
    (define-key map "i" 'ovpn-mode-info-vpn)
    (define-key map "b" 'ovpn-mode-buffer-vpn)
    (define-key map "e" 'ovpn-mode-edit-vpn)
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

;; this lets you juggle multiple dirs of confs and maintain state between them
(defun ovpn-mode-dir-set (dir)
  "set new base DIR for ovpn confs and redisplay"
  (interactive "sPath to .ovpn configurations: ")
  (setq ovpn-mode-base-directory dir)
  (setq ovpn-mode-configurations nil)
  (ovpn))

(defun ovpn-mode-pull-configurations (dir)
  "pull .ovpn configs from directory DIR"
  (setq ovpn-mode-configurations (directory-files dir t ".*\\.ovpn")))

(defun ovpn-mode-insert-line (line)
  "insert a LINE into the main ovpn-mode interface buffer"
  (with-current-buffer ovpn-mode-buffer
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert (format "%s\n" line))
    (let* ((ovpn-process (gethash line ovpn-mode-process-map)))
      (cond ((and ovpn-process (process-live-p (struct-ovpn-process-process ovpn-process)))
             (ovpn-mode-highlight-conf line 'hi-green))
            ;; if it's in the list but the process is dead, it's waiting for purging by q
            (ovpn-process
             (ovpn-mode-highlight-conf line 'hi-red-b))))
    (setq buffer-read-only t)))

(defvar ovpn-mode-process-map (make-hash-table :test 'equal))
(cl-defstruct struct-ovpn-process buffer buffer-name process conf pid link-remote)

;; we use tramp's password prompt matcher
(require 'tramp)

(defun ovpn-process-filter (proc string)
  (when (process-live-p proc)
    (let ((prompts
           ;; deal with openvpn auth prompts as well
           (format "\\(%s\\)\\|\\(^.*\\(Enter Auth Username\\|Enter Auth Password\\).*: *\\)"
                   tramp-password-prompt-regexp)))
      (save-match-data
        (if (string-match prompts string)
            (process-send-string proc (concat (read-passwd
                                               ;; strip any color control codes
                                               (replace-regexp-in-string
                                                "\e\\[[0-9;]*m" "" string)
                                               ) "\n"))
          (progn
            ;; Thu Aug  6 16:11:03 2015 UDPv4 link remote: [AF_INET]111.111.111.111:1194
            (save-match-data
              (when (string-match "link remote: \\(.*\\)" string)
                (let* ((ovpn-process (gethash proc ovpn-mode-process-map)))
                  (when ovpn-process
                    (setf (struct-ovpn-process-link-remote ovpn-process)
                          (match-string 1 string))
                    (message (format "link remote: %s"
                                     (struct-ovpn-process-link-remote ovpn-process)))))))
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
           (message (format "ovpn-process-sentinel: %s"
                            (replace-regexp-in-string "\n$" "" string)))))))

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

(defun ovpn-mode-start-vpn ()
  "starts openvpn conf, assumes any associated certificates live in the same dir as the .ovpn"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line))))
    (when (string-match ".*\\.ovpn" conf)
        (if (not (gethash conf ovpn-mode-process-map))
            (progn
              (let* ((process nil)
                     (default-directory (file-name-directory conf))
                     (buffer-name (file-name-nondirectory conf))
                     (buffer (generate-new-buffer buffer-name)))
                (message (format "%s" conf))
                (setq process (start-process
                               buffer-name
                               buffer
                               "sudo" "openvpn" conf))
                (if (process-live-p process)
                    (progn
                      (set-process-filter process 'ovpn-process-filter)
                      (set-process-sentinel process 'ovpn-process-sentinel)
                      (setq ovpn-process (make-struct-ovpn-process
                                          :buffer buffer
                                          :buffer-name buffer-name
                                          :process process
                                          :conf conf))
                      ;; so we can look up by both conf name as well as process
                      (puthash conf ovpn-process ovpn-mode-process-map)
                      (puthash process ovpn-process ovpn-mode-process-map)
                      ;; highlight the active conf
                      (ovpn-mode-highlight-conf conf 'hi-green))
                  (message (format "Could not start openvpn for %s"
                                   (file-name-nondirectory conf))))))
          (message (format "Already started %s" (file-name-nondirectory conf)))))))

;; as root through kill since we don't know about priv-drops and can't use signal-process
(defun ovpn-mode-signal-process (sig ovpn-process)
  "sends SIG to OVPN-PROCESS->process"
  (if ovpn-process
      (progn
        (let* ((process (struct-ovpn-process-process ovpn-process))
               (buffer (struct-ovpn-process-buffer ovpn-process))
               (conf (struct-ovpn-process-conf ovpn-process))
               (buffer-name (struct-ovpn-process-buffer-name ovpn-process)))
          (if (process-live-p process)
              (progn
                (setq process (start-process
                               (format "ovpn-mode-stop: %s (pid: %d)"
                                       buffer-name (process-id process))
                               buffer
                               "sudo" "kill" (format "-%d" sig) (format "%d" (process-id process))))
                (set-process-filter process 'ovpn-process-filter)
                (set-process-sentinel process 'ovpn-process-sentinel))
            (message "Target openvpn process no longer alive"))))
    (message (format "No active process found for this conf"))))

(defun ovpn-mode-stop-vpn ()
  "stops openvpn conf through SIGTERM"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    (when ovpn-process
      ;; sudo does not relay SIGKILL so use SIGTERM
      (ovpn-mode-signal-process 15 ovpn-process)
      (ovpn-mode-unhighlight-conf conf)
      ;; pull the hash table entry for this instance
      (remhash conf ovpn-mode-process-map)
      (remhash (struct-ovpn-process-process ovpn-process) ovpn-mode-process-map)
      ;; swap to the associated buffer for convenient killing if desired
      (message (format "Swapping to associated output buffer for %s (kill if you want)"
                       (file-name-nondirectory conf)))
      (switch-to-buffer (struct-ovpn-process-buffer ovpn-process)))))

(defun ovpn-mode-restart-vpn ()
  "restarts openvpn conf through SIGHUP"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    ;; relay SIGHUP through sudo
    (ovpn-mode-signal-process 1 ovpn-process)))

(defun ovpn-mode-info-vpn ()
  "dumps info stats on selected ovpn conf"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    (when ovpn-process
      (if (struct-ovpn-process-link-remote ovpn-process)
        (message (format "%s" (struct-ovpn-process-link-remote ovpn-process)))
      (message (format "No info for: %s" (file-name-nondirectory conf)))))))

(defun ovpn-mode-buffer-vpn ()
  "switches to the associated ovpn conf output buffer"
  (interactive)
  (let* ((conf (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (ovpn-process (gethash conf ovpn-mode-process-map)))
    (when ovpn-process
      (switch-to-buffer (struct-ovpn-process-buffer ovpn-process)))))

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
    (ovpn-mode-insert-line "Available openvpn configurations (s start, r restart, q stop):\n")
    (ovpn-mode-pull-configurations ovpn-mode-base-directory)
    (mapc #'(lambda (config) (ovpn-mode-insert-line config)) ovpn-mode-configurations)
    ;;(hl-line-mode t)
    (goto-char (point-min))))

;; reset default port and process var on buffer kill
(add-hook 'kill-buffer-hook '(lambda ()
                               (when (string= (buffer-name) ovpn-mode-buffer-name)
                                 (setq ovpn-mode-configurations nil
                                       ovpn-mode-buffer nil))))

(provide 'ovpn-mode)
;;; ovpn-mode.el ends here
