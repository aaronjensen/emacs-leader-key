;;; leader-key.el --- Leader Key Binding -*- lexical-binding:t -*-

;; Initially Copyright (C) 2012-2020 Sylvain Benner & Contributors
;; Modifications Copyright (C) 2020 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; Keywords: evil
;; Version: 0.0.1
;; Package-Requires: (bind-map)

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

;; Leader key binding helpers

;;; Code:
(require 'bind-map)

(defvar leader-key-default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defgroup leader-key nil
  "Leader key bindings"
  :group 'editing)

(defcustom leader-key-evil-leader-key "SPC"
  "When non-nil, use this key as the leader key for evil normal mode."
  :group 'leader-key
  :type 'string)

(defcustom leader-key-major-mode-evil-leader-key ","
  "When non-nil, use this key as the major mode leader key for evil normal mode."
  :group 'leader-key
  :type 'string)

(defcustom leader-key-emacs-leader-key "M-m"
  "When non-nil, use this key as the leader key for emacs."
  :group 'leader-key
  :type 'string)

(defcustom leader-key-major-mode-emacs-leader-key "C-M-m"
  "When non-nil, use this key as the major mode leader key for emacs."
  :group 'leader-key
  :type 'string)

(defun leader-key-acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

;;;###autoload
(defun leader-key-set (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`leader-key-evil-leader-key' and `leader-key-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(leader-key-set
   \"a\" \\='command1
   \"C-c\" \\='command2
   \"bb\" \\='command3\)"
  (while key
    (define-key leader-key-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defun leader-key--init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `leader-key-major-mode-evil-leader-key'
and `leader-key-major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (leader-key-acceptable-leader-p
                         leader-key-major-mode-evil-leader-key)
                    leader-key-major-mode-evil-leader-key))
         (leader2 (when (leader-key-acceptable-leader-p
                         leader-key-evil-leader-key)
                    (concat leader-key-evil-leader-key " m")))
         (emacs-leader1 (when (leader-key-acceptable-leader-p
                               leader-key-major-mode-emacs-leader-key)
                          leader-key-major-mode-emacs-leader-key))
         (emacs-leader2 (when (leader-key-acceptable-leader-p
                               leader-key-emacs-leader-key)
                          (concat leader-key-emacs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2)))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,emacs-leaders
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))

;;;###autoload
(defun leader-key-declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `leader-key-prefix-titles'."
  (with-eval-after-load 'which-key
    (let* ((command name)
           (full-prefix (concat leader-key-evil-leader-key " " prefix))
           (full-prefix-emacs (concat leader-key-emacs-leader-key " " prefix))
           (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
           (full-prefix-emacs-lst (listify-key-sequence
                                   (kbd full-prefix-emacs))))
      ;; define the prefix command only if it does not already exist
      (unless long-name (setq long-name name))
      (which-key-add-key-based-replacements
        full-prefix-emacs (cons name long-name)
        full-prefix (cons name long-name)))))
(put 'leader-key-declare-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defun leader-key-declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (with-eval-after-load 'which-key
    (let ((command (intern (concat (symbol-name mode) name)))
          (full-prefix (concat leader-key-evil-leader-key " " prefix))
          (full-prefix-emacs (concat leader-key-emacs-leader-key " " prefix))
          (is-major-mode-prefix (string-prefix-p "m" prefix))
          (major-mode-prefix (concat leader-key-major-mode-evil-leader-key
                                     " " (substring prefix 1)))
          (major-mode-prefix-emacs
           (concat leader-key-major-mode-emacs-leader-key
                   " " (substring prefix 1))))
      (unless long-name (setq long-name name))
      (let ((prefix-name (cons name long-name)))
        (which-key-add-major-mode-key-based-replacements mode
          full-prefix-emacs prefix-name
          full-prefix prefix-name)
        (when (and is-major-mode-prefix leader-key-major-mode-evil-leader-key)
          (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
        (when (and is-major-mode-prefix leader-key-major-mode-emacs-leader-key)
          (which-key-add-major-mode-key-based-replacements
            mode major-mode-prefix-emacs prefix-name))))))
(put 'leader-key-declare-prefix-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun leader-key-set-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`leader-key-major-mode-evil-leader-key' and
`leader-key-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `leader-key-set-leader-keys'."
  (let* ((map (intern (format "leader-key-%s-map" mode))))
    (when (leader-key--init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

;;;###autoload
(defun leader-key-set-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`leader-key-major-mode-evil-leader-key' and
`leader-key-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `leader-key-set-leader-keys'."
  (let* ((map (intern (format "leader-key-%s-map" mode))))
    (when (leader-key--init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

;;;###autoload
(defun leader-key-init ()
  (bind-map leader-key-default-map
    :prefix-cmd leader-key-cmds
    :keys (leader-key-emacs-leader-key)
    :evil-keys (leader-key-evil-leader-key)
    :override-minor-modes t
    :override-mode-name leader-key-leader-override-mode))

(provide 'leader-key)
;;; leader-key.el ends here
