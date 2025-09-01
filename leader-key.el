;;; leader-key.el --- Leader Key Binding -*- lexical-binding:t -*-

;; Initially Copyright (C) 2012-2020 Sylvain Benner & Contributors
;; Modifications Copyright (C) 2020 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; Keywords: evil
;; Version: 0.1.0
;; Package-Requires: ()

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

(defvar leader-key--major-modes-alist nil
  "Each elem is (ACTIVE-VAR MAJOR-MODES...).
Used to toggle per-major-mode leader maps when the mode is active.")

(defvar leader-key--evil-local-bindings nil
  "Elements are (OVERRIDE-MODE STATE KEY DEF).
Installed into evil local state maps from `evil-local-mode-hook' to ensure
leader keys override other minor modes in Evil states.")

(defun leader-key--kbd-keys (keys)
  "Apply `kbd' to KEYS filtering out nil/empty strings."
  (let (res)
    (dolist (key keys (nreverse res))
      (when (and (stringp key) (not (string= key "")))
        (push (kbd key) res)))))

(defun leader-key--add-to-major-mode-list (activate-var major-mode-list)
  "Register ACTIVATE-VAR for MAJOR-MODE-LIST in `leader-key--major-modes-alist'."
  (let ((current (assq activate-var leader-key--major-modes-alist)))
    (if current
        (setcdr current (append (cdr current) major-mode-list))
      (push (cons activate-var major-mode-list) leader-key--major-modes-alist))))

;;;###autoload
(defun leader-key-add-to-major-mode-list (activate-var major-mode-list)
  "Public API to associate ACTIVATE-VAR with MAJOR-MODE-LIST.
This makes a per-major-mode leader map (identified by ACTIVATE-VAR)
active for all modes in MAJOR-MODE-LIST.

Example:
  (leader-key-add-to-major-mode-list 'leader-key-org-mode-map-active '(org-journal-mode))
activates Org's leader map in `org-journal-mode' buffers as well."
  (leader-key--add-to-major-mode-list activate-var major-mode-list))


(defun leader-key--change-major-mode-after-body-hook ()
  "Activate per-major-mode leader maps for the current `major-mode'."
  (dolist (entry leader-key--major-modes-alist)
    (let ((active-var (car entry))
          (modes (cdr entry)))
      (when (boundp active-var)
        (setf (symbol-value active-var) (memq major-mode modes))))))
(add-hook 'change-major-mode-after-body-hook #'leader-key--change-major-mode-after-body-hook)

(defun leader-key--evil-local-mode-hook ()
  "Install local Evil state bindings for leader overrides."
  (dolist (entry leader-key--evil-local-bindings)
    (let* ((override-mode (nth 0 entry))
           (state (nth 1 entry))
           (key (nth 2 entry))
           (def (nth 3 entry))
           (map (intern (format "evil-%s-state-local-map" state)))
           (global-mode (intern (format "global-%s" override-mode)))
           ;; Emacs 31 renamed this variable to --set-explicitly
           (set-explicitly (let ((v1 (intern (format "%s--set-explicitly" override-mode)))
                                 (v2 (intern (format "%s-set-explicitly" override-mode))))
                             (cond
                              ((boundp v1) v1)
                              ((boundp v2) v2)
                              (t nil)))))
      (when (and (boundp global-mode) (boundp override-mode)
                 (boundp map) (keymapp (symbol-value map))
                 (symbol-value global-mode)
                 (not (and set-explicitly (symbol-value set-explicitly)
                           (null (symbol-value override-mode)))))
        (define-key (symbol-value map) key def)))))
(with-eval-after-load 'evil
  (add-hook 'evil-local-mode-hook #'leader-key--evil-local-mode-hook))

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

(defun leader-key--ensure-prefix (map)
  "Ensure a prefix command exists for MAP and return its symbol."
  (let ((prefix (intern (format "%s-prefix" map))))
    (unless (boundp prefix)
      (set prefix nil))
    (unless (and (boundp map) (keymapp (symbol-value map)))
      (set map (make-sparse-keymap)))
    (set prefix (symbol-value map))
    (setf (symbol-function prefix) (symbol-value map))
    prefix))

(defun leader-key--init-leader-mode-map (mode map &optional minor)
  "Create a per-MODE leader MAP and bind leader prefixes.
If MINOR is non-nil, MODE is treated as a minor mode, otherwise a major mode."
  (let* ((prefix (leader-key--ensure-prefix map))
         (root-map (intern (format "%s-root-map" map)))
         (leader1 (when (leader-key-acceptable-leader-p leader-key-major-mode-evil-leader-key)
                    leader-key-major-mode-evil-leader-key))
         (leader2 (when (leader-key-acceptable-leader-p leader-key-evil-leader-key)
                    (concat leader-key-evil-leader-key " m")))
         (emacs-leader1 (when (leader-key-acceptable-leader-p leader-key-major-mode-emacs-leader-key)
                          leader-key-major-mode-emacs-leader-key))
         (emacs-leader2 (when (leader-key-acceptable-leader-p leader-key-emacs-leader-key)
                          (concat leader-key-emacs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2)))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2)))
         (states '(normal motion visual evilified)))
    (unless (and (boundp root-map) (keymapp (symbol-value root-map)))
      (set root-map (make-sparse-keymap)))
    (if minor
        ;; Minor mode root map is active when the minor mode is active
        (add-to-list 'minor-mode-map-alist (cons mode (symbol-value root-map)))
      ;; Major mode root map is activated via a buffer-local toggle
      (let ((active-var (intern (format "%s-active" map))))
        (eval `(defvar-local ,active-var nil))
        (add-to-list 'minor-mode-map-alist (cons active-var (symbol-value root-map)))
        (leader-key--add-to-major-mode-list active-var (list mode))
        ;; Trigger once for current buffer
        (leader-key--change-major-mode-after-body-hook)))

    ;; Bind Emacs leaders into root-map (scoped by activation above)
    (dolist (key (leader-key--kbd-keys emacs-leaders))
      (define-key (symbol-value root-map) key prefix))

    ;; Bind Evil leaders for selected states, scoped to root-map
    (with-eval-after-load 'evil
      (dolist (key (leader-key--kbd-keys leaders))
        (dolist (state states)
          (define-key (evil-get-auxiliary-keymap (symbol-value root-map) state t)
            key prefix))))

    (boundp prefix)))

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
(defun turn-on-leader-key-leader-override-mode ()
  "Enable `leader-key-leader-override-mode' outside minibuffer."
  (unless (minibufferp) (leader-key-leader-override-mode 1)))

(define-minor-mode leader-key-leader-override-mode
  "Leader-key overriding minor mode."
  :global nil)

(define-globalized-minor-mode global-leader-key-leader-override-mode
  leader-key-leader-override-mode
  turn-on-leader-key-leader-override-mode)

(defun leader-key-init ()
  "Initialize the global leader maps."
  ;; Ensure prefix command for default leader map
  (let* ((prefix (leader-key--ensure-prefix 'leader-key-default-map))
         (root-map 'leader-key-default-root-map)
         (states '(normal motion visual)))
    (unless (and (boundp root-map) (keymapp (symbol-value root-map)))
      (set root-map (make-sparse-keymap)))

    ;; Enable overriding minor mode globally
    (global-leader-key-leader-override-mode 1)

    ;; Ensure our root map participates in emulation precedence
    (add-to-list 'emulation-mode-map-alists (list (cons 'leader-key-leader-override-mode (symbol-value root-map))))

    ;; Emacs leaders: bind in both root-map (override) and global map
    (dolist (key (leader-key--kbd-keys (list leader-key-emacs-leader-key)))
      (define-key (symbol-value root-map) key prefix)
      (global-set-key key prefix))

    ;; Evil leader key handling
    (with-eval-after-load 'evil
      (dolist (key (leader-key--kbd-keys (list leader-key-evil-leader-key)))
        (dolist (state states)
          ;; Install as local override in Evil states to ensure precedence
          (push (list 'leader-key-leader-override-mode state key prefix) leader-key--evil-local-bindings)
          (evil-global-set-key state key prefix)))
      (evil-normalize-keymaps))))

(provide 'leader-key)
;;; leader-key.el ends here
