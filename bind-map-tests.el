;;; bind-map-tests.el

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>

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

(require 'bind-map)
(require 'evil)

(ert-deftest bind-map-test-global-keys ()
  "Test binding in global maps."
  (let (minor-mode-alist minor-mode-map-alist minor-mode-list)
    (bind-map bind-map-test-map-global
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key bind-map-test-map-global "a" "b")
    (should (keymapp (lookup-key global-map "\C-a")))
    (should (string= (lookup-key global-map "\C-aa") "b"))
    (should (keymapp (lookup-key evil-motion-state-map "a")))
    (should (string= (lookup-key evil-motion-state-map "aa") "b"))
    (should (not (string= (lookup-key evil-visual-state-map "aa") "b")))
    (with-temp-buffer
      (evil-local-mode 1)
      (evil-motion-state)
      (should (string= (key-binding "\C-aa") "b")))
    (setq bind-map-test-map-global (make-sparse-keymap)
          bind-map-test-map-global-prefix (make-sparse-keymap))
    (global-set-key "\C-a" nil)))

(ert-deftest bind-map-test-major-mode-keys ()
  "Test binding for major-modes."
  (let (bind-map-major-modes-alist minor-mode-map-alist)
    (bind-map bind-map-test-map-major-mode
      :major-modes (emacs-lisp-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key bind-map-test-map-major-mode "a" "c")
    (with-temp-buffer
      (emacs-lisp-mode)
      (evil-normalize-keymaps)
      (should (equal (cdr (assoc
                           'bind-map-test-map-major-mode-active
                           bind-map-major-modes-alist))
                     '(emacs-lisp-mode)))
      (should bind-map-test-map-major-mode-active)
      (should (string= (key-binding "\C-aa") "c"))
      (should (keymapp (lookup-key
                        (evil-get-auxiliary-keymap
                         bind-map-test-map-major-mode-root-map 'motion) "a")))
      (should (string= (lookup-key
                        (evil-get-auxiliary-keymap
                         bind-map-test-map-major-mode-root-map 'motion) "aa") "c"))
      (should (string= "c" (key-binding "\C-aa"))))
    (setq bind-map-test-map-major-mode (make-sparse-keymap)
          bind-map-test-map-major-mode-prefix (make-sparse-keymap))))

(ert-deftest bind-map-test-minor-mode-keys ()
  "Test binding for minor-modes."
  (let (minor-mode-alist minor-mode-map-alist minor-mode-list)
    (bind-map bind-map-test-map-minor-mode
      :minor-modes (bind-map-test-minor-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (defvar bind-map-test-minor-mode)
    (define-key bind-map-test-map-minor-mode "a" "d")
    (let ((bind-map-test-minor-mode t))
      (evil-normalize-keymaps)
      (should (string= (key-binding "\C-aa") "d")))
    (setq bind-map-test-map-minor-mode (make-sparse-keymap)
          bind-map-test-map-major-mode-prefix (make-sparse-keymap))))

(ert-deftest bind-map-test-multiple-declarations ()
  (let (bind-map-major-modes-alist minor-mode-map-alist)
    (bind-map bind-map-test-map-mult-decl
      :major-modes (mm1 mm2))
    (bind-map bind-map-test-map-mult-decl
      :major-modes (mm3 mm4 mm5))
    (bind-map bind-map-test-map-mult-decl
      :major-modes (mm6))
    (should (equal (cdr (assq 'bind-map-test-map-mult-decl-active
                              bind-map-major-modes-alist))
                   '(mm1 mm2 mm3 mm4 mm5 mm6)))))

(ert-deftest bind-map-test-minor-inheritance ()
  (let (minor-mode-list minor-mode-map-alist minor-mode-alist)
    (bind-map bind-map-test-map-minor-parent
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    ;; FIXME: 
    (eval
     '(bind-map-for-mode-inherit bind-map-test-map-minor-child
         bind-map-test-map-minor-parent
        :minor-modes (bind-map-test-minor-mode-inheritance)))
    (defvar bind-map-test-minor-mode-inheritance)
    (define-key bind-map-test-map-minor-child "a" "e")
    (let ((bind-map-test-minor-mode-inheritance t))
      (should (string= (key-binding "\C-aa") "e"))
      (evil-local-mode 1)
      (evil-motion-state)
      (should (string= (key-binding "aa") "e")))
    (setq bind-map-test-map-minor-child (make-sparse-keymap)
          bind-map-test-map-minor-child-prefix (make-sparse-keymap))
    (global-set-key "\C-a" nil)))

(ert-deftest bind-map-test-major-inheritance ()
  (let (minor-mode-map-alist)
    (bind-map bind-map-test-map-major-parent
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    ;; FIXME: 
    (eval 
     '(bind-map-for-mode-inherit bind-map-test-map-major-child
          bind-map-test-map-major-parent
       :major-modes (emacs-lisp-mode)))
    (define-key bind-map-test-map-major-child "a" "f")
    (evil-normalize-keymaps)
    (emacs-lisp-mode)
    (should (string= (key-binding "\C-aa") "f"))
    (evil-local-mode 1)
    (evil-motion-state)
    (should (string= (key-binding "aa") "f"))
    (setq bind-map-test-map-major-child (make-sparse-keymap)
          bind-map-test-map-major-child-prefix (make-sparse-keymap))
    (global-set-key "\C-a" nil)))
