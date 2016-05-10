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
  (let ((tmpmap1 (make-sparse-keymap)))
    (bind-map tmpmap1
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key tmpmap1 "a" "b")
    (should (keymapp (lookup-key global-map "\C-a")))
    (should (string= (lookup-key global-map "\C-aa") "b"))
    (should (keymapp (lookup-key evil-motion-state-map "a")))
    (should (string= (lookup-key evil-motion-state-map "aa") "b"))
    (should (not (string= (lookup-key evil-visual-state-map "aa") "b")))))

(ert-deftest bind-map-test-major-mode-keys ()
  "Test binding for major-modes."
  (let ((tmpmap2 (make-sparse-keymap))
        (tmpmap2-root-map (make-sparse-keymap))
        tmpmap2-active
        tmpmap2-prefix
        bind-map-major-modes-alist
        minor-mode-alist
        minor-mode-map-alist)
    (with-temp-buffer
      (bind-map tmpmap2
        :major-modes (emacs-lisp-mode)
        :keys ("C-a")
        :evil-keys ("a")
        :evil-states (motion))
      (evil-normalize-keymaps)
      (emacs-lisp-mode)
      (define-key tmpmap2 "a" "b")
      (message "%s" (pp bind-map-major-modes-alist))
      (should (equal (cdr (assoc 'tmpmap2-active bind-map-major-modes-alist))
                     '(emacs-lisp-mode)))
      (should tmpmap2-active)
      (should (string= (key-binding "\C-aa") "b"))
      (should (keymapp (lookup-key (evil-get-auxiliary-keymap tmpmap2-root-map 'motion) "a")))
      (should (string= (lookup-key (evil-get-auxiliary-keymap tmpmap2-root-map 'motion) "aa") "b"))
      (should (string= "b" (key-binding "\C-aa"))))))

(ert-deftest bind-map-test-minor-mode-keys ()
  "Test binding for minor-modes."
  (let ((tmpmap3 (make-sparse-keymap))
        (tmpmap3-root-map (make-sparse-keymap))
        (fake-minor-mode t)
        minor-mode-map-alist
        minor-mode-alist)
    (bind-map tmpmap3
      :minor-modes (fake-minor-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (evil-normalize-keymaps)
    (define-key tmpmap3 "a" "b")
    (should (string= (key-binding "\C-aa") "b"))))

(ert-deftest bind-map-multiple-declarations ()
  (let ((tmpmap4 (make-sparse-keymap))
        tmpmap4-root-map
        minor-mode-map-alist bind-map-major-modes-alist)
    (bind-map tmpmap4
      :major-modes (mm1 mm2))
    (bind-map tmpmap4
      :major-modes (mm3 mm4 mm5))
    (bind-map tmpmap4
      :major-modes (mm6))
    (should (equal (cdr (assq 'tmpmap4-active bind-map-major-modes-alist))
                   '(mm1 mm2 mm3 mm4 mm5 mm6)))))

(ert-deftest bind-map-test-minor-mode-inheritance ()
  (with-temp-buffer
    (let ((tmpmap5 (make-sparse-keymap))
          (tmpmap6 (make-sparse-keymap))
          (fake-minor-mode t)
          minor-mode-alist
          minor-mode-map-alist)
      (bind-map tmpmap5
        :keys ("C-a")
        :evil-keys ("a")
        :evil-states (normal))
      (bind-map-for-mode-inherit tmpmap6 tmpmap5
        :minor-modes (fake-minor-mode))
      (define-key tmpmap6 "a" 'asdf)
      (should (string= (key-binding "\C-aa") 'asdf))
      (evil-local-mode 1)
      (evil-motion-state)
      (message "%s" evil-state)
      (should (string= (key-binding "aa") 'asdf)))))

(ert-deftest bind-map-test-major-mode-inheritance ()
  (with-temp-buffer
    (let ((tmpmap7 (make-sparse-keymap))
          (tmpmap8 (make-sparse-keymap))
          minor-mode-alist
          minor-mode-map-alist)
      (bind-map tmpmap7
        :keys ("C-a")
        :evil-keys ("a")
        :evil-states (normal))
      (bind-map-for-mode-inherit tmpmap8 tmpmap7
        :major-modes (emacs-lisp-mode))
      (define-key tmpmap8 "a" 'asdf)
      (evil-normalize-keymaps)
      (emacs-lisp-mode)
      (should (string= (key-binding "\C-aa") 'asdf))
      (evil-local-mode 1)
      (evil-motion-state)
      (should (string= (key-binding "aa") 'asdf)))))
