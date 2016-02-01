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
  (let ((tmpmap (make-sparse-keymap)))
    (bind-map tmpmap
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key tmpmap "a" "b")
    (should (keymapp (lookup-key global-map "\C-a")))
    (should (string= (lookup-key global-map "\C-aa") "b"))
    (should (keymapp (lookup-key evil-motion-state-map "a")))
    (should (string= (lookup-key evil-motion-state-map "aa") "b"))
    (should (not (string= (lookup-key evil-visual-state-map "aa") "b")))))

(ert-deftest bind-map-test-major-mode-keys ()
  "Test binding for major-modes."
  (let ((tmpmap (make-sparse-keymap))
        (tmpmap-root-map (make-sparse-keymap))
        tmpmap-active
        tmpmap-prefix
        bind-map-major-modes-alist
        minor-mode-map-alist)
    (bind-map tmpmap
      :major-modes (emacs-lisp-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (evil-normalize-keymaps)
    (emacs-lisp-mode)
    (define-key tmpmap "a" "b")
    (message "%s" (pp bind-map-major-modes-alist))
    (should (equal (cdr (assoc 'tmpmap-active bind-map-major-modes-alist))
                   '(emacs-lisp-mode)))
    (should tmpmap-active)
    (should (string= (key-binding "\C-aa") "b"))
    (should (keymapp (lookup-key (evil-get-auxiliary-keymap tmpmap-root-map 'motion) "a")))
    (should (string= (lookup-key (evil-get-auxiliary-keymap tmpmap-root-map 'motion) "aa") "b"))
    (should (string= "b" (key-binding "\C-aa")))))

(ert-deftest bind-map-test-minor-mode-keys ()
  "Test binding for minor-modes."
  (let ((tmpmap (make-sparse-keymap))
        (tmpmap-root-map (make-sparse-keymap))
        (fake-minor-mode t)
        minor-mode-map-alist)
    (bind-map tmpmap
      :minor-modes (fake-minor-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (evil-normalize-keymaps)
    (define-key tmpmap "a" "b")
    (should (string= (key-binding "\C-aa") "b"))))
