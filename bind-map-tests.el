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
  (let ((map (make-sparse-keymap)))
    (bind-map map
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key map "a" "b")
    (should (keymapp (lookup-key global-map "\C-a")))
    (should (string= (lookup-key global-map "\C-aa") "b"))
    (should (keymapp (lookup-key evil-motion-state-map "a")))
    (should (string= (lookup-key evil-motion-state-map "aa") "b"))
    (should (not (string= (lookup-key evil-visual-state-map "aa") "b")))))

(ert-deftest bind-map-test-minor-mode-keys ()
  "Test binding for minor-modes."
  (let ((map (make-sparse-keymap))
        (map-root-map (make-sparse-keymap))
        (fake-minor-mode t)
        minor-mode-map-alist)
    (bind-map map
      :minor-modes (fake-minor-mode)
      :keys ("C-a")
      :evil-keys ("a")
      :evil-states (motion))
    (define-key map "a" "b")
    (should (string= (key-binding "\C-aa") "b"))
    (should (keymapp (lookup-key (evil-get-auxiliary-keymap map-root-map 'motion) "a")))
    (should (string= (lookup-key (evil-get-auxiliary-keymap map-root-map 'motion) "aa") "b"))))
