;;; css-sort.el --- Sort CSS attributables.

;; Filename: css-sort.el
;; Description: Sort CSS buffer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-13 08:59:01
;; Version: 0.5
;; Last-Updated: 2018-11-22 13:51:20
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/css-sort.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Sort CSS attributables just need one command `css-sort'.
;;

;;; Installation:
;;
;; Put css-sort.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'css-sort)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET css-sort RET
;;

;;; Change log:
;;
;; 2018/11/22
;;	* Fix duplicate attribute issue: https://github.com/manateelazycat/css-sort/issues/1
;;
;; 2018/11/21
;;      * Skip @ start function.
;;      * Support sort @include statement now!
;;
;; 2018/11/20
;;      * CSS attributable order follow http://alloyteam.github.io/CodeGuide/#css-declaration-order
;;      * Support @ prefix attributable.
;;
;; 2018/07/13
;;      * First released.
;;      * Adjust order of `line-height' attributable.
;;

;;; Acknowledgements:
;;
;; https://github.com/diiq/css-sort.el is awesome!
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:

;; CSS attributable order follow http://alloyteam.github.io/CodeGuide/#css-declaration-order
(setq css-sort-attributes-order
      '(
        "display"
        "visibility"
        "float"
        "clear"
        "overflow"
        "overflow-x"
        "overflow-y"
        "clip"
        "zoom"

        "table-layout"
        "empty-cells"
        "caption-side"
        "border-spacing"
        "border-collapse"
        "list-style"
        "list-style-position"
        "list-style-type"
        "list-style-image"

        "-webkit-box-orient"
        "-webkit-box-direction"
        "-webkit-box-decoration-break"
        "-webkit-box-pack"
        "-webkit-box-align"
        "-webkit-box-flex"

        "position"
        "top"
        "right"
        "bottom"
        "left"
        "z-index"

        "margin"
        "margin-top"
        "margin-right"
        "margin-bottom"
        "margin-left"
        "-webkit-box-sizing"
        "-moz-box-sizing"
        "box-sizing"
        "border"
        "border-width"
        "border-style"
        "border-color"
        "border-top"
        "border-top-width"
        "border-top-style"
        "border-top-color"
        "border-right"
        "border-right-width"
        "border-right-style"
        "border-right-color"
        "border-bottom"
        "border-bottom-width"
        "border-bottom-style"
        "border-bottom-color"
        "border-left"
        "border-left-width"
        "border-left-style"
        "border-left-color"
        "-webkit-border-radius"
        "-moz-border-radius"
        "border-radius"
        "-webkit-border-top-left-radius"
        "-moz-border-radius-topleft"
        "border-top-left-radius"
        "-webkit-border-top-right-radius"
        "-moz-border-radius-topright"
        "border-top-right-radius"
        "-webkit-border-bottom-right-radius"
        "-moz-border-radius-bottomright"
        "border-bottom-right-radius"
        "-webkit-border-bottom-left-radius"
        "-moz-border-radius-bottomleft"
        "border-bottom-left-radius"
        "-webkit-border-image"
        "-moz-border-image"
        "-o-border-image"
        "border-image"
        "-webkit-border-image-source"
        "-moz-border-image-source"
        "-o-border-image-source"
        "border-image-source"
        "-webkit-border-image-slice"
        "-moz-border-image-slice"
        "-o-border-image-slice"
        "border-image-slice"
        "-webkit-border-image-width"
        "-moz-border-image-width"
        "-o-border-image-width"
        "border-image-width"
        "-webkit-border-image-outset"
        "-moz-border-image-outset"
        "-o-border-image-outset"
        "border-image-outset"
        "-webkit-border-image-repeat"
        "-moz-border-image-repeat"
        "-o-border-image-repeat"
        "border-image-repeat"
        "padding"
        "padding-top"
        "padding-right"
        "padding-bottom"
        "padding-left"
        "width"
        "min-width"
        "max-width"
        "height"
        "min-height"
        "max-height"

        "font"
        "font-family"
        "font-size"
        "font-weight"
        "font-style"
        "font-variant"
        "font-size-adjust"
        "font-stretch"
        "font-effect"
        "font-emphasize"
        "font-emphasize-position"
        "font-emphasize-style"
        "font-smooth"
        "line-height"
        "text-align"
        "-webkit-text-align-last"
        "-moz-text-align-last"
        "-ms-text-align-last"
        "text-align-last"
        "vertical-align"
        "white-space"
        "text-decoration"
        "text-emphasis"
        "text-emphasis-color"
        "text-emphasis-style"
        "text-emphasis-position"
        "text-indent"
        "-ms-text-justify"
        "text-justify"
        "letter-spacing"
        "word-spacing"
        "-ms-writing-mode"
        "text-outline"
        "text-transform"
        "text-wrap"
        "-ms-text-overflow"
        "text-overflow"
        "text-overflow-ellipsis"
        "text-overflow-mode"
        "-ms-word-wrap"
        "word-wrap"
        "-ms-word-break"
        "word-break"

        "color"
        "background"
        "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader"
        "background-color"
        "background-image"
        "background-repeat"
        "background-attachment"
        "background-position"
        "-ms-background-position-x"
        "background-position-x"
        "-ms-background-position-y"
        "background-position-y"
        "-webkit-background-clip"
        "-moz-background-clip"
        "background-clip"
        "background-origin"
        "-webkit-background-size"
        "-moz-background-size"
        "-o-background-size"
        "background-size"

        "outline"
        "outline-width"
        "outline-style"
        "outline-color"
        "outline-offset"
        "opacity"
        "filter:progid:DXImageTransform.Microsoft.Alpha(Opacity"
        "-ms-filter:\\'progid:DXImageTransform.Microsoft.Alpha"
        "-ms-interpolation-mode"
        "-webkit-box-shadow"
        "-moz-box-shadow"
        "box-shadow"
        "filter:progid:DXImageTransform.Microsoft.gradient"
        "-ms-filter:\\'progid:DXImageTransform.Microsoft.gradient"
        "text-shadow"

        "-webkit-transition"
        "-moz-transition"
        "-ms-transition"
        "-o-transition"
        "transition"
        "-webkit-transition-delay"
        "-moz-transition-delay"
        "-ms-transition-delay"
        "-o-transition-delay"
        "transition-delay"
        "-webkit-transition-timing-function"
        "-moz-transition-timing-function"
        "-ms-transition-timing-function"
        "-o-transition-timing-function"
        "transition-timing-function"
        "-webkit-transition-duration"
        "-moz-transition-duration"
        "-ms-transition-duration"
        "-o-transition-duration"
        "transition-duration"
        "-webkit-transition-property"
        "-moz-transition-property"
        "-ms-transition-property"
        "-o-transition-property"
        "transition-property"
        "-webkit-transform"
        "-moz-transform"
        "-ms-transform"
        "-o-transform"
        "transform"
        "-webkit-transform-origin"
        "-moz-transform-origin"
        "-ms-transform-origin"
        "-o-transform-origin"
        "transform-origin"
        "-webkit-animation"
        "-moz-animation"
        "-ms-animation"
        "-o-animation"
        "animation"
        "-webkit-animation-name"
        "-moz-animation-name"
        "-ms-animation-name"
        "-o-animation-name"
        "animation-name"
        "-webkit-animation-duration"
        "-moz-animation-duration"
        "-ms-animation-duration"
        "-o-animation-duration"
        "animation-duration"
        "-webkit-animation-play-state"
        "-moz-animation-play-state"
        "-ms-animation-play-state"
        "-o-animation-play-state"
        "animation-play-state"
        "-webkit-animation-timing-function"
        "-moz-animation-timing-function"
        "-ms-animation-timing-function"
        "-o-animation-timing-function"
        "animation-timing-function"
        "-webkit-animation-delay"
        "-moz-animation-delay"
        "-ms-animation-delay"
        "-o-animation-delay"
        "animation-delay"
        "-webkit-animation-iteration-count"
        "-moz-animation-iteration-count"
        "-ms-animation-iteration-count"
        "-o-animation-iteration-count"
        "animation-iteration-count"
        "-webkit-animation-direction"
        "-moz-animation-direction"
        "-ms-animation-direction"
        "-o-animation-direction"
        "animation-direction"

        "content"
        "quotes"
        "counter-reset"
        "counter-increment"
        "resize"
        "cursor"
        "-webkit-user-select"
        "-moz-user-select"
        "-ms-user-select"
        "user-select"
        "nav-index"
        "nav-up"
        "nav-right"
        "nav-down"
        "nav-left"
        "-moz-tab-size"
        "-o-tab-size"
        "tab-size"
        "-webkit-hyphens"
        "-moz-hyphens"
        "hyphens"
        "pointer-events"
        ))

(setq css-sort-attributes-prefix-order
      '(
        "@"
        ))

(defun css-sort-index (object list)
  (let ((counter 0)
        (found nil))
    (catch 'finished
      (dolist (listelement list counter)
        (if (equal object listelement)
            (progn
              (setq found t)
              (throw 'finished counter))
          (incf counter)))
      (if found counter nil))))

(defun css-sort-chomp (str)
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun css-sort-attribute-from-line (line)
  (css-sort-chomp (nth 0 (split-string line ":"))))

(defun css-sort-attribute-index (line)
  (or (css-sort-index
       (css-sort-attribute-from-line line) css-sort-attributes-order)
      -1))

(defun css-sort-attributes-prefix-index (prefix)
  (or (css-sort-index prefix css-sort-attributes-prefix-order)
      -1))

(defun css-sort-get-prefix (string)
  (let ((trim-string (string-trim string)))
    (if (> (length trim-string) 0)
        (substring trim-string 0 1)
      "")))

(defun css-sort-lines-in-region (start end)
  (split-string (buffer-substring-no-properties start end) "[\n]"))

(defun css-sort-beginning-of-attribute-block (start)
  (save-excursion
    (goto-char start)
    (search-backward "{")
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun css-sort-end-of-attribute-block (start)
  (save-excursion
    (goto-char start)
    (re-search-forward "[{}]")
    (beginning-of-line)
    (search-backward-regexp "[^ \t\n]" nil t)
    (forward-char)
    (point)))

(defun css-sort-attribute-compare (a b)
  (let* ((trim-a (string-trim a))
         (trim-b (string-trim b))
         (prefix-a (css-sort-get-prefix a))
         (prefix-b (css-sort-get-prefix b)))
    (cond
     ((and (css-sort-index prefix-a css-sort-attributes-prefix-order)
           (css-sort-index prefix-b css-sort-attributes-prefix-order))
      (cond ((string-equal prefix-a prefix-b)
             ;; Sort by alphabetical if two attributes have same prefix.
             (string< trim-a trim-b))
            (t
             ;; Sort by `css-sort-attributes-prefix-order' order if two attributes is different prefix.
             (< (css-sort-attributes-prefix-index prefix-a)
                (css-sort-attributes-prefix-index prefix-b)
                ))))
     ;; Make prefix attributes at last.
     ((css-sort-index prefix-a css-sort-attributes-prefix-order)
      nil)
     ((css-sort-index prefix-b css-sort-attributes-prefix-order)
      t)
     ;; Sort by `css-sort-attributes-order' order if is regular css attribute.
     (t
      (< (css-sort-attribute-index a)
         (css-sort-attribute-index b))))))

(defun css-sort-in-at-rule-line-p ()
  (save-excursion
    (let ((line-content (string-trim (css-sort-current-line-content))))
      (string-prefix-p "@" line-content)
      )))

(defun css-sort-current-line-content ()
  (buffer-substring-no-properties
   (save-excursion
     (beginning-of-line)
     (point))
   (save-excursion
     (end-of-line)
     (point))))

(defun css-sort-end-of-include-sexp (end)
  (save-excursion
    (let (point-before-move)
      (ignore-errors
        ;; It's a hacking way to get end position of '@include' statement.
        (while (< (point) end)
          (setq point-before-move (point))
          (forward-sexp)))
      ;; Restore point if `forward-sexp' out bound.
      (when (> (point) end)
        (goto-char point-before-move)))
    (point)))

(defun css-sort-parse-lines (start end)
  (let (css-regular-blocks
        css-at-prefix-blocks)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        ;; Collect CSS attribute statements.
        (if (css-sort-in-at-rule-line-p)
            (let ((at-prefix-start (save-excursion
                                     (beginning-of-line)
                                     (point)))
                  (at-prefix-end (css-sort-end-of-include-sexp end)))
              (add-to-list 'css-at-prefix-blocks (buffer-substring-no-properties at-prefix-start at-prefix-end) t)
              (goto-char at-prefix-end))
          (add-to-list 'css-regular-blocks (css-sort-current-line-content) t))
        ;; Move to next line.
        (forward-line)
        (beginning-of-line)))
    ;; Return CSS attribute statements.
    (cons css-regular-blocks css-at-prefix-blocks)))

(defun css-sort ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+{" (point-max) t)
      (if (css-sort-in-at-rule-line-p)
          ;; Skip line that start with @
          (up-list)
        ;; Sort CSS attribute order.
        (let* ((start (css-sort-beginning-of-attribute-block (point)))
               (end (css-sort-end-of-attribute-block (point)))
               (parse-result (css-sort-parse-lines start end))
               (regular-lines (car parse-result))
               (at-prefix-lines (cdr parse-result)))
          ;; Delete all attributes.
          (delete-region start end)
          ;; Insert @include attributes.
          (goto-char start)
          (insert (mapconcat 'identity at-prefix-lines "\n"))
          ;; Insert one return line between @include attributes and regular attributes.
          (when (and (> (length at-prefix-lines) 0)
                     (> (length regular-lines) 0))
            (insert "\n"))
          ;; Insert regular attributes.
          (insert (mapconcat 'identity (sort regular-lines #'css-sort-attribute-compare) "\n"))
          )))))

(provide 'css-sort)

;;; css-sort.el ends here
