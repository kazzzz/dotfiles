;; (put 'narrow-to-region 'disabled nil)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; Mac OS X の場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Windowsの場合のファイル名の設定
(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)
  (set-face-attribute 'default nil
		      :family "ＭＳ ゴシック"
		      :height 90)
  (set-fontset-font "fontset-default"
		    'japanese-jisx0208
		    '("ＭＳ ゴシック" . "jisx0208-sjis"))
)


;; 表示関連
(column-number-mode t)
(global-linum-mode t)

;; 起動時のウィンドウサイズ
(setq initial-frame-alist
      (append (list
	       '(width . 140)
	       '(height . 70)
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; ロードパスへの追加
;; ただし、add-to-load-path関数を作成した場合は不要
;; (add-to-list 'load-path "~/.emacs.d/elisp")

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;;; turn on syntax highlighting
(global-font-lock-mode 1)





;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; バックアップファイルの設定
(add-to-list 'backup-directory-alist
	     (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
     `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; if shebang exists add +x
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; elisp関数や変数の情報をエコーエリアに表示する
(defun elisp-mode-hooks()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

