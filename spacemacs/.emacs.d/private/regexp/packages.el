(defconst regexp-packages
  '(visual-regexp
    visual-regexp-steroids)
)

(defun regexp/init-visual-regexp ()
  (use-package visual-regexp
    :defer t
    ))

(defun regexp/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :bind ("C-s" . 'vr/isearch-forward)
    ))
