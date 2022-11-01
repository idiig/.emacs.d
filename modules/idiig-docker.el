(defun idiig//docker-dockerfile-setup-backend ()
  "Conditionally setup docker backend."
  (lsp-deferred))

(use-package dockerfile-mode
    :defer t
    :init (add-hook 'dockerfile-mode-local-vars-hook #'idiig//docker-dockerfile-setup-backend)
    :config
    (idiig/set-leader-keys-for-major-mode 'dockerfile-mode
      "cb" 'dockerfile-build-buffer
      "cB" 'dockerfile-build-no-cache-buffer))


(provide 'idiig-docker)
