(define-minor-mode map-mode "" :lighter "Map" :keymap
  '(((kbd "w") . mm/kill-number-at-point) ((kbd "s") . mm/start) ((kbd "r") . mm/find-and-display-seed)))

(setq mm/current-seed "0"
      mm/low-res-scale 10
      mm/high-res-scale 1.2
      mm/high-res-size "800"
      mm/debug nil)

(defun mm/start ()
  (interactive)
  (add-hook 'post-command-hook 'mm/find-and-display-seed))

(defun mm/kill-number-at-point ()
  (interactive)
  (kill-new (number-to-string (thing-at-point 'number))))

(defun mm/find-and-display-seed ()
  (interactive)
  (let ((seed (thing-at-point 'number)))
    (if (numberp seed) (mm/switch-to-seed (number-to-string seed)))))

(defun mm/switch-to-seed (seed)
  (setq mm/current-seed seed)
  (mm/try-high-res seed)
  (unless mm/high-res
    (mm/try-low-res seed)
    (mm/start-map-gen seed (if mm/debug (get-buffer-create factorio-cmd-buffer-name) nil))))

(defun mm/display-map-with-scale (filename scale)
  (if (file-exists-p filename)
      (with-current-buffer (get-buffer-create "*Map Preview*")
	(erase-buffer)
	(insert-image-file filename nil nil nil t)
	(image--change-size scale))))

(defun mm/try-high-res (seed)
  (let ((filename (concat "phase2-previews-detailed/" seed ".png")))
    (mm/display-map-with-scale filename mm/high-res-scale)
    (if (file-exists-p filename)
        (setq mm/high-res t)
      (setq mm/high-res nil))))

(defun mm/try-low-res (seed)
  (mm/display-map-with-scale (concat "phase2-previews/" seed ".png") mm/low-res-scale))

(setq factorio-cmd-buffer-name "*Factorio command debug*")

(defun mm/factorio-done-hook (process signal)
  (when (memq (process-status process) '(exit signal))
    (mm/try-high-res mm/current-seed)))

(defun mm/start-map-gen (seed output-buffer)
  (let* ((proc (start-file-process (concat "Factorio" seed) output-buffer "sh" "generatePreview.sh" seed mm/high-res-size)))
    (if (process-live-p proc)
	(set-process-sentinel proc #'mm/factorio-done-hook)
      (message "No process running."))))
