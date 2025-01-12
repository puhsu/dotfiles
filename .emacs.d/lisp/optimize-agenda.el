;; -*- lexical-binding: t; -*-
;; Idea from gist, adapted for denote: https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e

(defun optimize-agenda-project-p ()
  "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun optimize-agenda-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (optimize-agenda-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((keywords (denote-extract-keywords-from-path buffer-file-name))
             (original-keywords keywords)
             (file-type (denote-filetype-heuristics buffer-file-name))
             (title (denote-retrieve-title-value buffer-file-name file-type)))
        (message "Name: %s" buffer-file-name)
        (if (optimize-agenda-project-p)
            (setq keywords (cons "project" keywords))
          (setq keywords (remove "project" keywords)))
        
        (setq keywords (seq-uniq keywords))
                
        ;; Update only if changed
        (when (or (seq-difference keywords original-keywords)
                 (seq-difference original-keywords keywords))
          (let ((denote-rename-confirmations nil)) 
            (denote-rename-file buffer-file-name title keywords)))))))

(defun optimize-agenda-buffer-p ()
  "Return non-nil if current buffer is a denote note."
  (and buffer-file-name
       (denote-file-is-note-p buffer-file-name)))

(defun optimize-agenda-project-files ()
  "Return list of note files containing 'project' keyword."
  (seq-filter
   (lambda (file)
     (member "project" (denote-extract-keywords-from-path file)))
   (denote-directory-files)))

(defun optimize-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (optimize-agenda-project-files)))

(add-hook 'find-file-hook #'optimize-agenda-project-update-tag)
(add-hook 'before-save-hook #'optimize-agenda-project-update-tag)

(advice-add 'org-agenda :before #'optimize-agenda-files-update)
(advice-add 'org-todo-list :before #'optimize-agenda-files-update)

(provide 'optimize-agenda)
