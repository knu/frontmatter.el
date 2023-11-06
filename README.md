# frontmatter.el

This package provides functions for manipulating YAML frontmatter data.

# Usage

- `frontmatter-add-date`

  This command adds the "date" frontmatter property to the current buffer.

  ``` emacs-lisp
  (define-key markdown-mode-map (kbd "C-c d") #'frontmatter-add-date)
  (define-key gfm-mode-map      (kbd "C-c d") #'frontmatter-add-date)
  ```

- `frontmatter-update-timestamps`

  This command can be used to mimic what the [Update time on edit plugin](https://github.com/beaussan/update-time-on-edit-obsidian) does like so:

  ```emacs-lisp
  (require 'obsidian)

  (defun frontmatter-update-timestamps-before-save ()
    (add-hook 'before-save-hook #'frontmatter-update-timestamps nil t))
  (add-hook 'obsidian-mode-hook #'frontmatter-update-timestamps-before-save)
  ```

## Author

Copyright (c) 2023 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for details.

Visit [GitHub Repository](https://github.com/knu/frontmatter.el) for the latest information.
