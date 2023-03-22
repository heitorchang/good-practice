(defun my-add-one (x) (+ 1 x))

(defun my-hours-left-to-read (current-page last-page pages-per-hour)
  "Compute how many hours are left in the book, assuming the same rate of pages per hour."
  (/ (- last-page current-page) pages-per-hour 1.0))
