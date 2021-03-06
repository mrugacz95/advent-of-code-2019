#!/usr/local/bin/sbcl --script
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect (loop for c across line
      collect c))))
(defvar grid (get-file "day_18.in"))
(defun grid-at (y x)
  (nth x (nth y grid)))
(defun collect-neighbours (y x)
  (let ((result ()))
  (loop for dir in '((1 0) (-1 0) (0 1) (0 -1))
    do (let ((ypos (+ y (nth 0 dir)))
                  (xpos (+ x (nth 1 dir))))
                  (let ((cell (grid-at ypos xpos)))
                   (when (char/= cell #\#)
                    (push (list ypos xpos) result)))))
    result))
(defun is-in (elem l) (loop for e in l do (when (equal elem e) (return-from is-in t))) nil)
(defun calc-dist (from to)
  (let ((queue ())
        (visited ())
        (distance (list 0)))
    (push from queue)
    (loop while (not (null queue))
      do (let ((current (pop queue))
                (dist (pop distance)))
            (if (equal current to)
              (return-from calc-dist dist)
              (when (not (is-in current visited))
                (let ((neighbours (collect-neighbours (first current) (second current))))
                  (setq distance (append distance (loop for n in neighbours
                    collect (1+ dist))))
                  (setq queue
                    (append queue neighbours)))))

          (push current visited)))))
(defun is-lower (c) (eq (char-downcase c) c))
(defun is-upper (c) (eq (char-upcase c) c))
(defvar keys ())
(defun has-key (key)
  (return-from has-key (not (null (member key keys)))))
(defun can-pass (cell)
  (if (is-lower cell)
    (return-from can-pass t))
  (if (and (is-upper cell) (has-key (char-downcase cell)))
    (return-from can-pass t))
  nil)
(defun find-entrance ()
  (loop for y from 0 below (list-length grid)
    do (let ((row (nth y grid)))
       (loop for x from 0 below (list-length row)
          do (let ((cell (nth x row)))
            (when (char= cell #\@)
              (return-from find-entrance (list y x))))))))
(defun replace-at (y x value)
  (setf (nth x (nth y grid)) value))
(defun print-grid ()
  (loop for row in grid
    do (loop for c in row
          do (format t "~a" c))
          (terpri))
  (terpri))
(defun compute-hash (pos)
  (let ((sum (+ (first pos) (* 53 (second pos))))
        (p 53)
        (count 2))
  (loop for k in (sort (copy-list keys) 'char<=)
    when (char/= k #\@)
    do (setq sum (mod (+ sum (* (expt p count) (char-int k))) 100000007))
       (setq count (1+ count)))
  sum))
(defvar cache (make-hash-table))
(defvar min_dist 10000000000)
(defun search-symbols ()
  (let ((symbols ()))
    (loop for row in grid
      do (loop for c in row
        do (when (and (char/= c #\#) (char/= c #\.))
          (push c symbols))))
  symbols))
(defvar symbols (search-symbols))
(defun finished ()
  (eq (list-length keys) (/ (1- (list-length symbols)) 2)))
(defun find-position (symbol)
  (loop for y from 0 below (list-length grid)
    do (let ((row (nth y grid)))
       (loop for x from 0 below (list-length row)
          do (let ((cell (nth x row)))
            (when (char= cell symbol)
              (return-from find-position (list y x))))))))

(defun precompute-distances ()
  (loop for s1 in symbols
    collect (loop for s2 in symbols
      collect (calc-dist (find-position s1)  (find-position s2)))))
(defvar distances (precompute-distances))
(defun get-dist (s1 s2)
  (nth (position s2 symbols) (nth (position s1 symbols) distances)))

(defvar tree (make-hash-table))
(defun dependency-tree ()
  (let ((queue (list (list (find-entrance) #\@)))
        (visited ()))
    (loop while (not (null queue))
      do (let ((popped (pop queue)))
           (let ((current (first popped))
                (c (grid-at (first (first popped))  (second (first popped)))))
              (let ((parent (if (char= c #\.)
                              (second popped)
                              c )))
                (push current visited)
                (when (char/= c #\.)
                  (setf (gethash (second popped) tree) (append (gethash (second popped) tree) (list c)))
                  )
                (loop for n in (collect-neighbours (first current) (second current))
                  do (when (not (is-in n visited))
                       (setq queue (append queue (list (list n parent))))))))))))
(defun find-available ()
  (let ((available ())
        (queue (gethash #\@ tree)))
    (loop while (not (null queue))
            do (let ((current (pop queue)))
                (push current available)
                (loop for child in (gethash current tree)
                  do
                  (when (and (can-pass child)
                              (char/= child #\@)
                              (not (member child queue))
                              (or (not (null (member (char-downcase current) keys))) (not (null (member current keys)))))
                          (setq queue (append queue (list child)))))))
  (loop for m in available
    when (and (is-lower m) (not (member m keys)) (char/= m #\@))
    collect m)))
(defvar objects-pos (make-hash-table))
(defun collect-obj-pos ()
  (loop for s in symbols
    do (setf (gethash s objects-pos) (find-position s))))
(defun find-available-moves ()
    (loop for c in (find-available)
      collect (list (gethash c objects-pos) c)))
(defvar cache (make-hash-table))
(defun count-moves (pos current_dist)
  (let ((hash (compute-hash pos)))
   (when (not (null (gethash hash cache)))
     (return-from count-moves (gethash hash cache))))
  (when (> current_dist min_dist)
    (return-from count-moves min_dist))
  (let ((c (grid-at (first pos) (second pos))))
    (when (is-lower c)
      (push c keys))
    (let ((available (reverse (find-available-moves))))
      (when (null available)
        (when (is-lower c)
          (pop keys))
        (when (< current_dist min_dist)
          (format t "New champion: ~A" current_dist)(terpri)
          (setq min_dist current_dist))
        (return-from count-moves 0))
      (let ((min_rest 1000000000))
        (loop for move in available
          do (when (can-pass (second move))
            (let ((dist (get-dist c (second move))))
              (let ((cur_min_rest (count-moves (first move) (+ dist current_dist))))
                (when (< (+ dist cur_min_rest) min_rest)
                  (setq min_rest (+ dist cur_min_rest)))))))
        (when (is-lower c)
          (pop keys))
        (let ((hash (compute-hash pos)))
          (setf (gethash hash cache) min_rest))
    min_rest))))
(print-grid)
(search-symbols)
(dependency-tree)
(collect-obj-pos)
(format t "started walking")(terpri)
(format t "Answer: ~A" (count-moves (find-entrance) 0))
; (setq keys (list #\a #\b #\X #\c))
; (print (compute-hash (list 1 1)))
; (setq keys (list #\b #\X #\a #\c))
; (print (compute-hash (list 1 1)))
