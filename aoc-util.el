;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'elp)
(require 'mmt)
(require 'pcase)
(require 'queue)
(require 'seq)

;;; Strings and Symbols

(defun stringify (&rest objects)
  ;; NOTE: We want to preserve the text properties of any strings.
  (apply #'concat (mapcar (lambda (x) (if (stringp x) x (format "%s" x)))
                          objects)))

(defalias 'cat 'stringify)

(defun symbolicate (&rest objects)
  (intern (apply #'cat objects)))

(defalias 'symcat 'symbolicate)

;;; Printing

(defun prind (object &rest objects)
  ;; NOTE: Like `princ', except that it's variadic and preserves text properties
  ;; when `standard-output' is bound to a buffer.
  (let ((result (apply #'cat object objects)))
    (prog1 result
      (funcall (if (bufferp standard-output) #'insert #'princ) result))))

(defun printf (string &rest objects)
  (prind (apply #'format string objects)))

;;; Input

(defun read-puzzle (f test)
  (funcall f (if (file-exists-p (expand-file-name test))
                 (with-temp-buffer
                   (insert-file-contents test)
                   (buffer-string))
               test)))

(defmacro definput (var f test)
  (declare (indent 2))
  `(progn
     (defvar ,var (read-puzzle ,f ,test))
     ,var))

;;; Output

(defmacro expect (expr outcome)
  (mmt-once-only ((expr `(benchmark-progn ,expr))
                  outcome)
    `(prog1 ,expr
       (and ,outcome (not (equal ,expr ,outcome))
            (error "Fail! Got: %s, Expected: %s" ,expr ,outcome)))))

;;; Conversions

(defun int (x)
  (cl-etypecase x
    (string (cl-parse-integer x))
    (integer (cl-parse-integer (char-to-string x)))
    (boolean (if x 1 0))))

(defalias 'char 'string-to-char)

;;; Math

(defun non-zero-p (x)
  (not (zerop x)))

(defun clamp (a b x)
  (min (max a x) b))

;;; Sequences

(defun top-n (n list)
  (-take n (-sort #'> list)))

(defun join (list)
  (apply #'-concat list))

(defun transpose (lists)
  (apply #'cl-mapcar #'list lists))

(defun seq-windows (n seq)
  ;; NOTE: Like `-partition-in-steps' but works for any sequence.
  (cl-loop for i from 0 below (1+ (- (length seq) n))
           collect (seq-subseq seq i (+ i n))))

(defun seq-type (seq)
  (cl-etypecase seq
    (list 'list)
    (t (type-of seq))))

(defun flatten (seq)
  (if (sequencep seq)
      (seq-mapcat #'flatten seq (seq-type seq))
    (list seq)))

(defun tree-map (func seq &rest seqs)
  (let ((seqs (cons seq seqs)))
    (if (cl-every #'sequencep seqs)
        (-> (apply #'seq-mapn (-partial #'tree-map func) seqs)
            (seq-into (seq-type seq)))
      (apply func seqs))))

(defun tree-reduce (func seq &rest keys)
  (apply #'cl-reduce
         (lambda (acc x)
           (funcall func
                    (if (sequencep acc)
                        (apply #'tree-reduce func acc keys)
                      acc)
                    (if (sequencep x)
                        (apply #'tree-reduce func x keys)
                      x)))
         seq keys))

;;; Comparison

(defun compare-by (pred key)
  (lambda (x y) (funcall pred (funcall key x) (funcall key y))))

(defun lexicographical-compare (pred &rest rest)
  (let ((preds (cons pred rest)))
    (lambda (x y)
      (cl-loop for (pred . rest) = preds then rest
               when (funcall pred x y)
                 return t
               when (or (not rest) (funcall pred y x))
                 return nil))))

;;; Destructuring

(defmacro defun/p (name params &rest body)
  (let ((syms (cl-loop repeat (length params) collect (gensym))))
    `(defun ,name ,syms
       (pcase-let (,@(cl-loop for p in params
                              for s in syms
                              collect `(,p ,s)))
         ,@body))))

(defalias 'lambda/p 'pcase-lambda)

(eval-when-compile
  (defun seq-bindings (params)
    (cl-loop for p in params
             for g = (if (not (symbolp p)) (gensym) p)
             collect (or g p) into syms
             when g
               collect `(,p ,g) into seqs
             finally (cl-return (list syms seqs)))))

(defmacro defun/s (name params &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (syms seqs) (seq-bindings params)
    `(defun ,name ,syms
       (seq-let ,(mapcar #'cl-first seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

(defmacro lambda/s (params &rest body)
  (declare (indent 1))
  (cl-destructuring-bind (syms seqs) (seq-bindings params)
    `(lambda ,syms
       (seq-let ,(mapcar #'cl-first seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

;;; For

(defun for--expand-clause (clause form)
  (cl-flet ((make-loop (form clauses &optional vars)
              ;; NOTE: We drop `cl-loop' from CLAUSES because we're using it
              ;; only to aid indentation.
              (let ((loop `(cl-loop named ,(gensym) ,@(cdr clauses) do ,form)))
                (if vars `(let ,vars ,loop) loop))))
    (pcase-exhaustive clause
      (`(:let ,vars)
       `(pcase-let ,vars ,form))
      (`(:let* ,vars)
       `(pcase-let* ,vars ,form))
      (`(:do . ,forms)
       `(progn ,@forms ,form))
      (`(:return ,result . ,return)
       (cl-destructuring-bind (cond &optional expr) return
         `(if ,cond (cl-return ,(or expr result)) ,form)))
      (`(:when ,cond)
       `(when ,cond ,form))
      (`(:while ,cond)
       `(while ,cond ,form))
      (`(,(and kind (or :collect :append)) ,result ,expr)
       `(progn
          ,(cl-ecase kind
             (:collect `(push ,expr ,result))
             (:append `(setf ,result (nconc (reverse ,expr) ,result))))
          ,form))
      (`(,s (,(or := :step) . ,equals))
       (cl-destructuring-bind (init &optional step cond) equals
         (make-loop form `(cl-loop for ,s = ,init
                                   ,@(and step `(then ,step))
                                   ,@(and cond `(while ,cond))))))
      (`(,s (:range . ,range))
       (cl-destructuring-bind (from &optional (to nil top) step) range
         (cond
          (step (mmt-once-only (to step)
                  (make-loop form
                             `(cl-loop for ,s = ,from then (+ ,s ,step)
                                       while (or (not ,to)
                                                 (if (plusp ,step)
                                                     (< ,s ,to)
                                                   (> ,s ,to)))))))
          (top (mmt-once-only (to)
                 (make-loop form
                            `(cl-loop for ,s from ,from
                                      while (or (not ,to) (< ,s ,to))))))
          (from (make-loop form `(cl-loop for ,s from 0 below ,from))))))
      (`(,s (:in ,list))
       (make-loop form `(cl-loop for ,s in ,list)))
      (`(,s (:on ,list))
       (make-loop form `(cl-loop for ,s on ,list)))
      (`(,s (:across ,vector))
       (make-loop form `(cl-loop for ,s across ,vector)))
      (`((,k ,v) (:ht ,ht))
       (make-loop form `(cl-loop for ,k being the hash-key in ,ht
                                   using (hash-value ,v))))
      (`(,s ,(or `(:seq ,seq) seq))
       (make-loop form `(cl-loop for ,s being the elements of ,seq))))))

(defun for--expand (clauses form)
  (if (null clauses)
      form
    (for--expand-clause (car clauses) (for--expand (cdr clauses) form))))

(defun for--pass-result (clauses result)
  (--map (pcase it
           (`(,(and clause (or :collect :append :return)) . ,rest)
            `(,clause ,result ,@rest))
           (_ it))
         clauses))

(defmacro for-do (clauses &rest body)
  (declare (indent 1))
  (mmt-with-gensyms (result)
    (let ((form (for--expand (for--pass-result clauses result)
                             `(progn ,@body))))
      `(let ((,result nil))
         (cl-block nil
           ,form
           (nreverse ,result))))))

(defmacro for (clauses &rest body)
  (declare (indent 1))
  `(for-do (,@clauses (:collect (progn ,@body)))))

;;; Vectors

(defun vmap (func &rest vs)
  (apply #'cl-map 'vector func vs))

(cl-macrolet ((define-v-arith-fun (name op)
                `(defun ,name (&rest vs)
                   (apply #'vmap #',op vs)))
              (define-v-bool-fun (name op)
                `(defun ,name (&rest vs)
                   (cl-every #'identity (apply #'vmap #',op vs)))))
  (define-v-arith-fun v+ +)
  (define-v-arith-fun v- -)
  (define-v-arith-fun v* *)
  (define-v-bool-fun v< <)
  (define-v-bool-fun v<= <=)
  (define-v-bool-fun v> >)
  (define-v-bool-fun v> >=)
  (define-v-bool-fun v= =))

(cl-macrolet ((define-v-arith-macro (n name op)
                `(defmacro ,name (&rest vs)
                   (let* ((syms (mmt-make-gensym-list (length vs)))
                          (exprs (for ((i (:range ,n)))
                                   `(,',op ,@(for ((s syms)) `(aref ,s ,i))))))
                     `(let ,(-zip-lists syms vs)
                        (vector ,@exprs)))))
              (define-v-bool-macro (n name op)
                `(defmacro ,name (&rest vs)
                   (let* ((syms (mmt-make-gensym-list (length vs)))
                          (exprs (for ((i (:range ,n)))
                                   `(,',op ,@(for ((s syms)) `(aref ,s ,i))))))
                     `(let ,(-zip-lists syms vs)
                        (and ,@exprs))))))
  (define-v-arith-macro 2 v2+ +)
  (define-v-arith-macro 2 v2- -)
  (define-v-arith-macro 2 v2* *)
  (define-v-bool-macro 2 v2< <)
  (define-v-bool-macro 2 v2<= <=)
  (define-v-bool-macro 2 v2> >)
  (define-v-bool-macro 2 v2>= >=)
  (define-v-bool-macro 2 v2= =)
  (define-v-arith-macro 3 v3+ +)
  (define-v-arith-macro 3 v3- -)
  (define-v-arith-macro 3 v3* *)
  (define-v-bool-macro 3 v3< <)
  (define-v-bool-macro 3 v3<= <=)
  (define-v-bool-macro 3 v3> >)
  (define-v-bool-macro 3 v3>= >=)
  (define-v-bool-macro 3 v3= =))

(defun vrev (v)
  (let ((len (length v))
        (rev (copy-seq v)))
    (cl-loop for i from 0 below len
             do (setf (aref rev i) (aref v (- len 1 i)))
             finally (cl-return rev))))

;;; Tensors

(defun with-tensor--name (sym)
  (cl-second (s-match (rx (? "_") (group (+ nonl))) (symbol-name sym))))

(defmacro with-tensor (specs tensor &rest body)
  (declare (indent 2))
  (mmt-with-gensyms (cur)
    (let* ((dims (mmt-make-gensym-list (length specs)))
           (axes (-zip (-sort (-on #'string< #'with-tensor--name) specs) dims)))
      `(let* ((,cur ,tensor)
              ,@(cl-loop for i from 0
                         for d in dims
                         collect `(,d (progn
                                        ,@(and (/= i 0)
                                               `((setf ,cur (aref ,cur 0))))
                                        (length ,cur)))))
         (for-do (,@(cl-loop for s in specs
                             collect `(,s (:range ,(cdr (assoc s axes))))))
           ,@body)))))

(defmacro v--wrap-index (v index)
  (mmt-once-only (v index)
    `(if (< ,index 0) (+ (length ,v) ,index) ,index)))

(defun v. (v &rest indices)
  (cl-loop with cur = v
           for i in indices
           do (setf cur (aref cur (v--wrap-index cur i)))
           finally (cl-return cur)))

(cl-define-compiler-macro v. (v &rest indices)
  (cl-loop with cur = v
           for i in indices
           do (setf cur (mmt-once-only (cur)
                          `(aref ,cur (v--wrap-index ,cur ,i))))
           finally (cl-return cur)))

(defun v.. (v indices)
  (cl-loop with cur = v
           for i across indices
           do (setf cur (aref cur (v--wrap-index cur i)))
           finally (cl-return cur)))

(gv-define-setter v.. (value v indices)
  (mmt-with-gensyms (cur len i w)
    (mmt-once-only (indices)
      `(cl-loop with ,cur = ,v
                with ,len = (length ,indices)
                for ,i from 0 below (1- ,len)
                for ,w = (v--wrap-index ,cur (aref ,indices ,i))
                do (setf ,cur (aref ,cur ,w))
                finally (let ((,w (v--wrap-index
                                   ,cur (aref ,indices (1- ,len)))))
                          (cl-return (setf (aref ,cur ,w) ,value)))))))

(cl-macrolet ((define-v..-macro (n name)
                `(defmacro ,name (v indices)
                   (mmt-once-only (indices)
                     (let ((exprs (for ((i (:range ,n)))
                                    `(aref ,indices ,i))))
                       `(v. ,v ,@exprs))))))
  (define-v..-macro 2 v2..)
  (define-v..-macro 3 v3..))

(defun vdims (seq)
  (cl-loop with cur = seq
           while (or (vectorp cur) (stringp cur))
           collect (length cur) into d
           do (setf cur (aref cur 0))
           finally (cl-return (cl-coerce d 'vector))))

(defvar *neighbors-2*
  (let* ((n9 (for ((x '(-1 0 1)) (y '(-1 0 1))) `[,x ,y]))
         (n8 (remove [0 0] n9))
         (n4 (--filter (= (cl-count 0 it) 1) n8)))
    (ht (4 n4) (8 n8) (9 n9))))

(defvar *neighbors-3*
  (let* ((n27 (for ((x '(-1 0 1)) (y '(-1 0 1)) (z '(-1 0 1))) `[,x ,y ,z]))
         (n26 (remove [0 0 0] n27))
         (n6 (--filter (= (cl-count 0 it) 2) n26)))
    (ht (6 n6) (26 n26) (27 n27))))

;;; Hash tables

(defalias 'h. 'ht-get*)

(gv-define-setter h. (value table &rest keys)
  `(setf (ht-get* ,table ,@keys) ,value))
;;; Buffers

(defmacro with-buffer (expr &rest body)
  (declare (indent 1))
  (mmt-once-only ((expr (if (member expr '(nil _)) nil expr)))
    `(with-current-buffer (if ,expr
                              (get-buffer-create ,expr t)
                            (generate-new-buffer " *temp*" t))
       (let ((standard-output (current-buffer)))
         ,@body))))

;;; Debugging

(defmacro comment (&rest _)
  nil)

(defmacro dmsg (&rest forms)
  (and forms
       (cl-destructuring-bind (head . rest) forms
         (mmt-once-only ((h head))
           `(prog1 ,h
              (message "%S: %S" ',head ,h)
              ,@(cl-loop for form in rest
                         collect `(message "%S: %S" ',form ,form)))))))

(defmacro dprint (&rest forms)
  (and forms
       (cl-destructuring-bind (head . rest) forms
         (mmt-once-only ((h head))
           `(prog1 ,h
              (printf "%S: %S" ',head ,h)
              ,@(cl-loop for form in rest
                         collect `(printf ", %S: %S" ',form ,form))
              (princ "\n"))))))

;;; Profiling

(defmacro with-profiling (funcs &rest body)
  (declare (indent 1))
  (mmt-once-only (funcs)
    `(progn
       (elp-instrument-list ,funcs)
       (unwind-protect
           (progn (condition-case nil ,@body
                    ;; NOTE: Let the debugger handle the error first, but allow
                    ;; continuing in order to show the captured stats.
                    ((debug error)))
                  (elp-results))
         (elp-reset-list ,funcs)))))

(provide 'aoc-util)
