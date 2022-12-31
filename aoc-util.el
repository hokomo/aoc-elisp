;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)
(require 'dash)
(require 'elp)
(require 'mmt)
(require 'pcase)
(require 'queue)
(require 'seq)

;;; Strings and Symbols

(defun cat (&rest args)
  ;; NOTE: We want to preserve the text properties of any strings.
  (apply #'concat (mapcar (lambda (x) (if (stringp x) x (format "%s" x)))
                          args)))

(defun symcat (&rest args)
  (intern (apply #'cat args)))

(defun keywordize (sym)
  (let ((sym (symcat sym)))
    (if (keywordp sym)
        sym
      (symcat ":" sym))))

(defun unkeywordize (sym)
  (let ((sym (symcat sym)))
    (if (not (keywordp sym))
        sym
      (symcat (substring (symbol-name sym) 1)))))

;;; Printing

(defun prind (arg &rest args)
  ;; NOTE: Like `princ', except that it's variadic and preserves text properties
  ;; when `standard-output' is bound to a buffer.
  (let ((result (apply #'cat arg args)))
    (prog1 result
      (funcall (if (bufferp standard-output) #'insert #'princ) result))))

(defun printf (string &rest args)
  (prind (apply #'format string args)))

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

(cl-defmacro expect (expr outcome &key gc)
  (let* ((expr `(benchmark-progn ,expr))
         (expr (if gc `(let ((gc-cons-threshold ,gc)) ,expr) expr)))
    (mmt-once-only (expr outcome)
      `(prog1 ,expr
         (and ,outcome (not (equal ,expr ,outcome))
              (error "Fail! Got: %s, Expected: %s" ,expr ,outcome))))))

;;; Debugging

(defmacro comment (&rest _body)
  nil)

(defmacro dmsg (&rest forms)
  (and forms
       (cl-destructuring-bind (head . rest) forms
         (mmt-once-only ((h head))
           `(prog1 ,h
              ,(if (or (and (atom head) (not (symbolp head)))
                       (and (consp head) (eq (car head) 'quote)))
                   `(message "%s" ,h)
                 `(message "%S: %S" ',head ,h))
              ,@(cl-loop for form in rest
                         collect (mmt-once-only ((f form))
                                   (if (or (and (atom form) (not (symbolp form)))
                                           (and (consp form) (eq (car form) 'quote)))
                                       `(message "%s" ,f)
                                     `(message "%S: %S" ',form ,f)))))))))

(defmacro dprint (&rest forms)
  (and forms
       (cl-destructuring-bind (head . rest) forms
         (mmt-once-only ((h head))
           `(prog1 ,h
              (printf "%S: %S" ',head ,h)
              ,@(cl-loop for form in rest
                         collect `(printf ", %S: %S" ',form ,form))
              (princ "\n"))))))

(defmacro with-profiling (funcs &rest body)
  (declare (indent 1))
  (mmt-once-only (funcs)
    `(progn
       (elp-instrument-list ,funcs)
       (unwind-protect
           (prog1 (condition-case nil ,@body
                    ;; NOTE: Let the debugger handle the error first, but allow
                    ;; continuing in order to show the captured stats.
                    ((debug error)))
             (elp-results))
         (elp-reset-list ,funcs)))))

(defmacro with-sprofiling (kind &rest body)
  (declare (indent 1))
  (mmt-once-only ((kind `(or ,kind 'cpu+mem)))
    `(progn
       (profiler-start ,kind)
       (unwind-protect
           (prog1 (condition-case nil ,@body
                    ;; NOTE: Let the debugger handle the error first, but allow
                    ;; continuing in order to show the captured stats.
                    ((debug error)))
             (profiler-report))
         (profiler-stop)))))

;;; Conversions

(defun int (x)
  (cl-etypecase x
    (string (cl-parse-integer x))
    (integer (cl-parse-integer (char-to-string x)))
    (boolean (if x 1 0))))

(defalias 'char 'string-to-char)

(defun read-one-from-string (string)
  (car (read-from-string string)))

(defun read-all-from-string (string)
  (cl-loop with len = (length string)
           for cur = 0 then next
           while (< cur len)
           for (obj . next) = (read-from-string string cur)
           collect obj))

;;; Math

(defun sum-n (n)
  (/ (* n (1+ n)) 2))

(defun non-zero-p (x)
  (not (zerop x)))

(defun max* (&rest ns)
  (let ((ns (remove nil ns)))
    (and ns (apply #'max ns))))

(defun min* (&rest ns)
  (let ((ns (remove nil ns)))
    (and ns (apply #'min ns))))

(defun clamp (a b x)
  (min (max a x) b))

(defun wrap (a b x)
  (+ (mod (- x a) (1+ (- b a))) a))

(defun taxi-distance (u v)
  (cl-reduce #'+ (vmap #'abs (v- u v))))

(defun chess-distance (u v)
  (cl-reduce #'max (vmap #'abs (v- u v))))

(defun horner (coefs value)
  (cl-loop with result = 0
           for c in coefs
           do (setf result (+ (* result value) c))
           finally (cl-return result)))

(defun digits (n base)
  (cl-loop collect (mod n base) into ds
           do (setf n (/ n base))
           until (zerop n)
           finally (cl-return (nreverse ds))))

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

(defun list-compare (pred)
  ;; NOTE: This is a "shortlex" compare.
  (lambda (x y)
    (cl-loop for xs = x then (cdr xs)
             for ys = y then (cdr ys)
             when (or (not xs) (not ys))
               return (not xs)
             when (funcall pred (car xs) (car ys))
               return t
             when (funcall pred (car ys) (car xs))
               return nil)))

(defun vector-compare (pred)
  ;; NOTE: This is a "shortlex" compare.
  (lambda (x y)
    (let ((lenx (length x))
          (leny (length y)))
      (cond
       ((< lenx leny)
        t)
       ((> leny lenx)
        nil)
       (t
        (cl-loop for xe across x
                 for ye across y
                 when (funcall pred xe ye)
                   return t
                 when (funcall pred ye xe)
                   return nil))))))

(let ((f (vector-compare #'<)))
  (defun range< (x y)
    (funcall f x y)))

;;; Destructuring

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
       (seq-let ,(mapcar #'car seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

(defmacro lambda/s (params &rest body)
  (declare (indent 1))
  (cl-destructuring-bind (syms seqs) (seq-bindings params)
    `(lambda ,syms
       (seq-let ,(mapcar #'car seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

;;; `for'

(defun pfor--expand (clauses form)
  (let ((name nil)
        (init-done-p nil)
        (iter-done-p nil)
        (whens '())
        (collects '())
        (result (cl-gensym "result"))
        (loop-clauses '()))
    (cl-labels ((add-clauses (new)
                  ;; NOTE: We drop `cl-loop' from NEW because we're using it
                  ;; only to aid indentation.
                  (setf loop-clauses (nconc loop-clauses (cdr new))))
                (add-iter (clause new)
                  (when iter-done-p
                    (error (cat "A %S clause must appear before any control "
                                "clauses")
                           clause))
                  (add-clauses new)
                  (setf init-done-p t))
                (add-control (new)
                  (add-clauses new)
                  (setf init-done-p t
                        iter-done-p t)))
      (dolist (clause clauses)
        (pcase-exhaustive clause
          (`(:named ,sym)
           (when name
             (error "There can be at most one %S clause" :named))
           (when init-done-p
             (error (cat "A %S clause must appear before any iteration or "
                         "control clauses")
                    :named))
           (setf name sym))
          (`(:initially ,form)
           (add-clauses `(cl-loop initially ,form)))
          (`(:finally ,form)
           (add-clauses `(cl-loop finally ,form)))
          (`(:let ,vars)
           (add-clauses `(cl-loop ,@(cl-loop for (v e) in vars
                                             for prep = 'for then 'and
                                             append `(,prep ,v = ,e)))))
          (`(:let* ,vars)
           (add-clauses `(cl-loop ,@(cl-loop for (v e) in vars
                                             append `(for ,v = ,e)))))
          (`(:do . ,forms)
           (add-control `(cl-loop do (progn ,@forms))))
          (`(:return . ,return)
           (cl-destructuring-bind (cond &optional expr) return
             (add-control
              `(cl-loop do (when ,cond
                             (cl-return-from ,name ,(or expr result)))))))
          (`(:when ,cond)
           (push cond whens)
           (setf init-done-p t
                 iter-done-p t))
          (`(:while ,cond)
           (add-control `(cl-loop while ,cond)))
          (`(:until ,cond)
           (add-control `(cl-loop until ,cond)))
          (`(,(and kind (or :collect :append)) ,expr)
           (push `(,(unkeywordize kind) ,expr) collects)
           (setf init-done-p t
                 iter-done-p t))
          (`(,s (:step . ,step))
           (cl-destructuring-bind (init step &optional cond) step
             (add-iter :step `(cl-loop for ,s = ,init
                                       ,@(and step `(then ,step))
                                       ,@(and cond `(while ,cond))))))
          (`(,s (:range . ,range))
           (cl-destructuring-bind (from &optional (to nil top) step) range
             (add-iter
              :range
              (cond
               (step (mmt-with-gensyms (gto gstep)
                       `(cl-loop with ,gto = ,to
                                 with ,gstep = ,step
                                 for ,s = ,from then (+ ,s ,gstep)
                                 while (or (not ,gto)
                                           (if (plusp ,gstep)
                                               (< ,s ,gto)
                                             (> ,s ,gto))))))
               (top (mmt-with-gensyms (gto)
                      `(cl-loop with ,gto = ,to
                                for ,s from ,from
                                while (or (not ,gto) (< ,s ,gto)))))
               (from `(cl-loop for ,s from 0 below ,from))))))
          (`(,s (:in ,list))
           (add-iter :in `(cl-loop for ,s in ,list)))
          (`(,s (:on ,list))
           (add-iter :on `(cl-loop for ,s on ,list)))
          (`(,s (:across ,vector))
           (add-iter :across `(cl-loop for ,s across ,vector)))
          (`((,k ,v) (:ht ,ht))
           (add-iter :ht `(cl-loop for ,k being the hash-key in ,ht
                                     using (hash-value ,v))))
          (`(,k (:st ,st))
           (add-iter :st `(cl-loop for ,k being the hash-key in ,st)))
          (`(,s ,(or `(:seq ,seq) seq))
           (add-iter :seq `(cl-loop for ,s being the elements of ,seq))))))
    (let* ((collects (--> collects
                          (--map (append it `(into ,result)) it)
                          (-interleave it (-cycle '(and)))
                          (-flatten-n 1 it))))
      `(cl-loop named ,name
                ,@(and (not collects) `(with ,result = nil))
                ,@loop-clauses
                when ,(if whens `(and ,@whens) t)
                  ,@collects
                do ,form
                finally (cl-return-from ,name ,result)))))

(defmacro pfor-do (clauses &rest body)
  (declare (indent 1))
  (pfor--expand clauses `(progn ,@body)))

(defmacro pfor (clauses &rest body)
  (declare (indent 1))
  `(pfor-do (,@clauses (:collect (progn ,@body)))))

(defun for--expand-clause (clause form)
  (cl-flet ((make-loop (form clauses &optional vars)
              ;; NOTE: We drop `cl-loop' from CLAUSES because we're using it
              ;; only to aid indentation.
              (let ((loop `(cl-loop named ,(gensym) ,@(cdr clauses) do ,form)))
                (if vars `(let ,vars ,loop) loop))))
    (pcase-exhaustive clause
      (`,(or (and (pred vectorp) vec (let clauses (cl-coerce vec 'list)))
             `(:par ,clauses))
       (let ((clauses (if (cl-find :named clauses :key #'car)
                          clauses
                        `((:named ,(gensym)) ,@clauses))))
         `(pfor-do ,clauses ,form)))
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
      (`(,s (:step . ,step))
       (cl-destructuring-bind (init step &optional cond) step
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
      (`(,k (:st ,st))
       (make-loop form `(cl-loop for ,k being the hash-key in ,st)))
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

(defun vecify (seq)
  (cl-coerce seq 'vector))

(defun vmap (func &rest vs)
  (apply #'cl-map 'vector func vs))

(cl-macrolet ((define-v-arith-fun (name op)
                `(defun ,name (&rest vs)
                   (apply #'vmap #',op vs)))
              (define-v-bool-fun (name op &key (reduce 'cl-every))
                `(defun ,name (&rest vs)
                   (,reduce #'identity (apply #'vmap #',op vs)))))
  (define-v-arith-fun v+ +)
  (define-v-arith-fun v- -)
  (define-v-arith-fun v* *)
  (define-v-arith-fun v/ /)
  (define-v-arith-fun vmod mod)
  (define-v-arith-fun vclamp clamp)
  (define-v-arith-fun vwrap wrap)
  (define-v-bool-fun v< <)
  (define-v-bool-fun v<= <=)
  (define-v-bool-fun v> >)
  (define-v-bool-fun v>= >=)
  (define-v-bool-fun v= =)
  (define-v-bool-fun v/= /= :reduce cl-some))

(cl-macrolet ((define-v-arith-macro (n name op)
                `(defmacro ,name (&rest vs)
                   (let* ((syms (mmt-make-gensym-list (length vs)))
                          (exprs (for ((i (:range ,n)))
                                   `(,',op ,@(for ((s syms)) `(aref ,s ,i))))))
                     `(let ,(-zip-lists syms vs)
                        (vector ,@exprs)))))
              (define-v-bool-macro (n name op &key (reduce 'and))
                `(defmacro ,name (&rest vs)
                   (let* ((syms (mmt-make-gensym-list (length vs)))
                          (exprs (for ((i (:range ,n)))
                                   `(,',op ,@(for ((s syms)) `(aref ,s ,i))))))
                     `(let ,(-zip-lists syms vs)
                        (,',reduce ,@exprs))))))
  (define-v-arith-macro 2 v2+ +)
  (define-v-arith-macro 2 v2- -)
  (define-v-arith-macro 2 v2* *)
  (define-v-arith-macro 2 v2/ /)
  (define-v-arith-macro 2 v2mod mod)
  (define-v-arith-macro 2 v2clamp clamp)
  (define-v-arith-macro 2 v2wrap wrap)
  (define-v-bool-macro 2 v2< <)
  (define-v-bool-macro 2 v2<= <=)
  (define-v-bool-macro 2 v2> >)
  (define-v-bool-macro 2 v2>= >=)
  (define-v-bool-macro 2 v2= =)
  (define-v-bool-macro 2 v2/= /= :reduce or)
  (define-v-arith-macro 3 v3+ +)
  (define-v-arith-macro 3 v3- -)
  (define-v-arith-macro 3 v3* *)
  (define-v-arith-macro 3 v3/ /)
  (define-v-arith-macro 3 v3mod mod)
  (define-v-arith-macro 3 v3clamp clamp)
  (define-v-arith-macro 3 v3wrap wrap)
  (define-v-bool-macro 3 v3< <)
  (define-v-bool-macro 3 v3<= <=)
  (define-v-bool-macro 3 v3> >)
  (define-v-bool-macro 3 v3>= >=)
  (define-v-bool-macro 3 v3= =)
  (define-v-bool-macro 3 v3/= /= :reduce or))

(defun vrev (v)
  (let ((len (length v))
        (rev (copy-sequence v)))
    (cl-loop for i from 0 below len
             do (setf (aref rev i) (aref v (- len 1 i)))
             finally (cl-return rev))))

(defun v2cw (v &optional about)
  ;; NOTE: Rotate by 90 degrees CW (in a right-handed coordinate system).
  (with-vref
    (let* ((v (if about (v2- v about) v))
           (rot `[,v.y ,(- v.x)]))
      (if about (v2+ rot about) rot))))

(defun v2ccw (v &optional about)
  ;; NOTE: Rotate by 90 degrees CCW (in a right-handed coordinate system).
  (with-vref
    (let* ((v (if about (v2- v about) v))
           (rot `[,(- v.y) ,v.x]))
      (if about (v2+ rot about) rot))))

;;; `with-vref'

(defun with-vref--symbol-search (regexp data)
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match regexp name)
        (list (s-match regexp name)))))
   ((vectorp data)
    (apply #'nconc (mapcar (-partial #'with-vref--symbol-search regexp) data)))
   ((not (consp data)) nil)
   ((eq (car data) 'with-vref)
    ;; NOTE: Ignore nested ‘with-vref’ forms.
    (with-vref--symbol-search regexp (cadr data)))
   (t (append (with-vref--symbol-search regexp (car data))
              (with-vref--symbol-search regexp (cdr data))))))

(defun with-vref--collect (data)
  (--> (rx (group (+ nonl)) "." (group (any "abcdijkxyzw0123")) eos)
       (with-vref--symbol-search it data)
       (--map (seq-let [sym base field] it
                (list (intern sym)
                      (intern base)
                      (with-vref--field-index field)))
              it)))

(defun with-vref--field-index (field)
  (pcase field
    ((rx (any "abcd"))
     (- (char (match-string 0 field)) ?a))
    ((rx (any "ijk"))
     (- (char (match-string 0 field)) ?i))
    ((rx (any "xyz"))
     (- (char (match-string 0 field)) ?x))
    ("w" 3)
    ((rx (any "0123"))
     (int (match-string 0 field)))))

(defun/s with-vref--symbol-macrolet ([sym base field])
  `(,sym (with-vref (aref ,base ,field))))

(defmacro with-vref (&rest body)
  (declare (indent 0))
  (let ((syms (delete-dups (with-vref--collect body))))
    `(cl-symbol-macrolet ,(mapcar #'with-vref--symbol-macrolet syms)
       ,@body)))

;;; (Dense) Tensors

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
           finally (cl-return (vecify d))))

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

;;; (2D Dense) Grids

(defun format-grid-elem (fmt elem &rest rest)
  (if (functionp fmt)
      (apply fmt elem rest)
    (format fmt elem)))

(cl-defun print-grid (grid &key (rowsep "\n") (colsep "") (fmt "%s") (pad " ")
                      &allow-other-keys)
  (let ((maxlen 0)
        (cache nil))
    (when pad
      (setf cache (vmap (lambda (s) (make-vector (length s) nil)) grid))
      (with-tensor (i j) grid
        (let* ((elem (v. grid i j))
               (string (format-grid-elem fmt elem i j)))
          (setf (v. cache i j) string
                maxlen (max maxlen (length string))))))
    (pcase-let ((`[,m ,n] (vdims grid)))
      (let ((final (if pad (format "%%%s%ds" pad maxlen) "%s")))
        (dotimes (i m)
          (dotimes (j n)
            (let ((elem (v. grid i j)))
              (printf final
                      (if pad (v. cache i j) (format-grid-elem fmt elem i j)))
              (when (/= j (1- n))
                (prind colsep))))
          (when (/= i (1- m))
            (prind rowsep)))))))

(defun color-gradient-hex (start stop n)
  (let ((start (color-name-to-rgb start))
        (stop (color-name-to-rgb stop)))
    (vmap (-applify #'color-rgb-to-hex)
          (append (and (>= n 1) (list start))
                  (color-gradient start stop (- n 2))
                  (and (>= n 2) (list stop))))))

(cl-defun print-grid-binary (grid &rest keys
                             &key (fmt "%d") (pred #'non-zero-p) (true "white")
                               (false "black") &allow-other-keys)
  (cl-flet ((fmt (x &rest rest)
              (--> `(:foreground ,(if (funcall pred x) true false))
                   (propertize (apply #'format-grid-elem fmt x rest)
                               'font-lock-face it)
                   (format it x))))
    (->> `(:fmt ,#'fmt ,@keys :colsep " ")
         (apply #'print-grid grid))))

(cl-defun print-grid-heatmap (grid &rest keys
                              &key (fmt "%d") (high "white") (low "black")
                              &allow-other-keys)
  (let ((min nil)
        (max nil))
    (with-tensor (i j) grid
      (setf min (min* min (v. grid i j))
            max (max* max (v. grid i j))))
    (let ((colors (color-gradient-hex low high (1+ (- max min)))))
      (cl-flet ((fmt (elem &rest rest)
                  (--> `(:foreground ,(v. colors (- elem min)))
                       (propertize (apply #'format-grid-elem fmt elem rest)
                                   'font-lock-face it)
                       (format it elem))))
        (->> `(:fmt ,#'fmt ,@keys :colsep " ")
             (apply #'print-grid grid))))))

;;; Sparse Tensors

(defun bounds (table)
  (when-let* ((len (cl-block nil
                     (ht-each (lambda (k _) (cl-return (length k))) table)))
              (vmin (make-vector len nil))
              (vmax (make-vector len nil)))
    (ht-each (lambda (k _)
               (setf vmin (vmap #'min* vmin k)
                     vmax (vmap #'max* vmax k)))
             table)
    (list vmin vmax)))

;;; Hash Tables

(defalias 'h. 'ht-get*)

(gv-define-setter h. (value table &rest keys)
  `(setf (ht-get* ,table ,@keys) ,value))

;;; Sets

(defun setify (seq)
  (let ((set (ht)))
    (prog1 set
      (seq-doseq (e seq)
        (ht-set set e t)))))

(defun st (&rest args)
  ;; NOTE: We cannot use the name `set' because it's an Elisp primitive.
  (setify args))

(cl-define-compiler-macro st (&rest args)
  `(ht ,@(cl-loop for a in args collect `(,a t))))

(defalias 'set-copy 'ht-copy)

(defalias 'set-size 'ht-size)

(defalias 'set-empty-p 'ht-empty-p)

(defun set-add (set key)
  (ht-set set key t))

(defun set-with (set &rest keys)
  (let ((new (ht-copy set)))
    (prog1 new
      (dolist (key keys)
        (set-add new key)))))

(defun set-remove (set key)
  (ht-remove set key))

(defun set-without (set &rest keys)
  (let ((new (ht-copy set)))
    (prog1 new
      (dolist (key keys)
        (set-remove new key)))))

(defalias 's. 'ht-contains-p)

(gv-define-setter s. (value set key)
  (mmt-once-only (set key)
    `(if ,value
         (set-add ,set ,key)
       (set-remove ,set ,key))))

(defun set-intersection-2 (x y)
  (let ((new (ht)))
    (cl-destructuring-bind (smaller bigger)
        (if (< (ht-size x) (ht-size y))
            (list x y)
          (list y x))
      (ht-each (lambda (key _)
                 (when (ht-contains-p bigger key)
                   (setf (h. new key) t)))
               smaller))
    (and (not (ht-empty-p new)) new)))

(defun set-intersection (set &rest sets)
  (let* ((new (ht))
         (smallest (cl-loop with smallest = set
                            for s in sets
                            do (when (< (ht-size s) (ht-size smallest))
                                 (setf smallest s))
                            finally (cl-return smallest)))
         (sets (if (eq set smallest)
                   sets
                 (cl-remove (cons set sets) :test #'eq))))
    (ht-each (lambda (key _)
               (when (--every (ht-contains-p it key) sets)
                 (setf (h. new key) t)))
             set)
    (and (not (ht-empty-p new)) new)))

(cl-define-compiler-macro set-intersection (x y)
  `(set-intersection-2 ,x ,y))

(defalias 'set-union 'ht-merge)

(defalias 'set-nunion 'ht-update)

(defun set-map (func set)
  (let ((new (st)))
    (prog1 new
      (ht-each (lambda (key _)
                 (set-add new (funcall func key)))
               set))))

;;; `with-sref'

(defun with-sref--symbol-search (regexp data)
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match regexp name)
        (list (s-match regexp name)))))
   ((vectorp data)
    (apply #'nconc (mapcar (-partial #'with-sref--symbol-search regexp) data)))
   ((not (consp data)) nil)
   ((eq (car data) 'with-sref)
    ;; NOTE: Ignore nested ‘with-sref’ forms.
    (with-sref--symbol-search regexp (cadr data)))
   (t (append (with-sref--symbol-search regexp (car data))
              (with-sref--symbol-search regexp (cdr data))))))

(defun with-sref--collect (data)
  (--> (rx (group (+ nonl)) "." (group (+ nonl)) eos)
       (with-sref--symbol-search it data)
       (--map (seq-let [sym base field] it
                (list (intern sym) (intern base) field))
              it)))

(defun/s with-sref--symbol-macrolet (conc-name [sym base field])
  `(,sym (with-sref ,conc-name (,(symcat conc-name "-" field) ,base))))

(defmacro with-sref (conc-name &rest body)
  (declare (indent 1))
  (let ((syms (delete-dups (with-sref--collect body))))
    `(cl-symbol-macrolet
         ,(mapcar (-cut with-sref--symbol-macrolet conc-name <>) syms)
       ,@body)))

;;; `with-svref'

(defmacro with-svref (conc-name &rest body)
  (declare (indent 1))
  `(with-sref ,conc-name
     ;; NOTE: We force the macroexpansion of `with-vref' so that it picks out
     ;; and expands whatever it can detect (since it only deals with predefined
     ;; fields such as `x', `i', etc.), and then we let `with-sref' deal with
     ;; the rest. Forcing the expansion is important so that `with-sref' doesn't
     ;; erroneously treat the special `with-vref' fields as struct slots.
     ,(macroexpand-all `(with-vref ,@body) macroexpand-all-environment)))

;;; Graphs

(cl-defstruct (graph (:constructor graph-create (adjacent &optional nodes))
                     (:copier nil))
  nodes adjacent)

(defun graph-floyd-warshall (graph)
  (let ((nodes (graph-nodes graph))
        (dist (ht)))
    (prog1 dist
      (for-do ((u nodes))
        (setf (h. dist u) (ht (u 0)))
        (for-do (((v cost) (funcall (graph-adjacent graph) u)))
          (setf (h. dist u v) (or cost 1))))
      (for-do ((k nodes)
               (i nodes)
               (j nodes))
        (when-let ((d1 (h. dist i k))
                   (d2 (h. dist k j)))
          (let ((d (h. dist i j)))
            (setf (h. dist i j)
                  (if d (min d (+ d1 d2)) (+ d1 d2)))))))))

;;; Memoization

(defun memoize (func)
  (let ((table (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((value (ht-get table args)))
        (or value (setf (ht-get table args) (apply func args)))))))

;;; Buffers

(defmacro with-buffer (expr &rest body)
  (declare (indent 1))
  (mmt-once-only ((expr (if (member expr '(nil _)) nil expr)))
    `(with-current-buffer (if ,expr
                              (get-buffer-create ,expr t)
                            (generate-new-buffer " *temp*" t))
       (let ((standard-output (current-buffer)))
         ,@body))))

(provide 'aoc-util)
