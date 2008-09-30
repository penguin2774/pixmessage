
;; This software is distributed under the MIT License

;; Copyright (c) 2008 Nathanael D. Cunningham

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(eval-when (:compile-toplevel)
  (require 'lispbuilder-sdl)
  (require 'split-sequence))

(defpackage :pixmessage 
  (:use :cl :lispbuilder-sdl :split-sequence ))


(in-package :pixmessage)

(defparameter *charicter-codes* '(:EOF #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\. #\, #\? #\' #\$ #\% #\( #\) #\Newline #\Space #\+ #\- #\* #\/ #\\ #\[ #\] #\{ #\} #\!)
  "Contains all the charicters that are avalible for encoding with encode-message. Note: if you add any, add them to the end or you will ruin all your previously encoded messages!")

(defun load-word-list (filename)
  (with-open-file (file filename)
    (loop 
       for sym = (read file nil :EOF)
       unless (eq sym :EOF)
       collect sym into result
       when (eq sym :EOF)
       return result)))

(defparameter *word-list* (load-word-list "words.list"))
(defparameter *running-word-list* *word-list*)


(defun save-word-list (filename)
  (with-open-file (file filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for i in *running-word-list*
       do (print i file)))
  (setf *word-list* *running-word-list*))

(defun encode-charicter-to-color (color charicter)
  "Encodes the CHARICTER using *charicter-codes* into COLOR"
  (assert (find charicter *charicter-codes*) () "Could'nt find charicter code for ~a." charicter)
  (encode-number-to-color color (position charicter *charicter-codes*)))

(defun decode-color-to-charicter (color)
  "Decodes a charicter from a color using *charicter-codes*"
  (elt *charicter-codes* (decode-color-to-number color)))
	  


(defun encode-byte-to-color (color1 color2 datum)
  "Encodes a byte into 2 colors."
  (let ((color-1-bits (ldb (byte 4 4) datum))
	(color-2-bits (ldb (byte 4 0) datum)))
    (values (encode-number-to-color color1 
				    (if (= color-1-bits 0)
					(logior color-1-bits 16) ;avoid 0s
					(logior color-1-bits (ash (ldb (byte 2 0) (b color1)) 4))))
	    (encode-number-to-color color2 
				    (if (= color-2-bits 0)
					(logior color-2-bits 16)
					(logior color-2-bits (ash (ldb (byte 2 0) (b color2)) 4)))))))

(defun decode-color-to-byte (color1 color2)
  "Decodes a byete from 2 colors"
  (let ((nibble1 (decode-color-to-number color1))
	(nibble2 (decode-color-to-number color2)))
    (if (or (= nibble1 0)
	    (= nibble2 0))
	:eof
	(logior (ash (ldb (byte 4 0) nibble1) 4) (ldb (byte 4 0) nibble2)))))

(defun encode-number-to-color (color number)
  "Encodes a number into a color, cant be bigger then 6 bits (<64)"
  (assert (< number 64) () "Number cannot be larger then 6 bits!")
  (color :r (dpb (ldb (byte 2 0) number) (byte 2 0) (r color))
	 :g (dpb (ldb (byte 2 2) number) (byte 2 0) (g color))
	 :b (dpb (ldb (byte 2 4) number) (byte 2 0) (b color))))

(defun get-number-of-colors-needed (num)
  (ceiling (/ (+ 2 (integer-length num)) 6)))

(defun encode-multi-color-number-first-color (color size-needed num)
  (assert (<= size-needed 4))
  (encode-number-to-color color (dpb num (byte 4 2) (1- size-needed))))

(defun decode-multi-color-number-first-color (color)
  (let ((num (decode-color-to-number color)))
    (if (= num 0)
	:END-OF-FILE
	(values (1+ (ldb (byte 2 0) num)) (ldb (byte 4 2) num)))))

(defun encode-multi-color-number (number colors)
  (let* ((colors-needed (get-number-of-colors-needed number))
	(result (list (encode-multi-color-number-first-color 
		       (first colors) 
		       colors-needed 
		       (ldb (byte 4 0) number)))))
    (assert (= (length colors) colors-needed))
    (if (= number 1)
	result
	(append result 
		(loop for i in (rest colors)
		     for pos = 4 then (+ pos 6)
		     collect (encode-number-to-color i (ldb (byte 6 pos) number)))))))
		     
(defun decode-multi-color-number (colors)
  (multiple-value-bind (size num) (decode-multi-color-number-first-color (first colors))
    (assert (= size (length colors)))
    (loop for i in (rest colors)
       for pos = 4 then (+ pos 6)
       with result = num
       do (setf result (dpb (decode-color-to-number i) (byte 6 pos) result))
       finally 
	 (return result))))



(defun decode-color-to-number (color)
  (dpb (ldb (byte 2 0) (b color)) (byte 2 4) (dpb (ldb (byte 2 0) (g color)) (byte 2 2) (dpb (ldb (byte 2 0) (r color)) (byte 2 0) 0))))
    


(defun embed-message (string filename result-file)
  (with-init (sdl-init-video)
    (let ((image-surface (load-image filename))
	  (final-string (string-upcase string)))
      (loop repeat (length final-string) 
	 for i = 0 then (1+ i)
	 for x = 0 then (mod i (width image-surface))
	 for y = 0 then (floor (/ i (width image-surface)))
	 do (assert (< y (height image-surface)) ((width image-surface) (height image-surface) (elt final-string i) i)
		    "Message is too big for image!")
	   (draw-pixel-* x y
			 :color (encode-charicter-to-color (read-pixel-* x y :surface image-surface) (elt final-string i))
			 :surface image-surface)
	 finally
	   (let ((x (mod (1+ i) (width image-surface)))
		 (y (floor (/ (1+ i) (width image-surface)))))
	     (draw-pixel-* x y :color (encode-number-to-color (read-pixel-*  x y :surface image-surface) 0) :surface image-surface))) ; 0 = EOF
      (save-image image-surface result-file))))

(defun read-embedded-message (filename)
  (with-init (sdl-init-video)
    (let ((img (load-image filename))
	  (result (make-string-output-stream)))
      (loop for i = 0 then (1+ i)
	 for x = 0 then (mod i (width img))
	 for y = 0 then (floor (/ i (width img)))
	 for data = (decode-color-to-charicter (read-pixel-* x y :surface img))
	 when (and (>= x (width img))
		   (>= y (height img)))
	 do (error "No EOF in message (probebly corrupted).")
	 unless (eq data :EOF)
	 do (write-char data result)
	 when (eq data :EOF)
	 return (get-output-stream-string result)))))
	   

  
(defun embed-file (filename image-filename result-file)
  (with-init (sdl-init-video)
    (with-open-file (file filename :element-type '(unsigned-byte 8))
      (let ((image-surface (load-image image-filename)))
	(assert (> (1+ (/ (* (height image-surface) (width image-surface)) 2)) (file-length file)) () "File is too big for image by ~a bytes!" 
		(- (1+ (/ (* (height image-surface) (width image-surface)) 2)) (file-length file)))
	(loop repeat (file-length file)
	   for i = 0 then (+ i 2)
	   for x1 = 0 then (mod i (width image-surface))
	   for y1 = 0 then (floor (/ i (width image-surface)))
	   for x2 = 1 then (mod (1+ i) (width image-surface))
	   for y2 = 0 then (floor (/ (1+ i) (width image-surface)))
	   do (assert (< y2 (height image-surface)) ((width image-surface) (height image-surface) i)
		      "File is too big for image!")
	     (multiple-value-bind (color1 color2) (encode-byte-to-color (read-pixel-* x1 y1 :surface image-surface)  
							(read-pixel-* x2 y2 :surface image-surface)
							(read-byte file))
	       (draw-pixel-* x1 y1
			     :color color1
			     :surface image-surface)
	       (draw-pixel-* x2 y2
			     :color color2
			     :surface image-surface))
	     
	   finally
	     (let ((x (mod (1+ i) (width image-surface)))
		   (y (floor (/ (1+ i) (width image-surface)))))
	       (draw-pixel-* x y :color (encode-number-to-color (read-pixel-*  x y :surface image-surface) 0) :surface image-surface))) ; 0 = EOF
	(save-image image-surface result-file)))))

(defun read-embedded-file (image-filename filename)
  (with-init (sdl-init-video)
    (with-open-file (file filename :direction :output :element-type '(unsigned-byte 8) :if-exists :overwrite :if-does-not-exist :create)
      (let ((img (load-image image-filename)))
	(loop for i = 0 then (+ i 2)
	   for x1 = 0 then (mod i (width img))
	   for y1 = 0 then (floor (/ i (width img)))
	   for x2 = 1 then (mod (1+ i) (width img))
	   for y2 = 0 then (floor (/ (1+ i) (width img)))
	   for data = (decode-color-to-byte (read-pixel-* x1 y1 :surface img) (read-pixel-* x2 y2 :surface img))
	   when (and (>= x2 (width img))
		     (>= y2 (height img)))
	   do (error "No EOF in message (probebly corrupted).")
	   unless (eq data :EOF)
	   do (write-byte data file)
	   when (eq data :EOF)
	   return t)))))


(defparameter *charicters-keywords-alist* '((#\. . :period) (#\, . :comma) (#\! . :exclamation) (#\? . :question) 
					     (#\( . :open-parenthesis) (#\) . :close-parenthesis) (#\newline . :end-of-line) (#\' . :apostrophe) (#\" . :quote) (#\: . :colon) (#\; . :semicolon)))
(defparameter *charicters-needing-conversion* (loop for i in *charicters-keywords-alist* collect (car i)))
(defparameter *charciters-keywords* (loop for i in *charicters-keywords-alist* collect (cdr i)))
(defparameter *charicter-conversion-chart* (loop for (char . word) in *charicters-keywords-alist* append `(,char ,word)))

(defun convert-words-to-numbers (words)
  (labels ((get-number-of-word (word)
	     (cond
	       ((find word *running-word-list*)
		(position word *running-word-list*))
	       (t
		(setf *running-word-list* (append *running-word-list* (list word)))
		(position word *running-word-list*)))))
    (loop for word in words
       collect (get-number-of-word word))))

(defun parse-string-to-words (string)
  (labels ((valid-charicter-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)
		 (equal #\Space char)
		 (loop for i in *charicters-needing-conversion*
		      when (equal i char)
		      return t)))
	   (number-string-p (number)
	     (loop for i across number always (digit-char-p i)))
	   (convert-numaric-to-words (word)
	     (let ((words (split-sequence-if (lambda (x) (or (equal x #\space)
							     (equal x #\-))) 
					     (format nil "~r" (parse-integer word)))))
	       (loop for i in words
		  collect (intern (string-upcase i)))))
	   (change-chars-to-notes (word-list)
	     (loop for i in word-list
		append (loop for cnc in  *charicters-needing-conversion*
			  with word = i
			  if (find cnc i)
			  collect (getf *charicter-conversion-chart* cnc) into flags and
			  do (setf word (remove cnc word))
			  finally
			    (cond 
			      ((number-string-p i)
			       (return (convert-numaric-to-words i)))
			      (t
			       (return (append (list (intern (string-upcase word))) flags)))))))) ; good place for a spell check!
    (convert-words-to-numbers (change-chars-to-notes (split-sequence #\Space (remove-if-not #'valid-charicter-p string))))))

(defun apply-controls-to-word (word controls)
  (let ((rcharicter-conversion-chart (reverse *charicter-conversion-chart*)))
    (loop for i in controls
       with new-word = word
       do (case i
	    ((:period :comma :exclamation  :question :close-parenthesis  :end-of-line :apostrophe  :quote :colon :semicolon)
	     (setf new-word (concatenate 'string new-word (string (getf rcharicter-conversion-chart i)))))
	    ((:open-parenthesis)
	     (setf new-word (concatenate 'string (string (getf rcharicter-conversion-chart i)) new-word)))
	    (t
	     (error "Unhandled operator ~a!" i)))
       finally
       (return new-word))))
	    

(defun convert-words-to-syms (numbers)
  (loop for i in numbers
       collect (elt *running-word-list* i)))


(defun convert-syms-to-strings (words)
  (let ((result (loop for i in words
		   for j on words
		   with skip-keywords = nil
		   if (and (keywordp i) (not skip-keywords))
		   collect (loop for k in j
			      while (keywordp k)
			      collect k 
			      finally
				(setf skip-keywords t))
		   else if (symbolp i)
		   collect (symbol-name i) and
		   do (setf skip-keywords nil))))
    (print result)
    (loop for i in result
	 for next in (cdr result)
	 if (stringp i) 
	   if (listp next)
             collect (apply-controls-to-word i next)
           else
             collect i)))

(defun convert-strings-to-string (strings)
  (format nil "~{~a~^ ~}" strings))


(defun parse-words-to-string (words)
  (loop for word in words
       

(defun embed-words-into-image (string image-file result)
  (with-init (sdl-init-video)
    (let ((words (parse-string-to-words string))
	  (img (load-image image-file)))
      (labels ((get-x (i &optional (diff 0))
		 (mod (+ diff i) (width img)))
	       (get-y (i &optional (diff 0))
		 (floor (/ (+ i diff) (width img))))
	       (avalible-pixles-p (i &optional (num 0))
		 (> (* (width img) (height img)) (+ num i))))
	(loop 
	   for word in words
	   for pixels-pos = 0 then (+ pixels-pos step)
	   for step = (get-number-of-colors-needed word)
	   for x = (get-x pixels-pos)
	   for y = (get-y pixels-pos)
	   do (loop for color in (encode-multi-color-number word (loop repeat step
								    for offset = 0 then (1+ offset)
								    collect (read-pixel-* (get-x pixels-pos offset) (get-y pixels-pos offset)
											  :surface img)))
		 for offset = 0 then (1+ offset)
		 do (draw-pixel-* (get-x pixels-pos offset) (get-y pixels-pos offset)
				  :color color
				  :surface img))
	   finally
	     (draw-pixel-* (get-x pixels-pos 1) (get-y pixels-pos 1) :color (encode-number-to-color  (read-pixel-* (get-x pixels-pos 1)
														   (get-y pixels-pos 1)
														   :surface img) 0)
			   :surface img))
	(if (y-or-n-p "You have added the following words:~%~{~a~%~} Would you like to save the word list?" (set-difference *running-word-list* *word-list*)) ;eek constent!
	    (save-word-list "words.list")))
      (save-image img result))))
  
(defun read-words-from-ebedded-image (image-file)
  (with-init (sdl-init-video)
    (let ((img (load-image image-file)))
       (labels ((get-x (i &optional (diff 0))
		 (mod (+ diff i) (width img)))
	       (get-y (i &optional (diff 0))
		 (floor (/ (+ i diff) (width img))))
	       (avalible-pixles-p (i &optional (num 0))
		 (> (* (width img) (height img)) (+ num i))))
	 (convert-strings-to-string 
	  (convert-syms-to-strings 
	   (convert-words-to-syms 
	    (loop  for pixels-pos = 0 then (+ pixels-pos step)
	       for x = (get-x pixels-pos)
	       for y = (get-y pixels-pos)
	       for pixel = (read-pixel-* x y :surface img)
	       for step = (decode-multi-color-number-first-color pixel)
	       if (eq step :END-OF-FILE)
	       return result
	       else
	       collect (decode-multi-color-number (loop repeat step
						     for i = 0 then (1+ i)
						     collect (read-pixel-* (get-x pixels-pos i) 
									   (get-y pixels-pos i) 
									   :surface img))) into result))))))))
			
		      
    