
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




(defpackage :pixmessage 
  (:use :cl :lispbuilder-sdl))


(in-package :pixmessage)

(defparameter *charicter-codes* '(:EOF #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\. #\, #\? #\' #\$ #\% #\( #\) #\Newline #\Space #\+ #\- #\* #\/ #\\ #\[ #\] #\{ #\} #\!))

(defun encode-charicter-to-color (color charicter)
  (assert (find charicter *charicter-codes*) () "Could'nt find charicter code for ~a." charicter)
  (encode-number-to-color color (position charicter *charicter-codes*)))

(defun decode-color-to-charicter (color)
  (elt *charicter-codes* (decode-color-to-number color)))
	  


(defun encode-byte-to-color (color1 color2 datum)
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
  (let ((nibble1 (decode-color-to-number color1))
	(nibble2 (decode-color-to-number color2)))
    (if (or (= nibble1 0)
	    (= nibble2 0))
	:eof
	(logior (ash (ldb (byte 4 0) nibble1) 4) (ldb (byte 4 0) nibble2)))))

(defun encode-number-to-color (color number)
  (assert (< number 128) () "Number cannot be larger then 6 bits!")
  (color :r (dpb (ldb (byte 2 0) number) (byte 2 0) (r color))
	 :g (dpb (ldb (byte 2 2) number) (byte 2 0) (g color))
	 :b (dpb (ldb (byte 2 4) number) (byte 2 0) (b color))))



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
