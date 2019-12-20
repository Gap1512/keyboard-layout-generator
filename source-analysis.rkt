#lang racket

(require math racket/system "utilities.rkt")

(provide pdf->txt multiple-files-frequencies format-source multiple-files-pdf->txt relative-frequency multiple-files-frequencies-op)

(define (pdf->txt input-path output-path)
  (system (string-append "gswin64c -sDEVICE=txtwrite -o \"" output-path "\" \"" input-path "\"")))

(define (multiple-files-pdf->txt input-path-list gauge)
  (for-each (λ (path)
              (begin
                (pdf->txt path (path->string (path-replace-extension path #".txt")))
                (send gauge set-value (add1 (send gauge get-value)))))
            input-path-list))

;(multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt"))
(define (multiple-files-frequencies list-of-files (gauge #f) (group-size 1) (valid ALPHABET))
  (let-values ([(samples frequencies) (count-samples (read-multiple-groups list-of-files gauge group-size valid))])
    (map list samples frequencies)))
  
(define (read-multiple-groups list-of-files gauge group-size valid)
  (letrec ((rec (λ (lst result)
                  (cond
                    ((empty? lst) result)
                    (else (rec (cdr lst) (append (group (read-chars (car lst) valid gauge) group-size) result)))))))
    (rec list-of-files '())))

(define (format-source f-lst)
  (letrec ((rec (λ (lst chrs freqs)
                  (cond
                    ((empty? lst) (list chrs freqs))
                    (else (rec (cdr lst)
                            (cons (string (caaar lst)) chrs)
                            (cons (number->string (cadar lst)) freqs)))))))
    (rec f-lst '() '())))

(define (read-chars path valid gauge)
  (when gauge (send gauge set-value (add1 (send gauge get-value))))
  (filter (λ (c) (member c valid)) (map char-upcase (port->list read-char (open-input-file path)))))

;(relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
(define (relative-frequency letters)
  (let ((total (apply + (map second letters))))
    (map (λ (term) (list-set term 1 (/ (second term) total))) letters)))

;(multiple-files-frequencies-op '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\bn000011.txt"))
(define (multiple-files-frequencies-op list-of-files (gauge #f) (valid ALPHABET))
  (letrec ((rec (λ (lst result)
                  (cond
                    ((empty? lst) result)
                    (else (rec (cdr lst) (file-frequency (car lst) result valid gauge)))))))
    (rec list-of-files '(()()))))

(define (file-frequency file result valid gauge)
  (when gauge (send gauge set-value (add1 (send gauge get-value))))
  (letrec ((rec (λ (chars result)
                  (cond
                    ((empty? chars) result)
                    ((empty? (cdr chars)) (incfrequency (char-upcase (first chars)) #f result valid))
                    (else (rec (cdr chars) (incfrequency (char-upcase (first chars)) (char-upcase (second chars)) result valid)))))))
    (rec (port->list read-char (open-input-file file)) result)))

(define (incfrequency f-char s-char frequencies valid)
  (let ((f-valid (member f-char valid))
        (s-valid (member s-char valid))
        (single-freq (first frequencies))
        (pair-freq (second frequencies)))
    (list (if f-valid (incf (list f-char) single-freq) single-freq)
          (if (and f-valid s-valid) (incf (list f-char s-char) pair-freq) pair-freq))))

(define (incf term lst)
  (letrec ((rec (λ (lst result)
                  (cond
                    ((empty? lst) (cons (list term 1) result))
                    ((equal? term (caar lst)) (append (cdr lst) (cons (list-update (car lst) 1 add1) result)))
                    (else (rec (cdr lst) (cons (car lst) result)))))))
    (rec lst '())))