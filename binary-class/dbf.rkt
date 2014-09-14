#lang racket/base
(require racket/class racket/contract binary-class fast-convert 
         (for-syntax racket/base racket/contract))
(provide dbf-header%
         change-code-page!)

(define-binary-class dbf-header% object%
  ((db-type l1))
  #:dispatch (case db-type
               [(#x2) foxbase%]
               [(#x3 #x83) dbase3%]
               [(#x4 #x7B #x8B #x8E) dbase4%]
               [(#x30 #x31 #x32 #xF5 #xFB) visual-foxpro%]
               [else dbase3%])
  (define/public (goto-eof!) #f)
  (define/public (goto-record!) #f)
  (define/public (read-record) #f)
  (define/public (set-code-page!) #f))

(define (get-code-page desc)
  (case desc
    ((2)    (values "cp850" cp850))
    ((3)    (values "cp1252" cp1252))
    ((#x64) (values "cp852" cp852))
    ((#x65) (values "cp865" cp865))
    ((#x66) (values "cp866" cp866))
    ((#xC8) (values "cp1250" cp1250))
    ((#xC9) (values "cp1251" cp1251))
    (else (values "cp437" cp437))))

(define (change-code-page! dbf code-page)
  (cond 
    [(symbol? code-page)
     (change-code-page! dbf (case code-page
                             [(cp850) 2]
                             [(cp1252) 3]
                             [(cp852) #x64]
                             [(cp865) #x65]
                             [(cp866) #x66]
                             [(cp1250) #xC8]
                             [(cp437) #xC9]))]
    [else
     (send dbf set-code-page! code-page)]))

(define-binary-class dbase3-header% dbf-header%
  ([year l1]
   [month l1]
   [day l1]
   [records-count l4]
   [header-size l2]
   [record-size l2]
   [_ (discard 2)]
   [transaction l1]
   [code l1]
   [_ (discard 12)]
   [indexed l1]
   [code-page l1]
   [_ (discard 2)]
   [(charset code-page-table) (get-code-page code-page)])
  
  (define/override (set-code-page! Code-page)
    (set! code-page Code-page)
    (set!-values (charset code-page-table) (get-code-page Code-page)))
  
  (define/override (goto-record! stream n)
    (file-position stream
                   (+ header-size (* n record-size))))
  
  (define (read-field-datum stream field translate)
    (define data (read-value (get-field field-binary field) stream))
    (if (bytes? data)
        (translate code-page-table data)
        data))
    
  (define/override (goto-eof! stream)
    (goto-record! stream records-count))

  (define/override (read-record stream [translate convert])
    (define byte (read-byte stream))
    (cond 
      [(eof-object? byte) #f]
      [(= 32 byte) (for/vector ([field (in-list fields)])
                     (read-field-datum field translate))]
      [else ; deleted record, skip and read again
       (file-position stream
                      (+ (file-position stream)
                         (sub1 record-size)))
       (read-record stream translate)])))

(define (db-field-name length)
  (binary
   (λ (in)
     (define first-byte (peek-byte in))
     (when (= first-byte #xd) (raise 'in-padding))
     (define name (read-value (bytestring length) in))
     (define nul-char-position 
       (for/first ([i (in-naturals)] 
                   [c (in-bytes name)] 
                   #:when (zero? c)) i))
     (if nul-char-position
         (subbytes name 0 nul-char-position)
         name))
   (λ (out id)
     (define id-length (bytes-length id))
     (write-value (bytestring id-length) out id)
     (for [(i (in-range (- length id-length)))]
       (write-byte 0 out)))))

(define integer-field l4)
(define boolean-field
  (binary (λ (in)
            (case (read-value iso-8859-1-char in)
              [(#\T #\t #\Y #\y) #t]
              [(#\F #\f #\N #\n) #f]
              [else (void)]))
          (λ (out bool)
            (write-char (cond 
                          [(void? bool) #\?]
                          [bool #\T] 
                          [else #\F]) out))))
(define string-field bytestring)

(define (pad0 size string on-error)
  (define strlen (string-length string))
  (when (> strlen size)
    (on-error))
  (if (< strlen size)
      (string-append (make-string (- size strlen) #\0) string)
      string))

(define (number-error size precision value)
  (λ ()
    (raise-argument-error 'number-field 
                          (format "number with total length ~a and precision ~a"
                                  size
                                  precision)
                          value)))

(define (number-field size precision)
  (binary (λ (in)
            (string->number (bytes->string/latin-1 (read-bytes size in))))
          (λ (out value)
            (write (pad0 size 
                         (real->decimal-string value precision)
                         (number-error size precision value)) 
                   out))))

(define (get-field-binary field-type size precision)
  (case field-type
    [(#\I) integer-field]
    [(#\L) boolean-field]
    [(#\C) (string-field size)]
    [(#\N) (number-field size precision)]
    [else (raise (make-exn:fail:unsupported 
                  (format "Field type `~a' unsupported" field-type)
                  (current-continuation-marks)))]))

(define-binary-class xbase-field%
  ((name (db-field-name 11))
   (field-type iso-8859-1-char)
   (_ (discard 4))
   (size l1)
   (precision l1)
   (_ (discard 2))
   (workspace l1)
   (multi-user l2)
   (set-fields l1)
   (_ (discard 7))
   (index l1)
   (field-binary (get-field-binary field-type size precision))))

(define-binary-class visual-foxpro-field%
  ((name (db-field-name 11))
   (field-type l1)
   (_ (discard 4))
   (size l1)
   (precision l1)
   (flags l1)
   (autoincrement-next-value l4)
   (autoincrement-step-value l1)
   (_ (discard 8))
   (field-binary (get-field-binary field-type size precision))))

(define (read-field type in)
  (with-handlers ([(λ (x) (eq? x 'in-padding)) (λ (e) #f)])
    (read-value type in)))

(define (fields type length)
  (binary
   (λ (in)
     (let loop ([to-read length] [collect null])
       (cond 
         [(positive? to-read)
          (define field (read-field type in))
          (if field 
              (loop (- to-read 32) (cons field collect))
              (reverse collect))]
         [else (reverse collect)])))
  (λ (out frames)
    (let loop ([to-write length] [frames frames])
       (cond 
         [(null? frames)
          (for ([i (in-range to-write)]) (write-byte #xd out))]
         [else 
          (write-value type out (car frames))
          (loop (- to-write 32) (cdr frames))])))))

(define (xbase-fields length) (fields xbase-field% length))
(define (visual-foxpro-fields length) (fields visual-foxpro-field% length))

(define-binary-class dbase3% dbase3-header%
  ((fields (xbase-fields header-size))))

(define-binary-class dbase4% dbase3-header%
  ((fields (xbase-fields header-size))))

(define-binary-class foxbase% dbase3-header%
  ((fields (xbase-fields header-size))))

(define-binary-class visual-foxpro% dbase3-header%
  ((fields (visual-foxpro-fields header-size))))

(gen-tables cp437 cp850 cp852 cp865 cp866 cp1250 cp1251 cp1252)

#|
Stuff for MEMO fields. Doesn't work yet

(define xbase-memo-common% 
  (class xbase-common%
    (super-new)
    (field [code-page #f])
    (define/public (goto-record n) 
      (file-position (get-field stream this) (* n 512)))
    (define/public (translate-field-datum field datum)
      (convert cp437 datum))))

(define-binary-class dbt-header% xbase-memo-common%
  ((next-available-block u4)))

(define-binary-class dbase3-memo% dbt-header%
  ((_ (discard 508))))

(define-binary-class dbase4-memo% dbt-header%
  ((record-size l4)
   (_ (discard 504))))

(define-binary-class visual-foxpro-memo% xbase-memo-common%
  ((next-available-block u4)
   (_ (discard 2))
   (record-size u2)
   (_ (discard 504))))

(define (dbopen-memo stream type code-page)
  (file-position stream 0)
  (define memo (read-value type stream))
  (set-field! stream memo stream)
  (set-field! code-page memo code-page)
  memo)
|#
