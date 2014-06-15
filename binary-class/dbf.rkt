#lang racket/base
(require racket/class racket/contract binary-class)
(provide dbopen read-record)

(define (select-db-driver db-type)
  (case db-type
    [(#x2) foxbase%]
    [(#x3 #x83) dbase3%]
    [(#x4 #x7B #x8B #x8E) dbase4%]
    [(#x30 #x31 #x32 #xF5) visual-foxpro%]
    [else dbase3%]))

(define xbase-common%
  (class object%
    (super-new)
    (field [stream #f] [external-format #f])))

(define-binary-class dbf-header% xbase-common%
  ((db-type l1))
  #:dispatch (select-db-driver db-type))

(define (convert from datum)
  (define conv #f)
  (dynamic-wind
   (λ() (set! conv (bytes-open-converter from "")))
   (λ() (let-values ([(a b c) (bytes-convert conv datum)]) (bytes->string/locale a)))
   (λ() (bytes-close-converter conv))))

(define (default-translate driver field datum) 
  (send driver translate-field-datum field datum))

(define-binary-class dbase3-header% dbf-header%
  ((year l1)
   (month l1)
   (day l1)
   (records-count l4)
   (header-size l2)
   (record-size l2)
   (_ (discard 2))
   (transaction l1)
   (code l1)
   (_ (discard 12))
   (indexed l1)
   (code-page l1)
   (_ (discard 2)))
  (define/public (goto-record n)
    (file-position (get-field stream this)
                   (+ header-size (* n record-size))))
  (define/public (translate-field-datum field datum)
    (case (integer->char (get-field field-type field))
      ((#\I #\M) datum)
      (else
       (convert (external-format this) datum))))
  (define/public (read-field-datum field [translate default-translate])
    (define stream (get-field stream this))
    (case (integer->char (get-field field-type field))
      [(#\I) (translate this field (read-value l4 stream))]
      [else (translate this field (read-bytes (get-field size field) stream))])))

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
       (write-byte #xd out)))))

(define-binary-class xbase-field%
  ((name (db-field-name 11))
   (field-type l1)
   (_ (discard 4))
   (size l1)
   (precision l1)
   (_ (discard 2))
   (workspace l1)
   (multi-user l2)
   (set-fields l1)
   (_ (discard 7))
   (index l1)))

(define-binary-class visual-foxpro-field%
  ((name (db-field-name 11))
   (field-type l1)
   (_ (discard 4))
   (size l1)
   (precision l1)
   (flags l1)
   (autoincrement-next-value l4)
   (autoincrement-step-value l1)
   (_ (discard 8))))

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

(define xbase-memo-common% 
  (class xbase-common%
    (super-new)
    (field [code-page #f])
    (define/public (goto-record n) 
      (file-position (get-field stream this) (* n 512)))
    (define/public (translate-field-datum field datum)
      (convert (external-format this) datum))))

(define-binary-class dbt-header% xbase-memo-common%
  ((next-available-block u4)))

(define-binary-class dbase3-memo% dbt-header%
  ((_ (discard 508))))

(define-binary-class dbase4-memo% dbt-header%
  ((record-size l4)
   (_ (discard 504))))

(define-binary-class visual-foxpro-memo% xbase-memo-common%
  ((next-available-block u4)
   (reserved1 u2)
   (record-size u2)
   (_ (discard 504))))

(define (dbopen stream)
  (file-position stream 0)
  (define db (read-value dbf-header% stream))
  (set-field! stream db stream)
  db)

(define (dbopen-memo stream type code-page)
  (file-position stream 0)
  (define memo (read-value type stream))
  (set-field! stream memo stream)
  (set-field! code-page memo code-page)
  memo)

(define (goto-bof driver)
  (file-position (get-field stream driver) (get-field header-size driver)))

(define (external-format driver)
  (or (get-field external-format driver)
      (format "cp~a"
              (case (get-field code-page driver)
                ((2)    850)
                ((3)    1252)
                ((#x64) 852)
                ((#x65) 865)
                ((#x66) 866)
                ((#xC8) 1250)
                ((#xC9) 1251)
                (else 437)))))

(define (read-record driver [translate default-translate])
  (define stream (get-field stream driver))
  (define byte (read-byte stream))
  (cond 
    [(eof-object? byte) #f]
    [(= 32 byte) (for/vector ([field (in-list (get-field fields driver))])
                   (send driver read-field-datum field translate))]
    [else ; deleted record, skip and read again
     (file-position stream
                    (+ (file-position stream)
                       (sub1 (get-field record-size driver))))
     (read-record driver translate)]))
