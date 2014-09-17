binary-class-dbf
================

Interface for reading/writing DBF (DBase, Foxbase, ...) files

To test run

```racket
(require binary-class/dbf binary-class)
(define stream (open-input-file "test.dbf")) ; put your DBF file instead of test.dbf
(define db (read-value dbf-header% stream))
(send db read-record stream) ; returns first record
(send db read-record stream) ; returns second
```

and so on. To inspect db object you may use get-field. List of field see inside sources.
