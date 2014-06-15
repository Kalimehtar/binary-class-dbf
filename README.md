binary-class-dbf
================

Interface for reading/writing DBF (DBase, Foxbase, ...) files

To test run

```racket
(require binary-class/dbf)
(define stream (open-input-file "test.dbf")) ; put your DBF file instead of test.dbf
(define db (dbopen stream))
(read-record db) ; returns first record
(read-record db) ; returns second
```

and so on. To inspect db object you may use get-field. List of field see inside sources.
