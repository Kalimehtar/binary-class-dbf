#lang scribble/manual

@(require (for-label racket binary-class/dbf binary-class fast-convert))

@title{Binary-class/dbf: parsing and saving DBF (XBase) files}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@(defmodule binary-class/dbf)

The module @racket[binary-class/dbf]

@defclass[dbf-header% object% (binary<%>)]{
Class, implementing interface to DBF file

@defmethod[(set-code-page! [code-page (integer-in 0 255)]) void?]{Sets code-page to integer value. 
Better use @racket[change-code-page!].}

@defmethod[(goto-record! [stream stream?] [n exact-integer?]) void?]{
Moves @racket[file-position] in @racket[_stream] to record number @racket[_n]}

@defmethod[(goto-eof! [stream stream?]) void?]{
Moves @racket[file-position] in @racket[_stream] after the last record.}

@defmethod[(read-record [stream stream?] [translate (-> convert-table? bytes? string?) convert]) 
           (or/c #f vector?)]{
Reads record. Text fields processed via @racket[_translate]}

}

@defproc[(change-code-page! [dbf (is-a?/c dbf-header%)] 
                            [code-page (or/c (integer-in 0 255) 
                                             'cp437
                                             'cp850
                                             'cp852
                                             'cp865
                                             'cp866
                                             'cp1250
                                             'cp1251
                                             'cp1252)])
                            void?]{
Changes code page to given. @racket[code-page] may be either symbol with code page name 
or number of code page in  DBF format}

