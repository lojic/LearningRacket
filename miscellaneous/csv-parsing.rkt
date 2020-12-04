#lang racket

(require csv-reading)

(csv->list "\"foo,bar\",baz\napple,orange")