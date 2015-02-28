#lang typed/racket/base

;; simple exports
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/batch-io
 ; 2.1.1 IO Functions
 [read-file (Input-Source -> String)]
 [read-1strings (Input-Source -> (Listof String))]
 [read-lines (Input-Source -> (Listof String))]
 [read-words (Input-Source -> (Listof String))]
 [read-words/line (Input-Source -> (Listof (Listof String)))]
 [read-words-and-numbers/line (Input-Source -> (Listof (Listof (U Number String))))]
 [read-csv-file (Input-Source -> (Listof (Listof Any)))]
 [read-csv-file/rows (All [a] (Input-Source ((Listof Any) -> a) -> (Listof a)))]
 [read-xexpr (Input-Source -> XExpr)]
 [read-plain-xexpr (Input-Source -> XExpr)]
 [write-file (Output-Destination String -> String)]
 ; 2.1.2 Web Functions
 [read-xexpr/web (String -> XExpr)]
 [read-plain-xexpr/web (String -> XExpr)]
 [url-exists? (String -> Boolean)]
 [xexpr-as-string (XExpr -> String)]
 [url-html-neighbors (String -> (Listof String))])

;; derived / reimplemented exports
;; ---------------------------------------------------------------------------------------------------

(provide
 Input-Source Input-Source?
 Output-Destination Output-Destination?
 XExpr XExpr?
 XExpr-Attribute XExpr-Attribute?)

; provide xexpr? as a renamed predicate to get occurrence typing
(provide
 (rename-out
  [XExpr? xexpr?]))

;; derived type declarations
;; ---------------------------------------------------------------------------------------------------

(define-type Input-Source (U 'standard-in 'stdin String))
(define-type Output-Destination (U 'standard-out 'stdout String))
(define-type XExpr (U Symbol String Number
                       (Pairof Symbol (Pairof (Listof XExpr-Attribute) (Listof XExpr)))
                       (Pairof Symbol (Listof XExpr))))
(define-type XExpr-Attribute (List Symbol String))

(define-predicate Input-Source? Input-Source)
(define-predicate Output-Destination? Output-Destination)
(define-predicate XExpr? XExpr)
(define-predicate XExpr-Attribute? XExpr-Attribute)
