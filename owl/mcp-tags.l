;;;
;;; Some values needed by the thread controller, dumper and mcp 
;;;


(define poll-tag "mcp/polls")
(define buffer-tag "mcp/buffs")
(define link-tag "mcp/links")
(define mcp-tag "mcp/break")
(define (mcp-halt threads state) 1)
(define thread-quantum 20)

