#lang racket
(require "interpreter.rkt")
(require "ast.rkt")
(require "utils.rkt")
(define (main)
  (init-globals)
  (let ([args (vector->list (current-command-line-arguments))])
    (cond
      [(and (= (length args) 1))
       (let ([file (first args)])
         (displayln (format "Running script file: ~a" file))
         (run-script file)
         (displayln "Script execution finished."))]
      [(zero? (length args))
       (repl-loop)]
      [else
       (displayln "Usage: racket main.rkt [script-file]")
       (void)])))

;; 调用 main
(main)
