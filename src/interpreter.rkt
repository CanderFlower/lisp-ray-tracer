#lang racket
(require "ast.rkt"
         "eval-dsl.rkt")
(provide init-globals repl-loop run-script)


;; init global scene variable
(define (init-globals)
  (global-scene (make-default-scene)))

;; REPL 循环
(define (repl-loop)
  (displayln "Entering DSL REPL. Type (help) for commands. (exit) to quit.")
  (let loop ([env '()])
    (display "> ")
    (flush-output)
    (let ([expr (with-handlers ([exn:fail? (λ (e) 
                                             (displayln (exn-message e))
                                             #f)])
                  (read))])
      (cond
        [(eof-object? expr)
         (displayln "Bye.")
         (void)]
        [(not expr)
         ;; 读入或执行时出错已打印，继续
         (loop env)]
        [else
         (with-handlers ([exn:fail? (λ (e)
                                     (displayln (exn-message e))
                                     (loop env))])
           (handle-command expr env)
           (loop env))]))))

;; run-script: 依次读取文件中的 S-Expr，对每条调用 handle-command
(define (run-script filename)
  (parameterize ([current-input-port (open-input-file filename)])
    (let loop ()
      (let ([expr (with-handlers ([exn:fail? (λ (e)
                                               (displayln (format "Error reading script: ~a" (exn-message e)))
                                               #f)])
                    (read))])
        (cond
          [(eof-object? expr)
           (void)]
          [(not expr)
           ;; 读取或执行出错继续下一条
           (loop)]
          [else
           (with-handlers ([exn:fail? (λ (e)
                                       (displayln (format "Error executing command ~a: ~a" expr (exn-message e)))
                                       (loop))])
             (handle-command expr '())
             (loop))]) ))))

