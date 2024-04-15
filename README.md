Log Once, a debugging utility for Racket
========
`log-once` is a macro that allows the user to log both an expression and its value to the console, as well as to specify conditions under which to log. The macro is most effective in tight loops, where a naive logger would saturate stdout.

Furthermore, the macro can be configured to stop logging after a given number of invocations, using a lifted boolean to track the log count even after leaving the expression's scope.

## Type signature
|Argument|Type|Required?|Default value|Description|
|---|---|---|---|---|
|#:skip-count|Natural|False|0|Assuming other logging conditions are met, the number of invocations to ignore before starting to log|
|#:log-count|Natural|False|1|Assuming logging conditions are met, the number of times that calling log-once will log the expression|
|#:when|Condition|False|#t|log-once will try to log the expression if this condition is met|
|#:message|String|False|""|A string literal to write to stdout before writing the expression|
|#:newline/def|Boolean|False|False|Whether to write a newline between every logged expression|
|#:newline/defs|Boolean|False|True|Whether to write a trailing newline after writing all expressions|
|#:return|Expression|False|Void|The value to return from the invocation of log-once. Intended to be used for inline invocations|
|expr ...|Expression|True|-|The expressions whose syntax and values should be logged to the console|

## Examples
Waits until n is greater than 10, then log the following two values
```racket
(require log-once)

(for ([n (in-range 20)])
  (log-once n
            #:when (> n 10)
            #:log-count 2))

#| IN STDOUT:
n = 11
n = 12
|#
```

Use log-once in a nested scope without losing track of the log count
```racket
(require log-once)

(define (function-scope str)
  (log-once (~a #:align 'center #:width 20 str)))

(function-scope "first string")
(function-scope "second string")

#| IN STDOUT:
(~a #:align (quote center) #:width 20 str) = "    first string    ". 
|#
```
