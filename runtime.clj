(def sample-parsed
  '[(var acc "dummy")
    (var f
         (fcall
           (function []
                     [[(var acc (array))
                       (return
                         (function ['x]
                                   [[(return (if 'x
                                               (and (mcall 'push 'acc ['x])
                                                    (fcall 'f [(minus 'x 1)]))
                                               'acc))]]))]])
           []))])

(defn evaluate [expr env]
  "assumption: env won't change"
  (if (list? expr)
    (let [[car & cdr] expr]
      (condp = car
        'fcall (let [func (evaluate (first cdr) env)
                     args (map #(evaluate % env) (second cdr))]
                 (condp = func
                   'console.log (println (first args))
                   '+ (+ (first args) (second args))
                   (prn 'must-not-happen 'missing-function func)))
        'quote (get env (first cdr))
        expr))
    expr))

(defn run [stmts]
  (loop [[stmt & stmts] stmts env {'console.log 'console.log '+ '+}]
    (when stmt
      (let [[car & cdr] stmt]
        (condp = car
          'var (let [vname (first cdr)
                     vvalue (evaluate (second cdr) env)]
                 (recur stmts (assoc env vname vvalue)))
          'return (prn 'not-implemented-yet)
          (do
            (evaluate stmt env)
            (recur stmts env)))))))

(run '[(var x 1)
       (fcall 'console.log [(fcall '+ ['x 2])])])
