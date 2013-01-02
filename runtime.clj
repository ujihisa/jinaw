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

(defn js-boolean
  "https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Boolean"
  [value]
  (not (get #{0 'null false 'NaN 'undefined} value false)))

(defn js-number
  "https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Number"
  [value]
  (condp instance? value
    String (if (empty? value)
             0
             (let [x (read-string value)]
               (if (number? x) x 'NaN)))
    Long value
    'NaN))

(defn js-string
  "https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/String"
  [value]
  (.toString value))

(defn evaluate [expr env]
  "assumption: env won't change"
  (if (list? expr)
    (let [[car & cdr] expr]
      (case car
        fcall (let [func (evaluate (first cdr) env)
                     args (map #(evaluate % env) (second cdr))]
                 (case func
                   console.log (println (js-string (first args)))
                   + (if (every? number? args)
                       (+ (first args) (second args))
                       (str (js-string (first args)) (js-string (second args))))
                   (prn 'must-not-happen 'missing-function func)))
        quote (get env (first cdr))
        expr))
    expr))

(defn run [stmts]
  (loop [[stmt & stmts] stmts env {'console.log 'console.log '+ '+ 'null 'null 'undefined 'undefined 'NaN 'NaN}]
    (when stmt
      (let [[car & cdr] stmt]
        (case car
          var (let [vname (first cdr)
                    vvalue (evaluate (second cdr) env)]
                (recur stmts (assoc env vname vvalue)))
          return (prn 'not-implemented-yet)
          (do
            (evaluate stmt env)
            (recur stmts env)))))))

(run '[(var x 1)
       (fcall 'console.log [(fcall '+ ['x "hello"])])])
