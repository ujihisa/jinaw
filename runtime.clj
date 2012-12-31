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
  (if (= 'quote (first expr))
    (get env (second expr))
    expr))

(defn run [exprs]
  (loop [[expr & exprs] exprs env {'console.log 'console.log}]
    (when expr
      (if-let [[car & cdr] expr]
        (do
          #_(prn car cdr env)
          (condp = car
            'var (let [vname (first cdr)
                       vvalue (evaluate (second cdr) env)]
                   (recur exprs (assoc env vname vvalue)))
            'fcall (let [func (evaluate (first cdr) env)
                         args (map #(evaluate % env) (second cdr))]
                     (condp = func
                       'console.log (println (first args))
                       (prn 'must-not-happen 'missing-function)))
            (prn 'must-not-happen expr)))
        expr))))

(run '[(var x "hello") (fcall 'console.log ['x])])
