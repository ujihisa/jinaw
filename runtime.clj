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
  (not (contains? #{0 'null false 'NaN 'undefined} value)))

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

(declare run-)

(defn evaluate [expr env]
  "assumption: env won't change"
  (if (list? expr)
    (let [[car & cdr] expr]
      (case car
        if (let [[cond- then- else-] cdr]
             (if (js-boolean (evaluate cond- env))
               (evaluate then- env)
               (evaluate else- env)))
        function (let [params (first cdr)
                       body (second cdr)]
                   {:type :function :params params :body body})
        fcall (let [func (evaluate (first cdr) env)
                    args (map #(evaluate % env) (second cdr))]
                (case func
                  (if (= (:type func) :function)
                    (let [applied-params (into {} (map (fn [x y] [x y])
                                                       (:params func)
                                                       args))]
                      (run- (:body func) (merge env applied-params)))
                    (if (fn? func)
                      (func args)
                      (prn 'must-not-happen 'missing-function func)))))
        quote (get env (first cdr) 'missing-local-var)
        expr))
    expr))

(defn- run- [stmts env]
  (loop [[stmt & stmts] stmts env env]
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

(def ^:dynamic *builtins* {})

(def ^:dynamic *builtins*
  (assoc *builtins* 'console.log
         (fn [[x]]
           (println (js-string x)))))

(def ^:dynamic *builtins*
  (assoc *builtins* '+
         (fn [args]
           (if (every? number? args)
             (+ (first args) (second args))
             (str (js-string (first args)) (js-string (second args)))))))

(def ^:dynamic *builtins*
  (assoc *builtins* '===
         (fn [args]
           (= (first args) (second args)))))

(def ^:dynamic *builtins*
  (assoc *builtins* '!==
         (fn [args]
           (not= (first args) (second args)))))

(def ^:dynamic *builtins*
  (merge *builtins* {'null 'null 'undefined 'undefined 'NaN 'NaN}))

(defn run [stmts]
  (run- stmts *builtins*))

(run '[(var x 1)
       (fcall 'console.log [(fcall '+ ['x "hello"])])])
(run '[(fcall (function [x]
                        [(fcall 'console.log ['x])])
              [2])])
(run '[(fcall 'console.log [(if 0 2 3)])])
(run '[(var f (function [n]
                        [(fcall 'console.log ['n])
                         (if (fcall '=== ['n 10])
                           (fcall 'console.log ["end"])
                           (fcall 'f [(fcall '+ ['n 1])]))]))
       (fcall 'f [0])])
