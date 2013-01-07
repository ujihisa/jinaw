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

(defn js-type [value]
  (if (map? value)
    (if (= :function (value :type))
      "function"
      "object")
    ({"Long" "number" "Double" "number" "String" "string"}
      (.getSimpleName (type value)))))

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
                (cond
                  (= (:type func) :function)
                  (let [applied-params (into {} (map (fn [x y] [x y])
                                                     (:params func)
                                                     args))]
                    (run- (:body func) (merge env applied-params)))
                  (fn? func) (func args)
                  :else (prn 'must-not-happen 'missing-function func)))
        typeof (let [x (first cdr)]
                 (if (= clojure.lang.Symbol (type x))
                   (if-let [x (get env x)]
                     (js-type (evaluate x env))
                     'undefined)
                   (js-type (evaluate x env))))
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
          return (evaluate (first cdr) env)
          (do
            (evaluate stmt env)
            (recur stmts env)))))))

(def ^:dynamic *builtins* {})
(defmacro defbuiltin [x y & z]
  `(def ^:dynamic *builtins*
     (assoc *builtins* '~x
            (fn ~[y]
              ~@z))))

(defbuiltin console.log [x]
  (println (js-string x))
  'undefined)

(defbuiltin + [x y]
  (if (and (number? x) (number? y))
    (+ x y)
    (str (js-string x) (js-string y))))


(defbuiltin === [x y]
  (= x y))

(defbuiltin !== [x y]
  (not= x y))

(defbuiltin == [x y]
  (let [x-type (js-type x)
        y-type (js-type y)]
    (cond
      (= x-type y-type)
      (= x y)

      (or (#{"number" "boolean"} x-type)
          (#{"number" "boolean"} y-type))
      (= (js-number x) (js-number y))

      (or (= "string" x-type) (= "string" y-type))
      (= (js-string x) (js-string y))

      (and (= "object" x-type) (= "object" y-type))
      (prn 'not-implemented-yet)

      :else
      (prn 'hmm...?))))

(defbuiltin -aref [x i]
  (if (= "object" (js-type x))
    (get x (js-string i) 'undefined)
    'undefined))

(def ^:dynamic *builtins*
  (merge *builtins* {'null 'null 'undefined 'undefined 'NaN 'NaN}))

(defn run [stmts]
  (run- stmts *builtins*))

#_(run
  '[(var fib
         (function [n]
           [(return
              (if (fcall '=== ['n 0])
                0
                (if (fcall '=== ['n 1])
                  1
                  (fcall '+ [(fcall 'fib [(fcall '+ ['n -1])])
                             (fcall 'fib [(fcall '+ ['n -2])])]))))]))
    (fcall 'console.log [(fcall 'fib [30])])])

#_(run '[(var x 1)
       (fcall 'console.log [(fcall '+ ['x "hello"])])])
#_(run '[(fcall (function [x]
                        [(fcall 'console.log ['x])])
              [2])])
#_(run '[(fcall 'console.log [(if 0 2 3)])])
#_(run '[(var f (function [n]
                        [(fcall 'console.log ['n])
                         (if (fcall '=== ['n 10])
                           (fcall 'console.log ["end"])
                           (fcall 'f [(fcall '+ ['n 1])]))]))
       (fcall 'f [0])])
#_(run '[(var f (function []
                        [(fcall 'console.log [1])
                         (return 9)
                         (fcall 'console.log [2])]))
       (fcall 'console.log [(fcall 'f [])])])
#_(run '[(fcall 'console.log [(typeof 1)])
       (var x 1)
       (fcall 'console.log [(typeof x)])
       (fcall 'console.log [(typeof y)])
       (fcall 'console.log [(typeof (fcall '+ ['x 2]))])])

#_(run '[(fcall 'console.log [(fcall '== [1 1])])
       (fcall 'console.log [(fcall '== [1 2])])
       (fcall 'console.log [(fcall '== ["1" 1])])
       (fcall 'console.log [(fcall '== ["1" 2])])
       (fcall 'console.log [(fcall '== ["1" "1"])])
       (fcall 'console.log [(fcall '== ["1" "2"])])
       (fcall 'console.log [(fcall '== ["aaa" "aab"])])])

(comment
  (run '[(fcall 'console.log [(fcall '-aref [{"0" "a" "1" "b" "2" "c"} 0])])])
  (run '[(fcall 'console.log [(fcall '-aref [{"0" "a" "1" "b" "2" "c"} "1"])])])
  (run '[(fcall 'console.log [(fcall '-aref [{"0" "a" "1" "b" "2" "c"} 3])])])
  (run '[(fcall 'console.log [(fcall '-aref [{"0" "a" "1" "b" "2" "c"} "4"])])]))


