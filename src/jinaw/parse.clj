(ns jinaw.parse
  (:require [clojure.data.json :as json]))

(declare parse)

(defn- parse-expr [expression]
  (case (expression "type")
    "ArrayExpression"
    (let [value-vec (map parse-expr (expression "elements"))]
      (into {} (map vector (map str (iterate inc 0)) value-vec)))
    "BinaryExpression"
    (list 'fcall (symbol (expression "operator"))
          [(parse-expr (expression "left"))
           (parse-expr (expression "right"))])
    "CallExpression"
    (list 'fcall (parse-expr (expression "callee"))
          (mapv parse-expr (expression "arguments")))
    "FunctionExpression"
    (list 'function
          (mapv parse-expr (expression "params"))
          (mapv parse ((expression "body") "body")))
    "ConditionalExpression"
    (list 'if
          (parse-expr (expression "test"))
          (parse-expr (expression "consequent"))
          (parse-expr (expression "alternate")))
    "MemberExpression"
    (if (expression "computed")
      (list 'fcall '-aref
            [(parse-expr (expression "object"))
             (parse-expr (expression "property"))])
      (prn 'not-computed!?))
    "Identifier" (symbol (expression "name"))
    "Literal" (expression "value")
    #_(prn 'hmmmmmmmmm)))

(defn parse [node]
  (case (node "type")
    "ExpressionStatement"
    (parse-expr (node "expression"))
    "VariableDeclaration"
    (for [declarations (node "declarations")]
      (list 'var (parse-expr (declarations "id"))
            ((declarations "init") "value")))
    (do
      (prn 'else)
      (prn node)
      nil)))

(defn -main []
  (let [esparse "/home/ujihisa/node_modules/esprima/bin/esparse.js"
        json (slurp
               (.getInputStream
                 (.exec (Runtime/getRuntime)
                        (into-array [esparse "short-sample.js"]))))
        body ((json/read-str json) "body")]
    (doseq [node body]
      #_(prn node)
      (prn (parse node)))))
