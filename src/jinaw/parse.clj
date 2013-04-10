(ns jinaw.parse
  (:require [clojure.data.json :as json]))

(defn- get1 [node]
  (case (node "type")
    "Identifier" (symbol (node "name"))
    "Literal" (node "value")
    (prn 'omg! node)))

(declare parse)

(defn- parse-expr [expression]
  (case (expression "type")
    "BinaryExpression"
    (list 'fcall (symbol (expression "operator"))
          [(get1 (expression "left"))
           (get1 (expression "right"))])
    "CallExpression"
    (list 'fcall (parse-expr (expression "callee"))
          (mapv parse-expr (expression "arguments")))
    "FunctionExpression"
    (list 'function
          (mapv get1 (expression "params"))
          (mapv parse ((expression "body") "body")))
    "ConditionalExpression"
    (list 'if
          (parse-expr (expression "test"))
          (parse-expr (expression "consequent"))
          (parse-expr (expression "alternate")))
    (get1 expression)
    #_(prn 'hmmmmmmmmm)))

(defn parse [node]
  (case (node "type")
    "ExpressionStatement"
    (parse-expr (node "expression"))
    "VariableDeclaration"
    (for [declarations (node "declarations")]
      (list 'var (get1 (declarations "id"))
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
