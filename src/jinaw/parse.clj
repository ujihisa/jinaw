(ns jinaw.parse
  (:require [clojure.data.json :as json]))

(defn- get1 [node]
  (case (node "type")
    "Identifier" (symbol (node "name"))
    "Literal" (node "value")
    (prn 'omg!)))

(defn parse [node]
  (case (node "type")
    "ExpressionStatement"
    (let [expression (node "expression")]
      (case (expression "type")
        "BinaryExpression"
        (list 'fcall (symbol (expression "operator"))
              [(get1 (expression "left"))
               (get1 (expression "right"))])
        "CallExpression"
        (list 'fcall (get1 (expression "callee"))
              (mapv get1 (expression "arguments")))
        "FunctionExpression"
        (list 'function
              (mapv get1 (expression "params"))
              (mapv parse ((expression "body") "body")))
        (prn 'hmmmmmmmmm)))
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
