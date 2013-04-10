(ns jinaw.parse
  (:require [clojure.data.json :as json]))

(defn -main []
  (let [esparse "/home/ujihisa/node_modules/esprima/bin/esparse.js"
        json (slurp
               (.getInputStream
                 (.exec (Runtime/getRuntime)
                        (into-array [esparse "sample.js"]))))
        body ((json/read-str (slurp "sample-output.json")) "body")]
    (doseq [node body]
      (prn (case (node "type")
             "ExpressionStatement"
             (let [expression (node "expression")]
               (list 'fcall (symbol (expression "operator"))
                     [((expression "left") "value")
                      ((expression "right") "value")]))
             "VariableDeclaration"
             (for [declarations (node "declarations")]
               (list 'var (symbol ((declarations "id") "name"))
                     ((declarations "init") "value")))
             'else)))))
