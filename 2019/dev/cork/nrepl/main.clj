(ns cork.nrepl.main
  (:require cork.nrepl
            rebel-readline.clojure.main
            rebel-readline.core))

(defn -main
  [& args]
  (rebel-readline.core/ensure-terminal
    (rebel-readline.clojure.main/repl)
    (System/exit 0)))
