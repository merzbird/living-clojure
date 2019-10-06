(ns cljr-macros.core)

(defmacro def-hello-world [who phrase]
  (list 'defn
        (symbol (str "hello-" who))
        []
        (list 'str "Hello, " phrase)))

(defmacro def-bye-world [who phrase]
  `(defn
     ~(symbol (str "bye-" who))
     []
     ~(str "Bye, " phrase)))
