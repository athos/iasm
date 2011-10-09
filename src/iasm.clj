(ns iasm
  (:refer-clojure :exclude [compile])
  (:use iasm.compile))

(defmacro iasm-fn [& form]
  (let [c (compile form)]
    `(construct-proxy (Class/forName ~(.getName c)))))
