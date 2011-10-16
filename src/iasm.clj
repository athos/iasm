(ns iasm
  (:refer-clojure :exclude [compile])
  (:use iasm.compile))

(defmacro fn-iasm [& form]
  (let [c (compile form)]
    `(let [f# (construct-proxy (Class/forName ~(.getName c)))]
       f#)))

(defmacro defn-iasm [name args & body]
  `(def ~name (fn-iasm ~args ~@body)))
