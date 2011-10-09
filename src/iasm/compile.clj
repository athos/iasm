(ns iasm.compile
  (:refer-clojure :exclude [compile]))

(gen-class
  :name iasm.compile.ObjExpr
  :extends clojure.lang.Compiler$ObjExpr
  :init init
  :state emitter
  :constructors {[Object Object] [Object]})

(defn -init [tag emitter]
  [[tag] emitter])

(defn -emitMethods [this ^clojure.asm.ClassVisitor cv]
  ((.emitter this) cv))

(import 'iasm.compile.ObjExpr
        'clojure.lang.Compiler$ObjExpr)

(defn- make-objexpr [form]
  (let [emitter (constantly nil)
        fnexpr (ObjExpr. (:tag (meta form)) emitter)]
    (letfn [(set-field! [^String field-name, ^Object value]
              (doto (.getDeclaredField Compiler$ObjExpr field-name)
                (.setAccessible true)
                (.set fnexpr value)))]
      (set-field! "src" nil)
      (set-field! "onceOnly" false)
      (set-field! "name" "Hoge")
      (set-field! "internalName" "Hoge")
      (set-field! "objtype" (clojure.asm.Type/getObjectType "Hoge"))
      (set-field! "thisName" nil)
      (set-field! "line" @clojure.lang.Compiler/LINE)
      (set-field! "keywords" {})
      (set-field! "vars" {})
      (set-field! "constants" [])
      (set-field! "keywordCallsites" [])
      (set-field! "protocolCallsites" [])
      (set-field! "varCallsites" [])
      (set-field! "constantsID" 0)

      fnexpr)))

(def ^{:private true} compile-method
  (first (filter #(= (.getName %) "compile")
                 (.getDeclaredMethods Compiler$ObjExpr))))

(def ^{:private true} getcompiledclass-method
  (first (filter #(= (.getName %) "getCompiledClass")
                 (.getDeclaredMethods Compiler$ObjExpr))))

(defn compile [form]
  (let [^ObjExpr objexpr (make-objexpr form)]
    (.invoke compile-method
             objexpr
             (object-array ["clojure/lang/AFunction" nil false]))
    (.invoke getcompiledclass-method
             objexpr
             (object-array []))))
