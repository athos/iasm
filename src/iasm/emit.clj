(ns iasm.emit
  (:import [clojure.asm Type Opcodes]
           [clojure.asm.commons Method GeneratorAdapter]))

(defn- error [template & args]
  (throw (RuntimeException. (apply format template args))))

(def ^{:private true} OBJECT_TYPE (Type/getType Object))
(def ^{:private true} EXCEPTION_TYPES (into-array Type []))

(defn- argument-type [n]
  (into-array Type (repeat n OBJECT_TYPE)))

(defmulti emit-insn
  (fn [insn args labels]
    (if (keyword? insn)
      :label
      (keyword (.toLowerCase (name (first insn)))))))

(defmethod emit-insn :default [insn args labels]
  (error "Unknown instruction: %s" insn))

(defn make-emitter [[args & insns]]
  (fn [^ClassVisitor cv]
    (let [^Method method (Method. "invoke"
                                  OBJECT_TYPE
                                  (argument-type (inc (count args))))
          ^GeneratorAdapter gen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                                                   method
                                                   nil
                                                   EXCEPTION_TYPES
                                                   cv)]
      (.visitCode gen)
      (let [labels (atom {})]
        (doseq [insn insns]
          (emit-insn insn args labels)))
      (.endMethod gen))))
