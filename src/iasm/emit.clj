(ns iasm.emit
  (:import [clojure.asm Type Opcodes ClassVisitor MethodVisitor Label]
           [clojure.asm.commons Method GeneratorAdapter])
  (:use [clojure.string :only (lower-case upper-case)]))

(defn- error [template & args]
  (throw (RuntimeException. (apply format template args))))

(def ^{:private true} OBJECT_TYPE (Type/getType Object))
(def ^{:private true} EXCEPTION_TYPES (into-array Type []))

(defn- opcode [x]
  (->> (if (or (keyword? x) (symbol? x))
         (name x)
         (str x))
       upper-case
       (str "Opcodes/")
       symbol))

(defn- argument-type [n]
  (into-array Type (repeat n OBJECT_TYPE)))

(defmulti emit-insn
  (fn [insn labels gen]
    (if (keyword? insn)
      :label
      (keyword (lower-case (name (first insn)))))))

(defmethod emit-insn :default [insn labels gen]
  (error "Unknown instruction: %s" insn))

(defn make-emitter [[args & insns]]
  (fn [^ClassVisitor cv]
    (let [^Method method (Method. "invoke"
                                  OBJECT_TYPE
                                  (argument-type (count args)))
          ^GeneratorAdapter gen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                                                   method
                                                   nil
                                                   EXCEPTION_TYPES
                                                   cv)
          labels (atom {})]
      (.visitCode gen)
      (doseq [insn insns]
        (emit-insn insn labels gen))
      (.endMethod gen))))

(defmacro def-noarg-insn-emitters [& ops]
  `(do ~@(for [op ops]
           `(defmethod emit-insn ~op [insn# labels# ^GeneratorAdapter gen#]
              (.visitInsn gen# ~(opcode op))))))

(def-noarg-insn-emitters
  :nop :aconst_null :iconst_m1 :iconst_0
  :iconst_1 :iconst_2 :iconst_3 :iconst_4
  :iconst_5 :lconst_0 :lconst_1 :fconst_0
  :fconst_1 :fconst_2 :dconst_0 :dconst_1
;;   :bipush_:sipush_:ldc_:ldc_w
;;   :ldc2_w :iload :lload :fload
;;   :dload :aload :iload_0 :iload_1
;;   :iload_2 :iload_3_:lload_0_:lload_1
;;   :lload_2_:lload_3_:fload_0_:fload_1
;;   :fload_2 :fload_3 :dload_0 :dload_1
;;   :dload_2 :dload_3 :aload_0 :aload_1
  #_:aload_2 #_:aload_3 :iaload :laload
  :faload :daload :aaload :baload
  :caload :saload #_:istore #_:lstore
;;   :fstore :dstore :astore :istore_0
;;   :istore_1 :istore_2 :istore_3 :lstore_0
;;   :lstore_1 :lstore_2 :lstore_3 :fstore_0
;;   :fstore_1 :fstore_2 :fsotre_3 :dstore_0
;;   :dstore_1 :dstore_2 :dstore_3 :astore_0
  #_:astore_1 #_:astore_2 #_:astore_3 :iastore
  :lastore :fastore :dastore :aastore
  :bastore :castore :sastore :pop
  :pop2 :dup :dup_x1 :dup_x2
  :dup2 :dup2_x1 :dup2_x2 :swap
  :iadd :ladd :fadd :dadd
  :isub :lsub :fsub :dsub
  :imul :lmul :fmul :dmul
  :idiv :ldiv :fdiv :ddiv
  :irem :lrem :frem :drem
  :ineg :lneg :fneg :dneg
  :ishl :lshl :ishr :lshr
  :iushr :lushr :iand :land
  :ior :lor :ixor :lxor
  #_:iinc :i2l :i2f :i2d
  :l2i :l2f :l2d :f2i
  :f2l :f2d :d2i :d2l
  :d2f :i2b :i2c :i2s
  :lcmp :fcmpl :fcmpg :dcmpl
  :dcmpg #_:ifeq #_:ifne #_:iflt
  #_:ifge #_:ifgt #_:ifle #_:if_icmpeq
  #_:if_icmpne #_:if_icmplt #_:if_icmpge #_:if_icmpgt
  #_:if_icmple #_:if_acmpeq #_:if_acmpne #_:goto
  #_:jsr #_:ret :tableswitch :lookupswitch
  :ireturn :lreturn :freturn :dreturn
  :areturn :return #_:getstatic #_:putstatic
  #_:getfield #_:putfield #_:invokevirtual #_:invokespecial
  #_:invokestatic #_:invokeinterface #_:new #_:newarray
  #_:anewarray :arraylength :athrow #_:checkcast
  #_:instanceof :monitorenter :monitorexit #_:wide
  #_:multianewarray #_:ifnull #_:ifnonnull #_:goto_w
  #_:jsr_w #_:breakpoint #_:impdep1 #_:impdep2)

(defn- var-insn-template [base-insn types ns]
  `(do ~@(for [t types, n ns]
           (let [insn (gensym)
                 insn-name (keyword (str t base-insn (if (= n '_) "" (str "_" n))))]
             `(defmethod emit-insn ~insn-name [~insn labels# ^GeneratorAdapter gen#]
                (.visitVarInsn gen#
                               ~(opcode (str t base-insn))
                               ~(if (= n '_)
                                  `(second ~insn)
                                  n)))))))

(defmacro def-load-insn-emitters [types ns]
  (var-insn-template "load" types ns))

(def-load-insn-emitters
  [i l f d a] [0 1 2 3 _])

(defmacro def-store-insn-emitters [types ns]
  (var-insn-template "store" types ns))

(def-store-insn-emitters
  [i l f d a] [0 1 2 3 _])

(defn- get-label [labels label-name]
  (or (@labels label-name)
      (let [label (Label.)]
        (swap! labels conj [label-name label])
        label)))

(defmethod emit-insn :label [label-name labels ^GeneratorAdapter gen]
  (let [^Label label (get-label labels label-name)]
    (.mark gen label)))

(defmacro def-jump-insn-emitters [& ops]
  `(do ~@(for [op ops]
           `(defmethod emit-insn ~op [insn# labels# ^GeneratorAdapter gen#]
              (let [label-name# (second insn#)
                    ^Label label# (get-label labels# label-name#)]
                (.visitJumpInsn gen#
                                ~(opcode op)
                                label#))))))

(def-jump-insn-emitters
  :ifeq :ifne :iflt
  :ifge :ifgt :ifle :if_icmpeq
  :if_icmpne :if_icmplt :if_icmpge :if_icmpgt
  :if_icmple :if_acmpeq :if_acmpne :goto
  :jsr :ifnull :ifnonnull)

(defmacro def-field-insn-emitters [& ops]
  `(do ~@(for [op ops]
           `(defmethod emit-insn ~op [insn# labels# ^GeneratorAdapter gen#]
              (let [arg# (second insn#)
                    ^String owner# (Type/getInternalName (Class/forName (namespace arg#)))
                    ^String name# (name arg#)
                    ^String desc# (Type/getDescriptor (Class/forName (name (:tag (meta arg#)))))
                    op# (int ~(opcode op))]
                (.visitFieldInsn gen# op# owner# name# desc#))))))

(def-field-insn-emitters
  :getstatic :putstatic :getfield :putfield)

(defmacro def-method-insn-emitters [& ops]
  `(do ~@(for [op ops]
           `(defmethod emit-insn ~op [insn# labels# ^GeneratorAdapter gen#]
              (let [arg# (second insn#)
                    ^String owner# (Type/getInternalName (Class/forName (namespace arg#)))
                    ^String name# (name arg#)
                    ^Type return-type# (Type/getType (Class/forName (name (:tag (meta arg#)))))
                    ^"[Lclojure.asm.Type;" arg-types# (into-array Type (map #(Type/getType (Class/forName (name %))) (nth insn# 2)))
                    ^String desc# (Type/getMethodDescriptor return-type# arg-types#)
                    op# (int ~(opcode op))]
                (.visitMethodInsn gen# op# owner# name# desc#))))))

(def-method-insn-emitters
  :invokevirtual :invokespecial :invokestatic :invokeinterface)
