(use 'iasm)

(defn-iasm fib [n]
  (iconst_1)
  (dup)
  (istore_2)
  (istore_3)
  (aload_1)
  (checkcast Integer)
  (invokevirtual ^int Integer/intValue [])
  (istore 4)
  :loop
  (iload 4)
  (iconst_0)
  (if_icmpeq :end)
  (iload_3)
  (dup)
  (istore 5)
  (iload_2)
  (iadd)
  (istore_3)
  (iload 5)
  (istore_2)
  (iinc 4 -1)
  (goto :loop)
  :end
  (iload_2)
  (invokestatic ^Integer Integer/valueOf [int])
  (areturn))