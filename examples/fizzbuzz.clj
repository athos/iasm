(ns fizzbuzz
  (:use iasm))

(defn-iasm fizzbuzz [n]
  (aload_1)
  (checkcast Integer)
  (invokevirtual ^int Integer/intValue [])
  (istore_2)
  (iconst_1)
  (istore_3)
  :loop
  (iload_2)
  (iload_3)
  (if_icmplt :end)
  (iload_3)
  (ldc 15)
  (irem)
  (ifne :try-five)
  (ldc "FizzBuzz")
  (goto :print-and-next)
  :try-five
  (iload_3)
  (iconst_5)
  (irem)
  (ifne :try-three)
  (ldc "Buzz")
  (goto :print-and-next)
  :try-three
  (iload_3)
  (iconst_3)
  (irem)
  (ifne :otherwise)
  (ldc "Fizz")
  (goto :print-and-next)
  :otherwise
  (iload_3)
  (invokestatic ^String Integer/toString [int])
  :print-and-next
  (getstatic ^java.io.PrintStream System/out)
  (swap)
  (invokevirtual ^void java.io.PrintStream/println [String])
  (iinc 3 1)
  (goto :loop)
  :end
  (aconst_null)
  (areturn))
