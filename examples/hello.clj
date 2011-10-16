(ns hello
  (:use iasm)
  (:import java.io.PrintStream))

(defn-iasm hello []
  (getstatic ^PrintStream System/out)
  (ldc "Hello, world!")
  (invokevirtual ^void PrintStream/println [String])
  (aconst_null)
  (areturn))
