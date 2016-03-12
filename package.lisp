;;;; package.lisp

(defpackage #:cl-coin
  (:use #:cl)
  (:import-from :com.gigamonkeys.binary-data
                #:read-value
                #:write-value
                #:define-binary-type
                #:define-binary-class)
  (:export

   ;; Classes.
   :tx-in :prevout-hash :prevput-index :script :seq
   :tx-out :value :script
   :transaction :version :inputs :outputs :lock-time

   ;; Methods.
   :read-value
   :write-value

   ;; Binary types.
   :u4/le :u8/le :varint :lp-blob :opcode :script :many :blob

   ;; Test functions.
   :test-parse-tx
   :test-parse-tx2))
