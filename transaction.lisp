(uiop:define-package #:bitcoin/src/transaction/transaction
    (:nicknames #:transaction)
  (:use :closer-common-lisp
        :cl-coin
        :bitcoin/src/utilities/hash
        :bitcoin/src/utilities/binarydata)
  (:mix :alexandria
        )
  (:export

   ;; Variables.
   :*tx-table*

   ;; Functions.
   :register-tx))

(in-package :transaction)

(defparameter *tx-table* (make-hash-table :test 'equalp)
  "The hash table containing all transactions.")

(defun register-tx (tx hash-table)
  "Function.
   ARGLIST: 'BTC-Transaction
   RETURNS: '(unsigned-byte 8 (32)). NOTE: Side effects: insert into hash table.
   Inserts the tx into the hashtable. Key is the double hash of the transaction."
  (let ((hash (double-sha256 (write-and-return-value 'BTC-Transaction tx))))
    (setf (gethash hash hash-table) tx)))
