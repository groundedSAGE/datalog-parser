(ns datalog.parser.test.util
  (:require [clojure.test :as t]))

#?(:clj
   (defmethod t/assert-expr 'thrown-msg? [msg form]
     (let [[_ match & body] form]
       `(try ~@body
             (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
             (catch Throwable e#
               (let [m# (.getMessage e#)]
                 (t/do-report
                  {:type     (if (= ~match m#) :pass :fail)
                   :message  ~msg
                   :expected '~form
                   :actual   e#}))
               e#)))))


