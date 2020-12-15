(ns datalog.parser.test.cljs
  (:require [cljs.test :as t])
  #?(:cljs (:require-macros [datalog.parser.test.cljs])))

#?(:clj
   (defmethod t/assert-expr 'thrown-msg? [_ msg form]
     (let [[_ match & body] form]
       `(try
          ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch :default e#
            (let [m# (.-message e#)]
              (if (= ~match m#)
                (t/do-report {:type :pass, :message ~msg, :expected '~form, :actual e#})
                (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual e#}))
              e#))))))