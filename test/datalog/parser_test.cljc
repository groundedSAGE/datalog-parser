(ns datalog.parser-test
  #?(:cljs (:require-macros [datalog.parser.test.cljs]))
  (:require #?(:cljs [cljs.test :refer-macros [are deftest]]
               :clj  [clojure.test :refer [are deftest]])
            [datalog.parser.type :as t]
            [datalog.parser :as parser]
            [datalog.parser.test.util]))


(defn query-returnmap-template [mapping-type]
 (t/->Query
  (t/->FindRel [(t/->Variable '?e)])
  nil
  [(t/->BindScalar (t/->SrcVar '$))
   (t/->BindScalar (t/->Variable '?fname))
   (t/->BindScalar (t/->Variable '?lname))]
  [(t/->Pattern
    (t/->DefaultSrc)
    [(t/->Variable '?e)
     (t/->Constant :user/firstName)
     (t/->Variable '?fname)])
   (t/->Pattern
    (t/->DefaultSrc)
    [(t/->Variable '?e)
     (t/->Constant :user/lastName)
     (t/->Variable '?lname)])]
  nil
  nil
  (t/->ReturnMaps mapping-type
                  [(t/->MappingKey 'foo)])))


(deftest validation
  (are [q result] (= result (parser/parse q))

    '[:find ?e
      :in $ ?fname ?lname
      :keys foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]

    (query-returnmap-template :keys)

    '[:find ?e
      :in $ ?fname ?lname
      :strs foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]

    (query-returnmap-template :strs)

    '[:find ?e;
      :in $ ?fname ?lname
      :syms foo
      :where [?e :user/firstName ?fname]
      [?e :user/lastName ?lname]]

    (query-returnmap-template :syms)


    '{:find [?e]
      :in [$ ?fname ?lname]
      :keys [foo]
      :where [[?e :user/firstName ?fname]
              [?e :user/lastName ?lname]]}

    (query-returnmap-template :keys)))




(deftest validation-fails
  (are [q msg] (thrown-msg? msg (parser/parse q))
               '[:find ?e :where [?x]]
               "Query for unknown vars: [?e]"

               '[:find ?e :with ?f :where [?e]]
               "Query for unknown vars: [?f]"

               '[:find ?e ?x ?t :in ?x :where [?e]]
               "Query for unknown vars: [?t]"

               '[:find ?x ?e :with ?y ?e :where [?x ?e ?y]]
               ":find and :with should not use same variables: [?e]"

               '[:find ?e :in $ $ ?x :where [?e]]
               "Vars used in :in should be distinct"

               '[:find ?e :in ?x $ ?x :where [?e]]
               "Vars used in :in should be distinct"

               '[:find ?e :in $ % ?x % :where [?e]]
               "Vars used in :in should be distinct"

               '[:find ?n :with ?e ?f ?e :where [?e ?f ?n]]
               "Vars used in :with should be distinct"

               '[:find ?x :where [$1 ?x]]
               "Where uses unknown source vars: [$1]"

               '[:find ?x :in $1 :where [$2 ?x]]
               "Where uses unknown source vars: [$2]"

               '[:find ?e :where (rule ?e)]
               "Missing rules var '%' in :in"

               '[:find ?e :where [?e] :limit [42]]
               "Cannot parse :limit, expected positive integer"

               '[:find ?e :where [?e] :offset [666]]
               "Cannot parse :offset, expected positive integer"
               
               '[:find ?e :keys foo bar :where [?e] :offset 666]
               "Count of :keys/:strs/:syms must match count of :find"

               '[:find ?e ?f :keys foo :where [?e ?f] :offset 666]
               "Count of :keys/:strs/:syms must match count of :find"

               '[:find [?e ?f] :keys foo bar :where [?e ?f] :offset 666]
               "Count of :keys/:strs/:syms must match count of :find"

               '[:find ?e :strs '(foo bar) :keys '("foo" "bar") :where [?e] :offset 666]
               "Only one of these three options is allowed: :keys :strs :syms"))
