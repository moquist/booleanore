(ns booleanore.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [booleanore.core :as core]))

(def env
  {"a" true
   "b" true
   "c" false
   "d" false})

(deftest beval-test
  (is (core/beval env [:and "a" "b"
                       [:or "d" "a"
                        [:and "a" "c"]]]))
  (is (not (core/beval env [:and "a" "b"
                            [:or "c"
                             [:and "a" "c"]]])))
  )

(deftest bform->flattened-disjunct-test
  (is (= [:or [:and "a"] [:and "b"] [:and "c" "d"]]
         (core/bform->flattened-disjunct [:or "a" "b" [:and "c" "d"]])))
  (is (= [:or [:and "a"] [:and "b"] [:and "c" "d"] [:and "e" "f" "g" "h"]]
         (core/bform->flattened-disjunct [:or "a" "b"
                                          [:and "c" "d"]
                                          [:and "e" "f"
                                           [:and "g" "h"]]])))
  (is (= (-> [:or
              [:and "a" "b" "c" "e"]
              [:and "a" "b" "d" "e"]
              [:and "a" "b" "c" "f"]
              [:and "a" "b" "d" "f"]]
             rest
             set)
         (-> (core/bform->flattened-disjunct [:and "a" "b"
                                              [:or "c" "d"]
                                              [:or "e" "f"]])
             rest
             set)))
  (is (= [:or
          [:and "a" "b" "c"]
          [:and "a" "b" "d" "e"]]
         (core/bform->flattened-disjunct
          [:and "a" "b"
           [:or "c"
            [:and "d" "e"]]]))))
