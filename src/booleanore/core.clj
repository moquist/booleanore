(ns booleanore.core
  (:require [clojure.pprint]))

(def env
  {"a" true
   "b" true
   "c" false
   "d" false})

(defn beval
  "Just a bonus one for fun... not used in the logic-tree transforms.
  Eval form with respect to the given string->boolean env-map and the special forms #{:and :or}"
  [env form]
  #_
  (clojure.pprint/pprint {:beval-call form})
  (let [result
        (cond
          (string? form)
          (let [v (get env form "not-found")]
            (if (not= "not-found" v)
              v
              (throw (ex-info (format "Bummer... unknown env-id %s is not found in env %s." form env)
                              {:form form :env env}))))

          (vector? form)
          (let [[x & new-form] form]
            (cond
              (= :and x)
              (->> new-form
                   (map #(beval env %))
                   #_
                   (#(do (clojure.pprint/pprint %) %))
                   (every? identity))

              (= :or x)
              (->> new-form
                   (map #(beval env %))
                   #_
                   (#(do (clojure.pprint/pprint %) %))
                   (some identity)
                   boolean)

              :default
              (throw (ex-info (format "Bummer... unknown boolean term %s" x)
                              {:form form}))))

          :default
          (throw (ex-info (format "Bummer... unhandled cond for form: %s" form)
                          {:form form})))]
    #_
    (clojure.pprint/pprint {:result result})
    result))

(defn ttable-fork [f]
  (fn ttable-fork* [ttable form]
    #_
    (clojure.pprint/pprint {:fork-ttable ttable :fork-form form})
    (let [[x & _ :as result] (f ttable form)
          result
          (cond
            (vector? x)
            (mapv #(into ttable %) result)

            (keyword? x)
            result

            :default
            (throw (ex-info (format "Dunno: :ttable %s, :form %s, :x %s" ttable form x)
                            {:ttable ttable :form form :x x})))]
      #_
      (clojure.pprint/pprint {:fork-ttable ttable :fork-form form :result result})
      result)))

(defn bform->flattened-disjunct
  ([form] (bform->flattened-disjunct [:or] form))
  ([ttable form]
   {:post [(vector? %)
           #_
           (every? (fn [result]
                     (and (vector? result)
                          (-> result first keyword?)
                          (every? string? (rest result))))
                   %)]}
   #_
   (clojure.pprint/pprint {:bform->flattened-disjunct-ttable ttable :form form})
   (let [result
         (cond
           (string? form)
           (if (seq (rest ttable))
             (into [:or] (mapv #(conj % form)
                               (rest ttable)))
             (conj ttable [:and form]))

           (vector? form)
           (let [[x & new-form] form]
             (cond
               (= :and x)
               (reduce (ttable-fork bform->flattened-disjunct)
                       ttable
                       new-form)

               (= :or x)
               ;; finish bubbling up
               (reduce
                (fn [r [_or & x]]
                  (into r x))
                [:or]
                (mapv #(bform->flattened-disjunct ttable %) new-form))

               :default
               (throw (ex-info (format "Bummer... unknown boolean term %s" x)
                               {:form form})))))]
     #_
     (clojure.pprint/pprint {:bform->flattened-disjunct-ttable ttable :form form :result result})
     result))
  )

(comment

  [:and "a" [:or "c" "d"]]

  [["a"]]

  [["a" "c"] ["a" "d"]]




  )



[[[["a" "b" "c" "d"]
   ["a" "b" "c" "e"]
   [[[["a" "b" "c" "f" "g"]
      ["a" "b" "c" "f" "h"]
      "i"]
     [["a" "b" "c" "f" "g"]
      ["a" "b" "c" "f" "h"]
      "j"]]]]]]

(comment
  (def env
    {"a" true
     "b" true
     "c" false
     "d" false})

  (evaluate env "c")
  (evaluate env [:and "c" "d"])
  (evaluate env [:or "c" "d"])
  (evaluate env [:or "c" [:and "d" "e"]])

  ;; could write "evaluate" as a recursive-descent parser
  ;; could write "evaluate" as a transform to a single disjunct of conjuncts
  )
