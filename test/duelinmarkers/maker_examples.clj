(ns duelinmarkers.maker-examples
  "This namespace aims to demonstrate all the features of maker. The
  `testing` forms below have explanatory commentary littered throughout.

  Some examples use fns from `clojure.data.generators`, which are handy
  for generating test data, but maker has no dependency on that lib."
  (:use clojure.test)
  (:require [duelinmarkers.maker
             :refer (add-prototype
                     make
                     gen
                     gen-from
                     gen-from-fn)]
            [clojure.data.generators :as gen]))

(deftest maker-usage

  (testing "the simplest prototype"

    ;; You add prototypes with `add-prototype`, probably in some run-once,
    ;; test-setup code (like a `clojure.test` `:once` fixture). It takes a
    ;; key and a map.

    (add-prototype :simple {:name "Name"})

    ;; In your tests, call make. When you pass just the key, it returns
    ;; the corresponding map.

    (is (= {:name "Name"} (make :simple)))

    ;; You can override values or add additional kv pairs by additionally
    ;; passing make either a map or interleaved kvs.

    (is (= {:name "Override"}
           (make :simple {:name "Override"})
           (make :simple :name "Override")))
    (is (= {:name "Name" :other "Val" :third 3}
           (make :simple {:other "Val" :third 3})
           (make :simple :other "Val" :third 3))))

  (testing "prototypes based on other prototypes"

    ;; When you `add-prototype`, you can extend another prototype, and the
    ;; map you pass will be conj'd onto the extended prototype map.

    (add-prototype :extended [:simple] {:grade 3})

    (is (= {:name "Name" :grade 3} (make :extended)))

    (add-prototype :very-extended [:extended] {:dob "1978-02-11"})

    (is (= {:name "Name" :grade 3 :dob "1978-02-11"}
           (make :very-extended)))

    ;; The base prototype must be defined before you can extend it.

    (is (thrown-with-msg? IllegalArgumentException
          #"Unknown base prototype\(s\): \(:doesnt-exist\)"
          (add-prototype :wont-work [:doesnt-exist] {:a 1}))))

  (testing "a multiply-extended prototype"

    ;; You can provide multiple base prototypes, and they'll be applied
    ;; from left to right, so the values in the right-most will "win."

    (add-prototype :graded-and-ranged {:grade "from graded" :range "all"})
    (add-prototype :multiple-ext-ab [:extended :graded-and-ranged] {})
    (add-prototype :multiple-ext-ba [:graded-and-ranged :extended] {})

    (is (= {:name "Name" :grade "from graded" :range "all"}
           (make :multiple-ext-ab)))
    (is (= {:name "Name" :grade 3 :range "all"}
           (make :multiple-ext-ba))))

  (testing "prototypes with independent generated values"

    ;; Use `gen` to provide a fn and args to have values generated when
    ;; you call make.

    (add-prototype :generated {:some-long (gen gen/long)})
    (add-prototype :generated-with-args
                   {:constrained-long (gen gen/long 5 11)})

    (is (number? (:some-long (make :generated))))
    (is (not= (:some-long (make :generated))
              (:some-long (make :generated))))
    ;; Note: That could fail if the `java.util.Random` in
    ;;       `clojure.data.generators` were feeling lazy.

    (is (<= 5
            (:constrained-long (make :generated-with-args))
            10))

    ;; If the fn you pass to gen is `make`, then you get maker-generated
    ;; maps as values in other maker-generated maps.

    (add-prototype :nested-generated
                   {:name "Nested"
                    :gen-simple (gen make :simple)
                    :gen-child (gen make :generated)})

    (let [nested1 (make :nested-generated)
          nested2 (make :nested-generated)]
      (is (number? (-> nested1 :gen-child :some-long)))
      (is (number? (-> nested2 :gen-child :some-long)))
      (is (not= (-> nested1 :gen-child :some-long)
                (-> nested2 :gen-child :some-long)))))

  (testing "prototypes with dependent generated values"

    ;; Generated values can also depend on other values, generated or not,
    ;; using `gen-from` and `gen-from-fn`.

    ;; `gen-from-fn` will apply the fn passed as the last arg to the
    ;; values whose keys you pass as the first arg.

    ;; XXX Maybe it would make more sense if it went (gen-from-fn inc :b)?
    (add-prototype :a-depends-b {:a (gen-from-fn [:b] inc)
                                 :b 1})

    (is (= {:a 2 :b 1} (make :a-depends-b)))
    (is (= {:a 3 :b 2} (make :a-depends-b {:b 2})))

    ;; gen-from is syntactic sugar for gen-from-fn when you want to create
    ;; the fn inline with references to the values you depend on.

    (add-prototype :phone
                   {:npa "212"
                    :nxx "555"
                    :xxxx "1212"
                    :formatted
                    (gen-from [npa nxx xxxx]
                              (str "(" npa ") " nxx "-" xxxx))})

    (is (= "(212) 555-1212" (:formatted (make :phone))))
    (is (= "555-1212"
           (:formatted
            (make :phone
                  :formatted (gen-from [nxx xxxx] (str nxx "-" xxxx))))))

    ;; So that gen-from is the same as:

    #_(gen-from-fn [:npa :nxx :xxxx]
                   (fn [npa nxx xxxx]
                     (str "(" npa ") " nxx "-" xxxx)))

    (add-prototype :chained-dependencies {:a (gen-from [b] (inc b))
                                          :b (gen-from-fn [:c :d] +)
                                          :c 1
                                          :d (gen-from-fn [:c] inc)})

    (is (= {:a 4 :b 3 :c 1 :d 2} (make :chained-dependencies)))
    (is (= {:a 2 :b 1 :c 1 :d 0} (make :chained-dependencies :d 0))))

  (testing "prototypes with circular value dependencies"

    (add-prototype :circular {:a (gen-from-fn [:b] inc)
                              :b (gen-from-fn [:c] inc)
                              :c (gen-from-fn [:a] inc)})

    (is (thrown? IllegalArgumentException (make :circular)))
    (is (= {:a 2 :b 1 :c 0} (make :circular :c 0)))
    (is (= {:a 3 :b 5 :c 4} (make :circular :a 3)))
    (is (= {:a 7 :b 6 :c 8} (make :circular :b 6)))))
