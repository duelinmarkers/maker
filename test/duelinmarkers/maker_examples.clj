(ns duelinmarkers.maker-examples
  (:use clojure.test)
  (:require [duelinmarkers.maker :as m :refer (add-prototype make)]
            [clojure.data.generators :as gen]))

(deftest maker-usage

  (testing "the simplest prototype"

    (add-prototype :simple {:name "Name"})

    (is (= {:name "Name"} (make :simple)))
    (is (= {:name "Override"}
           (make :simple {:name "Override"})
           (make :simple :name "Override")))
    (is (= {:name "Name" :other "Val"}
           (make :simple {:other "Val"})
           (make :simple :other "Val"))))

  (testing "a prototype based on another"

    (add-prototype :extended [:simple] {:grade 3})

    (is (= {:name "Name" :grade 3} (make :extended)))
    (is (= {:name "Override" :grade 4}
           (make :extended {:name "Override" :grade 4})
           (make :extended :name "Override" :grade 4)))
    (is (= {:name "Name" :grade 3 :other "Val"}
           (make :extended {:other "Val"})
           (make :extended :other "Val"))))

  (testing "a deeply extended prototype"

    (add-prototype :very-extended [:extended] {:dob "1978-02-11"})

    (is (= {:name "Name" :grade 3 :dob "1978-02-11"}
           (make :very-extended))))

  (testing "a multiply-extended prototype"

    (add-prototype :graded-and-ranged {:grade "from graded" :range "all"})
    (add-prototype :multiply-extended-ab [:extended :graded-and-ranged] {})
    (add-prototype :multiply-extended-ba [:graded-and-ranged :extended] {})

    (is (= {:name "Name" :grade "from graded" :range "all"}
           (make :multiply-extended-ab)))
    (is (= {:name "Name" :grade 3 :range "all"}
           (make :multiply-extended-ba))))

  (testing "prototypes with independent generated values"

    (add-prototype :generated {:some-long ^::m/gen [gen/long]})
    (add-prototype :generated-with-args
                   {:constrained-long ^::m/gen [gen/long 5 11]})
    (add-prototype :nested-generated
                   {:name "Nested"
                    :gen-simple ^::m/gen [make :simple]
                    :gen-child ^::m/gen [make :generated]})

    (is (number? (:some-long (make :generated))))
    (is (not= (:some-long (make :generated))
              (:some-long (make :generated))))
    (is (<= 5 (:constrained-long (make :generated-with-args)) 10))
    (let [nested1 (make :nested-generated)
          nested2 (make :nested-generated)]
      (is (number? (-> nested1 :gen-child :some-long)))
      (is (number? (-> nested2 :gen-child :some-long)))
      (is (not= (-> nested1 :gen-child :some-long)
                (-> nested2 :gen-child :some-long)))))

  (testing "prototypes with dependent generated values"

    (add-prototype :a-depends-b {:a ^{::m/gen :from} [[:b] inc]
                                 :b 1})

    (is (= {:a 2 :b 1} (make :a-depends-b)))
    (is (= {:a 3 :b 2} (make :a-depends-b {:b 2})))

    (add-prototype :chained-dependencies {:a ^{::m/gen :from} [[:b] inc]
                                          :b ^{::m/gen :from} [[:c :d] +]
                                          :c 1
                                          :d ^{::m/gen :from} [[:c] inc]})

    (is (= {:a 4 :b 3 :c 1 :d 2} (make :chained-dependencies)))
    (is (= {:a 2 :b 1 :c 1 :d 0} (make :chained-dependencies :d 0)))))
