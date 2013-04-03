(ns duelinmarkers.maker-examples
  (:use clojure.test duelinmarkers.maker))

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

    (is (= {:name "Name" :grade 3 :dob "1978-02-11"} (make :very-extended))))

  (testing "a multiply-extended prototype"

    (add-prototype :graded-and-ranged {:grade "from graded" :range "all"})
    (add-prototype :multiply-extended-ab [:extended :graded-and-ranged] {})
    (add-prototype :multiply-extended-ba [:graded-and-ranged :extended] {})

    (is (= {:name "Name" :grade "from graded" :range "all"}
           (make :multiply-extended-ab)))
    (is (= {:name "Name" :grade 3 :range "all"}
           (make :multiply-extended-ba))))

  (testing "a record prototype"

    (defrecord FooBear [hair claws])
    (add-prototype :foo-bear (->FooBear "brown" "big and scary"))

    (is (= (->FooBear "brown" "big and scary") (make :foo-bear)))
    (is (= (map->FooBear {:hair "thinning" :claws "big and scary" :other "Val"})
           (make :foo-bear {:hair "thinning" :other "Val"})
           (make :foo-bear :hair "thinning" :other "Val")))))
