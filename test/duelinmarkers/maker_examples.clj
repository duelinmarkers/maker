(ns duelinmarkers.maker-examples
  (:use clojure.test duelinmarkers.maker))

(add-prototype :simple {:name "Name"})

(add-prototype :extended [:simple] {:grade 3})

(add-prototype :deeply-extended [:extended] {:dob "1978-02-11"})

(deftest make-from-prototypes

  (testing "the simplest prototype"
    (is (= {:name "Name"} (make :simple)))
    (is (= {:name "Override"}
           (make :simple {:name "Override"})
           (make :simple :name "Override")))
    (is (= {:name "Name" :other "Val"}
           (make :simple {:other "Val"})
           (make :simple :other "Val"))))

  (testing "a prototype based on another"
    (is (= {:name "Name" :grade 3} (make :extended)))
    (is (= {:name "Override" :grade 4}
           (make :extended {:name "Override" :grade 4})
           (make :extended :name "Override" :grade 4)))
    (is (= {:name "Name" :grade 3 :other "Val"}
           (make :extended {:other "Val"})
           (make :extended :other "Val")))))
