(ns conwayGUI
  (:require [clojure.test :refer :all])
  (:require [quil.core :as q]
            [quil.helpers.seqs :refer [range-incl]]
            [quil.middleware :as m]))


(defn neighbors-of [[x y]]
  (into #{} (remove (fn [cell] (= [x y] cell)) (for [i [-1 0 1] j [-1 0 1]] [(- x i) (- y j)]))))

(defn num-living-neighbors [[x y] living]
  (count (filter (fn [x] (not= nil x))  (for [i living] ((neighbors-of [x y]) i)))))

(defn will-live? [[x y] living]
  (def neighbors (num-living-neighbors [x y] living))
  (if (or (< neighbors 2) (> neighbors 3) (and (= (living [x y]) nil) (not (= neighbors 3))))
    false true))

(defn next-generation-living [state]
  (let [living (into #{} state)]
  (into #{} (filter (fn [x] (will-live? x living)) (reduce clojure.set/union (into #{} (map neighbors-of living)))))))



(with-test (defn neighbors-of-test [[x y]] (neighbors-of [x y])) (is (= #{[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]} (neighbors-of-test [0 0]))))
(with-test (defn num-living-neighbors-test [cell living] (num-living-neighbors cell living)) (is (= 3 (num-living-neighbors-test [0 0] #{[0 1] [1 0] [-1 -1] [6 9]}))))

(with-test (defn will-live-test [cell living] (will-live? cell living)) (is (= false (will-live-test [0 0] #{}))) (is (= false (will-live-test [0 0] #{[1 1] [-1 -1]}))) (is (= true (will-live-test [0 0] #{[1 1] [1 0] [-1 -1]}))))

(with-test (defn next-generation-living-test [living] (next-generation-living living)) (is (= #{} (next-generation-living-test #{[0 0]}))) (is (= #{[0 0]} (next-generation-living-test #{[0 0] [1 1] [-1 -1]}))))

(defn tests [opts]
  (run-all-tests))

(defn setup []
  (q/frame-rate 1)
  ;; (q/background 0 45 0)
  (remove nil? (into #{}  (for [i (range 20) j (range 20)] (if (< (rand 1) 0.1) [i j])))))
;GUI function

(defn draw [old-state]
  (println old-state)
  (q/background (q/random 255) 45 200) 
  (doseq [i old-state] (q/rect (* 10 (first i)) (* 10 (second i)) 10 10))
  )
  

(q/defsketch conway
  :title "Conway Game of LIfe"
  :setup #'setup
  :size [500 500]
  :update #'next-generation-living
  :draw #'draw
  :middleware [m/fun-mode])

(defn -main [& args])


