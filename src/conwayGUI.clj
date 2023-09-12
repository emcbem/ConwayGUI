(ns conwayGUI
  (:require [clojure.test :refer :all])
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Our extra credit is a new color of dots each game we run

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
  (q/frame-rate 100)
  (def color [(q/random 255) (q/random 255) (q/random 255)])
  (remove nil? (into #{}  (for [i (range 40) j (range 40)] (if (< (rand 1) 0.1) [i j])))))

(defn draw [old-state]
  (q/background 220 224 221)
  (doseq [i old-state] ((q/fill (first color) (second color) (nth color 2)) (q/stroke 80 0 255) (q/rect (* 10 (first i)) (* 10 (second i)) 10 10))))

(q/defsketch conway
  :title "Conway Game of LIfe"
  :setup #'setup
  :size [500 500]
  :update #'next-generation-living
  :draw #'draw
  :middleware [m/fun-mode])

(defn -main [& args])


