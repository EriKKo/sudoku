(ns sudoku.core
  (:gen-class))

(def sum #(reduce + %))

(defn get-range
  [s r1 c1 r2 c2]
  (for [r (range r1 r2) c (range c1 c2)]
    ((s r) c)))

(defn good-range
  [s r1 c1 r2 c2]
  (let [values (get-range s r1 c1 r2 c2)]
    (= (sum values) (sum (distinct values)))))

(defn good?
  [s]
  (and
    (every? #(good-range s % 0 (+ % 1) 9) (range 0 9))
    (every? #(good-range s 0 % 9 (+ % 1)) (range 0 9))
    (every? true? (for [x [0 3 6] y [0 3 6]] (good-range s x y (+ x 3) (+ y 3))))))

(defn forbidden-values
  [s r c]
  (let [r0 (* (quot r 3) 3) c0 (* (quot c 3) 3)]
  (distinct
    (into
      (get-range s r0 c0 (+ r0 3) (+ c0 3))
      (into 
        (into [] (get-range s r 0 (+ r 1) 9))
        (get-range s 0 c 9 (+ c 1)))))))

(defn next-cell
  "Returns the coordinates of a cell with the lowest possible branching factor (possible digits)"
  [s]
  (let [empty-cells (filter #(< ((s (% 0)) (% 1)) 1) (for [r (range 0 9) c (range 0 9)] [r c]))]
    (if (empty? empty-cells)
      nil
      (apply max-key #(count (forbidden-values s (% 0) (% 1))) empty-cells))))

(defn solve
  [s]
  (let [cell (next-cell s)]
    (if (not cell)
      s
      (let [forbidden (forbidden-values s (cell 0) (cell 1))]
        (loop [i 1]
          (if (<= i 9)
            (if (not (some #(= % i) forbidden))
              (let [res (solve (assoc s (cell 0) (assoc (s (cell 0)) (cell 1) i)))]
                (if res
                  res
                  (recur (+ i 1))))
              (recur (+ i 1)))
            nil))))))

(defn parse-cell
  [cell]
  (if (re-matches #"[1-9]" cell)
    (. Integer parseInt cell)
    0))

(defn parse-line
  [line]
  (into [] (map parse-cell (clojure.string/split line #" "))))

(defn read-sudoku
  [input]
  (into [] (map parse-line (line-seq input))))

(defn print-sudoku
  [s]
  (doseq [row s]
    (println (into [] (map #(if (= % 0) "." %) row)))))

(defn -main
  [& args]
  (let [m (read-sudoku (java.io.BufferedReader. *in*))]
    (println "Input sudoku:")
    (print-sudoku m)
    (let [solved (solve m)]
      (println "Output sudoku:")
      (print-sudoku solved)
      (if (not (good? solved))
        (println "Something is wrong here, sudoku no good :(")))))
