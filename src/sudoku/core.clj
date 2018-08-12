(ns sudoku.core
  (:gen-class))

(def sum #(reduce + %))

(defn get-cell-values
  "Returns the values from a sub-section of a grid"
  [grid row-start col-start row-end col-end]
  (for [row (range row-start row-end) col (range col-start col-end)]
    ((grid row) col)))

(defn good-range?
  "Checks if a sub-section of a grid contains no duplicates (empty cells are ignored)"
  [grid row-start col-start row-end col-end]
  (let [values (get-cell-values grid row-start col-start row-end col-end)]
    (= (sum values) (sum (distinct values)))))

(defn good?
  "Checks if the filled in numbers of a sudoku are consistent with each other"
  [grid]
  (and
    (every? #(good-range? grid % 0 (+ % 1) 9) (range 0 9))
    (every? #(good-range? grid 0 % 9 (+ % 1)) (range 0 9))
    (every? true? (for [row [0 3 6] col [0 3 6]] (good-range? grid row col (+ row 3) (+ col 3))))))

(defn forbidden-values
  "Values that can not go into a given empty cell. I.e. values that occur in the same row, column or square"
  [grid row col]
  (let [sq-row (* (quot row 3) 3) sq-col (* (quot col 3) 3)]
  (distinct
    (into
      (get-cell-values grid sq-row sq-col (+ sq-row 3) (+ sq-col 3))
      (into 
        (into [] (get-cell-values grid row 0 (+ row 1) 9))
        (get-cell-values grid 0 col 9 (+ col 1)))))))

(defn next-cell
  "Returns the coordinates of a cell with the lowest possible branching factor (possible digits)"
  [grid]
  (let [empty-cells (filter #(< ((grid (% 0)) (% 1)) 1) (for [row (range 0 9) col (range 0 9)] [row col]))]
    (if (empty? empty-cells)
      nil
      (apply max-key #(count (forbidden-values grid (% 0) (% 1))) empty-cells))))

(defn solve
  [grid]
  (let [cell (next-cell grid)]
    (if (not cell)
      grid
      (let [forbidden (forbidden-values grid (cell 0) (cell 1))]
        (loop [i 1]
          (if (<= i 9)
            (if (not (some #(= % i) forbidden))
              (let [solved-grid (solve (assoc grid (cell 0) (assoc (grid (cell 0)) (cell 1) i)))]
                (if solved-grid
                  solved-grid
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
  (let [input-grid (read-sudoku (java.io.BufferedReader. *in*))]
    (println "Input sudoku:")
    (print-sudoku input-grid)
    (let [solved-grid (solve input-grid)]
      (println "Output sudoku:")
      (print-sudoku solved-grid)
      (if (not (good? solved-grid))
        (println "Something is wrong here, sudoku no good :(")))))
