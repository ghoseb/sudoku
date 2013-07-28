(ns sudoku)

;;; A sudoku solver in Clojure using a simple backtracking search algorithm.

(def rows
  [[ 0  1  2  3  4  5  6  7  8]
   [ 9  10 11 12 13 14 15 16 17]
   [18 19 20 21 22 23 24 25 26]
   [27 28 29 30 31 32 33 34 35]
   [36 37 38 39 40 41 42 43 44]
   [45 46 47 48 49 50 51 52 53]
   [54 55 56 57 58 59 60 61 62]
   [63 64 65 66 67 68 69 70 71]
   [72 73 74 75 76 77 78 79 80]])

(def cols
  [[0  9 18 27 36 45 54 63 72]
   [1 10 19 28 37 46 55 64 73]
   [2 11 20 29 38 47 56 65 74]
   [3 12 21 30 39 48 57 66 75]
   [4 13 22 31 40 49 58 67 76]
   [5 14 23 32 41 50 59 68 77]
   [6 15 24 33 42 51 60 69 78]
   [7 16 25 34 43 52 61 70 79]
   [8 17 26 35 44 53 62 71 80]])

(def boxes
  [[ 0  1  2  9 10 11 18 19 20]
   [ 3  4  5 12 13 14 21 22 23]
   [ 6  7  8 15 16 17 24 25 26]
   [27 28 29 36 37 38 45 46 47]
   [30 31 32 39 40 41 48 49 50]
   [33 34 35 42 43 44 51 52 53]
   [54 55 56 63 64 65 72 73 74]
   [57 58 59 66 67 68 75 76 77]
   [60 61 62 69 70 71 78 79 80]])

(def units (concat rows cols boxes))

(defn read-sq
  "read the value of a square from the board"
  [board pos]
  (nth board pos))


(defn fill-sq
  "set the value of pos on board to n"
  [board pos n]
  (concat (take pos board) (cons n (drop (inc pos) board))))


(defn find-blank
  "find the first blank position on board"
  [board]
  (when (seq board)
    (if (zero? (first board))
      0
      (inc (find-blank (rest board))))))


(defn fill-all
  "fill given pos on board with all possible values [1,9]"
  [board pos]
  (for [i (range 9)]
    (fill-sq board pos (inc i))))

(defn valid-board?
  "check if a given board is valid"
  [board]
  (letfn [(unit-ok? [unit]
            (let [xs (map #(nth board %) unit)]
              (every? #(= % 1) (vals (dissoc (frequencies xs) 0)))))]
    (every? unit-ok? units)))


(defn next-boards
  "generate all possible next valid boards from board"
  [board]
  (filter valid-board? (fill-all board (find-blank board))))


(defn solved?
  "check if a board is solved"
  [board]
  (every? pos? board))


(defn solve
  "solve a valid sudoku board"
  [board]
  (if (solved? board)
    board
    (some solve (next-boards board))))


(comment
  (def *easy-board
    [2 7 4 0 9 1 0 0 5
     1 0 0 5 0 0 0 9 0
     6 0 0 0 0 3 2 8 0
     0 0 1 9 0 0 0 0 8
     0 0 5 1 0 0 6 0 0
     7 0 0 0 8 0 0 0 3
     4 0 2 0 0 0 0 0 9
     0 0 0 0 0 0 0 7 0
     8 0 0 3 4 9 0 0 0])

  (def *hard-board
    [5 0 0 0 0 4 0 7 0
     0 1 0 0 5 0 6 0 0
     0 0 4 9 0 0 0 0 0
     0 9 0 0 0 7 5 0 0
     1 8 0 2 0 0 0 0 0
     0 0 0 0 0 6 0 0 0
     0 0 3 0 0 0 0 0 8
     0 6 0 0 8 0 0 0 9
     0 0 8 0 7 0 0 3 1])

  (def *hardest-board
    [0 0 5 3 0 0 0 0 0
     8 0 0 0 0 0 0 2 0
     0 7 0 0 1 0 5 0 0
     4 0 0 0 0 5 3 0 0
     0 1 0 0 7 0 0 0 6
     0 0 3 2 0 0 0 8 0
     0 6 0 5 0 0 0 0 9
     0 0 4 0 0 0 0 3 0
     0 0 0 0 0 9 7 0 0]))
