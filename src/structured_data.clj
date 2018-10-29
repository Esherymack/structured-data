(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
        (Math/pow xx xx)))

(defn spiff [v]
  (if (empty? v)
    false
    (if (<= (count v) 2)
      false
      (let [a (get v 0)
            b (get v 2)]
        (+ a b)))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (empty? v)
    false
    (if (<= (count v) 2)
      false
      (let [[x _ z] v]
        (+ x z)))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])
 
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle
          h (- y2 y1)
          w (- x2 x1)]
      (if (== h w)
        true
        false)))

(defn area [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle
        h (- y2 y1)
        w (- x2 x1)]
      (* h w)))

(defn contains-point? [rectangle point]
  (let [[x1 y1] (get rectangle 0)
        [x2 y2] (get rectangle 1)
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (let [na (author-count book)]
    (if (> na 1)
      true
      false)))

(defn add-author [book new-author]
 (let [na(conj (:authors book) new-author)]
    (assoc book :authors na)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [g (fn [x] (get x 1))]
    (map g collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [a (map :authors books)]
    (apply clojure.set/union a)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn a-birth-and-death-years [b, d]
  (cond
   (nil? b) ""
   (nil? d) (format " (%s - )" b)
   :else (format " (%s - %s)" b, d))
)

(defn author->string [author]
  (let [a (author :name)]
    (let [b (author :birth-year)]
      (let [c (author :death-year)]
        (format "%s%s" a (a-birth-and-death-years b c))))))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [b-title (book :title)]
    (let [b-strings (authors (list book))]
      (format "%s, written by %s" b-title (authors->string b-strings)))))

(defn books->string [books]
  (let [a (apply str(interpose ", " (map book->string books)))]
    (cond
      (= 0 (count books)) "No books."
      (= 1 (count books)) (format "1 book. %s." a)
      :else (format "%d books. %s." (count books) a))))

(defn books-by-author [author books]
  (filter (fn [a] (has-author? a author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (a :name))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
