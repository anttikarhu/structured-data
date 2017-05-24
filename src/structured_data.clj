(ns structured-data)

(defn do-a-thing [x]
  (let [x+x (+ x x)]
   (Math/pow x+x x+x)))

(defn spiff [v]
  (+ (get v 0) (get v 2))) ; nth toimisi tässä samalla tavalla

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first _ third] v]
   (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let [[[x1] [x2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
   (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
   (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point] ; Jos destrukturoidaan tässä, voidaan nimetä että ':as rectangle' esimerkiksi
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
   (and
    (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
   (and
     (contains-point? outer p1)
     (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [{authors :authors} book] ;voiko authorsin destrukturoida kätevämmin?
   (assoc book :authors
    (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [collection] (get collection 1))]
   (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

(defn contains-duplicates? [a-seq]
 (not=
   (count a-seq)
   (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (let [authors (get book :authors)] ; sama kysymys kuin edellä
   (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth-year-known? (fn [author] (contains? author :birth-year))
        death-year-known? (fn [author] (contains? author :death-year))
        name (get author :name)
        birth-year (get author :birth-year)
        death-year (get author :death-year)]
   (if (death-year-known? author) ; tämä on tosi kömpelön oloisesti tehty...
    (str name " (" birth-year " - " death-year ")")
    (if (birth-year-known? author)
     (str name " (" birth-year " - )")
     (str name)))))

(defn authors->string [authors]
 (apply str (interpose ", " (set (map author->string authors)))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors)))) ; ilmeisesti voi laittaa että (:title book) ja (:authors book)?

(defn books->string [books] ; tämä on kanssa ihme ripulia
  (let [bookCount (count books)]
   (if (= 0 bookCount)
    "No books."
    (str
     bookCount " book" (if (< 1 bookCount) "s. " ". ")
     (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
