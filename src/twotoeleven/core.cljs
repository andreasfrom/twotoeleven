(ns twotoeleven.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >!]]))

;;; Debugging

(enable-console-print!)
(def p (fn [x] (println x) x))

;;; Utilities

(def reversev (comp vec reverse))

(def transpose
  "Turn the columns of a matrix into rows:
     |
     v
   [[1 2 3]      ->[[1 1 1]
    [1 2 3]   =>    [2 2 2]
    [1 2 3]]        [3 3 3]]"
  (partial apply mapv vector))

;;; Game Logic

(def empty-tile 0)
(def empty-tile? (partial = empty-tile))

(defn empty-coords
  "Returns a list of coordinates of empty tiles."
  [board]
  (->> board
       (map-indexed (fn [i x] (map-indexed (fn [j y] (when (empty-tile? y) [i j])) x)))
       (mapcat identity)
       (remove nil?)))

(defn add-tile
  "Returns a board with an added tile of either 2 or 4.
   Has same probabilities as original game."
  [board]
  (let [emptys (empty-coords board)
        [x y] (nth emptys (rand-int (count emptys)))]
    (assoc-in board [x y] (if (< (rand) 0.9) 2 4))))

(defn row-left
  "Steps a row left, letting tiles slide over empty ones and merge when appropriate.
   Takes a plain row and returns a map of the new :row and the gained :score."
  [row]
  (loop [acc []
         [x y & rs] row
         emptys 0
         score 0]
    (condp = [x y]
      ;; end of list, return the new row and gained score
      [nil y] {:row (into acc (repeat emptys empty-tile)) :score score}

      ;; skip emptys, but remember them for later
      [empty-tile y] (recur acc (vec (conj rs y)) (inc emptys) score)
      [x empty-tile] (recur acc (vec (conj rs x)) (inc emptys) score)

      ;; merge the two tiles and increase score
      [y x] (recur (conj acc (+ x y)) (vec rs) (inc emptys) (+ score x y))

      ;; simply move the tile
      (recur (conj acc x) (vec (conj rs y)) emptys score))))

(defn row-right
  "Steps a row right by reversing it so we can just step left and then undoing the reversal."
  [row]
  (-> row reversev row-left (update-in [:row] reversev)))

(defn assemble
  "Assembles individual rows and scores into a map of
   the :board as a matrix and the total :score."
  [rows]
  {:board (mapv :row rows)
   :score (reduce + (map :score rows))})

(def step-left (comp assemble (partial mapv row-left)))
(def step-right (comp assemble (partial mapv row-right)))

(defn pure-move [dir board]
  "Moves a whole board in the specified direction: :up, :down, :left or :right
   Returns map of :board and :score"
  (let [transpose-in #(update-in % [:board] transpose)]
    (case dir
      :left (step-left board)
      :right (step-right board)
      :up (-> board transpose step-left transpose-in)
      :down (-> board transpose step-right transpose-in))))

(defn move
  "Not only moves, but also adds a tile if moving in the
   given direction actually moves any tiles.
   Returns nil otherwise."
  [dir board]
  (let [{new-board :board :as new} (pure-move dir board)]
    (when-not (= new-board board)
      (update-in new [:board] add-tile))))

;;; Game UI

(def dirs
  "Maps arrow-key keycodes to directions."
  {38 :up, 40 :down, 37 :left, 39 :right})

(defn board
  "Visual representation of a board."
  [data owner {:keys [board row cell score click delete?]}]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className board}
               (dom/div #js {:className "board-header"}
                        (dom/div #js {:className score} "Score: " (:score (peek (:now data))))
                        (when delete?
                          (dom/div #js {:className "delete" :onClick #(put! click [:delete (:id @data)])} "Delete"))
                        (dom/br nil))
               (apply dom/div #js {:className (:active data)
                                   :onClick #(when click (put! click [:select (:id @data)]))}
                      (for [row (:board (peek (:now data)))]
                        (apply dom/div #js {:className row}
                               (for [tile row]
                                 (dom/div #js {:className (str cell " _" (if (empty-tile? tile) "empty" tile))}
                                          (if (empty-tile? tile) "_" tile))))))))))

(defn game
  "Handles the concept of a game: moving pieces, score and history."
  [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (let [dir (<! (:moves @data))
              [old old-score] ((juxt :board :score) (peek (:now @data)))]
          (when-let [{:keys [board score]} (move dir old)]
            (let [new (assoc (peek (:now @data))
                        :board board
                        :score (+ old-score score)
                        :id (inc (:id (peek (:now @data)))))]
              (om/transact! data :now #(conj % new))
              (om/update! data :forward '()))))
        (recur)))

     om/IRender
     (render [_]
       (dom/div nil
                (om/build board data
                          {:opts {:board "main-board" :row "main-row" :cell "main-cell" :score "main-score"}})

                (dom/button #js {:onClick (fn [e] (let [l (peek (:now @data))]
                                                    (when (second (:now @data))
                                                      (om/transact! data [:forward] #(conj % l))
                                                      (om/transact! data [:now] pop))))}
                            (str "Undo (" (dec (count (:now data))) ")"))

                (dom/button #js {:onClick (fn [e] (when-let [n (peek (:forward @data))]
                                                    (om/transact! data [:now] #(conj % n))
                                                    (om/transact! data [:forward] pop)))}
                            (str "Redo (" (count (:forward data)) ")"))))))

(defn widget
  "Master widget: Administrates overall layout, which games are selected, where key events are sent etc."
  [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [c (chan)]
        (.addEventListener js/document "keydown" #(put! c %))
        (go-loop []
          (when-let [dir (dirs (.-keyCode (<! c)))]
            (>! (:moves (get (:games @data) (:active @data))) dir))
          (recur))

        (go-loop []
          (let [[cmd id] (<! (:click @data))]
            (condp = cmd
              :select (om/update! data :active id)
              :delete (do (om/transact! data :games #(dissoc % id))
                          (om/update! data :active (first (first (:games @data)))))))
          (recur))))

    om/IRender
    (render [_]
      (dom/div #js {:id "wrapper"}
               (om/build game (get (:games data) (:active data)) {:key :id})

               (dom/button #js {:onClick (fn [_] (let [id (inc (:running-id @data))
                                                       game (get (:games @data) (:active @data))]
                                                   (om/transact! data :games #(assoc % id (assoc game :id id :moves (chan))))
                                                   (om/update! data :active id)
                                                   (om/update! data :running-id id)))}
                           "Branch")

               (apply dom/div #js {:className "minis"}
                      (let [games (map (fn [game] (assoc game :active (when (= (:id game) (:active data)) "active")))
                                       (vals (:games data)))]
                        (om/build-all board games
                                    {:opts {:board "mini-board" :row "mini-row" :cell "mini-cell" :score "mini-score"
                                            :click (:click data) :delete? (second (:games data))}})))))))

(def app-state
  "All necessary data for the application.
  A hash-map of games with their own history, the active game, etc."
  (atom {:games {0 {:now [{:board (add-tile (add-tile (vec (repeat 4 (vec (repeat 4 empty-tile))))))
                           :score 0
                           :id 0}]
                    :forward []
                    :id 0
                    :moves (chan)}}
         :active 0
         :click (chan)
         :running-id 0}))

(om/root widget app-state {:target (.getElementById js/document "app")})
