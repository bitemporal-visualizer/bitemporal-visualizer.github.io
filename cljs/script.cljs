(require '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[clojure.string :as str])
(def sample1 "filll | app_start | app_end | sys_start | sys_end
             ------------+-------------+-------
             red | 3 | 4 | 0 | 1
             pink | 1 | 3 | 0 | 1
             lightblue | 0 | 1 | 0 | 2
             orange | 2 | 4 | 1 | 2
             lightpink | 1 | 2 | 1 | 2")

(def sample "product_id	| sys_start |	sys_end |	app_start |	app_end |	price
            1002 |	2019-01-01 |	2019-02-09|	2019-01-05|	9999-12-31|	100
            1002|	2019-02-09|	9999-12-31|	2019-01-05|	2019-01-10|	100
            1002|	2019-02-09|	2019-03-02|	2019-01-10|	2019-02-08|	100
            1002|	2019-02-09|	2019-02-10|	2019-02-08|	9999-12-31|	105
            1002|	2019-02-10|	2019-03-02|	2019-02-08|	2019-02-15|	105
            1002|	2019-02-10|	2019-02-12|	2019-02-15|	2019-02-17|	105
            1002|	2019-02-10|	2019-02-12|	2019-02-17|	9999-12-31|	103
            1002|	2019-02-12|	2019-03-02|	2019-02-15|	9999-12-31|	102
            1002|	2019-03-02|	2019-04-01|	2019-01-10|	9999-12-31|	103
            1002|	2019-04-01|	9999-12-31|	2019-01-10|	2019-04-02|	103
            1002|	2019-04-01|	9999-12-31|	2019-04-02|	9999-12-31|	111")

(def color-selected "greenyellow")
(def color-selected-data "lightgreen")
(def color-hovered "lightblue")
(def color-hovered-data "lightsteelblue")

(defn table-string->maps [s]
  (let [[ks & vs] (->> (-> s
                           (str/split #"\n"))
                       (filter #(not (or (str/starts-with? % "-")
                                         (str/starts-with? % "+"))))
                       (map #(if (str/starts-with? % "|")
                               (subs % 1)
                               %))
                       (map #(map str/trim (str/split % #"\|"))))]
    (map #(zipmap (map keyword ks) %) vs)))
(defn map->row [m]
  (assoc {:app-start (:app_start m)
          :app-end (:app_end m)
          :sys-start (:sys_start m)
          :sys-end (:sys_end m)}
         :row m))

;; row_start, row_end (system time aliases)

(def state (r/atom {:clicks 0 :t sample :collapsed true}))

(def mouse-coordinates (r/atom {:x 100 :y 100}))

(defn data= [a b]
  (apply = (map #(dissoc % "row_start" "row_end" "system_time_end" "system_time_start" "application_time_end" "application_time_start" :sys_start :sys_end :app_start :app_end)
                [a b])))


(defn my-component [viz]
  [:div
   {:onMouseMove (fn [event]
                   (reset! mouse-coordinates {:x (.-clientX event) :y (.-clientY event)}))}
   [:div {:style {:display "grid" :grid-template-columns (if (:collapsed @state) "30% 70%" "30% 30% 40%") :grid-template-rows "80vh"}}
    (when-not (:collapsed @state) [:div [:textarea {:style {:width "95%" :height "95%" :overflow-x "scroll" :overflow-y "scroll"}
                                                    :on-change #(reset! state (assoc @state :t (.. % -target -value)))}
                                         (:t @state)]])
    [:div {:style {:overflow-x "auto" :overflow-y "auto"}}
     [:p [:a {:href "#"
              :onMouseUp #(swap! state #(update % :collapsed not))
              } (if (:collapsed @state)
                  "Show Textarea"
                  "Hide Textarea")]]
     (let [d (table-string->maps (:t @state))
           ks (keys (first d))]
       (into [:table {:style {:border "1px solid black" :border-collapse "collapse"}} [:tr (for [h ks] [:th {:style {:border "1px solid black" :padding "4pt"}} h])]]
             (for [[i j] (map-indexed vector d)]
               ^{:key (str i j)} [:tr {:onMouseOver #(reset! state (assoc @state :hovered j))
                                       :onMouseOut #(reset! state (dissoc @state :hovered))
                                       :onMouseUp #(reset! state (if (= j (:selected @state))
                                                                   (dissoc @state :selected)
                                                                   (assoc @state :selected j)))
                                       :style {:cursor "pointer"
                                               :background-color (cond
                                                                   (= j (:selected @state)) color-selected
                                                                   (data= j (:selected @state)) color-selected-data
                                                                   (= j (:hovered @state)) color-hovered
                                                                   (data= j (:hovered @state)) color-hovered-data
                                                                   :else "")}}
                                  (for [k ks] [:td {:style {:border "1px solid black" :padding "4pt"}} (get j k)])])))
     ]
    [:div 

     ;;[:p "x: " (:x @mouse-coordinates)]
     ;;[:p "y: " (:y @mouse-coordinates)]
     (viz (map map->row (table-string->maps (:t @state))))]]
   [:p "Created for " [:a {:href "https://xtdb.com"} "XTDB"] " using " [:a {:href "https://github.com/babashka/scittle"} "Scittle"]]])


#_(rdom/render [my-component] (.getElementById js/document "app"))

(def m "+------+---------------+------------+------------+
       | id   | name          | date_start | date_end   |
       +------+---------------+------------+------------+
       |    1 | Daily Special | 2019-05-03 | 2019-05-14 |
       |    2 | Daily Special | 2019-05-20 | 2019-06-14 |
       |    3 | Daily Special | 2019-04-01 | 2019-05-30 |
       |    4 | Daily Special | 2019-04-01 | 2019-06-12 |
       +------+---------------+------------+------------+")

(def x "first_name |  last_name  | films
       ------------+-------------+-------
       \"GINA\"     | \"DEGENERES\" | 42")


;; secondary-highlight all same values also (hovered and selected)

;; highlight hovered value
;; select to persist

(defn greyscale
  ([n] (into [] (greyscale (dec n) 0)))
  ([n i] (lazy-seq (cons #_(format "#%06x" (long (* (/ 16rFFFFFF n) i)))
                         (str "#" (let [dd (.padStart (.toString (.round js/Math (+ 16r44 (* (/ 16r88 n) i))) 16) 2 "0")] (str dd dd dd)))
                         (when (< i n)
                           (greyscale n (inc i)))))))

#_(let [n 3]
    (for [i (range n)]
      (long (* (/ 100 n) i))
      ))
;; convert long to hex clojure



(defn hiccup [v]
  (cond (vector? v)
        (let [tag (first v)
              attrs (second v)
              attrs (when (map? attrs) attrs)
              elts (if attrs (nnext v) (next v))
              tag-name (name tag)]
          (str "<" tag-name (hiccup attrs) ">" (hiccup elts) "</" tag-name ">\n"))
        (map? v)
        (str/join ""
                  (map (fn [[k v]]
                         (str " " (name k) "=\"" v "\"")) v))
        (seq? v)
        (str/join " " (map hiccup v))
        :else (str v)))



(defn svg [attributes & content]
  (str "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n"
       "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
       \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
       (hiccup
         (apply vector :svg
                (conj attributes
                      ["xmlns" "http://www.w3.org/2000/svg"]
                      ["xmlns:xlink" "http://www.w3.org/1999/xlink"]
                      ["version" "1.1"])
                content))))

(defn iso-timestamp [x] (identity x))

;; WARNING: doesn't currently use tx-id to tiebreak tx-time, so may generate visual errors where different txes happen in the same millisecond
(defn normalize-entries [w h xs]
  (let [xs (sort-by (juxt :sys-start :app-start) xs) ;; core1/simplified draw order needs :asc sys-then-app
        fills (greyscale (count xs))
        sys (sort (distinct (concat (map :sys-start xs) (map :sys-end xs)))) 
        sys-map (zipmap sys (range))
        cell-w (/ w (dec (count sys)) 1.0)
        app (sort (distinct (concat (map :app-start xs) (map :app-end xs))))
        app-map (zipmap app (range))
        cell-h (/ h (dec (count app)) 1.0)]
    [[w h cell-w cell-h sys app]
     (for [[xi {:keys [app-start app-end sys-start sys-end] :as x}] (map-indexed vector xs)]
       (let [x-w (* (- (sys-map sys-end) (sys-map sys-start)) cell-w)
             x-h (* (- (app-map app-end) (app-map app-start)) cell-h)]
         [(* (sys-map sys-start) cell-w)
          (- h x-h (* (app-map app-start) cell-h))
          x-w
          x-h
          (assoc x :fill (nth fills xi))]))]))

(defn axes->g [[width height cell-w cell-h sys app]]
  (apply conj [:g] (concat (for [[si s] (map-indexed vector sys)]
                             [:g {:transform (str "translate("
                                                  (+ (* si cell-w) (* -0.025 width))
                                                  ","
                                                  (+ height (* 0.075 height))
                                                  ")")}
                              [:text {:fill "black"
                                      :font-size "1.5em"
                                      :transform-box "fill-box"
                                      :transform "rotate(30)"}
                               (str s)]])
                           (for [[ai a] (map-indexed vector app)]
                             [:text {:fill "black"
                                     :font-size "1.5em"
                                     :x (+ width (* 0.025 width))
                                     :y (- (- height (* 0.025 height)) (* ai cell-h))}
                              (str a)]))

         ))

(defn history->hiccup [width height color-att entries]
  (let [[axes normalized-entries] (normalize-entries width height entries)]
    (->> normalized-entries

         (map (fn [[x y w h e :as entry]]
                ; (prn entry)
                ;(prn e (:row e) (:hovered @state))
                [:g
                 [:rect {:x x
                         :y y
                         :width w
                         :height h
                         :onMouseOver #(reset! state (assoc @state :hovered (:row e)))
                         :onMouseOut #(reset! state (dissoc @state :hovered))
                         :onMouseUp #(reset! state (if (= (:row e) (:selected @state))
                                                     (dissoc @state :selected)
                                                     (assoc @state :selected (:row e))))
                         :fill (or (cond (= (:row e) (:selected @state)) color-selected
                                         (data= (:row e) (:selected @state)) color-selected-data
                                         (= (:row e) (:hovered @state)) color-hovered
                                         (data= (:row e) (:hovered @state)) color-hovered-data)
                                   (:fill (:row e))
                                   (color-att e)
                                   #_(str "#" (format "%06x" (rand-int 16rFFFFFF))) ;; clj
                                   (str "#" (.toString (rand-int 16rFFFFFF) 16)))}]
                 [:line {:style {:stroke-width "4px"
                                 :stroke-style "dashed"
                                 :stroke "black"}
                         :x1 (+ x 2) :y1 y :x2 (+ x 2) :y2 (+ y h)}]
                 [:line {:style {:stroke-width "4px"
                                 :stroke-style "dashed"
                                 :stroke "black"}
                         :x1 x :y1 (+ y h 2) :x2 (+ x w) :y2 (+ y h 2)}]
                 #_[:text {:fill "black"
                           :x (+ x (* 0.05 height))
                           :y (+ (- h (* 0.05 height)) y)}
                    (str (or (:fill (:row e))
                             (:fill e)))]]))
         (apply conj [:g (axes->g axes)]))))


(defn history->tt-vt [width height color-att history]
  ; (prn history) (prn sample)
  [:svg
   {:width "100%" :height "80%" :viewBox (str "-30 -30 " width " " height)}
   (history->hiccup
     (/ width 1.2 1.0)
     (/ height 1.2 1.0)
     color-att
     history)])

(defn my-component2 []
  [:div
   [my-component (partial history->tt-vt 600 600 :fill)]
   ])

(rdom/render [my-component2] (.getElementById js/document "app"))

