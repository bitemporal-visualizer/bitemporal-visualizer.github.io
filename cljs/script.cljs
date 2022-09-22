(require '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[clojure.string :as str])


(defn common-subs [a-str b-str]
  (loop [[a & as] a-str
         [b & bs] b-str
         i 0]
    (if (= a b)
      (recur as bs (inc i))
      (subs a-str 0 i))))

(defn truncate-sortable-strings [ss]
  (let [ssn (into {} (map #(vector % %) ss))
        o (map first (sort-by second ssn))
        sf (loop [in ssn
                  out {}]
             (if (= (count in) 1)
               (sort (map second (merge out in)))
               (let [shortest-count (if (= 0 (count out))
                                      10 ;; ISO date
                                      (reduce min (map count (vals out))))
                     [longest-k longest-str] (last (sort-by (comp count second) in))
                     longest-str* (subs longest-str 0 (max 0 (dec (count longest-str))))
                     in* (assoc in longest-k longest-str*)
                     all (merge in* out)
                     eq (= o (map first (sort-by second all)))
                     no-dupes (= (count (vals all)) (count (into #{} (vals all))))]
                 (if (and (<= shortest-count (count longest-str*)) eq no-dupes)
                   (recur in* out)
                   (recur (dissoc in longest-k) (assoc out longest-k longest-str))
                   ))))]
    (map #(vector %2 (subs %1 (count %2))) ss sf)))
;; TODO prevent over-truncation? e.g. bcd011 should ideally shorted to bcd0
;;(truncate-sortable-strings ["bce456" "abcd789" "abc" "abce0123" "bcd123" "bcf" "bcd011"])
;;=>("abc" "abcd" "abce" "bcd" "bcd1" "bce" "bcf")

(def sample1 "filll | app_start | app_end | sys_start | sys_end
             ------------+-------------+-------
             red | 3 | 4 | 0 | 1
             pink | 1 | 3 | 0 | 1
             lightblue | 0 | 1 | 0 | 2
             orange | 2 | 4 | 1 | 2
             lightpink | 1 | 2 | 1 | 2")

(def sample "Price (and tax) scheduling data example
Source: https://bitemporal.net/generate-bitemporal-intervals/

id | price | sys_start | sys_end | app_start | app_end
1002| 100 | 2019-01-01| 2019-02-09| 2019-01-05| 9999-12-31
1002| 100 | 2019-02-09| 9999-12-31| 2019-01-05| 2019-01-10
1002| 100 | 2019-02-09| 2019-03-02| 2019-01-10| 2019-02-08
1002| 105 | 2019-02-09| 2019-02-10| 2019-02-08| 9999-12-31
1002| 105 | 2019-02-10| 2019-03-02| 2019-02-08| 2019-02-15
1002| 105 | 2019-02-10| 2019-02-12| 2019-02-15| 2019-02-17
1002| 103 | 2019-02-10| 2019-02-12| 2019-02-17| 9999-12-31
1002| 102 | 2019-02-12| 2019-03-02| 2019-02-15| 9999-12-31
1002| 103 | 2019-03-02| 2019-04-01| 2019-01-10| 9999-12-31
1002| 103 | 2019-04-01| 9999-12-31| 2019-01-10| 2019-04-02
1002| 111 | 2019-04-01| 9999-12-31| 2019-04-02| 9999-12-31")

(def color-selected "greenyellow")
(def color-selected-data "lightgreen")
(def color-hovered "lightblue")
(def color-hovered-data "lightsteelblue")

(defn table-string->maps [s]
  (let [[ks & vs] (->> (-> s
                           (str/split #"\n"))
                       (filter #(not (or (str/starts-with? % "-") ;; table header bottom-border
                                         (str/starts-with? % "+") ;; table decorations (mariadb)
                                         (= % "") ;; empty lines before table
                                         (and (str/starts-with? % "(") ;; query count
                                              (= 1 (count (str/split % #"\|"))))
                                         (= 1 (count (str/split % #"\|"))) ;; misc. catch-all including SQL query lines (fingers crossed)
                                         )))
                       (map #(if (str/starts-with? % "|")
                               (subs % 1)
                               %))
                       (map #(map str/trim (str/split % #"\|"))))]
    (map #(zipmap (map keyword ks) %) vs)))
(defn map->row [m]
  (assoc {:app-start (or (:application_time_start m) (:app_start m))
          :app-end (or (:application_time_end m) (:app_end m))
          :sys-start (or (:system_time_start m) (:sys_start m))
          :sys-end (or (:system_time_end m) (:sys_end m))}
         :row m))

;; row_start, row_end (system time aliases)

(def state (r/atom {:clicks 0 :t sample :collapsed true :truncate true :about false}))

(def mouse-coordinates (r/atom {:x 100 :y 100}))

(defn data= [a b]
  (apply = (map #(dissoc % "row_start" "row_end" "system_time_end" "system_time_start" "application_time_end" "application_time_start" :sys_start :sys_end :app_start :app_end :application_time_start :application_time_end :system_time_start :system_time_end)
                [a b])))


(defn my-component [viz]
  (let [wide (< 1.2 (/ (.. js/window -innerWidth)
                       (.. js/window -innerHeight)))]
    [:div
     {:onMouseMove (fn [event]
                     (reset! mouse-coordinates {:x (.-clientX event) :y (.-clientY event)}))}
     [:p
      [:a {:href "#"
           :onMouseUp #(swap! state #(update % :collapsed not))}
       (if (:collapsed @state)
         "Show Textarea"
         "Hide Textarea")]
      " | "
      [:a {:href "#"
           :onMouseUp #(swap! state #(update % :truncate not))}
       (if (:truncate @state)
         "Full timestamps"
         "Truncate timestamps")]
      " | "
      [:a {:href "#"
           :onMouseUp #(swap! state #(update % :about not))}
       (if (:about @state)
         "Hide About"
         "Show About")]]
     (into  [:div {:style {:display "grid" :grid-template-columns (if wide
                                                                    (if (:collapsed @state) "50% 50%" "30% 30% 40%")
                                                                    "95vw")
                           :grid-template-rows (if wide
                                                 "85vh"
                                                 (if (:collapsed @state) "45vh 40vh" "40vh 25vh 20vh"))}}]
            (let [d (table-string->maps (:t @state))
                  ks (keys (first d))
                  textarea (when-not (:collapsed @state) [:div [:textarea {:style {:width "95%" :height "95%" :overflow-x "scroll" :overflow-y "scroll"}
                                                                           :on-change #(reset! state (assoc @state :t (.. % -target -value)))}
                                                                (:t @state)]])
                  rows (map map->row (table-string->maps (:t @state)))
                  vizz (if (or (< (count rows) 1)
                               (not (every? #(and (some? (:app-start %))
                                                  (some? (:app-end %))
                                                  (some? (:sys-start %))
                                                  (some? (:sys-end %))) rows)))
                         [:div [:p {:style {:padding "1em"}} (str "ERROR: " (if (< (count rows) 1)
                                                    "No Rows"
                                                    "4 timestamp columns required (e.g. app_start, app_end, sys_start, sys_end)"))]]
                         [:div

                          ;;[:p "x: " (:x @mouse-coordinates)]
                          ;;[:p "y: " (:y @mouse-coordinates)]
                          (viz rows)])
                  table [:div {:style {:overflow-x "auto" :overflow-y "auto" :margin-left "auto" :margin-right "auto" #_ #_:align-self "center"}}
                         (let [description (take-while #(and (not= "" %)
                                                             (= 1 (count (str/split % #"\|")))) (str/split (:t @state) #"\n" ))]
                           (when (< 0 (count description))
                             [:p {:style {:white-space "pre-wrap"}}(str/join "\n" description)]))
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
                                                    (for [k ks] [:td {:style {:border "1px solid black" :padding "4pt"}} (get j k)])]))
                         [:p (str (count d) " Rows")]
                         ]]
              (if wide
                [textarea table vizz]
                [vizz table textarea])
              ))
     (when (:about @state)
       [:div
        [:p "Created for " [:a {:href "https://xtdb.com"} "XTDB"] " using " [:a {:href "https://github.com/babashka/scittle"} "Scittle"]]
        [:p "Note that the tool doesn't detect or prevent overlapping (or otherwise invalid) regions"]
        [:p "This tool was originally created by the XTDB team but is open to ideas and feature requests (and contributions!) that might be useful with other databases also. Please feel free to open " [:a {:href "https://github.com/bitemporal-visualizer/bitemporal-visualizer.github.io/issues"} "issues"] "."]])]))


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
  (let [bw 1]
    (apply conj [:g] (concat (for [[si s] (map-indexed vector (truncate-sortable-strings sys))]
                               (let [x (* si cell-w)
                                     y 0
                                     h height]
                                 [:g
                                  #_[:line {:style {:stroke-width (str bw "px")
                                                  :stroke-style "dashed"
                                                  :stroke "cyan"
                                                  :opacity "0.5"}
                                          :x1 x :y1 y :x2 x :y2 (+ y h)}]
                                  [:g {:transform (str "translate("
                                                       (+ x (* -0.025 width))
                                                       ","
                                                       (+ height (* 0.075 height))
                                                       ")")}
                                   [:text {:fill "black"
                                           :font-size "1.5em"
                                           :transform-box "fill-box"
                                           :transform "rotate(30)"}
                                    (if (= (first s) "9999-12-31")
                                      "∞ System Time"
                                      (str (first s) (if (:truncate @state) (when (not= "" (second s)) " ...") (second s))))]]]))
                             (for [[ai a] (map-indexed vector (truncate-sortable-strings app))]
                               (let [y (* (dec ai) cell-h)
                                     h cell-h
                                     x 0
                                     w width]
                                 [:g
                                  #_[:line {:style {:stroke-width (str bw "px")
                                                  :stroke-style "dashed"
                                                  :stroke "cyan"
                                                  :opacity "0.5"}
                                          :x1 x :y1 (- (+ y h 2) bw) :x2 (+ x w) :y2 (- (+ y h 2) bw)}]
                                  [:text {:fill "black"
                                          :font-size "1.5em"
                                          :x (+ width (* 0.025 width))
                                          :y (- (- height (* 0.025 height)) (* ai cell-h))}
                                   (if (= (first a) "9999-12-31")
                                     "∞ Application Time"
                                     (str (first a) (if (:truncate @state) (when (not= "" (second a)) " ...") (second a))))]]))))))

(defn history->hiccup [width height color-att entries]
  (let [bw 4
        [axes normalized-entries] (normalize-entries width height entries)]
    (-> [(map (fn [[x y w h e :as entry]]
                                        ; (prn entry)
                                        ;(prn e (:row e) (:hovered @state))
                [:g
                 {:onMouseOver #(reset! state (assoc @state :hovered (:row e)))
                  :onMouseOut #(reset! state (dissoc @state :hovered))
                  :onMouseUp #(reset! state (if (= (:row e) (:selected @state))
                                              (dissoc @state :selected)
                                              (assoc @state :selected (:row e))))}
                 [:rect {:x x
                         :y y
                         :width w
                         :height h
                         :fill (or (cond (= (:row e) (:selected @state)) color-selected
                                         (data= (:row e) (:selected @state)) color-selected-data
                                         (= (:row e) (:hovered @state)) color-hovered
                                         (data= (:row e) (:hovered @state)) color-hovered-data)
                                   (:fill (:row e))
                                   (color-att e)
                                   #_(str "#" (format "%06x" (rand-int 16rFFFFFF))) ;; clj
                                   (str "#" (.toString (rand-int 16rFFFFFF) 16)))}]
                 [:line {:style {:stroke-width (str bw "px")
                                 :stroke-style "dashed"
                                 :stroke "black"}
                         :x1 (+ x 2) :y1 y :x2 (+ x 2) :y2 (+ y h)}]
                 [:line {:style {:stroke-width (str bw "px")
                                 :stroke-style "dashed"
                                 :stroke "black"}
                         :x1 x :y1 (- (+ y h 2) bw) :x2 (+ x w) :y2 (- (+ y h 2) bw)}]
                 #_[:text {:fill "black"
                           :x (+ x (* 0.05 height))
                           :y (+ (- h (* 0.05 height)) y)}
                    (str (or (:fill (:row e))
                             (:fill e)))]])
              normalized-entries)]
        (concat [[:g (axes->g axes)]]))))


(defn history->tt-vt [width height color-att history]
  ; (prn history) (prn sample)
  [:svg
   {:width "100%" :height "100%" :viewBox (str "10 -30 " (+ width 100) " " (+ height 100))}
   (history->hiccup
     (/ width 1.2 1.0)
     (/ height 1.4 1.0)
     color-att
     history)])

(defn my-component2 []
  [:div
   [my-component (partial history->tt-vt #_ #_ (/ (.. js/window -innerWidth) 3) (/ (.. js/window -innerHeight) 3) 400 400 :fill)]
   ])

(rdom/render [my-component2] (.getElementById js/document "app"))
