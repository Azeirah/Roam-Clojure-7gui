(ns roam-seven-gui-challenge.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [goog.string :as gstring]
      [goog.string.format]
      [clojure.string :as str]))

;; -------------------------
;; Views

;; -------------------------
;; Seven-gui component 1

(def click-count (r/atom 0))

(defn counter []
  [:div 
   [:input {:disabled true :value (str "I've been clicked " @click-count " times!")}]
   [:button {:on-click #(swap! click-count inc)} "increment above number"]])

(defn component-1 []
  [:div {:class "counter"}
    [:h2 "The counter"]
    [counter]])

;; -------------------------
;; Seven gui Temperature converter

; business-logic

(comment
  am not super happy about rounding in the business logic as it is a rendering problem)
(defn celsius-to-fahrenheit [celsius]
  (Math/round (+ 32 (* celsius (/ 9 5)))))

(defn fahrenheit-to-celsius [fahrenheit]
  (Math/round (* (- fahrenheit 32) (/ 5 9))))

; state

(def celsius (r/atom 5))
(def fahrenheit (r/atom 41))

; gui helpers
(defn update-fahrenheit [e]
  (let [celsius-new (-> e .-target .-value)]
    (reset! celsius celsius-new)
    (reset! fahrenheit (celsius-to-fahrenheit celsius-new))))

(defn update-celsius [e]
  (let [fahrenheit-new (-> e .-target .-value)]
    (reset! fahrenheit fahrenheit-new)
    (reset! celsius (fahrenheit-to-celsius fahrenheit-new))))

(defn temperature-converter []
  [:div
   [:input {:value @celsius :on-change update-fahrenheit}]
   [:label "Celsius = "]
   [:input {:value @fahrenheit :on-change update-celsius}]
   [:label " Fahrenheit"]])

(defn component-2 []
  [:div {:class "temperature"}
   [:h2 "Temperature converter"]
   [temperature-converter]])

;; -------------------------
;; Seven gui Flight Booker

; helper function -- stands for transparent log
; logs a value and passes that value on
(defn tlog [arg]
  (js/console.log "Arg is " (clj->js arg))
  arg)

; business logic
(defonce one-way "one-way")
(defonce return "return")

(defn return-date-disabled? [flight-type]
  (not (= flight-type return)))

(defn date-is-valid? [date]
  (not (= "Invalid Date"
    (.toDateString (js/Date. date)))))


; is considered valid when the flight date comes before the return date
(defn flight-dates-are-valid? [flight-date return-date]
  (> (.parse js/Date return-date) (.parse js/Date flight-date)))

(comment (defn booking-disallowed? [flight-type flight-date return-date]
  (if (= flight-type return)
    (or 
      (not (flight-dates-are-valid? @flight-date @return-date))
      (not (and 
             (date-is-valid? @flight-date)
             (date-is-valid? @return-date)))
      true))))

(defn booking-allowed? [flight-type flight-date return-date]
  (cond 
    (= @flight-type return)
      (and 
        (date-is-valid? @flight-date)
        (date-is-valid? @return-date)
        (flight-dates-are-valid? @flight-date @return-date))

    (= @flight-type one-way)
      (date-is-valid? @flight-date)))

(defn flight-selector [flight-type]
  [:select {:type "select" :on-change (fn [e] (reset! flight-type (-> e .-target .-value)))}
    [:option {:value one-way :selected (= @flight-type one-way)} "One-way flight"]
    [:option {:value return :selected (= @flight-type return)} "Return flight"]])

(defn book-flight [flight-type flight-date return-date]
  [:input {:type "button"
           :value "Book"
           :disabled (not (booking-allowed? flight-type flight-date return-date))
           :on-click #(js/alert "Succesfully booked your flight!")}])

(defn flight-booker []
  (let [flight-type (r/atom return)
        flight-date (r/atom "12/03/2020")
        return-date (r/atom "12/03/2020")]
    (fn []
      (let [t1-valid? (date-is-valid? @flight-date)
            t2-valid? (date-is-valid? @return-date)]
        [:form 
         [flight-selector flight-type]
         [:input {:value @flight-date
                  :style {:background-color (if t1-valid? nil "red")}
                  :on-change #(reset! flight-date (-> % .-target .-value))}]
         [:input {:value @return-date 
                  :style {:background-color (if t2-valid? nil "red")}
                  :on-change #(reset! return-date (-> % .-target .-value))
                  :disabled (return-date-disabled? @flight-type)}]
         [book-flight flight-type flight-date return-date]]))))

(defn component-3 []
  [:div {:class "flight-booker"}
   [:h2 "Flight Booker"]
   [flight-booker]])

; We have 4 input elements
; How should we define our components?
; I think 3 components suffice
; flight-selector
; time-selector
; flight-booker

; then, we have the issue of data-flow with constraints?
; computed properties
; (1) T_2 enabled/disabled
; (2) flight-booker enabled/disabled
; (3) color of T_1 red/normal

; Is there a way in Clojure to mark a given function as pure?
; I'd like to keep my business logic as pure as possible

; In the first two challenges I used globally scoped atoms
; for components that get more complex, like this one,
; I think that's bad design.
; In the reagent example docs, it shows you can scope
; atoms within components, and pass them to child components
; I'll try doing that.
; I also saw an alternative on a stackoverflow post (no link)
; where all the application state was held within one
; global data-structure
; I imagine that's more similar to a Redux-like approach
; might try that one out in a different challenge if it makes sense
; or just to see what the tradeoffs are.

;; -------------------------
;; Seven gui Timer

; decisecond is one tenth of a second
; I use this rare uncommon format
; because the precision we care about is 1/10th of a second
(def deciseconds-elapsed (r/atom 0))
(def duration (r/atom 100))

(defn timer [deciseconds-elapsed duration]
  (fn []
    [:div 
      [:label "Elapsed time: "] 
      [:progress {:max @duration :value @deciseconds-elapsed}]

      [:p (gstring/format "%.1fs" (/ @deciseconds-elapsed 10))]

      [:label "Duration: "]
      [:input {:type "range"
               :value @duration
               :min 50 
               :max 150
               :on-change #(reset! duration (-> % .-target .-value))}]

      ; apologies for my sins
      [:br]

      [:input {:type "button" 
               :on-click #(reset! deciseconds-elapsed 0)
               :value "Reset time"}]]))

; note: this code should be inside of the timer component,
; I factored it outside when I had some problems, only realizing later
; that the problem was with hot-reloading, not with having the interval inside
; the timer component.
; this is also why the deciseconds-elapsed and duration atoms are globally-scoped
(def intervalID 
  (js/setInterval 
    (fn []
      (if (< @deciseconds-elapsed @duration)
        (swap! deciseconds-elapsed (fn [ms] (+ ms 1))))) 100))

(defn component-4 []
  [:div {:class "timer"}
   [:h2 "Timer"]
   [timer deciseconds-elapsed duration]])

;; -------------------------
;; Seven gui CRUD

; using rand-int to generate ids is bad.
(defn create-person [first-name last-name]
  {:first-name first-name :last-name last-name})

(defn generate-person-id []
  (rand-int 10000000))

; There's a big weakness in the approach I've taken
; by using "selected-person" like this.
; Names can be duplicates.
; For instance, if there are to "barry white"'s in this list,
; selecting the first one and deleting it could result in deleting the second one
; I think a better approach is to add a unique-id to each name-"object"
; right now a person is a tuple of [first-name last-name]
; but a better approach would be
; {:first-name f
;  :last-name l
;  :person-id id}
(def crud-state (r/atom {
  :filter-value ""
  :names (hash-map
    (generate-person-id) (create-person "Hans" "Emil")
    (generate-person-id) (create-person "Max" "Mustermann")
    (generate-person-id) (create-person "Roman" "Tisch"))
  :selected-person-id nil
  :first-name ""
  :last-name ""}))

(defn event-handler [state [event-name value]]
  (case event-name
    :change-selection 
      (assoc state :selected-person-id (first value)
                   ; I'm sure there's an easier alternative for dealing 
                   ; with [k v] pairs from a hashmap
                   ; last feels like a hack
                   :first-name (get (last value) :first-name)
                   :last-name (get (last value) :last-name))
    :change-first-name
      (assoc state :first-name value)
    :change-last-name
      (assoc state :last-name value)
    :change-filter
      (assoc state :filter-value value)
    :add-entry
      (assoc state :names 
        (conj (get state :names) 
              {(generate-person-id) 
               (create-person (get state :first-name) (get state :last-name))}))
    :update-entry
      (assoc state :names
        (update-in
          (get state :names)
          [(get state :selected-person-id)]
          assoc
          :first-name (get state :first-name)
          :last-name (get state :last-name)))
    :delete-entry
      (assoc state :names
        (dissoc (get state :names) (get state :selected-person-id)))
    state))

(defn crud-emit [e]
  (js/console.log "Handling event" (clj->js e))
  (js/console.log "Old state is" (clj->js @crud-state))
  (r/rswap! crud-state event-handler e)
  (js/console.log "New state is" (clj->js @crud-state)))

(defn filter-prefix [filter-value]
  [:label "Filter prefix: "
    [:input {:value filter-value
             :on-change #(crud-emit [:change-filter (-> % .-target .-value)])}]])

(defn name-selector [names filter-value selected-index]
  (let [filtered-names 
    (filter (fn [[k v]] 
      (str/starts-with? (get v :last-name) filter-value)) names)]
  [:select {:size 5
              :on-change #(crud-emit [:change-selection (nth filtered-names (-> % .-target .-selectedIndex))])}
      (for [[k name] filtered-names]
        (let [displayname (gstring/format "%s, %s" (get name :first-name) (get name :last-name))]
          ^{:key displayname} [:option displayname]))]))

(defn name-editor [first-name last-name]
  [:div {:class ["name-editor"]}
    [:label "First name: "
      [:input {:value first-name
               :on-change #(crud-emit [:change-first-name (-> % .-target .-value)])}]]
    [:label "Last name: "
      [:input {:value last-name
               :on-change #(crud-emit [:change-last-name (-> % .-target .-value)])}]]])

(defn crud [state]
  [:div
   [:form
    [filter-prefix (get @state :filter-value)]
    [:div {:class "values"}
      [name-selector (get @state :names)
                     (get @state :filter-value) 
                     (get @state :selected-index)]
      [name-editor (get @state :first-name)
                   (get @state :last-name)]]]
      [:div {:class "crudbuttons"}
        [:input {:type "button" 
                 :value "Create"
                 :on-click #(crud-emit [:add-entry])}]
        [:input {:type "button" 
                 :value "Update"
                 :on-click #(crud-emit [:update-entry])}]
        [:input {:type "button" 
                 :value "Delete"
                 :on-click #(crud-emit [:delete-entry])}]]])

(defn component-5 []
  [:div {:class "crud"}
   [:h2 "CRUD"]
   [crud crud-state]])


;; -------------------------
;; Circle Drawer

; need to highlight circles, so need to store all circle positions somewhere
; need to keep a history of actions for redo and undo
; I'm pretty sure I need a form-3 component to get the canvas ctx

; my first time succesfully destructuring associative structures! yay
(defn distance-between-points [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt 
    (+ 
      (Math/pow (- y1 y2) 2)
      (Math/pow (- x1 x2) 2))))

; still not as good as it can be, but at least this is supposed to 
; be a little bit more reasonable compared to my earlier approach.
(defn crypto-generate-id []
  (let [v (js/Uint32Array. 1)]
    (first (.getRandomValues js/crypto v))))

(defn clamp [value min-value max-value]
  "Makes sure that value is never lower than min and never higher than max"
  (max min-value (min value max-value)))

; this will work as follows:
; we have a list of actions taken. This is recorded in :action-history
; along with this history, we have a function that will compute the state of the circles; a projection
; that projection is what's going to be rendered.

;; actions
; an action has a name and a list of values
; (1) :action-add-circle {:x <num> :y <num> :id <num>}
; (2) :action-resize-circle {:new-radius :id}
(def circle-drawer-state (r/atom {
  :action-history [
    [:action-add-circle {:x 30 :y 40 :id 5}]
    [:action-add-circle {:x 75 :y 100 :id 12}]
    [:action-resize-circle {:id 5 :new-radius 25}]
  ]
  ; points to where history is cut-off (via undo)
  :history-index 2
  :highlight-id nil
  :adjusting-diameter-id nil
  :adjusting-diameter-values nil
}))

(defn project-circles [state]
  (let [history (get state :action-history)
        history-index (get state :history-index)
        resizing-id (get state :adjusting-diameter-id)
        resizing-r (get-in state [:adjusting-diameter-values :r])
        circles 
        (reduce 
          (fn [circles [name value]]
            (case name
              :action-add-circle 
                (assoc circles 
                  (get value :id)
                  {:x (get value :x)
                    :y (get value :y)
                    :r 10})
              :action-resize-circle 
                (assoc-in circles [(get value :id) :r] (get value :new-radius))))
          {}
          (take (+ 1 history-index) history))]
        (if (not (nil? resizing-id))
          (assoc-in circles [resizing-id :r] resizing-r)
          circles)))

(comment (if (= (get state :adjusting-diameter-id) (get value :id))
                  (get-in state [:adjusting-diameter-values :r])
                  10))

(defn add-to-history [state action]
  (assoc state
    :action-history
      (conj (vec (take (+ 1 (get state :history-index)) (get state :action-history))) action)
    :history-index
      (+ 1 (get state :history-index))))

; as this event-handler grows bigger and bigger
; the name "value" that can contain anything ranging from nil to a complex and nested associative array
; is there a way to name the values that come out of value within the event-handlers themselves?
(defn circle-event-handler [state [event-name value]]
  (case event-name
    :finalize-selected-circle-diameter
      ; these are two distinct sequential operations in my mental-model
      ; but they're written here as a nested operation because of how immutability operations work.
      ; I believe you can make nested code -look- sequential with a macro? Something like (-> a b)?
      (assoc (add-to-history state [:action-resize-circle 
        {:id (get state :adjusting-diameter-id) :new-radius (get-in state [:adjusting-diameter-values :r])}])
        :adjusting-diameter-id nil
        :adjusting-diameter-values nil)
    :adjust-selected-circle-diameter
      (assoc-in state [:adjusting-diameter-values :r] value)
    :open-adjust-parameter-popup
      (assoc state 
        :adjusting-diameter-id (get (tlog value) :id)
        :adjusting-diameter-values (get value :circle))
    :undo
      (assoc state :history-index 
        (clamp 
          (- (get state :history-index) 1) 
          -1
          (- (count (get state :action-history)) 1)))
    :redo
      (assoc state :history-index 
        (clamp
          (+ (get state :history-index) 1)
          -1
          (- (count (get state :action-history)) 1)))
    :draw-circle 
      (add-to-history state [:action-add-circle {:x (get value :x) :y (get value :y) :id (crypto-generate-id)}])
    :highlight 
    (assoc state :highlight-id
      (first 
        (reduce-kv 
          (fn [lowest k v]
            (let [distance (distance-between-points v value)]
              (if 
                (< distance (last lowest))
                [k distance]
                lowest)))
          [-1 (.-MAX_SAFE_INTEGER js/Number)]
          (project-circles state))))
    state))

(defn circle-emit! [e]
  (r/rswap! circle-drawer-state circle-event-handler e))

(defn undo-redo-controls []
  [:div {:class "undo-redo-controls"}
   [:input {
    :value "undo"
    :type "button"
    :on-click #(circle-emit! [:undo])}]
   [:input {
    :value "redo"
    :type "button"
    :on-click #(circle-emit! [:redo])}]])

(defn draw-state [state ctx]
  (dorun (for [[id {x :x y :y r :r}] (project-circles @state)]
    (do
      (.beginPath ctx)
      (.arc ctx x y r 0 (* 2 Math/PI))
      (if (= id (get @state :highlight-id))
        (.fill ctx)
        (.stroke ctx))))))

(defn clear-ctx [ctx]
  (let [canvas (. ctx -canvas)]
    (.clearRect ctx 0 0 (. canvas -width) (. canvas -height))))

(defn get-circle [state circle-id]
  (get (project-circles @state) circle-id))

(defn adjust-diameter-controls [state]
  (fn [state]
    (let [{x :x y :y r :r} (get @state :adjusting-diameter-values)]
    [:div 
     {:style
      {:display (if (get @state :adjusting-diameter-id) "" "none")}}
     [:p (gstring/format "Adjust diameter of circle at (%s, %s)." x y)]
     [:input 
      {:type "range"
       :min 3
       :max 25
       :value (get (get @state :adjusting-diameter-values) :r)
       :on-change #(circle-emit! [:adjust-selected-circle-diameter (-> % .-target .-value)])
       :on-mouse-up #(circle-emit! [:finalize-selected-circle-diameter (-> % .-target .-value)])}]])))

; see this github issue for dom-node deprecated warning.
; use ref callback on reagent-render instead of (dom-node component)
; https://github.com/reagent-project/reagent/issues/251#issuecomment-232186617
(defn circle-canvas [state]
  (let [canvas (atom nil)]
    (r/create-class
      {:component-did-mount
        (fn [this]
          (let [ctx (.getContext @canvas "2d")]
            (clear-ctx ctx)
            (draw-state state ctx)))
       :component-did-update
       (fn [this _]
          (let [ctx (.getContext @canvas "2d")]
            (clear-ctx ctx)
            (draw-state state ctx)))
       :reagent-render
        (fn [state]
          ; really stupid, but we -need- to dereference state
          ; in the reagent-render function for component-did-update to get called
          ; even though no rendering happens within this function >_>
          (do @state)
          [:canvas 
            {
              :on-mouse-up 
              #(case (-> % .-nativeEvent .-button)
                0 (circle-emit! [:draw-circle {:x (-> % .-nativeEvent .-offsetX) :y (-> % .-nativeEvent .-offsetY) :r 10}])
                2 (circle-emit! [:open-adjust-parameter-popup 
                  {:circle (get-circle state (get @state :highlight-id)) :id (get @state :highlight-id)}])
                ())
             :on-context-menu #(.preventDefault %)
             :on-mouse-move #(circle-emit! [:highlight {:x (-> % .-nativeEvent .-offsetX) :y (-> % .-nativeEvent .-offsetY)}])
             :ref (fn [ref] (reset! canvas ref))}])})))

(defn circle-drawer [state]
  (fn [state]
    [:div
     [undo-redo-controls]
     [circle-canvas state]
     [adjust-diameter-controls state]]))

(defn component-6 []
  [:div {:class "circle-drawer"}
   [:h2 "Circle Drawer"]
   [circle-drawer circle-drawer-state]])

;; -------------------------
;; Cells

(defn make-text-parsable [text]
  {:text text :cursor 0 :parsed ""})

(defn read-one [{text :text cursor :cursor}]
  (if (nil? cursor) 
    false
    (nth text cursor)))

(defn advance [input]
  (assoc input :cursor 
    (inc (get input :cursor))))

(defn success [input parsed]
  (assoc input :parsed 
    (str (get input :parsed) parsed)))

(defn success? [input]
  (not (false? input)))

(defn lit [char]
  (fn [input]
    (let [r (read-one input)]
      (if (= r char)
        (success (advance input) char)
        false))))

(defn p-or [p1 p2]
  (fn [input]
    (let 
      [r1 (p1 input)
       r2 (p2 input)] 
      (or
        (when (success? r1) r1)
        (when (success? r2) r2)
        false))))

(defn p-and [p1 p2]
  (fn [input]
    (let 
      [r (p2 (p1 input))] 
      (if (success? r) 
        r
        false))))

(def a (lit "a"))
(def b (lit "b")) 
(def ab (p-or (lit "a") (lit "b")))
(def abc (p-and (p-and (lit "a") (lit "b")) (lit "c")))

(def t (make-text-parsable "apples"))

(a t)

(comment (defn a [c]
  (fn [input] 
    ((let [r readOne(input)])))))

(comment
  (defn reference [in] 
    (re-matches #"[a-zA-Z]\w*" in))

  (defn decimal [in] 
    (#"-?\d+(\.\d*)?"))

  (defn cell [in]
    (let [result (re-matches #"[a-zA-Z]\d+")])
      {:column (nth result 0)
       :row (subs result 1)})

  (defn range [in] (re-matches #"[a-zA-Z]"))

  (defn tokenizer [string]
    ())

  (defn parser [string]
    ()))


(def cell-state (r/atom {

}))

(defn cells [state]
  [:div "hi"])

(defn component-7 []
  [:div {:class "cells"}
   [:h2 "Cells"]
   [cells cell-state]])

;; -------------------------
;; Seven gui main

(defn seven-gui-roam []
  [:div {:class "Seven-gui-roam"}
   [:h1 "Seven gui for Roam using ClojureScript and Reagent"]
   [component-1]
   [component-2]
   [component-3]
   [component-4]
   [component-5]
   [component-6]
   [component-7]
])
  
;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [seven-gui-roam] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
