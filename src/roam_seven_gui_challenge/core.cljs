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
        (swap! deciseconds-elapsed (fn [ms] (+ ms 1)))
        (js/console.log "finished"))) 100))

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
  :selected-person nil
  :first-name ""
  :last-name ""}))

(defn event-handler [state [event-name value]]
  (case event-name
    :change-selection 
      (assoc state :selected-person value
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
        (assoc-in 
          (assoc-in (get state :names) [(first (get state :selected-person)) :first-name] (get state :first-name))
          [(first (get state :selected-person)) :last-name] (get state :last-name)))
    :delete-entry
      (assoc state :names
        (dissoc (get state :names) (first (get state :selected-person))))
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
    (tlog filtered-names)
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
;; Seven gui main

(defn seven-gui-roam []
  [:div {:class "Seven-gui-roam"}
   [:h1 "Seven gui for Roam using ClojureScript and Reagent"]
   [component-1]
   [component-2]
   [component-3]
   [component-4]
   [component-5]
   ])
  
;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [seven-gui-roam] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
