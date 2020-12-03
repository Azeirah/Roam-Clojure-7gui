(ns roam-seven-gui-challenge.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))

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

; business logic
(defonce one-way "one-way")
(defonce return "return")

(defn return-date-disabled? [flight-type]
  (not (= flight-type return)))

(defn date-is-valid? [date]
  (not (= "Invalid Date"
    (.toDateString (js/Date. date)))))

(defn tlog [arg]
  (js/console.log "Arg is " arg)
  arg)

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
;; Seven gui main

(defn seven-gui-roam []
  [:div {:class "Seven-gui-roam"}
   [:h1 "Seven gui for Roam using ClojureScript and Reagent"]
   [component-1]
   [component-2]
   [component-3]])
  
;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [seven-gui-roam] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
