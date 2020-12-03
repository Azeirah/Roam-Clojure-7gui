(ns roam-seven-gui-challenge.prod
  (:require
    [roam-seven-gui-challenge.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
