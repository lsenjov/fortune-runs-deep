(ns frd-char-creator.prod
  (:require [frd-char-creator.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
