(ns user
  (:require [nextjournal.clerk :as clerk]))

(comment

  (require '[nextjournal.clerk :as clerk])
  ;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/serve! {:browse? true})

  ;; either call `clerk/show!` explicitly
  (clerk/show! "src/day5.clj")

  ;; or let Clerk watch the given `:paths` for changes
  (clerk/serve! {:watch-paths ["src"]})

  ;; start with watcher and show filter function to enable notebook pinning
  ;(clerk/serve! {:watch-paths ["src"] :show-filter-fn #(clojure.string/starts-with? % "notebooks")})
  )