(ns pottery.clj.models)

(def current-user (atom
                   {:users
                    {"profile" {:userProfileName nil
                                :userLoginPassword nil
                                :passwords [{:pName nil
                                             :pContent nil
                                             :pNotes nil}]}}}))

