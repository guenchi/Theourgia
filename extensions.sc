#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; (theourgia extensions) -- the verbs registered from outside the core
;;; table, as data.
;;;
;;; NEVER: THIS LIBRARY IS DATA AND IMPORTS NOTHING THAT DOES WORK. Every
;;; start of the command line and of the daemon registers these entries
;;; before it parses a request, so whatever this library loads, every start
;;; pays for. A handler is named as (library . name) and its library is
;;; entered only when its verb is dispatched (rpc.sc, register-verbs!).
;;;
;;; An entry is (verb usage description protocol? route value-options
;;; flag-options (library . name)).
(library (theourgia extensions)
  (export extension-verbs commitments-usage)
  (import (rnrs base))

  (define commitments-usage
    '(commitments ["--open"] ["--all"] ["--drifted"] ["--since" <cut>] ["--under" <id>]))

  (define extension-verbs
    (list
      (list 'commitments commitments-usage
            "List the decisions still owed: by default those neither implemented (an incoming implements edge) nor marked done or dropped; with --all every decision. --drifted keeps those with an implementation whose latest change was seen neither by its implements link nor by the decision's latest edit."
            #f 'daemon
            '("--since" "--under") '("--open" "--all" "--drifted")
            '((theourgia commitments) . commitments-verb)))))
