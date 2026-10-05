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
  (export extension-verbs commitments-usage tasks-usage template-usage names-usage uses-usage
          query-usage context-usage)
  (import (rnrs base))

  (define commitments-usage
    '(commitments ["--open"] ["--all"] ["--drifted"] ["--since" <cut>] ["--under" <id>]))

;; `template apply <name>`, `template apply --file <template-file>` and
  ;; `template export`: the action is the first positional.
  (define template-usage
    '(template <action> [<name>] ["--file" <template-file>] ["--premises" <datum>]))

  (define tasks-usage
    '(tasks ["--status" <status>] ["--batch" <batch>] ["--under" <id>]))

  (define names-usage '(names <id>))

  (define uses-usage '(uses <name> ["--under" <id>]))

  ;; One goal, as a datum in one argument; or the relations, with --relations.
  (define query-usage '(query [<goal>] ["--relations"]))

  ;; The block to read for and the size of the answer, in tokens.
  (define context-usage '(context "--for" <id> "--budget" <tokens> ["--all-validity"]))

  (define extension-verbs
    (list
      (list 'commitments commitments-usage
            "List the decisions still owed: by default those neither implemented (an incoming implements edge) nor marked done or dropped; with --all every decision. --drifted keeps those with an implementation whose latest change was seen neither by its implements link nor by the decision's latest edit."
            #f 'daemon
            '("--since" "--under") '("--open" "--all" "--drifted")
            '((theourgia commitments) . commitments-verb))
      (list 'tasks tasks-usage
            "List the tasks: by default those under the root the store's template names for tasks, else every task; each with its status (todo, doing, done or dropped), its batch, what it implements, and (unlinked) when it implements no live decision."
            #f 'daemon
            '("--status" "--batch" "--under") '()
            '((theourgia tasks) . tasks-verb))
      (list 'template template-usage
            "Apply a template to this store (apply <name>, or apply --file <template-file>): create the template block and each document root the store lacks, changing nothing that exists. Or print the store's template (export)."
            #f 'daemon
;; APPLY WRITES FROM THE REDUCTION -- what exists decides what is
            ;; created -- so it refuses a load that could not read a writer;
            ;; export only prints.
            '("--file" "--premises") '()
            '((theourgia template) . template-verb)
            '(refuse "apply"))
      (list 'names names-usage
            "Which names a code block uses, as its stored code shows them, and which libraries a library block imports. A datum block's body is walked as data: a symbol is a use unless something in the form binds it, and an unknown macro's operands count as uses. A text block's uses are every token its language's identifier pattern matches, comments and strings included. No name is resolved."
            #f 'daemon
            '() '()
            '((theourgia name-use) . names-verb))
      (list 'uses uses-usage
            "The live code blocks that use a name, compared whole and exactly, one row per block with its library and mode; with --under, those inside that block. It is not find-references: a use is listed whether or not anything defines the name, and a text block that defines the name also lists it."
            #f 'daemon
            '("--under") '()
            '((theourgia name-use) . uses-verb))
      (list 'query query-usage
            "Answer one goal of the query language over the committed state: every binding of its variables, one row each, sorted by their bytes, with a digest of the rows. A goal is a fact relation, a rule of the library, a test, or (and <goal> ...). --relations lists the fact relations and the rules."
            #f 'daemon
            '() '("--relations")
            '((theourgia query) . query-verb))
      (list 'context context-usage
            "The material to read before working on a block, within a budget of tokens. What must be shown is the answer of five queries, each block placed once: for, constraints, evidence or to-verify by its validity and class; what else fits follows in a stated order (counterexamples, background). The receipt covers every block that must be shown, whether it fitted or not, and commit --premises takes it back."
            #f 'daemon
            '("--for" "--budget") '("--all-validity")
            '((theourgia context) . context-verb)))))
