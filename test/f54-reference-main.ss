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

;; ARM B OF test/startup-ratio.sc (F54, D2): import the reference library and
;; reference an exported variable, so the library's body is invoked, then exit
;; 0. A `.ss` file: the fixture runner takes *.sc and *.py, so this program is
;; never run as a fixture and never counted among its scripts.

(import (chezscheme) (f54-reference))

(exit (if (> f54-reference-token 0) 0 1))
