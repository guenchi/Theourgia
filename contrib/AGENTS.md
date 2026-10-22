# Mail and review through a Theourgia store

Text for the reader's side: append it to the AGENTS.md of a project whose
sessions read a Theourgia store. Replace `codex` with your actor name if it
is another.

## Mail

- A message to you is a block of kind `doc` whose `to` is `"codex"` and
  whose `status` is `"unread"`. Your hooks name such blocks at the start of
  a turn and after a tool call; a line `mail: block <id> at <cut>` typed at
  your prompt names one too. Read the block with `read <id>` (at the cut
  the line names, `read <id> --cut <cut>`).
- When you have read it, set its `status` to `"read"`. Only you set it.
- To find your mail yourself, query
  `(and (field ?m "to" "codex") (field ?m "status" "unread"))`.

## A review letter

- A letter has `cut`, `roots` and `scope`. Open the store at `scope` (not
  the store the letter is in) and read the letter copy there; its
  `baseline` is the cut to read the copies at (`read <id> --cut
  <baseline>`). The copies are all there is to read. If you need more,
  say so in a section; do not go looking for it elsewhere.
- A letter with `freeze` is a review of a CANDIDATE: the copies hold the
  change as it would land. A copy the change rewrites has a section
  `before` under it, with the source as it is now. A copy with `deleted`
  set to `"proposed"` is one the change removes; an `origin` that begins
  with `new-` is a file the change adds, its pieces `<id>/<K>` in order. Sections under the letter copy
  that are not yours are the author's notes for this review.
- Write your VERDICT as a section under the letter copy with these fields:
  `verdict`, one of `no-objection`, `findings` or `could-not-review`;
  `sources`, the ids of the copies you read, at least one (a list, or one
  string of ids separated by spaces; a `before` or packet section may be
  listed too, but does not count: at least one must be a copy); and `model`, `approval`, `sandbox`
  and `effort` of this session. Write each FINDING as a section under the
  verdict with `claim`, `evidence` and `confidence`, and an `about` edge to
  the copy it concerns (`link <finding> about <copy>`). Results missing any
  of these are not collected: the review reads as incomplete.
- Your launch sets THEOURGIA_HOME to the scope's own home; never write in
  the scoped store with another.
- When the verdict is written, set the letter copy's `status` to
  `"answered"`.
- An edit of a copy is a proposal. You have your own writer in the scoped
  store: you may edit the copies, export them, run what you need to test a
  hypothesis and commit there; nothing you write reaches the main store
  except your results. Each copy whose source you committed a change to
  is handed to the author as a draft, against the finding about it (else
  the verdict); a copy you did not change is not.

## Discipline

- Reply only on a blocker, a conflict, a disagreement, or done. Never reply
  only to acknowledge.
- After three rounds without agreement, stop and raise it to the human.
- Never approve on the user's behalf.
- The store's state outranks any message: when they disagree, the store is
  right.
