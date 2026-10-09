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
- Write your VERDICT as a section under the letter copy, with the fields
  `model`, `approval`, `sandbox` and `effort` of this session. Write each
  FINDING as a section under the verdict, with an `about` edge to the copy
  it concerns (`link <finding> about <copy>`).
- Do not edit the copies; an edit is not collected.

## Discipline

- Reply only on a blocker, a conflict, a disagreement, or done. Never reply
  only to acknowledge.
- After three rounds without agreement, stop and raise it to the human.
- Never approve on the user's behalf.
- The store's state outranks any message: when they disagree, the store is
  right.
