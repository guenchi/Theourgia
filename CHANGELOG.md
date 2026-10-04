# Changelog

## Unreleased

* Intel macOS: the package for macOS on x86_64 now passes its unit suite
  against the core it is pinned to (theourgia 5c28e42, which measured that
  machine), so it can be published with the other three targets. 1.0.0 was
  packaged for Apple Silicon only.
* The search view answered "the store did not answer the search" for every
  query that found anything, since 1.0.0: the extension read the fields a
  hit matched in as a shape the core never printed. Fixed; a search now
  lists its hits.
* A datum-mode block -- a library imported with `import-code --datum`, or one
  definition in it -- now opens read-only, as the datum export writes its
  library's file, at the block's own place. Before, it opened as an empty
  buffer, and an edit saved through it was answered as saved and never
  reached the code.
* A file opened under 1.0.0 on a datum block is now refused on save instead
  of appearing to save.
* Search and the hover's mentions now ask the core with `--wire`. When every
  hit was in a superseded or refuted block, the pinned core prints nothing on
  the human route, and the search view said "nothing in the store matches",
  as for words found nowhere; it now says the words are found only in blocks
  that are not in force. A newer core prints an `(excluded ...)` line there,
  which the human-route readers would have refused as "the store did not
  answer the search" and as a failed hover; on the wire route the hover
  mentions nothing.
* The setting `theourgia.writer` is removed. Each window has written its
  drafts under a name of its own since before 1.0.0, so the setting decided
  nothing; a `settings.json` that still sets it shows it as an unknown
  setting, and nothing changes. The extension no longer passes
  `THEOURGIA_WRITER` to the core, and ignores one in the environment VS Code
  was started from.

## 1.0.0 — 2026-09-29

First published version. It needs the theourgia core installed on the machine
(see README, "Before you start").

* Supplying what the editor knows: three commands collect signatures,
  keywords, call edges and diagnostics from VS Code's language servers
  over the store's code projection and hand them to the core, which keeps
  them beside the blocks with where they came from; answers built on them
  say which editor supplied them and how many are stale.
* Everything listed under 0.1.0 below.

## 0.1.0 — 2026-09-29 (not published)

* An outline tree of the store, opened level by level; blocks open as
  markdown documents and save through a durable working draft and an
  explicit commit, with the block's version checked on every write.
* A subtree opens as one composed, read-only document.
* A directory view of the store as its files export would write it: new
  file here, move and rename, each guarded by the block's version.
* Code blocks: a source file split where the editor's symbols start;
  go to definition through the store's own lookup; a text-mode block opens
  as UTF-8 and a block that is not UTF-8 is refused by name.
* Every refusal the core can make is classified: kept for a person,
  retried, parked in settings, or reported as nobody knows, with a
  census of the core's constructor routes behind it.
* An incomplete reading is shown as its rows plus a warning that names
  what could not be read, and says only what the core can promise.
* Publication names the record it replaces; open and reconcile check
  identity, not bytes; a save that lands behind is reported, and a
  refused save is an error notice.
