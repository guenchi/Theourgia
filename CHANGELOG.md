# Changelog

## 0.1.0 — 2026-09-29

First published version. It needs the theourgia core installed on the machine
(see README, "Before you start").

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
