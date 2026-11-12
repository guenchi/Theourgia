((trace-lines "S2: delivery replays what validation scanned"
  "(trace memory (validation WRITER 1 scanned) 12)"
  "(trace memory (delivery WRITER 1 replayed) 12)")
 (trace-lines-removed "S2: delivery replays what validation scanned"
  "(trace parse #f 113)" "(trace parse #f 110)" "(trace parse #f 109)" "(trace parse #f 64)"
  "(trace parse #f 59)" "(trace parse #f 64)" "(trace parse #f 70)" "(trace parse #f 72)"
  "(trace parse #f 511)" "(trace parse #f 216)" "(trace parse #f 81)" "(trace parse #f 49)"))
