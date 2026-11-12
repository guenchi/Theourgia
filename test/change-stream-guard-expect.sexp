((trace-lines "S2: delivery replays what validation scanned"
  "(trace memory (validation WRITER 1 scanned) 12)"
  "(trace memory (delivery WRITER 1 replayed) 12)")
 (trace-lines-removed "S2: delivery replays what validation scanned"
  "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)"
  "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)"
  "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)" "(trace parse #f #)"))
