#!/bin/sh
# THE ENVIRONMENT THIS FILE READS, DECLARED (F89). docs-check.sc's DOC-3
# takes the runner's names from the READ line below and requires the README
# to document each; its TWIN requires every THEOURGIA_* name anywhere in this
# file, comments and strings included, to be on one of the two lines. READ
# is what the runner takes from the environment it was started with; NAMED,
# NOT READ is every other name here (set by the runner itself, or only
# mentioned). A new name nobody classified turns that TWIN red.
# ENVIRONMENT READ: THEOURGIA_FIXTURE_LIMIT THEOURGIA_LIBDIR THEOURGIA_RUNNER_NORMALISED THEOURGIA_TEST_ROOT
# ENVIRONMENT NAMED, NOT READ: THEOURGIA_INJECT THEOURGIA_RUN THEOURGIA_SUITE_TOKEN THEOURGIA_TEST_SOCK
# EVERY SCRIPT IN THE DIRECTORY IS RUN, AND EVERY OUTCOME IS PRINTED BY
# NAME. A hand-written list is silent about the file it forgot, and this
# directory's list had forgotten smoke-wire -- red for days, because it
# counts `mismatches` and the list-runner grepped for `failures`.
#
# THE CLASS COMES FROM THE OUTCOME, NOT FROM READING THE SOURCE. Several
# fixtures call (command-line) to find their own path, so "takes
# arguments" cannot be decided by grep: a probe run with no arguments
# prints its usage line and stops, and that is what identifies it.
#
# THREE CRITERIA FOR A FIXTURE, ALL OF THEM: the sentinel "<name>
# complete" says it ran to its end; `failures` and `mismatches` are the
# two counters fixtures use and a non-zero either way is red; and a hard
# line (FAIL / MISMATCH / Exception) is red whatever the counters say.
#
# THE RUNNER'S OWN SIGNAL STATE IS PUT RIGHT FIRST (launcher design L7).
# A non-interactive sh started with TERM ignored cannot trap TERM (measured
# on bash 3.2), so a runner started under an ignored or blocked signal
# would have no working stop. It re-execs itself once through perl with
# every catchable signal at its default and an empty mask. The marker is
# its own pid, which exec keeps, and it is removed at once, so a runner
# started by a fixture puts itself right again.
if [ "${THEOURGIA_RUNNER_NORMALISED:-}" != "$$" ]; then
  THEOURGIA_RUNNER_NORMALISED=$$
  export THEOURGIA_RUNNER_NORMALISED
  exec perl -e '
    use POSIX ();
    require Config;
    my %s;
    my @n = split " ", $Config::Config{sig_name};
    my @v = split " ", $Config::Config{sig_num};
    for my $i (0 .. $#n) { $s{$n[$i]} = $v[$i] unless exists $s{$n[$i]}; }
    for my $no (values %s) {
      next if $no < 1 || $no > 31 || $no == $s{KILL} || $no == $s{STOP};
      POSIX::sigaction($no, POSIX::SigAction->new("DEFAULT", POSIX::SigSet->new, 0));
    }
    POSIX::sigprocmask(POSIX::SIG_SETMASK(), POSIX::SigSet->new);
    exec { "/bin/sh" } "sh", @ARGV;
    exit 127;
  ' "$0" "$@"
fi
unset THEOURGIA_RUNNER_NORMALISED
# EVERY LAUNCH GOES THROUGH launch.pl, beside this file: a helper, not a
# fixture (no glob below matches .pl). Without it nothing can be launched
# as the design requires, and the runner does not fall back to launching
# directly.
if [ ! -f ./launch.pl ]; then
  echo "REFUSING: launch.pl is not beside run-fixtures.sh"
  exit 1
fi
# THE TIME LIMIT OF ONE FIXTURE, in seconds: 900 unless a test sets
# THEOURGIA_FIXTURE_LIMIT to exercise the limit itself.
fixture_limit=${THEOURGIA_FIXTURE_LIMIT:-900}
case "$fixture_limit" in
  ""|*[!0-9]*|0)
    echo "REFUSING: THEOURGIA_FIXTURE_LIMIT is not a whole number of seconds above 0: $fixture_limit"
    exit 1
    ;;
esac
out=$1
mkdir -p "$out"

# ---- one run, two roots, both made here and removed on every exit (F71) -----
#
# KEY: EVERY SCRATCH PATH A FIXTURE USES LIVES UNDER ONE OF TWO DIRECTORIES
# THIS RUN CREATES, AND BOTH ARE REMOVED WHEN THIS RUNNER EXITS, WHATEVER
# THE RESULT -- a refusal before anything ran as much as a red verdict.
# Fixtures that wrote under /tmp by name left eight prefixes with 130 to
# 214 directories each, and two of them went red in a gate because they
# reused an old run's directory that had the same process id.
#
# TWO, BECAUSE A SOCKET PATH MUST BE SHORT. It must fit in sun-path-max,
# 104 bytes with the NUL on macOS, and a caller's scratch root can be
# nearly that long by itself. So files and directories go under
# THEOURGIA_TEST_ROOT, `<base>/run-<token>`, and sockets -- with what the
# product keeps beside a socket, its lock file and serve.log -- under
# THEOURGIA_TEST_SOCK, `/tmp/ths.<token>`, at most 20 bytes. It is made
# first, by mktemp, and its six-character suffix IS the token, so both
# names say whose they are.
#
# NEVER: THIS RUNNER REMOVES ONLY WHAT IT CREATED THIS RUN. Another run may
# be alive beside it, and its directories look exactly like these.
sock_root=""
run_root=""
snap_dir=""
made_run_root=0
keep_roots=0
keep_reason=""
remove_roots() {
  status=$1
  [ -n "$sock_root" ] || exit "$status"
  # WHAT CANNOT BE SEEN TO BE FINISHED KEEPS THE ROOTS (launcher design
  # L6): a group that could not be read, a member that outlived KILL, a
  # result that could not be read. Nothing is removed, and the run does
  # not exit 0.
  if [ "$keep_roots" = 1 ]; then
    echo "NOT REMOVED: $keep_reason; the roots are left for it: $run_root $sock_root $snap_dir"
    [ "$status" = 0 ] && status=5
    exit "$status"
  fi
  entries=$(find "$sock_root" 2>/dev/null | wc -l | tr -d " ")
  gone="$sock_root"
  if [ "$made_run_root" = 1 ]; then
    entries=$((entries + $(find "$run_root" 2>/dev/null | wc -l | tr -d " ")))
    gone="$run_root and $sock_root"
    chmod -R u+rwx "$run_root" 2>/dev/null
    rm -rf "$run_root"
  fi
  chmod -R u+rwx "$sock_root" 2>/dev/null
  rm -rf "$sock_root"
  [ -n "$snap_dir" ] && rm -rf "$snap_dir"
  echo "removed this run's roots: $gone ($entries entries, the roots among them)"
  # GONE MEANS SEEN TO BE GONE. `[ -e ]` also answers false when the parent
  # cannot be searched, and a root behind such a parent is still there; and
  # it answers false for a dangling symbolic link, which is there too. So a
  # root counts as removed only when its parent can be searched and nothing
  # of that name is left; anything else is NOT REMOVED.
  left=""
  if [ "$made_run_root" = 1 ]; then
    if [ ! -x "${run_root%/*}" ] || [ -e "$run_root" ] || [ -L "$run_root" ]; then left="$left $run_root"; fi
  fi
  if [ ! -x "${sock_root%/*}" ] || [ -e "$sock_root" ] || [ -L "$sock_root" ]; then left="$left $sock_root"; fi
  if [ -n "$snap_dir" ]; then
    if [ ! -x "${snap_dir%/*}" ] || [ -e "$snap_dir" ] || [ -L "$snap_dir" ]; then left="$left $snap_dir"; fi
  fi
  if [ -n "$left" ]; then
    echo "NOT REMOVED:$left"
    [ "$status" = 0 ] && status=5
  fi
  exit "$status"
}
# ONE PLACE, ON EXIT: every refusal below, every verdict, and a run sent
# SIGTERM (bash runs this trap for it). It is set before the socket root
# is made, so nothing after mktemp can fail or be interrupted outside it;
# with no socket root yet it removes nothing. It cannot
# see SIGKILL. Measured: a runner inside a screen session that is quit
# keeps running and removes both roots; SIGTERM sent at once to the
# screen process, its login, the shell under it and the runner left both.
# What is left is the two directories named with the token.
trap 'remove_roots $?' EXIT
# ---- launching: every fixture and both preflights, through launch.pl ---------
#
# THE LAUNCHER DESIGN (theourgos core/briefs/launcher-design.md, closed at
# v4.1) is carried out by launch.pl: a watcher outside the fixture's
# process group owns its time limit, the grace after it returns, the stop,
# the census of the group, and a result file. The runner starts the
# watcher, waits for it, and reads the result; it never reads a group
# itself.
#
# NEVER: THE REDIRECTIONS ARE ON THE BACKGROUND COMMAND, NOT ON A CALL. A
# trap runs with the redirections of the function it interrupted in
# force: with them on the call, the runner's own SIGNALLED line and its
# removal line were written into the fixture's output file (measured).
tracked_w=""
tracked_name=""
# Set here as well, so a value inherited from the environment is never
# named as a launch this run made (r2 review).
last_ran=""
tracked_id=""
tracked_result=""
launch_n=0
probe_parent=""
aborted=0
abort_reason=""
left_group=0
# EVERY ps THE RUNNER CALLS IS BOUNDED: an alarm that exec keeps ends a ps
# that does not return, and the caller reads its failure.
bps() {
  perl -e 'alarm shift; exec { "ps" } "ps", @ARGV; exit 127' 5 "$@"
}
bpgrep() {
  perl -e 'alarm shift; exec { "pgrep" } "pgrep", @ARGV; exit 127' 5 "$@"
}
# IS PROCESS $1 STILL RUNNING? Gone if `kill -0` fails. A zombie is gone
# too: it has ended and waits only to be reaped, though `kill -0` says yes
# to it, so ps is asked. A ps that fails or answers nothing, for a pid
# that `kill -0` still finds, is taken as "running", so a bounded wait
# runs to its bound. THE SAME RULE, IN SCHEME, IS test/pid-state.ss, which
# the fixtures include; the two are the only copies, and a change to one
# is a change to both.
alive() {
  kill -0 "$1" 2>/dev/null || return 1
  al_st=$(bps -o stat= -p "$1" 2>/dev/null)
  al_rc=$?
  # ONLY A ps THAT SUCCEEDED AND SAID Z MEANS GONE (codex r8 B1).
  if [ "$al_rc" = 0 ]; then
    case "$al_st" in
      Z*) return 1 ;;
    esac
  fi
  return 0
}
# ELAPSED TIME IS READ FROM CLOCK_MONOTONIC (codex r9 A1), the clock
# launch.pl's deadlines use: a wall-clock step (NTP, a manual set, a resume)
# would hold a bound open or end it early. The runner reads no wall clock.
# MILLISECONDS, not whole seconds (codex r10 A1): a whole-second end falls
# up to 1 s short, and a wait of 2 s measured 1.1 s. About 6 ms a call.
mono() {
  perl -MTime::HiRes=clock_gettime,CLOCK_MONOTONIC -e 'printf "%d\n", clock_gettime(CLOCK_MONOTONIC) * 1000'
}
# POLL AGAINST AN ABSOLUTE END, NOT A COUNT OF TURNS (codex r8 A3): each
# alive() can itself take up to 5 s, so a count of turns bounds nothing.
# wait_gone <pid> <seconds>: returns 0 once the pid is gone, 1 at the end,
# which is never earlier than <seconds> after the call; the last alive()
# may finish up to 5 s past it. A clock that cannot be read ends the wait
# at once, returning 2: a reading that is not a number must not become a
# wait with no end.
# NEVER: THE CLOCK'S FAILURE IS ITS OWN RETURN (F90). It used to return 1,
# the same as a bound that ran out, so a caller could only report "did not
# stop within N s" for a wait that never started counting.
wait_gone() {
  wg_now=$(mono) || return 2
  case "$wg_now" in ""|*[!0-9]*) return 2 ;; esac
  wg_end=$((wg_now + $2 * 1000))
  while alive "$1"; do
    wg_now=$(mono) || return 2
    case "$wg_now" in ""|*[!0-9]*) return 2 ;; esac
    [ "$wg_now" -ge "$wg_end" ] && return 1
    sleep 0.1
  done
  return 0
}
# THE RESULT OF ONE LAUNCH (launcher design L6). Accepted only if the file
# is there, names this launch on its first line and ends with "end";
# anything else is RESULT NOT READ, which keeps the roots and launches
# nothing more. A group that was left, or outlived KILL, or could not be
# read, is said here, at its fixture, while the name still means something.
read_result() {
  rr_file=$1
  rr_id=$2
  rr_name=$3
  rr_status=""
  if [ ! -f "$rr_file" ] || [ "$(sed -n '1p' "$rr_file" 2>/dev/null)" != "id $rr_id" ] \
     || [ "$(sed -n '$p' "$rr_file" 2>/dev/null)" != "end" ]; then
    echo "RESULT NOT READ for $rr_name: $rr_file is missing, partial or not this launch's"
    keep_roots=1
    keep_reason="the result of $rr_name could not be read"
    aborted=1
    abort_reason="the result of $rr_name could not be read"
    return 1
  fi
  rr_status=$(sed -n 's/^status //p' "$rr_file")
  rr_cleanup=$(sed -n 's/^cleanup //p' "$rr_file")
  sed -n "s/^note /NOTE $rr_name: /p" "$rr_file"
  case "$rr_cleanup" in
    clean) ;;
    left)
      sed -n "s/^member /LEFT IN GROUP: $rr_name /p" "$rr_file"
      left_group=1
      ;;
    survivor)
      sed -n "s/^member /OUTLIVED KILL in the group of $rr_name: /p" "$rr_file"
      keep_roots=1
      keep_reason="a process of $rr_name's group outlived KILL"
      aborted=1
      abort_reason="a process of $rr_name's group outlived KILL"
      ;;
    *)
      echo "GROUP NOT READ for $rr_name: the members of its group could not be read"
      keep_roots=1
      keep_reason="the members of $rr_name's group could not be read"
      aborted=1
      abort_reason="the members of $rr_name's group could not be read"
      ;;
  esac
  return 0
}
# run_tracked <seconds> <output file> <name> <command> <args>...: one launch.
# Its status is the fixture's (launcher design L3), taken from the result
# when the result was read.
run_tracked() {
  rt_secs=$1
  rt_out=$2
  tracked_name=$3
  shift 3
  launch_n=$((launch_n + 1))
  tracked_id="$token.$launch_n"
  tracked_result="$snap_dir/result.$launch_n"
  perl ./launch.pl --limit "$rt_secs" --out "$rt_out" --result "$tracked_result" \
       --id "$tracked_id" --census-pid "$$" -- "$@" < /dev/null &
  tracked_w=$!
  wait "$tracked_w"
  rt_st=$?
  tracked_w=""
  last_ran=$tracked_name
  rt_was_aborted=$aborted
  if read_result "$tracked_result" "$tracked_id" "$tracked_name" && [ -n "$rr_status" ]; then
    rt_st=$rr_status
  fi
  # THE RUN THAT STOPS LAUNCHING SAYS WHY, AT THE FIXTURE THAT STOPPED IT
  # (F95). Its reason and that fixture's output are both known here, and
  # the output lives in this run's scratch, which is removed at the end --
  # so a run inside another fixture's scratch (runner-self, launch-self)
  # used to leave only its exit code, 6, and nothing to read the cause from.
  if [ "$rt_was_aborted" = 0 ] && [ "$aborted" = 1 ]; then
    echo "ABORTED AT $tracked_name: $abort_reason"
    echo "  the last 20 lines of $tracked_name's output ($rt_out):"
    if [ -r "$rt_out" ]; then
      tail -n 20 "$rt_out" | sed 's/^/  | /'
    else
      echo "  | (no output file)"
    fi
  fi
  return "$rt_st"
}
# THE PROBE IS STOPPED THE SAME BOUNDED WAY: TERM, a bounded poll, KILL.
stop_probe() {
  [ -n "$probe_parent" ] || return 0
  kill -TERM "$probe_parent" 2>/dev/null
  if ! wait_gone "$probe_parent" 5; then
    kill -KILL "$probe_parent" 2>/dev/null
    wait_gone "$probe_parent" 2
  fi
  # NEVER AN UNBOUNDED WAIT: only a probe seen to be gone is waited for.
  alive "$probe_parent" || wait "$probe_parent" 2>/dev/null
  probe_parent=""
}
# A SIGNAL TO THE RUNNER (launcher design L7): the running launch is
# stopped through its watcher, with a bound; the probe is stopped; this
# run's processes are still counted, so a leaver is reported on this path
# too; and only then does the runner exit, 128+n, into the removal.
#
# THE WAIT FOR W IS W'S OWN STATED BOUND PLUS 5 s. launch.pl's stop path
# is bounded at 15 s after the request (stop_group, then the anchor's
# reap); a KILL at 15 s would race W's last phase and lose its result
# file. A stop request is never followed by a KILL inside W's own bound.
W_STOP_BOUND=15
W_STOP_WAIT=$((W_STOP_BOUND + 5))
# stop_watcher: the running launch's watcher is asked to stop and waited
# for; one that is not seen to stop is killed and its group is not read,
# and the roots are kept. TWO CAUSES, TWO MESSAGES (F90): a watcher that
# outlived the bound, and a monotonic clock that could not be read, so the
# wait never counted at all.
stop_watcher() {
  kill -TERM "$tracked_w" 2>/dev/null
  wait_gone "$tracked_w" "$W_STOP_WAIT"
  sw_rc=$?
  if [ "$sw_rc" = 0 ]; then
    wait "$tracked_w" 2>/dev/null
    read_result "$tracked_result" "$tracked_id" "$tracked_name"
    return 0
  fi
  kill -KILL "$tracked_w" 2>/dev/null
  wait_gone "$tracked_w" 2
  alive "$tracked_w" || wait "$tracked_w" 2>/dev/null
  keep_roots=1
  if [ "$sw_rc" = 2 ]; then
    echo "GROUP NOT READ for $tracked_name: CLOCK NOT READ -- the monotonic clock gave no number while waiting for its watcher to stop, so the wait could not be bounded and the watcher was killed"
    keep_reason="the monotonic clock could not be read while stopping the watcher of $tracked_name"
  else
    echo "GROUP NOT READ for $tracked_name: its watcher did not stop within $W_STOP_WAIT s"
    keep_reason="the watcher of $tracked_name did not stop within $W_STOP_WAIT s"
  fi
}
on_signal() {
  trap '' INT TERM HUP
  echo "SIGNALLED: stopping the running launch and the probe, counting this run's processes, then removing its roots"
  # THE LAUNCH THE SIGNAL CUT IS NAMED, AS AN ABORT IS (F104, I3). SIGNALLED
  # alone left a reader of the log unable to tell which launch was cut. The
  # name is the running launch's; between launches no launch was cut, and
  # the line says so and names the last launch that ran to its end. It is not
  # tracked_name: that is set before a launch starts, so a signal in between
  # would have named a launch that never ran (r1 review).
  if [ -n "$tracked_w" ]; then
    echo "ABORTED AT $tracked_name: signal $(($1 - 128))"
  else
    echo "ABORTED AT (no launch running; the last to run was ${last_ran:-none}): signal $(($1 - 128))"
  fi
  if [ -n "$tracked_w" ]; then
    stop_watcher
    tracked_w=""
  fi
  stop_probe
  if type count_leaks > /dev/null 2>&1; then count_leaks; fi
  exit "$1"
}
trap 'on_signal 129' HUP
trap 'on_signal 130' INT
trap 'on_signal 143' TERM
sock_root=$(mktemp -d /tmp/ths.XXXXXX) || {
  echo "REFUSING: mktemp -d /tmp/ths.XXXXXX made no socket root"
  exit 1
}
token=${sock_root#/tmp/ths.}
base=${THEOURGIA_TEST_ROOT:-/tmp}
mkdir -p "$base"
run_root="$base/run-$token"
if ! mkdir "$run_root" 2>/dev/null; then
  echo "REFUSING: the scratch root $run_root already exists or cannot be made"
  exit 1
fi
made_run_root=1
export THEOURGIA_TEST_ROOT="$run_root" THEOURGIA_TEST_SOCK="$sock_root" THEOURGIA_SUITE_TOKEN="$token"
echo "this run: token $token, scratch root $run_root, socket root $sock_root"
# WHAT THE CENSUS CANNOT SEE, said once (launcher design, stated limit): on
# FreeBSD with either of these at 0, a member of a fixture's group whose
# user or groups stop overlapping the runner's can vanish from the census
# and from group signalling, and FreeBSD reports the skip as success.
if [ "$(uname)" = FreeBSD ]; then
  vis_u=$(sysctl -n security.bsd.see_other_uids 2>/dev/null)
  vis_g=$(sysctl -n security.bsd.see_other_gids 2>/dev/null)
  if [ "$vis_u" = 0 ] || [ "$vis_g" = 0 ]; then
    echo "census cannot see every process of this user's fixtures (security.bsd.see_other_uids=$vis_u see_other_gids=$vis_g)"
  fi
fi

# ---- the leak count: this run's processes, by the mark they carry (F70) -----
#
# It used to be `pgrep -f "scheme --script"`, which matches command lines
# machine-wide: it counted the process asking, any shell loop whose own
# command line held that text, and every other session's scheme. A waiter
# once waited fifty minutes for itself. Now a process is this run's when its
# ENVIRONMENT carries THEOURGIA_SUITE_TOKEN=<token> as a whole word --
# whatever its executable, since a leaked python daemon or shell is a leak
# too. Every process a fixture starts inherits it; setsid changes the
# process group, not the environment.
#
# NEVER: NOT IN ARGV. A command line may spell the token (a grep for it, a
# script's arguments); only the environment says a process was started by
# this run. Three snapshots are taken -- command lines, environments,
# command lines again -- and the token is looked for only in what the
# environment snapshot adds after a process's command line. The command
# line used is the longer of the two that the environment snapshot starts
# with, so a process born, or re-executed, or given more arguments
# between the snapshots is still read by its environment and not by its
# arguments. A process that changes twice inside the few milliseconds the
# three take is not read.
#
# The runner itself and its direct children at the instant of counting --
# the ps that takes the snapshot is one -- are this run's and are not a
# leak.
case "$(uname)" in
  Darwin) env_flag=-E ;;
  FreeBSD) env_flag=-e ;;
  *) env_flag="" ;;
esac
# A SNAPSHOT THAT WAS NOT TAKEN SAYS SO. It returns non-zero when any of
# the three files could not be written or came out empty, because an
# empty snapshot reads as "nothing carries the token". Measured: with the
# files under the scratch root, a fixture that made the base unsearchable
# turned the count into "none left running" beside a live leak.
snapshot() {
  [ -n "$snap_dir" ] || return 1
  bps -A -ww -o pid= -o ppid= -o command= > "$snap_dir/.ps-argv" 2>/dev/null || return 1
  if [ -n "$env_flag" ]; then
    bps -A $env_flag -ww -o pid= -o ppid= -o command= > "$snap_dir/.ps-env" 2>/dev/null || return 1
  else
    : > "$snap_dir/.ps-env" 2>/dev/null || return 1
  fi
  bps -A -ww -o pid= -o ppid= -o command= > "$snap_dir/.ps-argv2" 2>/dev/null || return 1
  [ -s "$snap_dir/.ps-argv" ] && [ -s "$snap_dir/.ps-argv2" ] || return 1
  if [ -n "$env_flag" ]; then [ -s "$snap_dir/.ps-env" ] || return 1; fi
  return 0
}
carrying_token() {
  awk -v self="$$" -v word="THEOURGIA_SUITE_TOKEN=$THEOURGIA_SUITE_TOKEN" '
    function rest(line) { sub(/^[ ]*[0-9]+[ ]+[0-9]+[ ]/, "", line); return line }
    function starts(all, cmd) { return (cmd != "" && substr(all, 1, length(cmd)) == cmd) }
    FILENAME ~ /\.ps-argv$/ { argv1[$1] = rest($0); next }
    FILENAME ~ /\.ps-argv2$/ { argv2[$1] = rest($0); next }
    { env[$1] = $0 }
    END {
      for (pid in env) {
        split(env[pid], f, " ")
        if (pid == self || f[2] == self) continue
        all = rest(env[pid]); cmd = ""
        if ((pid in argv1) && starts(all, argv1[pid])) cmd = argv1[pid]
        if ((pid in argv2) && starts(all, argv2[pid]) && length(argv2[pid]) > length(cmd)) cmd = argv2[pid]
        if (cmd == "") continue
        n = split(substr(all, length(cmd) + 1), w, " ")
        for (i = 1; i <= n; i++) if (w[i] == word) { print pid; break }
      }
    }' "$snap_dir/.ps-argv" "$snap_dir/.ps-argv2" "$snap_dir/.ps-env"
}
# THE COUNT OF WHAT THIS RUN LEFT, as one function, because it runs at the
# end of the run and also on the signal path (launcher design L7), where
# a leaver must be reported too. Before the probe has finished it still
# counts by token, and says what that cannot see.
probe_done=0
run_root_before=""
count_leaks() {
  sleep 3
  leaked=0
  run_root_after=""
  [ -n "$run_root_before" ] && run_root_after=$(count_run_root)
  if [ -z "$run_root_before" ]; then
    echo "real run root: not counted, the run was stopped before its first count"
  elif [ "$run_root_after" -gt "$run_root_before" ]; then
    echo "LEAKED-INTO-REAL-RUN-ROOT: $real_run_root grew from $run_root_before to $run_root_after"
    echo "  a fixture let the product compute a path and did not set THEOURGIA_RUN"
    leaked=1
  else
    echo "real run root: $run_root_before before, $run_root_after after -- nothing added"
  fi
  # THE LEAKED LINE NAMES WHAT IT COUNTED, pids on the line and each command
  # below it, so a reader can check them rather than trust a number.
  if [ "$probe_done" != 1 ]; then
    echo "leak count: by this run's token, before the probe had finished; a process whose environment this platform hides is not counted"
    if snapshot && found=$(carrying_token); then
      left_pids=$(printf '%s\n' "$found" | tr '\n' ' ' | sed 's/ *$//')
      if [ -n "$left_pids" ]; then
        echo "LEAKED-PROCESSES: $left_pids -- processes carrying this run's token $token are still running"
        for p in $left_pids; do
          echo "  $p $(bps -ww -o command= -p "$p" 2>/dev/null)"
        done
        leaked=1
      else
        echo "processes: none carrying this run's token $token is left running"
      fi
    else
      echo "LEAK COUNT NOT TAKEN: the process snapshots under $snap_dir could not be taken or read; whatever is still running was not looked at"
      leaked=1
    fi
  elif [ "$env_visible" = 1 ]; then
    if [ "$(uname)" = Darwin ]; then
      echo "leak count: by this run's token in the environment; macOS does not show the environment of its system binaries (/bin, /usr/bin), so a leaked shell or sleep is not counted"
    fi
    counted=1
    left_pids=""
    # THE MATCHER'S OWN FAILURE COUNTS. Its status is read before any pipe,
    # which would otherwise hand on the status of the last command in it --
    # an awk that could not read a snapshot then read as "none carrying".
    if snapshot && found=$(carrying_token); then
      left_pids=$(printf '%s\n' "$found" | tr '\n' ' ' | sed 's/ *$//')
    else
      counted=0
    fi
    if [ "$counted" = 0 ]; then
      echo "LEAK COUNT NOT TAKEN: the process snapshots under $snap_dir could not be taken or read; whatever is still running was not looked at"
      leaked=1
    elif [ -n "$left_pids" ]; then
      echo "LEAKED-PROCESSES: $left_pids -- processes carrying this run's token $token are still running"
      for p in $left_pids; do
        echo "  $p $(bps -ww -o command= -p "$p" 2>/dev/null)"
      done
      leaked=1
    else
      echo "processes: none carrying this run's token $token is left running"
    fi
  elif [ "$snap_tried" = 1 ] && [ "$snap_worked" = 0 ]; then
    echo "LEAK COUNT NOT TAKEN: every process snapshot tried while probing failed under $snap_dir; whatever is still running was not looked at"
    leaked=1
  else
    echo "leak count: machine-wide by executable name, this platform does not show environments"
    schemes_after=$(bpgrep -x scheme 2>/dev/null | wc -l | tr -d " ")
    if [ "$schemes_after" -gt "$schemes_before" ]; then
      left_pids=$(bpgrep -x scheme 2>/dev/null | tr '\n' ' ' | sed 's/ *$//')
      echo "LEAKED-PROCESSES: $left_pids -- scheme processes went from $schemes_before to $schemes_after, by name"
      for p in $left_pids; do
        echo "  $p $(bps -ww -o command= -p "$p" 2>/dev/null)"
      done
      leaked=1
    else
      echo "processes: $schemes_before scheme processes before, $schemes_after after, by name"
    fi
  fi
}
# A REFUSAL BEFORE THE END STILL COUNTS WHAT THIS RUN LEFT (launcher
# design L8): the refusal is printed, the count runs, and a leak or a
# member left in a group -- ranked above the remaining refusals -- decides
# the status if there was one; otherwise the refusal's own code does.
early_refuse() {
  echo "REFUSING: $1"
  count_leaks
  if [ "$leaked" != 0 ] || [ "$left_group" != 0 ]; then
    echo "REFUSING: this run left something behind"
    exit 3
  fi
  exit "$2"
}
# THE LEAK COUNT'S OWN FILES LIVE IN A DIRECTORY OF THE RUNNER'S OWN, made
# by mktemp in TMPDIR (or beside the socket root), mode 700, never exported,
# and removed with the two roots. Its name carries the token too, so what
# a run killed outright leaves behind is still recognisably that run's
# (measured: a SIGKILLed run left an anonymous tmp.* directory). Both
# roots are handed to fixtures, and a careless fixture that locks or
# fills what it was handed must not blind the count that judges it.
# (Measured: with the files under the scratch root, a locked base turned
# the count into "none left running" beside a live leak; under the socket
# root, a changed mode on the files did the same.)
# NOTE: THE TEMPLATE IS EXPLICIT. Measured on macOS 26: `mktemp -d` and
# `mktemp -d -t` both ignore TMPDIR and use the per-user temporary
# directory, whatever TMPDIR says. Where TMPDIR is unset the directory
# goes beside the socket root, the system's own temporary directory.
snap_parent=${TMPDIR:-${sock_root%/*}}
snap_dir=$(mktemp -d "${snap_parent%/}/ths-snap.$token.XXXXXX") || {
  snap_dir=""
  early_refuse "mktemp -d made no directory for the leak count's snapshots" 1
}
chmod 700 "$snap_dir"
# CAN THIS PLATFORM SHOW A PROCESS'S ENVIRONMENT? Asked of a process that
# carries the token and is not this runner's direct child: a subshell's
# own child, a scheme, which is what a fixture leaks. It is looked for
# once a second until it appears or ten seconds pass. If it does not come
# back -- or its pid could not be read -- the count cannot be by token,
# and the output says so.
#
# NOTE: THE PROBE IS A SCHEME, NOT A sleep. Measured on macOS 26: `ps -E`
# shows the environment of scheme and of Homebrew's python, and not of
# the system's own binaries -- /bin/sleep, /bin/sh, /usr/bin/perl came back
# without one. A probe made of sleep read "no environments" on a platform
# that shows the ones that matter.
env_visible=0
#
# NEVER: NO SIGNAL IS AIMED AT A PID READ BACK FROM A FILE. The pid file is
# only compared with what the count lists. The probe's own subshell stops
# its child and waits for it when it is sent TERM, and the runner signals
# and waits only for that subshell, which it started and whose pid it
# holds. So when the count begins, the probe is gone -- a probe still
# running would be counted "before" in the fallback and hide one leak.
# The pid is written to a temporary file and moved into place, so a read
# sees all of it or none.
echo "(sleep (make-time 'time-duration 0 30))" > "$run_root/.probe.ss"
(
  child=""
  trap 'if [ -n "$child" ]; then kill "$child" 2>/dev/null; wait "$child" 2>/dev/null; fi; exit 0' TERM
  scheme --script "$run_root/.probe.ss" &
  child=$!
  echo "$child" > "$run_root/.probe-pid.tmp" && mv "$run_root/.probe-pid.tmp" "$run_root/.probe-pid"
  wait "$child"
) &
probe_parent=$!
probe_pid=""
tries=0
# A SNAPSHOT THAT FAILS IS NOT "NOT VISIBLE YET". Only a probe that could
# not be seen in snapshots that were taken sends the count to its
# by-name fallback; if every snapshot or match attempted here failed, the
# count cannot be taken at all, and the end of the run says so.
snap_tried=0
snap_worked=0
while [ "$tries" -lt 10 ] && [ "$env_visible" = 0 ]; do
  sleep 1
  tries=$((tries + 1))
  probe_pid=$(cat "$run_root/.probe-pid" 2>/dev/null)
  case "$probe_pid" in
    ""|*[!0-9]*) continue ;;
  esac
  snap_tried=1
  snapshot || continue
  found=$(carrying_token) || continue
  snap_worked=1
  case " $(printf '%s\n' "$found" | tr '\n' ' ') " in
    *" $probe_pid "*) env_visible=1 ;;
  esac
done
stop_probe
probe_done=1
if [ "$env_visible" = 0 ]; then
  schemes_before=$(bpgrep -x scheme 2>/dev/null | wc -l | tr -d " ")
fi

# ---- what this run must not leave behind ------------------------------------
#
# KEY: A FIXTURE THAT WRITES INTO THE USER'S OWN DIRECTORIES, OR LEAVES A
# DAEMON RUNNING, BREAKS NOTHING AND REDDENS NOTHING. Three fixtures in
# this tree did both at once and every row in every one of them stayed
# green: the daemons they leaked held sockets and logs under
# `$HOME/.theourgia/run`, which is the product's REAL run root, because
# the fixtures had not set `THEOURGIA_RUN` and the product computes that
# path when nothing says otherwise. Fifteen daemons and thirty-three
# directories accumulated before anybody counted.
#
# NEVER: SO IT IS COUNTED HERE RATHER THAN BY A PERSON. The same hole turned
# up in three separate fixtures; after the third, "we will notice" stopped
# being a plan.
#
# NOTE: THE CRITERION IS GROWTH, NOT A TOTAL. Another session's daemons may
# be running and the user's run root may legitimately hold something;
# what this run is answerable for is the difference it made.
real_run_root="$HOME/.theourgia/run"
count_run_root() { ls -d "$real_run_root"/*/ 2>/dev/null | wc -l | tr -d " "; }
run_root_before=$(count_run_root)

# KEY: AND A FLOOR UNDER ALL OF IT: every fixture this runner starts gets a
# run root of its own, here, before any of them runs. Six fixtures had to
# be corrected one at a time because the product computes
# `$HOME/.theourgia/run` when nothing says otherwise, and each was found
# only by counting what had appeared in it. A seventh should not have to
# be found that way.
#
# NOTE: THIS DOES NOT REPLACE THE FIXTURES' OWN. They set `THEOURGIA_RUN` to
# their own directory because they must not disturb EACH OTHER -- two
# fixtures sharing one run root share sockets and locks. This is the
# floor for a fixture that sets nothing, not a substitute for the ones
# that do.
#
# NOTE: AND IT DOES NOT REPLACE THE GATE EITHER. The gate still judges by
# growth in the REAL run root: what it catches now is something that
# overrode or escaped this, which is exactly the case nobody would think
# to look for.
export THEOURGIA_RUN="$out/run-root"
mkdir -p "$THEOURGIA_RUN"

# A FIXTURE MAY NOT SHARE A BASENAME WITH A LIBRARY, and this refuses
# before the run rather than after the delivery.
#
# A DELIVERY IS ONE FLAT DIRECTORY -- the libraries sit beside the
# fixtures -- so two files called `code-suggest.sc` cannot both be in it.
# Building one copied the fixtures and then the libraries into the same
# place and the second copy silently replaced the first; which of the two
# survived depended on the order, and NEITHER outcome is necessarily red:
# a missing library takes the whole suite down in a way that looks like a
# broken import, and a missing fixture is simply a smaller total that
# nothing compares against.
#
# IT ALSO BROKE A FIXTURE IN THE REPOSITORY, where the two directories
# are separate. `code-text-audit.sc` looks for a library beside itself
# before looking one level up, found `test/code-suggest.sc`, walked the
# fixture instead of the library, found nothing to complain about, and
# printed four green rows.
#
# WHERE IT CANNOT RUN, IT SAYS SO. In a delivery there is no separate
# library directory to compare against -- by then the clash has already
# happened -- so this prints that it did not run instead of printing
# nothing, which would read as a pass.
# WHAT COUNTS AS A LIBRARY DIRECTORY IS "IT HOLDS SOURCES", not "it
# holds core.sc". Keying the whole comparison on one filename meant a
# parent full of libraries with that one file missing or renamed took
# the NOT CHECKED branch and the run continued.
# THE LIBRARY DIRECTORY IS CHECKED BEFORE ANYTHING RUNS. The core reaches
# igropyr through three facades, so a pin that holds only theourgia/ --
# or an igropyr/ that is there but cannot answer -- produces a hundred
# fixtures at rc=255, every one of them saying a library was not found.
# That reads as a broken tree. One line naming what is missing is the
# difference between a variable to fix and an afternoon of bisecting.
#
# THREE THINGS, BECAUSE THEY FAIL SEPARATELY: the directory is there, the
# library this core actually imports from is there, and it exports the
# name this core actually uses. The last one is what tells a WRONG
# igropyr from a missing one.
pinned=${THEOURGIA_LIBDIR:-$CHEZSCHEMELIBDIRS}
if [ -n "$pinned" ]; then
  first=$(printf %s "$pinned" | cut -d: -f1)
  if [ ! -d "$first/igropyr" ]; then
    echo "LIBRARY PATH: $first holds no igropyr/ -- the core imports (igropyr crypto),"
    echo "  (igropyr sexpr) and (igropyr platform) through its three facades."
    early_refuse "nothing below this line would be a reading." 1
  fi
  if [ ! -f "$first/igropyr/crypto.sc" ]; then
    echo "LIBRARY PATH: $first/igropyr has no crypto.sc -- (igropyr crypto) cannot resolve."
    early_refuse "nothing below this line would be a reading." 1
  fi
  if ! grep -q "sha256" "$first/igropyr/crypto.sc"; then
    echo "LIBRARY PATH: $first/igropyr/crypto.sc does not mention sha256 -- this is an"
    echo "  igropyr, but not one this core can use."
    early_refuse "nothing below this line would be a reading." 1
  fi
  echo "library path: $first holds igropyr/ and theourgia/"
fi

libdir=..
if [ "$(cd "$libdir" && pwd)" != "$(pwd)" ] && ls "$libdir"/*.sc > /dev/null 2>&1; then
  clash=""
  for f in *.sc; do
    [ -f "$libdir/$f" ] && clash="$clash $f"
  done
  if [ -n "$clash" ]; then
    echo "NAME CLASH: fixtures sharing a basename with a library in $(cd "$libdir" && pwd):$clash"
    early_refuse "a flat delivery cannot hold both copies." 1
  fi
  echo "fixture/library names: no clash against $(ls "$libdir"/*.sc | wc -l | tr -d " ") libraries"
else
  echo "fixture/library names: NOT CHECKED -- no separate library directory beside this one"
fi

# THE CHEAP GATE THAT NAMES A CAUSE RUNS BEFORE THE ONES THAT SHOW A
# SYMPTOM. This is the same repair as moving the structural gates above
# the verdict: a check is worth what it is worth AT THE MOMENT IT RUNS.
#
# `expansion-branches.sc` loads each expansion branch of this tree -- the
# one with THEOURGIA_INJECT unset and the one with it on -- and answers
# in about two seconds, naming the file and line when a branch will not
# build. Nothing else here asks that question early.
#
# MEASURED, AND THIS IS WHY IT IS FIRST. A facade change left
# `string-contains?` unbound inside the injected branch of `ffi.sc`.
# Every ordinary fixture stayed green. `cli1`, which sorts EARLIER than
# `expansion-branches`, starts two children that then died on load,
# spun out its bounded wait for them, and blocked forever writing to a
# fifo with no reader -- so the suite read the defect as a 900-second
# alarm, once per fault-injection fixture, three and a half hours before
# reaching the two-second gate that names it.
#
# NEVER: A RED PREFLIGHT STOPS THE RUN. Every later reading would be about a
# tree that cannot be built in one of the two shapes it ships in.
#
# It is NOT excluded from the loop below: it runs again there, as an
# ordinary fixture, so that the classifier still sees every script in
# this directory exactly once and the count-back gate stays true. Two
# seconds is a cheap price for leaving that invariant alone.
# NEVER: AND IT IS NOT JUDGED BY ITS EXIT STATUS. Measured: on a tree whose
# injected branch would not build, `expansion-branches.sc` printed the
# unbound identifier, the file and the line -- and exited 0. It has no
# `(exit ...)` at all, and neither do fifty-eight of the other fixtures
# here: THIS SUITE DECIDES ON OUTPUT, not on status, and the loop below
# says so at length. A preflight that trusted `if scheme --script ...`
# would have been green on the very tree that produced it.
#
# So the preflight applies the loop's own four measures, in one place
# rather than two: the sentinel, the hard lines, the counters, the
# status.
if [ -f expansion-branches.sc ]; then
  run_tracked 120 "$out/preflight.out" preflight scheme --script expansion-branches.sc
  pf_rc=$?
  pf_sent=$(grep -c "^expansion-branches complete" "$out/preflight.out")
  pf_hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/preflight.out")
  pf_cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/preflight.out" | grep -vc "^0 ")
  if [ "$pf_rc" = 0 ] && [ "$pf_sent" != 0 ] && [ "$pf_hard" = 0 ] && [ "$pf_cnt" = 0 ]; then
    echo "preflight: every expansion branch of this tree builds"
  else
    echo "PREFLIGHT RED (rc=$pf_rc sentinel=$pf_sent hard=$pf_hard counters=$pf_cnt)"
    echo "  -- an expansion branch of this tree does not build:"
    sed "s/^/  /" "$out/preflight.out"
    early_refuse "nothing below this line would be a reading." 1
  fi
else
  echo "preflight: NOT CHECKED -- expansion-branches.sc is not in this directory"
fi

# THE SECOND PREFLIGHT, AND IT ANSWERS A QUESTION NOTHING ELSE ASKS.
#
# A MISSING CLOSER IS NOT A SYNTAX ERROR: the reader takes it, and the
# definitions after the short form become part of its body. What comes
# out is an "unbound identifier" naming something defined far below,
# reported where it is USED. Measured on daemon.sc, 2026-09-18: one `)`
# swallowed twenty-six definitions and the report was `unbound
# identifier directory-of at line 219`, a hundred and eighty lines from
# the cause -- and the whole file still balanced, because a second edit
# had one closer too many.
#
# NEVER: IT RUNS BEFORE THE LOOP for the same reason expansion-branches does:
# a tree in that state produces a hundred fixtures failing to import
# something, which reads as a broken environment. One line naming the
# file and the first definition that was swallowed is the difference.
#
# NOTE: IT IS IN `*.sc` NOW, AND IT DOES NOT RUN AGAIN IN THE LOOP: the
# loop names it among the helpers, as it named the Python file before it.
# It was `structure.py`, the one check in this suite that needed Python,
# so a machine without `python3` printed NOT CHECKED here and took a
# reading with a hole in it. It is Scheme since F9, a port read against
# the Python on the whole tree and on copies with each kind of defect,
# and it runs wherever the fixtures do.
if [ -f structure.sc ] && [ "$aborted" = 1 ]; then
  echo "preflight: NOT RUN -- $abort_reason; nothing more is launched"
elif [ -f structure.sc ]; then
  run_tracked 120 "$out/structure.out" structure scheme --script structure.sc
  st_rc=$?
  st_sent=$(grep -c "^structure complete" "$out/structure.out")
  st_hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/structure.out")
  st_cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/structure.out" | grep -vc "^0 ")
  if [ "$st_rc" = 0 ] && [ "$st_sent" != 0 ] && [ "$st_hard" = 0 ] && [ "$st_cnt" = 0 ]; then
    echo "preflight: $(grep '^checked ' "$out/structure.out"), every definition where its parentheses say"
    grep "^NOT CHECKED " "$out/structure.out" | sed "s/^/  /"
  else
    echo "PREFLIGHT RED (rc=$st_rc sentinel=$st_sent hard=$st_hard counters=$st_cnt)"
    echo "  -- a form in this tree does not close where it looks like it does:"
    sed "s/^/  /" "$out/structure.out"
    early_refuse "nothing below this line would be a reading." 1
  fi
else
  echo "preflight: NOT CHECKED -- structure.sc is not in this directory"
fi

# NEVER: PYTHON FIXTURES RUN TOO, AND THEY DID NOT USED TO. This loop was
# `for f in *.ss`, so ten fixtures in this directory -- every MCP cell,
# the daemon lifecycle cells, the eval cells, `q8-cli`,
# `working-processes`, `datum-processes` -- were never run by any suite.
# They passed when somebody remembered to run them by hand and rotted
# when nobody did: measured on the day the loop was widened, two of them
# were red, against a Python daemon deleted a batch earlier, and no
# reading had ever said so. "The environment could satisfy it and it was
# not run" is a defect, not an opt-in.
#
# NOTE: THE THREE CRITERIA ARE THE SAME for both kinds, and so is the
# classifier: a sentinel line, no non-zero counter, no hard line. What
# differs is only the interpreter.
bad=0; ran=0; libs=""; probes=""; helpers=""; pyran=0; pyred=0
for f in *.sc *.py; do
  # A GLOB THAT MATCHED NOTHING IS ITS OWN WORD in sh: a directory with no
  # python fixture handed this loop the literal `*.py`, which was launched,
  # read RED, counted as a fixture the directory does not hold, and then
  # refused as "the directory changed under the run" -- the wrong cause.
  [ -e "$f" ] || continue
  # NOTHING MORE IS LAUNCHED once a launch's group or result could not be
  # read, or a member of it outlived KILL (launcher design L6).
  [ "$aborted" = 1 ] && break
  case "$f" in
    *.sc) n=${f%.sc}; runner="scheme --script";;
    *.py) n=${f%.py}; runner="python3";;
  esac
  # NOTE: `paths.py` IS A HELPER, NOT A FIXTURE: it is imported by the
  # others and prints no sentinel of its own. It is named here rather
  # than detected, because "imports nothing and prints nothing" is also
  # what a broken fixture looks like.
  # NOTE: SIX HELPERS, NAMED RATHER THAN DETECTED. `paths` is imported by
  # the python fixtures; `structure` is the preflight above;
  # `reduce-hash-check` is a filter `reduce1.sc` pipes bytes through --
  # run bare it prints a hash and no sentinel, which is also what a
  # broken fixture looks like, so the list says which it is;
  # `import-walk` is the shared walker seven fixtures `load`;
  # `own-verbs` is the reader of the programs' dispatch tables that
  # `options-gate` and `docs-check` `load` (F64); and `f54-reference` is
  # the reference library arm B of `startup-ratio` imports (F54): a
  # library, not a fixture, whose name is not (theourgia ...) because
  # the reference must import nothing of the tree, so the library test
  # below does not catch it. Its program, `f54-reference-main.ss`, is a
  # `.ss` and never reaches this loop. A seventh helper, `launch.pl`,
  # starts every fixture; no glob here matches `.pl`, so it is never
  # classified at all.
  #
  # NEVER: `import-walk` JOINED THIS LIST BECAUSE OF ITS EXTENSION. It was
  # `.scm` and so was never in `*.ss`, which is what the note above the
  # loop meant by "it does not run again". Renaming it to `.sc` put it in
  # the loop for the first time, where it would have run bare, printed no
  # sentinel, and been counted as a broken fixture -- an extension change
  # altering WHICH FILES ARE TESTS.
  case "$n" in
    paths|structure|reduce-hash-check|import-walk|own-verbs|f54-reference) helpers="$helpers $n"; continue;;
  esac
  if grep -q "^(library (theourgia" "$f"; then libs="$libs $n"; continue; fi
  # NEVER: STANDARD INPUT IS /dev/null, FOR EVERY FIXTURE. Inherited from the
  # runner, it is whatever the person running the suite happened to have:
  # under a terminal -- a run inside `screen` -- a fixture that reaches a
  # verb reading standard input waits for an end of file that never comes,
  # and the alarm below kills it 900 seconds later having printed half its
  # rows. Measured: `client-program` hung at 21 of 41 rows and the leak
  # gate reported the three processes the hung chain held; the same
  # fixture with `</dev/null` answered 38 of 38 in 25 seconds.
  #
  # NOTE: A FIXTURE THAT NEEDS INPUT MUST HAND IT OVER ITSELF -- a pipe, a
  # here-string, a file -- rather than inheriting whatever is there. What
  # a run measures may not depend on where it was started from.
  # IN A GROUP OF ITS OWN, THROUGH run_tracked: so that a signal to the
  # runner can stop the fixture and everything it started in that group
  # (see on_signal). A detached daemon leaves the group and is found by
  # the leak count instead.
  run_tracked "$fixture_limit" "$out/$n.out" "$n" $runner "$f"
  rc=$?
  sent=$(grep -c "^$n complete" "$out/$n.out")
  # TWO USAGE SHAPES, BECAUSE THERE ARE TWO KINDS OF CALLER. A probe
  # writes a plain `usage:` line for a person; the CLI answers with one
  # S-expression on stdout whatever happens, so its usage is `(usage
  # ...)`. Forcing the CLI to the probes' shape would break its own
  # contract to make a runner simpler.
  # AND A USAGE LINE IS NOT A PLACE TO HIDE. The probe branch skipped
  # every failure check below it, so a script that printed its usage and
  # THEN failed was filed as a probe and the run exited zero. Measured on
  # the classifier alone, three ways: rc=255 with an exception after the
  # usage line; rc=1 with nothing else at all; and rc=0 with `1 failures`
  # printed after it. All three gave `bad=0 ran=0` and a clean exit.
  # A probe has to have ended cleanly BY EVERY MEASURE THIS RUNNER HAS --
  # no hard line, no non-zero counter, and one of the two exit statuses
  # this directory's probes actually use.
  #
  # NEVER: AND THE TWO ARE MEASURED, NOT CHOSEN. Requiring 0 alone reclassified
  # nine of the fourteen probes as red in one run -- `barrier-probe`,
  # `dirfault`, `dirflush`, `fault-file`, `fault-pipe`, `probe`, `row`,
  # `shared-lock` and `stagefault` all print `usage: <name>.sc ...` and
  # exit 2, which is their convention; `q8-report` and the five scripts
  # added this batch exit 0. Both are usage exits and neither is a
  # failure. Anything else -- including a signal, which is 128 and up --
  # is not a probe and goes through the checks below.
  hard_here=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/$n.out")
  cnt_here=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/$n.out" | grep -vc "^0 ")
  if [ "$sent" = 0 ] && [ "$hard_here" = 0 ] && [ "$cnt_here" = 0 ] \
     && { [ "$rc" = 0 ] || [ "$rc" = 2 ]; } \
     && { grep -q "^usage: $n" "$out/$n.out" || grep -q "^(usage " "$out/$n.out"; }; then
    probes="$probes $n"; continue
  fi
  ran=$((ran+1))
  case "$f" in *.py) pyran=$((pyran+1));; esac
  cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/$n.out" | grep -vc "^0 ")
  hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/$n.out")
  if [ "$rc" != 0 ] || [ "$sent" = 0 ] || [ "$cnt" != 0 ] || [ "$hard" != 0 ]; then
    bad=$((bad+1))
    case "$f" in *.py) pyred=$((pyred+1));; esac
    printf "RED %-20s rc=%-3s sentinel=%s counters=%s hard=%s\n" "$n" "$rc" "$sent" "$cnt" "$hard"
  fi
done
# AND THE CLASSES ARE COUNTED BACK -- WHICH DETECTS THE DIRECTORY
# CHANGING UNDER THE RUN, not a classifier that drops a file.
#
# The classifier cannot drop one: its last branch catches everything, so
# every script lands in exactly one of the three buckets and the sum
# always equals the count. What can differ is the DIRECTORY, because the
# total below is taken after the loop: a file added between the two makes
# the total larger, a file removed makes it smaller, and a run whose
# pending script is deleted dies with no output at all.
#
# Measured, by running two suites in this directory at once while one of
# them was adding and removing a control fixture. It produced `85 scripts
# in the directory, 86 classified` in one direction and a plausible
# "clean run that exits non-zero" in the other -- the second was reported
# as a defect in an unrelated gate before the cause was found. ONE RUNNER
# AT A TIME IN A DIRECTORY.
total=$(ls *.sc *.py 2>/dev/null | wc -l | tr -d " ")
nlibs=$(echo $libs | wc -w | tr -d " ")
nprobes=$(echo $probes | wc -w | tr -d " ")
# THE SUMMARY SAYS WHAT IT COUNTS (F80). It used to print the number of
# python fixtures RUN inside the parentheses after the reds, where it read
# as the number of reds that were python: "3 are python" of three reds
# that were all .sc.
echo "fixtures run: $ran   not-green: $bad   (python among them: $pyred)"
# VOID IS NOT GREEN. A row whose instrument failed its own control prints
# "VOID <row>: <reason>" and is not a failure; counted here by fixture, so a
# row that is void on every run -- a row that tests nothing -- is seen in
# the summary rather than read as passing. Always printed, 0 included.
nvoid=0; voids=""
for vf in "$out"/*.out; do
  [ -f "$vf" ] || continue
  vn=$(grep -c "^VOID " "$vf")
  if [ "$vn" -gt 0 ]; then nvoid=$((nvoid+vn)); voids="$voids $(basename "$vf" .out)($vn)"; fi
done
echo "void rows: $nvoid${voids:+ in:$voids}"
echo "python fixtures run: $pyran"
echo "libraries ($nlibs):$libs"
echo "probes, printed a usage line ($nprobes):$probes"
nhelpers=$(echo $helpers | wc -w | tr -d " ")
echo "helpers, not fixtures ($nhelpers):$helpers"
sum=$((ran + nlibs + nprobes + nhelpers))
if [ "$aborted" = 1 ]; then
  echo "NOT ALL LAUNCHED: $abort_reason; $sum of $total scripts classified before the run stopped launching"
elif [ "$sum" != "$total" ]; then
  echo "UNACCOUNTED: $total scripts in the directory, $sum classified"
  early_refuse "the directory changed under the run" 1
else
  echo "all $total scripts accounted for"
fi

# THE TWO STRUCTURAL CHECKS BELOW RUN BEFORE THE REFUSAL, NOT AFTER IT.
# They were written after it, and this suite has three fixtures that are
# red by design -- so `exit 2` fired first on every single run and
# NEITHER of them had ever executed. A check placed after an exit that
# always happens is a check that has never had a first reading. They ask
# about the SHAPE of the fixture set, which is a fact about the
# directory and not about whether today's run was green, so they belong
# ahead of the verdict.

# A FIXTURE THAT CAN BE KILLED BY AN ANSWER IS NOT A FIXTURE. Rows read
# answers apart, so a seeded defect that changes an answer's SHAPE makes
# the accessor raise while a row is being computed -- and the file ends
# there, every row below it unrun, with no `FAIL` printed at all. Three
# defects were scored as crashes with no failures that way, for answers
# the store had got right and said plainly.
#
# THE GUARD IS `want` BEING A MACRO OVER BOTH SIDES, and every fixture
# that has rows needs it. Nothing else notices its absence: a file
# without it behaves identically until the day a mutant lands on it, and
# then it reports quiet instead of a kill. So the absence is counted
# here rather than left to be discovered.
# THE ROW BASELINE CHECK LIVES IN ITS OWN SCRIPT, so that it can be run on
# made-up input without running the suite. It reads `rows-baseline.txt` and
# `$out/<name>.out` from this directory and prints two lines, one about the
# hashes and one about the counts; only the hash line refuses. The reason it
# is a separate file is written at the top of it.
# NOT UNDER AN EXPLICIT THEOURGIA_FIXTURE_LIMIT. A run that cuts its fixtures
# short has no rows to judge against the table: every cut fixture would read
# as a row count that differs. One line says the step did not run.
if [ -n "${THEOURGIA_FIXTURE_LIMIT:-}" ]; then
  echo "row baseline: not checked: THEOURGIA_FIXTURE_LIMIT=$fixture_limit cuts fixtures short, so their rows are not judged"
  baseline_rc=0
else
  sh row-baseline-check.sh "$out"
  baseline_rc=$?
fi
if [ "$baseline_rc" = 2 ]; then
  echo "ROW BASELINE CHECK WAS CALLED WRONG -- it needs the output directory"
  early_refuse "the row baseline check was called wrong" 1
fi

baseline_bad=0
if [ "$baseline_rc" != 0 ]; then
  baseline_bad=1
fi
# AND THE GATE ABOVE ONLY SEES FIXTURES THAT PRINT A COUNT. The ones that
# do not are invisible to it -- the same shape of silence it exists to
# close -- so they are counted and named here rather than left out.
counted=0; uncounted=""
for f in *.sc; do
  n=${f%.sc}
  [ -f "$out/$n.out" ] || continue
  case " $libs $probes " in *" $n "*) continue;; esac
  if grep -q "^rows: " "$out/$n.out"; then counted=$((counted+1))
  else uncounted="$uncounted $n"; fi
done
echo "row counts: $counted fixture(s) print one; $(echo $uncounted | wc -w | tr -d " ") do not:$uncounted"

ungirded=""
for f in *.sc; do
  grep -q "^ *(define-syntax want$" "$f" || continue
  # MATCHED AS WHOLE LINES (want's head, the include), NOT AS SUBSTRINGS: an
  # earlier form of this check, `grep -q "define-syntax caught"`, stayed
  # silent for a macro renamed `caught-disabled` -- a false silence, the one
  # failure a check of this kind must not have.
  # READ IN THE want FORM ITSELF, from its line to the parenthesis that closes
  # it (counted outside ; comments): a mention elsewhere in the file -- a
  # comment, a fixture's text in a string -- says nothing about what want
  # does. Its computed value, want-1's second argument, goes through a guard
  # (caught, tolerant, or an inline guard); its expected value goes through
  # with-expected (expected.ss), included as a whole line: left bare, a row
  # whose expected value raises ends the file, and caught like the computed
  # one, two rows that raise the same message compare equal and read green.
  form=$(awk '/^ *\(define-syntax want$/ { on = 1 }
              on { line = $0; sub(/;.*/, "", line); print $0
                   depth += gsub(/\(/, "(", line) - gsub(/\)/, ")", line)
                   if (depth <= 0) exit }' "$f")
  { printf '%s\n' "$form" | grep -q "(with-expected " \
      && printf '%s\n' "$form" | grep -Eq "\(want-1 [^ ()]+ \((caught|tolerant|guard) " \
      && grep -q '^ *(include "expected.ss")$' "$f"; } \
    || ungirded="$ungirded ${f%.sc}"
done
guard_bad=0
if [ -n "$ungirded" ]; then
  echo "UNGUARDED FIXTURES (a row can end the file instead of failing, or two raises read as agreeing):$ungirded"
  guard_bad=1
fi
# AND THE CHECK ABOVE ONLY LOOKS AT FIXTURES THAT DECLARED THE MACRO.
# A fixture whose `want` is a PROCEDURE has both arguments evaluated
# before the call, so it is unguarded by construction and invisible to a
# test that asks "did you declare `caught`". Twenty of them are in this
# directory. Making them red would redden most of the suite at once, so
# they are counted and named -- the absence is a reading rather than a
# silence, and the number is what a decision can be made against.
byproc=""
for f in *.sc; do
  grep -q "^ *(define (want " "$f" || continue
  byproc="$byproc ${f%.sc}"
done
echo "unguarded by construction ($(echo $byproc | wc -w | tr -d " ") fixtures define want as a procedure):$byproc"

# A COUNT THAT IS PRINTED AND NOT RETURNED IS NOT A CHECK EITHER. `$bad`
# was incremented, printed, and never reached the exit status, so every
# caller that tested this runner's exit code was reading a constant: a
# delivery could be built, pinned and frozen with red fixtures inside it
# and nothing in the chain would object. The only thing standing between
# that and a bad delivery was a person reading the number.
# ---- and the two counts again ------------------------------------------------
#
# NOTE: A SETTLE BEFORE THE SECOND READING. A daemon told to go does not go
# instantly, and a run that counted the moment its last fixture returned
# would report its own tidy-up as a leak.
count_leaks

# ---- every refusal, after everything has been said ---------------------------
#
# The order is by what a reader should fix first, and each code is distinct
# so a caller can tell them apart without parsing the text.
#
# NEVER: NOT ONLY THE FIRST. Exiting at the first refusal hid every later one
# behind it: a tree with known reds always stopped at "not green", and its
# unguarded fixtures and row baseline were never refused where a caller looks.
# Every refusal is printed, in this order; the exit code is the first one's.
refusal_code=0
refuse() {
  echo "REFUSING: $2"
  if [ "$refusal_code" = 0 ]; then refusal_code=$1; fi
}
if [ "$bad" != 0 ]; then
  refuse 2 "$bad fixture(s) not green"
fi
if [ "$leaked" != 0 ] || [ "$left_group" != 0 ]; then
  refuse 3 "this run left something behind"
fi
if [ "$aborted" != 0 ]; then
  refuse 6 "$abort_reason; nothing more was launched"
fi
if [ "$guard_bad" != 0 ]; then
  refuse 4 "unguarded fixture(s)"
fi
if [ "$baseline_bad" != 0 ]; then
  refuse 1 "the row baseline does not describe this tree"
fi
if [ "$refusal_code" != 0 ]; then
  exit "$refusal_code"
fi

