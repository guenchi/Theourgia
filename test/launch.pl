#!/usr/bin/env perl
# Copyright 2026 guenchi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# THE ENVIRONMENT THIS FILE READS, DECLARED, for docs-check.sc's
# DOC-3 and its TWIN, as in run-fixtures.sh: it reads no THEOURGIA_* name,
# and names none.
# ENVIRONMENT READ:
# ENVIRONMENT NAMED, NOT READ:
#
# ONE LAUNCH, AND THE WHOLE OF ITS PROCESS GROUP (the launcher design,
# theourgos core/briefs/launcher-design.md, closed at v4.1):
#
#   perl launch.pl --limit SECONDS --out FILE --result FILE --id LAUNCH-ID
#                  [--census-pid PID] [--fault-setpgid ERRNO-NAME[:MODE]]
#                  -- COMMAND ARGS...
#
# --fault-setpgid is a measuring injection for launch-self.sc and nothing
# else: the watcher does not call setpgid on the anchor and takes it as
# failed with that errno, then reads the anchor's group as it always does.
# Two modes reach the anchor too. ":anchor-skips": the anchor does not call
# its own setpgid(0,0) either and takes it as failed with the same errno, so
# the group is not set and the anchor's own note is written.
# ":anchor-late": the anchor waits, before its setpgid(0,0), until the
# watcher has made its first read of the group, so that read finds the group
# not yet set and only the read after the anchor's setup finds it. The
# option is read by the watcher and by the anchor, a fork of this process,
# and never forwarded to the command; the suite's runner never passes it. An
# unknown name or mode is refused before the fork.
#
# This process is the WATCHER, W. It stays in its caller's process group.
# It forks the ANCHOR, A, which leads a new group G and forks the COMMAND,
# C, so the command is in G but does not lead it. W owns everything about
# G: the time limit, the grace after C returns, the stop, the census of G's
# members, and the result file. The caller holds W's pid; TERM, INT or HUP
# to W is a request to stop.
#
# W exits with the fixture's status: C's exit code; 128+n if a signal n
# ended C; 127 if the exec failed with ENOENT and 126 for any other exec
# failure; 142 if the time limit ended the launch; 128+n if W itself was
# asked to stop by signal n. It writes the result file by a checked write,
# close and rename, with these lines in this order:
#
#   id LAUNCH-ID
#   status N
#   cleanup clean|left|survivor|unknown
#   member PID COMMAND        (zero or more; for left and survivor)
#   note TEXT                 (zero or more)
#   end
#
# A reader accepts only a file that ends with "end" and names its id.
#
# NEVER: W NEVER WAITS UNBOUNDED. Every wait is against an absolute
# deadline, every census runs under its own bound, and a census that cannot
# be read or validated makes the cleanup state unknown, never clean.
use strict;
use warnings;
use POSIX ();
use Time::HiRes ();

my $GRACE = 2;
my $D1 = 5;
my $D2 = 5;
my $CENSUS_BOUND = 5;
my $QUIET_GAP = 0.1;

# EVERY DEADLINE IS ON CLOCK_MONOTONIC (codex r9 A1). The wall clock can be
# stepped (NTP, a manual set, a resume): a step back holds a bound open for as
# long as the step, a step forward ends it early. Elapsed time is read here
# and nowhere else; the wall clock appears in no deadline.
sub now { Time::HiRes::clock_gettime(Time::HiRes::CLOCK_MONOTONIC()) }

sub usage {
  print STDERR "usage: launch.pl --limit SECONDS --out FILE --result FILE --id LAUNCH-ID [--census-pid PID] [--fault-setpgid ERRNO-NAME[:MODE]] -- COMMAND ARGS...\n";
  exit 2;
}

my %opt;
while (@ARGV && $ARGV[0] ne '--') {
  my $k = shift @ARGV;
  usage() unless $k =~ /^--(limit|out|result|id|census-pid|fault-setpgid)$/ && @ARGV;
  $opt{$1} = shift @ARGV;
}
usage() unless @ARGV && $ARGV[0] eq '--';
shift @ARGV;
my @cmd = @ARGV;
usage() unless @cmd && defined $opt{limit} && defined $opt{out} && defined $opt{result} && defined $opt{id};
usage() unless $opt{limit} =~ /^[0-9]+$/ && $opt{limit} > 0;
usage() unless $opt{id} =~ /^[A-Za-z0-9._-]+$/;
my $census_pid = defined $opt{'census-pid'} ? $opt{'census-pid'} : getppid();
usage() unless $census_pid =~ /^[0-9]+$/;

# ERRNO NAMES BY VALUE, from POSIX's own constants, so a note can say which
# error it was; a value no name has is written "errno n". The candidate names
# are Errno's list (POSIX fills its export tags only on import, and this file
# imports nothing from it); a name POSIX has no constant for is left out.
# Names are taken in sorted order and the first keeps a shared value
# (EAGAIN, not EWOULDBLOCK).
require Errno;
my (%errno_value, %errno_name);
my @errno_names = grep { /^E[A-Z0-9]+$/ } @Errno::EXPORT_OK;
for my $n (sort @errno_names) {
  my $v = eval { no strict 'refs'; &{"POSIX::$n"}() };
  next unless defined $v;
  $errno_value{$n} = $v;
  $errno_name{$v} = $n unless exists $errno_name{$v};
}
sub errno_text { my ($v) = @_; exists $errno_name{$v} ? $errno_name{$v} : "errno $v" }
my $fault_setpgid;
my $fault_anchor_skips = 0;
my $fault_anchor_late = 0;
my $fault_refused;
if (defined $opt{'fault-setpgid'}) {
  my ($fname, $fmode) = split /:/, $opt{'fault-setpgid'}, 2;
  if (!exists $errno_value{$fname}) {
    $fault_refused = "unknown errno name for --fault-setpgid: $opt{'fault-setpgid'}";
  } elsif (defined $fmode && $fmode ne 'anchor-skips' && $fmode ne 'anchor-late') {
    $fault_refused = "unknown mode for --fault-setpgid: $opt{'fault-setpgid'}";
  } else {
    $fault_setpgid = $errno_value{$fname};
    $fault_anchor_skips = (defined $fmode && $fmode eq 'anchor-skips') ? 1 : 0;
    $fault_anchor_late = (defined $fmode && $fmode eq 'anchor-late') ? 1 : 0;
  }
}

my $W = $$;
my $start = now();
my $deadline = $start + $opt{limit};
my @notes;
my $stop_signal = 0;

# The signals 1..31 except KILL and STOP, by number, from perl's own table.
my %signo;
{
  require Config;
  my @n = split ' ', $Config::Config{sig_name};
  my @v = split ' ', $Config::Config{sig_num};
  for my $i (0 .. $#n) { $signo{$n[$i]} = $v[$i] unless exists $signo{$n[$i]}; }
}
my @catchable = grep { $_ >= 1 && $_ <= 31 && $_ != $signo{KILL} && $_ != $signo{STOP} } sort { $a <=> $b } values %signo;
my %seen_no;
@catchable = grep { !$seen_no{$_}++ } @catchable;

# INSTALLS ARE CHECKED BY sigaction'S ANSWER, never by reading %SIG back:
# %SIG reads back a handler for KILL that the kernel refused (measured).
sub install {
  my ($no, $code) = @_;
  my $act = POSIX::SigAction->new($code, POSIX::SigSet->new, 0);
  return POSIX::sigaction($no, $act) ? 1 : 0;
}

# ---- W's own stop handlers -------------------------------------------------
#
# W is started by sh as a background job, which gives it INT ignored; an
# explicit handler replaces that.
for my $name (qw(INT TERM HUP)) {
  my $no = $signo{$name};
  install($no, sub { $stop_signal = $no unless $stop_signal; }) or push @notes, "watcher could not handle $name: $!";
}

# ---- the census --------------------------------------------------------------
#
# ps under a bound: its pipe is closed and the child reaped whatever
# happens. A census is valid only if ps exited 0 AND its table shows W and
# the census pid (Apple's ps can print nothing and exit 0). Returns undef
# when it is not valid, otherwise a list of [pid, command] for the live
# (non-zombie) members of group $g.
#
# EVERY WAY IT TURNS UNKNOWN WRITES A NOTE, with ps's exit code and
# the last 200 bytes of its stderr, so a result that says `cleanup unknown`
# also says why. It used to say nothing: the census's stderr went to
# /dev/null and every failure path returned undef in silence, so a run that
# stopped on "the members of its group could not be read" left no cause to
# read. Identical notes are written once.
my $CENSUS_ERR_TAIL = 200;
sub census_note {
  my ($what, $err) = @_;
  my $tail = defined $err ? $err : '';
  $tail = substr($tail, -$CENSUS_ERR_TAIL) if length($tail) > $CENSUS_ERR_TAIL;
  $tail =~ s/\s+$//;
  my $n = "census unknown: $what; ps stderr tail: " . (length($tail) ? $tail : '(empty)');
  push @notes, $n unless grep { $_ eq $n } @notes;
  return undef;
}
# PS'S STDERR GOES TO A FILE, NOT A PIPE (sb4 r4). On the base it went to
# /dev/null; a pipe changed what ps met when it wrote there -- it could
# block, fill, or be closed under it (EPIPE) -- and three review rounds each
# found a drain policy that traded one of those for another. A regular file
# does none of them, so ps behaves as it did on the base -- short of a full
# disk, or a file-size limit below what ps writes on its own errors, the two
# limits a regular file has and /dev/null does not -- and the loop below
# reads stdout only, as the base's did. The file is made
# beside --result, one per census, read for its tail once the census is
# decided, and removed. A file that cannot be made leaves ps's stderr at
# /dev/null, as on the base, and the note says so.
my $census_seq = 0;
sub census_errfile {
  $census_seq++;
  my $path = "$opt{result}.census-$$-$census_seq";
  my $fh;
  unless (sysopen($fh, $path, POSIX::O_RDWR() | POSIX::O_CREAT() | POSIX::O_EXCL(), 0600)) {
    return (undef, undef, "(not captured: $!)");
  }
  return ($fh, $path, undef);
}
# THE TAIL IS READ THROUGH THE HANDLE THAT MADE THE FILE, NOT BY ITS NAME,
# and at most $CENSUS_ERR_TAIL bytes of it (r4 review). Opening the name
# again followed whatever the name pointed at by then -- a symlink or a
# FIFO put in its place -- and reading to end of file followed a
# descendant of ps that was still appending. The size is taken once; one
# bounded read ends it. A read that fails says so in the tail, and a name
# that cannot be removed is a note of its own.
sub census_errtail {
  my ($fh, $path) = @_;
  my $tail;
  my $size = (stat($fh))[7];
  if (!defined $size) {
    $tail = "(not read: $!)";
  } else {
    my $from = $size > $CENSUS_ERR_TAIL ? $size - $CENSUS_ERR_TAIL : 0;
    my $got = sysseek($fh, $from, 0) ? sysread($fh, my $buf, $size - $from) : undef;
    $tail = defined $got ? $buf : "(not read: $!)";
  }
  close $fh;
  unless (unlink $path) {
    my $n = "census stderr file not removed: $path: $!";
    push @notes, $n unless grep { $_ eq $n } @notes;
  }
  return $tail;
}
# ONE RETRY WHEN PS MISSES ITS DEADLINE. A ps that has not finished
# its table in time is, on this machine, most often a slow ps and not a
# wedged one (RS-14, LS-8: "ps did not finish before the deadline"), and one
# more attempt settles it. Only a miss is retried; anything else (an exit
# code, a missing row, a failed read) is an answer, not a delay.
#
# NEVER PAST THE LAUNCH'S DEADLINE, BUT PAST A PHASE'S. Every census runs under a caller's deadline no longer than the
# census bound -- quiet_by is called with GRACE (2 s) for the grace poll and
# with D1 and D2 (5 s) by stop_group and the anchor's stop -- so a miss has
# always spent the caller's whole window, and a retry held to that window
# could never run. The retry therefore runs under the census bound and the
# launch's absolute deadline ($deadline, from --limit) only: a phase's
# deadline is local, and the retry may exceed the window of those callers by
# at most $CENSUS_BOUND, never past $deadline. It is not a bound on W's total:
# the stop polls after W's own deadline keep their D1 and D2
# windows, as on the base, and there the launch deadline has passed, so a
# census there is not retried -- it makes one attempt, as without the retry. The
# note carries both attempts' elapsed times; a second miss is unknown, as a
# first miss was before.
sub census_attempt {
  my ($g, $deadline_of_caller) = @_;
  my $t0 = now();
  my ($efh, $epath, $uncaptured) = census_errfile();
  my ($members, $what, $missed) = census_run($g, $deadline_of_caller, $efh);
  my $tail = defined $epath ? census_errtail($efh, $epath) : $uncaptured;
  return ($members, $what, $missed, $tail, now() - $t0);
}
sub census {
  my ($g, $deadline_of_caller) = @_;
  my ($members, $what, $missed, $tail, $took) = census_attempt($g, $deadline_of_caller);
  return $members if defined $members;
  return census_note($what, $tail) unless $missed && $deadline - now() > 0;
  my ($m2, $what2, $missed2, $tail2, $took2) = census_attempt($g, $deadline);
  my $times = sprintf("attempt 1 %.1f s, attempt 2 %.1f s", $took, $took2);
  if (defined $m2) {
    my $n = "census retried: attempt 1 did not finish before its deadline; $times";
    push @notes, $n unless grep { $_ eq $n } @notes;
    return $m2;
  }
  return census_note("$what2; on the second of two attempts ($times)", $tail2);
}
# Answers (members) for a valid census, or (undef, what) naming why not.
sub census_run {
  my ($g, $deadline_of_caller, $efh) = @_;
  pipe(my $r, my $w) or return (undef, "could not start ps (pipe: $!)");
  my $pid = fork;
  unless (defined $pid) {
    my $why = "could not start ps (fork: $!)";
    close $r; close $w;
    return (undef, $why);
  }
  if ($pid == 0) {
    POSIX::setpgid(0, 0);
    close $r;
    open(STDOUT, '>&', $w) or POSIX::_exit(126);
    if ($efh) { open(STDERR, '>&', $efh) or POSIX::_exit(126); }
    else { open(STDERR, '>', '/dev/null'); }
    open(STDIN, '<', '/dev/null');
    {
      no warnings 'exec';
      exec { 'ps' } 'ps', '-A', '-ww', '-o', 'pid=', '-o', 'pgid=', '-o', 'stat=', '-o', 'command=';
    }
    POSIX::_exit(127);
  }
  close $w;
  # BOUNDED BY THE CALLER'S DEADLINE TOO: a census never carries a phase
  # past the absolute deadline it runs under. One cut short is unknown.
  my $until = now() + $CENSUS_BOUND;
  $until = $deadline_of_caller if defined $deadline_of_caller && $deadline_of_caller < $until;
  my $text = '';
  my $rin = '';
  vec($rin, fileno($r), 1) = 1;
  my $eof = 0;
  # $ended says why the loop stopped when stdout had not ended.
  my $ended = '';
  while (!$eof) {
    my $left = $until - now();
    if ($left <= 0) { $ended = 'deadline'; last; }
    my $rout;
    my $n = select($rout = $rin, undef, undef, $left);
    if ($n < 0) { next if $!{EINTR}; $ended = "select failed: $!"; last; }
    if ($n == 0) { $ended = 'deadline'; last; }
    my $got = sysread($r, my $buf, 65536);
    if (!defined $got) { next if $!{EINTR}; $ended = "reading ps's output failed: $!"; last; }
    if ($got == 0) { $eof = 1; last; }
    $text .= $buf;
  }
  close $r;
  # ps runs in a group of its own, so one that does not finish is ended
  # with whatever it started.
  POSIX::setpgid($pid, $pid);
  kill 'KILL', -$pid unless $eof;
  my $st;
  my $reaped = 0;
  # The reap too ends by the caller's deadline (codex r8 A1).
  my $reap_until = now() + 1;
  $reap_until = $until if $until < $reap_until;
  while (now() < $reap_until) {
    my $w2 = waitpid($pid, POSIX::WNOHANG());
    if ($w2 == $pid) { $st = $?; $reaped = 1; last; }
    last if $w2 == -1 && !$!{EINTR};
    my $left = $reap_until - now();
    Time::HiRes::sleep($left < 0.01 ? $left : 0.01) if $left > 0;
  }
  # NEVER AN UNBOUNDED REAP: a ps that will not be reaped is left, and
  # this census is unknown.
  unless ($reaped) {
    kill 'KILL', -$pid; kill 'KILL', $pid;
    # A ps that ran into the deadline is also the one left unreaped (the reap
    # window ends at the same deadline), and "did not finish" is the cause:
    # the note says both.
    return (undef, $eof ? "ps could not be reaped by the deadline"
                 : $ended eq 'deadline' ? "ps did not finish before the deadline, and could not be reaped by it"
                 : "$ended, and ps could not be reaped by the deadline",
            (!$eof && $ended eq 'deadline') ? 1 : 0);
  }
  my $how = ($st & 127) ? "was killed by signal " . ($st & 127) : "exited " . ($st >> 8);
  unless ($eof) {
    return (undef, $ended eq 'deadline' ? "ps did not finish before the deadline (ps $how)"
                                        : "$ended (ps $how)",
            ($ended eq 'deadline') ? 1 : 0);
  }
  return (undef, "ps $how") unless $st == 0;
  my ($has_w, $has_c) = (0, 0);
  my @members;
  for my $line (split /\n/, $text) {
    next unless $line =~ /^\s*(\d+)\s+(\d+)\s+(\S+)\s*(.*)$/;
    my ($p, $pg, $stat, $comm) = ($1, $2, $3, $4);
    $has_w = 1 if $p == $W;
    $has_c = 1 if $p == $census_pid;
    push @members, [$p, $comm] if $pg == $g && $stat !~ /^Z/;
  }
  unless ($has_w && $has_c) {
    my $missing = !$has_w && !$has_c ? "W ($W) or the census pid ($census_pid)"
                : !$has_w ? "W ($W)" : "the census pid ($census_pid)";
    my $lines = () = $text =~ /\n/g;
    return (undef, "ps exited 0 but its table did not show $missing ($lines lines)");
  }
  return (\@members);
}

# Sleep for $secs, all of it: a signal that cuts the sleep short does not
# shorten it, the remainder is slept again (codex r8 A2). But never past
# $until. Answers 1 if the whole $secs passed before $until, 0 if $until
# came first.
sub nap {
  my ($secs, $until) = @_;
  my $end = now() + $secs;
  while (1) {
    my $now = now();
    return 0 if $now >= $until;
    return 1 if $now >= $end;
    my $left = ($end < $until ? $end : $until) - $now;
    Time::HiRes::sleep($left) if $left > 0;
  }
}

# Poll until G is QUIET -- two valid empty censuses at least 100 ms apart
# -- or until $until. Answers ('empty'), ('occupied', members) or
# ('unknown'). 'occupied' carries the last census's members, which may be
# none when the deadline came before an empty census could be confirmed.
# A sleep never runs past $until; a census that misses $until may retry once
# past it, bounded by the launch's deadline (see census).
sub quiet_by {
  my ($g, $until) = @_;
  my $last = [];
  while (1) {
    return ('occupied', $last) if now() >= $until;
    my $c = census($g, $until);
    return ('unknown') unless defined $c;
    if (!@$c) {
      return ('occupied', $c) unless nap($QUIET_GAP, $until);
      my $c2 = census($g, $until);
      return ('unknown') unless defined $c2;
      return ('empty') unless @$c2;
      $last = $c2;
    } else {
      $last = $c;
    }
    nap($QUIET_GAP, $until);
  }
}

# Stop G. CLEAN ONLY AFTER KILL AND TWO EMPTY CENSUSES, on every path
# (launcher design v4, the stop as a request): TERM first when G is known
# to hold members, poll up to D1, then KILL whatever the poll said, and
# poll up to D2. Answers ('clean'), ('survivor', members) or ('unknown').
sub stop_group {
  my ($g, $occupied) = @_;
  if ($occupied) {
    kill 'TERM', -$g;
    quiet_by($g, now() + $D1);
  }
  kill 'KILL', -$g;
  my @q = quiet_by($g, now() + $D2);
  return ('clean') if $q[0] eq 'empty';
  return ('unknown') if $q[0] eq 'unknown';
  return ('survivor', $q[1]);
}

# ---- the result file ---------------------------------------------------------
#
# A temporary file beside the result, a checked write and close, then a
# rename on the same filesystem. Any failure leaves no result, which the
# reader must treat as unknown.
sub write_result {
  my ($status, $cleanup, $members) = @_;
  my $tmp = "$opt{result}.tmp.$W";
  my $body = "id $opt{id}\nstatus $status\ncleanup $cleanup\n";
  for my $m (@{ $members || [] }) {
    my ($p, $c) = @$m;
    $c =~ s/[\r\n]/ /g;
    $body .= "member $p $c\n";
  }
  for my $n (@notes) {
    my $t = $n;
    $t =~ s/[\r\n]/ /g;
    $body .= "note $t\n";
  }
  $body .= "end\n";
  my $fh;
  open($fh, '>', $tmp) or return 0;
  my $ok = print {$fh} $body;
  $ok = close($fh) && $ok;
  unless ($ok) { unlink $tmp; return 0; }
  unless (rename($tmp, $opt{result})) { unlink $tmp; return 0; }
  return 1;
}

sub finish {
  my ($status, $cleanup, $members) = @_;
  # A STOP ASKED FOR AT ANY POINT IS THE STATUS, even one that arrived
  # during the grace or the cleanup (launcher design L7).
  $status = 128 + $stop_signal if $stop_signal;
  write_result($status, $cleanup, $members);
  POSIX::_exit($status);
}

# ---- startup: a handshake, so G exists before anything runs in it ------------
#
# W blocks INT, TERM and HUP before it forks; A sets its own group, puts
# back the mask it inherited, installs its handlers and then waits for one
# byte from W. W sets A's group too (the double call), checks it, and only
# then unblocks and releases A. A stop that arrives before the release
# means A is never released and no command starts.
if (defined $fault_refused) {
  push @notes, $fault_refused;
  finish(126, 'clean', []);
}
my $blocked = POSIX::SigSet->new(map { $signo{$_} } qw(INT TERM HUP));
my $orig_mask = POSIX::SigSet->new;
POSIX::sigprocmask(POSIX::SIG_BLOCK(), $blocked, $orig_mask) or push @notes, "watcher could not block its stop signals: $!";

pipe(my $go_r, my $go_w) or finish(126, 'unknown', []);
pipe(my $note_r, my $note_w) or finish(126, 'unknown', []);
# The anchor-late injection's own pipe: W writes one byte on it after its
# first read of the group, and the anchor waits for that byte (or the pipe's
# end) before its setpgid(0,0). It exists only under that mode.
my ($late_r, $late_w);
if ($fault_anchor_late) {
  pipe($late_r, $late_w) or finish(126, 'unknown', []);
}
my $A = fork;
unless (defined $A) {
  push @notes, "fork of the anchor failed: $!";
  finish(126, 'clean', []);
}

if ($A == 0) {
  # ---- the anchor ----------------------------------------------------------
  close $go_w;
  close $note_r;
  # A SETS ITS OWN GROUP, AND SAYS SO WHEN IT CANNOT. W makes the same call
  # from its side; the two race, and W's errno alone cannot say which side
  # failed. On this path nothing reaches the note pipe before A's call has
  # been made, so the pipe turning readable means either that or that A
  # died first; W reads the group again rather than trust the readiness.
  if ($fault_anchor_late) {
    close $late_w;
    my $b;
    my $g;
    do { $g = sysread($late_r, $b, 1) } while (!defined $g && $!{EINTR});
    close $late_r;
  }
  my ($own_set, $own_errno) = (1, undef);
  if ($fault_anchor_skips) {
    ($own_set, $own_errno) = (0, $fault_setpgid);
  } else {
    $own_set = POSIX::setpgid(0, 0);
    $own_errno = $own_set ? undef : $! + 0;
  }
  my $stopped = 0;
  my $term = $signo{TERM};
  my @failed;
  for my $no (@catchable) {
    my $code = ($no == $term) ? sub { $stopped = 1; } : sub { };
    push @failed, $no unless install($no, $code);
  }
  POSIX::sigprocmask(POSIX::SIG_SETMASK(), $orig_mask);
  unless ($own_set) {
    syswrite($note_w, "anchor setpgid(0,0) failed: " . errno_text($own_errno) . "\n");
  }
  if (@failed) {
    syswrite($note_w, "anchor could not handle signals @failed\n");
  }
  close $note_w;
  my $byte;
  my $got;
  do { $got = sysread($go_r, $byte, 1) } while (!defined $got && $!{EINTR});
  close $go_r;
  POSIX::_exit(0) unless defined $got && $got == 1;
  POSIX::_exit(128 + $term) if $stopped;
  my $C = fork;
  POSIX::_exit(126) unless defined $C;
  if ($C == 0) {
    # ---- the command -------------------------------------------------------
    for my $no (@catchable) {
      POSIX::sigaction($no, POSIX::SigAction->new('DEFAULT', POSIX::SigSet->new, 0));
    }
    POSIX::sigprocmask(POSIX::SIG_SETMASK(), POSIX::SigSet->new);
    open(STDIN, '<', '/dev/null') or POSIX::_exit(126);
    open(STDOUT, '>', $opt{out}) or POSIX::_exit(126);
    open(STDERR, '>&', \*STDOUT) or POSIX::_exit(126);
    {
      no warnings 'exec';
      exec { $cmd[0] } @cmd;
    }
    POSIX::_exit($!{ENOENT} ? 127 : 126);
  }
  my $st;
  while (1) {
    my $w = waitpid($C, 0);
    if ($w == $C) { $st = $?; last; }
    next if $w == -1 && $!{EINTR};
    POSIX::_exit(126);
  }
  POSIX::_exit(($st & 127) ? 128 + ($st & 127) : ($st >> 8));
}

# ---- the watcher -------------------------------------------------------------
close $go_r;
close $note_w;
close $late_r if $fault_anchor_late;
$SIG{PIPE} = 'IGNORE';
my $G = $A;
my ($set, $set_errno);
if (defined $fault_setpgid) {
  $set = 0;
  $set_errno = $fault_setpgid;
} else {
  $set = POSIX::setpgid($A, $A);
  $set_errno = $set ? undef : $! + 0;
}
# AN ERROR IS NOT A STATE. Whatever setpgid answered, the state is A's group
# read back: perl's getpgrp(PID) is getpgid(2), and answers -1 with errno
# set when it fails (ESRCH for a pid that is gone, a zombie included). After
# a failed setpgid, a group that is not A's may have been read before A's
# own setpgid(0,0) ran, so it is read once more when the note pipe turns
# readable, within D1: A writes or closes it only after that call, or by
# dying first, and the second read is what decides either way.
sub anchor_group {
  my $g = getpgrp($A);
  return (defined $g && $g >= 0) ? ($g, undef) : (undef, $! + 0);
}
sub anchor_setup_by {
  my ($until) = @_;
  my $rin = '';
  vec($rin, fileno($note_r), 1) = 1;
  while (1) {
    my $left = $until - now();
    return 0 if $left <= 0;
    my $rout;
    my $n = select($rout = $rin, undef, undef, $left);
    return 1 if $n > 0;
    next if $n < 0 && $!{EINTR};
    return 0;
  }
}
my ($pg, $pg_errno) = anchor_group();
if ($fault_anchor_late) {
  syswrite($late_w, 'r');
  close $late_w;
}
if (!$set && defined $pg && $pg != $A) {
  ($pg, $pg_errno) = anchor_group() if anchor_setup_by(now() + $D1);
}
my $W_group = getpgrp(0);
# set: A leads its own group, whatever setpgid answered. not-set: A is
# alive in another group. gone: the read failed ESRCH. not-read: it failed
# with any other errno.
my $group_state = defined $pg ? ($pg == $A ? 'set' : 'not-set')
                : $pg_errno == POSIX::ESRCH() ? 'gone' : 'not-read';
push @notes, "watcher setpgid " . errno_text($set_errno) . "; the anchor's group was already $A"
  if !$set && $group_state eq 'set';
my $anchor_gone = $group_state ne 'set';
POSIX::sigprocmask(POSIX::SIG_SETMASK(), $orig_mask);

sub read_anchor_notes {
  my $rin = '';
  vec($rin, fileno($note_r), 1) = 1;
  my $rout;
  while (select($rout = $rin, undef, undef, 0) > 0) {
    my $got = sysread($note_r, my $buf, 4096);
    last unless $got;
    push @notes, grep { length } split /\n/, $buf;
  }
}

my $reap_errno;
sub reap_anchor_bounded {
  my ($until) = @_;
  $reap_errno = undef;
  while (now() < $until) {
    my $w = waitpid($A, POSIX::WNOHANG());
    return $? if $w == $A;
    if ($w == -1 && !$!{EINTR}) { $reap_errno = $! + 0; return undef; }
    Time::HiRes::sleep(0.02);
  }
  return undef;
}

if ($anchor_gone) {
  # NEVER: AN UNAVAILABLE PGID IS NEVER USED AS A GROUP TARGET. A is gone,
  # or alive outside a group of its own, or its group could not be read: no
  # retry, no release byte (A reads the closed pipe and exits 0), a bounded
  # reap, and a note that says which.
  close $go_w;
  my $st = reap_anchor_bounded(now() + $D1);
  read_anchor_notes();
  # No command ever started. G is never signalled here (its pgid is not
  # available), only read: clean needs two empty censuses as ever.
  my @q = quiet_by($A, now() + $D2);
  my $cleanup = $q[0] eq 'empty' ? 'clean' : $q[0] eq 'unknown' ? 'unknown' : 'survivor';
  my $members = $q[0] eq 'occupied' ? $q[1] : [];
  # THE NOTE NAMES THE STATE READ, NOT THE ERRNO: ANCHOR DIED only when the
  # group read failed ESRCH; ANCHOR GROUP NOT SET when A was alive in another
  # group; ANCHOR GROUP NOT READ for any other failure of the read. The same
  # line carries setpgid's errno, the group read (or its errno), A's pid,
  # W's own group, and how the reap ended; a reap that ended in a waitpid
  # error says so, not "timeout".
  my $signal = (defined $st && ($st & 127)) ? ($st & 127) : 0;
  my $head = $group_state eq 'not-set' ? 'ANCHOR GROUP NOT SET'
           : $group_state eq 'not-read' ? 'ANCHOR GROUP NOT READ'
           : $signal ? "ANCHOR DIED by signal $signal" : 'ANCHOR DIED';
  my $facts = " (setpgid " . (defined $set_errno ? errno_text($set_errno) : 'ok')
    . ", getpgid " . (defined $pg ? $pg : "failed " . errno_text($pg_errno))
    . ", anchor $A, watcher group $W_group"
    . ", reap " . (defined $st ? ($signal ? "signal $signal" : "exit " . ($st >> 8))
                   : defined $reap_errno ? "failed " . errno_text($reap_errno) : 'timeout')
    . ")";
  push @notes, "$head before the release" . $facts;
  finish($signal ? 128 + $signal : 126, $cleanup, $members);
}

my $go_open = 1;
sub release_closed { if ($go_open) { close $go_w; $go_open = 0; } }

sub stopped_launch {
  my ($status) = @_;
  release_closed();
  my @s = stop_group($G, 1);
  reap_anchor_bounded(now() + $D2);
  read_anchor_notes();
  finish($status, $s[0], $s[1] || []);
}

# THE CHECK AND THE RELEASE ARE ONE STEP: the stop signals are blocked
# across them, so a stop either comes before (no release, no command) or
# after (the wait below handles it, and KILL ends a command that started).
POSIX::sigprocmask(POSIX::SIG_BLOCK(), $blocked);
if ($stop_signal || now() >= $deadline) {
  POSIX::sigprocmask(POSIX::SIG_SETMASK(), $orig_mask);
  stopped_launch($stop_signal ? 128 + $stop_signal : 142);
}
syswrite($go_w, 'g');
release_closed();
POSIX::sigprocmask(POSIX::SIG_SETMASK(), $orig_mask);

# ---- the wait: the command's end, a stop request, or the time limit ---------
my $ast;
while (1) {
  my $w = waitpid($A, POSIX::WNOHANG());
  if ($w == $A) { $ast = $?; last; }
  stopped_launch(128 + $stop_signal) if $stop_signal;
  stopped_launch(142) if now() >= $deadline;
  Time::HiRes::sleep(0.05);
}
read_anchor_notes();

my $status;
if ($ast & 127) {
  push @notes, "ANCHOR DIED by signal " . ($ast & 127);
  $status = 128 + ($ast & 127);
} else {
  $status = $ast >> 8;
}

# ---- the grace, then G is stopped on every path -------------------------------
#
# A member that ends by itself within the grace is not reported; one still
# live after it is LEFT IN GROUP. The grace is a bound, not a sleep: an
# empty group answers at once. Then G is stopped whatever the grace found
# -- KILL and two empty censuses, TERM first if members were seen -- and an
# unknown grace still stops G, and stays unknown.
my @q = quiet_by($G, now() + $GRACE);
my $grace_unknown = $q[0] eq 'unknown';
my $left = ($q[0] eq 'occupied' && @{ $q[1] }) ? $q[1] : [];
my @s = stop_group($G, $grace_unknown || @$left);
finish($status, 'unknown', []) if $grace_unknown || $s[0] eq 'unknown';
finish($status, 'survivor', $s[1]) if $s[0] eq 'survivor';
finish($status, 'left', $left) if @$left;
finish($status, 'clean', []);
