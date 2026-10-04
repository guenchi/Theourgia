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

# A SCRIPTED DAEMON for the streaming client's rows (change-stream-client.sc).
# It listens on SOCKET, accepts one connection, reads the request line,
# answers an enveloped acceptance, and then plays SCRIPT, one step a line:
#   w <text>          writes <text> and a newline
#   s <ms>            sleeps
#   p <n>             writes the start of a frame and n more bytes, no newline
#   f <count> <size>  writes count whole frames of about size bytes each,
#                     revisions 1, 2, ...
#   c                 closes
# The connection is closed at the end of the script in any case.

use strict;
use IO::Socket::UNIX;
use Time::HiRes qw(usleep);

my ($path, $script) = @ARGV;
unlink $path;
my $srv = IO::Socket::UNIX->new(Type => SOCK_STREAM(), Local => $path, Listen => 1)
  or die "listen: $!";
my $c = $srv->accept() or die "accept: $!";

sub put {
  my ($t) = @_;
  my $o = 0;
  while ($o < length $t) {
    my $n = syswrite($c, $t, length($t) - $o, $o);
    die "write: $!" unless defined $n;
    $o += $n;
  }
}

my $req = '';
while ($req !~ /\n/) {
  my $b;
  my $n = sysread($c, $b, 65536);
  last unless $n;
  $req .= $b;
}

put('(answer (stdout "(ok (subscribed 0) (daemon \"1-2\") (current 0))\n") (stderr "") (exit 0) (origin core))' . "\n");

open(my $f, '<', $script) or die "script: $!";
my $rev = 0;
while (my $l = <$f>) {
  chomp $l;
  my ($op, $rest) = split / /, $l, 2;
  if ($op eq 'w') {
    put($rest . "\n");
  } elsif ($op eq 's') {
    usleep($rest * 1000);
  } elsif ($op eq 'p') {
    put('(changes (rev ' . ($rev + 1) . ') ' . ('x' x $rest));
  } elsif ($op eq 'f') {
    my ($count, $size) = split / /, $rest;
    for (1 .. $count) {
      $rev++;
      my $h = '(changes (rev ' . $rev . ') (daemon "1-2") (from-cut ()) (cut ()) (items (added "';
      my $pad = $size - length($h) - 5;
      $pad = 1 if $pad < 1;
      put($h . ('p' x $pad) . '")))' . "\n");
    }
  } elsif ($op eq 'c') {
    last;
  }
}
close $c;
unlink $path;
