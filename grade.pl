#!/usr/bin/env perl

# Copyright (c) 2015, Nathaniel Wesley Filardo
# All rights reserved.
# See COPYING for details.

# A grading script!  Compatible in spirit with the CMU Operating System
# grading infrastructure, if not in actual thought or deed quite yet.
#
# See README.rst for syntax and usage.

use strict;
use warnings;

sub max($$) { return @_[$_[0] < $_[1]]; }
sub min($$) { return @_[$_[0] > $_[1]]; }

open (DEFINES,($ARGV[0] or die "No defines file?"))
  or die "Could not open DEFINES file $ARGV[0]: $!";

my $ptssum = 0;
my $ptsmax = 0;
# Section state {{{
my $sectionoffset;
my $secname;
my $secty;
my $secmax;
my $secextra;
my $pointsadj;
my $pointsset;
my $dings;
my $dingsum;
my $dingtext;
my $commenttext;

sub initSection() {
  $sectionoffset = undef;
  $secname = undef;
  $secty = undef;
  $secmax = undef;
  $secextra = undef;
  $pointsadj = undef;
  $pointsset = undef;
  $dings = 0;
  $dingsum = 0;
  $dingtext = [ ];
  $commenttext = [ ];
}

initSection();
# }}}
sub untilDefinesDot($) { # {{{
  my ($cb) = @_;
  while(my $line = <DEFINES>) {
    chomp $line;
    last if $line =~ /^\.$/;
    &$cb($line) ; 
  }
} # }}}
sub findDefinesSection($) { # {{{
  my ($newsecname) = @_;
  seek DEFINES, 0, 0;
  my $dline;
  while($dline = <DEFINES>) {
    if ($dline =~ /^\@$newsecname\s+(.*)$/) {
      $sectionoffset = tell(DEFINES);
      my $secargs = $1;
      if ($secargs =~ /^(\S+)\s+(\S+)\s+(\S+)\s+(.*)$/) {
        ($secty, $secmax, $secextra, $secname) = ($1,$2,$3,$4);
      } else { die "Malformed section line"; }
      return;
    }
    if ($dline =~ /^:/) {
      untilDefinesDot(sub ($) { return; });
    }
  }
} # }}}
sub doDing($) { # {{{ 
  my ($ding) = @_;

  $dings += 1;

  seek DEFINES, $sectionoffset, 0;
  while(my $line = <DEFINES>) {
    last if ($line =~ /^@/);
    if ($line =~ /^:$ding\s+(.*)$/) {
      $dingsum += $1;
      push @$dingtext, "($1)";
      untilDefinesDot(sub ($) { push @$dingtext, @_; });
      push @$dingtext, "";
      return;
    }
  }

  die "$ding not defined?\n";
} # }}}
sub finishSection() { # {{{

  # Compute score
  my $score;

  if ($secty == 0) {
    $score = $secmax + ($dingsum or 0) + ($pointsadj or 0);
  } else { die "Unknown section type $secty for $secname\n"; }

  if (defined $pointsset) {
    $score = $pointsset;
  }

  die if not defined $score;

  $score = max(0, min($secmax, $score));
  
  print "$secname: [$score/$secmax]\n\n";
  print "", (join "\n", map { "  $_" } @$dingtext), "\n";

  if (scalar @$commenttext != 0) {
    print " Grader comments: \n\n";
    print "", (join "\n", map { "  $_" } @$commenttext), "\n";
  }

  print "\n\n";

  $ptssum += $score;
  $ptsmax += $secmax;

  initSection();
} # }}}
sub finishOverall() { # {{{

  print "Overall: [$ptssum/$ptsmax] ",
    (sprintf ("(%.1f%%)", (100*$ptssum/$ptsmax))),
    "\n";

} # }}}
# Main dispatch {{{
while(my $line = <STDIN>) {
  $line =~ s/#.*$//;	# trim line comments
  next if ($line =~ /^\s*$/) ;
  chomp $line;

  # @section directive?
  if ($line =~ /^\@(\S+)\s*$/) {
    my $newsecname = $1;
    finishSection() if defined $secname;

    # Find this section in the defines file
    my $dline = findDefinesSection($newsecname);
    
    die "Unknown section $newsecname" if not defined $secname;
  }

  # :define directive?
  elsif ($line =~ /^:(\S+)\s*$/) {
    die "No active section" if not defined $secname;
    doDing($1);
  }

  # !points-like directive?
  elsif ($line =~ /^!points(adj|set)\s+(([+-])(\.|\d)+)$/) {
    my ($mode, $arg) = ($1, $2);
    if ($mode eq "adj") {
      die "Both adjusting and setting points" if defined $pointsset;
      $pointsadj = ($pointsadj or 0) + scalar $arg;
    } elsif ($mode eq "set") {
      die "Both adjusting and setting points" if defined $pointsadj;
      die "Setting points twice" if defined $pointsset;
      $pointsset = $arg;
    } else { die "Unknown points-like directive: $line\n"; };
  }

  # $BEGIN_COMMENTS directive?
  elsif ($line =~ /^\$BEGIN_COMMENTS/) {
    my $seen_text = 0;
    while (my $cline = <STDIN>) {
      chomp $cline;
      last if $cline =~ /^\$END_COMMENTS/;
      next if (not $seen_text and $cline =~ /^$/);
      $seen_text = 1;
      push @$commenttext, $cline;
    }
  }

  else { die "Unknown directive in: $line\n"; }
}

finishSection() if defined $secname;
finishOverall();

# }}}

# vim:foldmethod=marker
