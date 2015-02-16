#!/usr/bin/env perl

# Copyright (c) 2015, Nathaniel Wesley Filardo
# All rights reserved.
# See COPYING for details.

use strict;
use warnings;

my $sawsec = 0;
my $section = undef;
my $skipsec = 0;
my $skipcom = 0;

sub comments() {
  print "\n\$BEGIN_COMMENTS\n\n\$END_COMMENTS\n\n";
}

while(my $line = <STDIN>) {
  chomp $line;

  # @section directive?
  if ($line =~ /^@(\S+)\s+(\S+)\s+/) {
    $sawsec = 1;
    comments() if defined $section and not $skipsec;
    if ($2 =~ /^!/) {
      $skipsec = 1;
      $skipcom = 1;
      $section = undef;
      next;
    } else {
      $skipsec = 0;
      $skipcom = 0;
      $section = $1;
      print "\@$section\n";
    }
  }

  # :define directive?
  elsif ($line =~ /^(:\S+)\s+/) {
    die "Directive not within section" if not defined $section and not $skipsec;
    print "#$1\n" if not $skipsec;
    while (my $cline = <STDIN>) { chomp $cline; last if $cline eq "."; }
  }

  # "#..." and not "#!..." get passed to template
  elsif ($line =~ /^\s*#/) {
    if ($line =~ /^\s*#!\\n$/) { print "\n" if not $skipcom; }
    if ($line =~ /^\s*#!noskip$/) { $skipcom = 0; }
    if ($line =~ /^\s*#!reskip$/) { $skipcom = $skipsec; }
    if ($line !~ /^\s*#!/) {
      print "$line\n" if not $skipcom;
    }
  }

  elsif ($line =~ /^\s*$/) { ; }

  else { die "Unknown line in definition file ($line)"; }
}

die "No sections encountered" if not $sawsec;
comments() if not $skipsec;
