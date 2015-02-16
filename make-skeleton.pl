#!/usr/bin/env perl

# Copyright (c) 2015, Nathaniel Wesley Filardo
# All rights reserved.
# See COPYING for details.

use strict;
use warnings;

my $section = undef;
my $skipsec = 0;

sub comments() {
  print "\n\$BEGIN_COMMENTS\n\n\$END_COMMENTS\n\n";
}

while(my $line = <STDIN>) {
  chomp $line;

  # @section directive?
  if ($line =~ /^@(\S+)\s+(\S+)\s+/) {
    comments() if defined $section and not $skipsec;
    $section = $1;
    $skipsec = 0;
    if ($2 =~ /^!/) { $skipsec = 1; next; }
    print "\@$section\n";
  }

  # :define directive?
  elsif ($line =~ /^(:\S+)\s+/) {
    die "Directive not within section" if not defined $section;
    print "#$1\n" if not $skipsec;
    while (my $cline = <STDIN>) { chomp $cline; last if $cline eq "."; }
  }

  # "#..." and not "#!..." get passed to template
  elsif ($line =~ /^\s*#/) {
    if ($line !~ /^\s*#!/) {
      print "$line\n" if not $skipsec;
    }
  }

  elsif ($line =~ /^\s*$/) { ; }

  else { die "Unknown line in definition file ($line)"; }
}

die "No sections encountered" if not defined $section;
comments() if not $skipsec;
