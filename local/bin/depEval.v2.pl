#!/usr/bin/perl -wl

use strict;

use Getopt::Std;

our(%opts);

getopt('giml',\%opts);

unless( defined $opts{g} ) {
  print STDERR "Usage: depEval.pl [-dvc] [-m minlength] [-l maxLength] -g gold [-i wordsToIgnore] < depParse";
  print STDERR "\t-i: path to file containing words to ignore (i.e. \"umm\")";
  print STDERR "\t-d: print out sentence by sentence scores (like EVALB)";
  print STDERR "\t-v: verbose output (basically print ID of skipped sentences)";
  print STDERR "\t-c: concise output (evaltype eval)";
  exit;
}

my $ignoreFile = defined $opts{i} ? $opts{i} : 0;
my $minLength = defined $opts{m} ? $opts{m} : 0;
my $maxLength = defined $opts{l} ? $opts{l} : 0;
my $concise = defined $opts{c};

my %ignore = ();
if( $ignoreFile ) {
  open IGNORE, "<", $ignoreFile or die "Can't open ignoreFile $ignoreFile for reading: $!\n";

    while(<IGNORE>) {
      chomp;
      s/: / /;
      next unless /\d$/;
      my ($id, $ignoreTerms) = split ' ',$_,2;

      my @ignoreTerms = split / +/, $ignoreTerms;
      foreach my $ignoreTerm ( @ignoreTerms ) {
        $ignore{ $id }{ $ignoreTerm } = 1;
      }
    }

  close IGNORE;
}

my $goldFile = $opts{g};

my %gold = ();
my %undirectedGold = ();
my %nedGold = ();
open GOLD, "<", $goldFile or die "Can't open goldFile $goldFile for reading: $!\n";

  while(<GOLD>) {
    chomp;
    s/: / /;
    my ($id, $parents) = split ' ',$_,2;
    next if $parents =~ /,\s+,/;
    $id =~s/:$//;
    $parents =~ s/ *\] *$//;
    $parents =~ s/^\[ *//;
    my @parents = split /, */, $parents;

    next unless scalar @parents >= $minLength &&
      ( scalar @parents <= $maxLength || $maxLength == 0 );

    $gold{ $id } = \@parents;
    my @undirectedDependencies = ();
    for( my $i = 0; $i <= $#parents; $i++ ) {
      my $undir = $i > $parents[$i] ? $i.".".$parents[$i] : $parents[$i].".".$i;
      $undirectedGold{$id}{$undir}= 1;

      my $nedStandard = $parents[$i]."-->".$i;
      my $nedChild = $i."-->".$parents[$i];
      my $nedGP = $parents[$i] > $#parents ? 0 : $parents[$parents[$i]]."-->".$i;


      $nedGold{$id}{$nedStandard} = 1;
      $nedGold{$id}{$nedChild} = 1;
      $nedGold{$id}{$nedGP} = 1 unless $nedGP eq "0";
    }
    #$undirectedGold{ $id } = \@undirectedDependencies;
  }

close GOLD;

#print STDERR join "\n", @{ [keys %gold ] }[0..20];



if( $opts{d} ) {
  print join "	",
    "label",
    "dir.match",
    "undir.match",
    "numWords",
    "dir.acc",
    "undir.acc",
    "ned.acc";
}

my $trueParents=0;
my $totalParents=0;
my $skippedSentences=0;
my $usedSentences=0;
my $skippedMaxLength=0;
my $undirectedHits=0;
my $nedHits=0;
while(<>) {
  chomp;

  s/: / /;

  my ($iterAndId, $parents) = split ' ',$_,2;
  my ($iter,$type,$id) = split ':', $iterAndId,3;

  die "Wrong parse type; $type" if $type ne 'dependency';

  $parents =~ s/ ?\] *$//;
  $parents =~ s/^ *\[ ?//;
  my @parents = split /, */, $parents;

  next unless scalar @parents >= $minLength &&
    ( scalar @parents <= $maxLength || $maxLength == 0 );


  unless( defined $gold{$id} ) {
    $skippedSentences++;
    my $length = scalar @parents;
    $skippedMaxLength = $length if $length > $skippedMaxLength;
    if( defined $opts{v} ) {
      print STDERR "skipping $id";
    }
    next;
  }

  my @thisGold = @{$gold{$id}};

  die( $id .": " .(scalar @parents)." != ".( scalar @thisGold )) if $#parents ne $#thisGold;
  if( $#parents != $#thisGold ) {
    print STDERR "WARNING: skipping $id because gold has ".
      ( scalar @thisGold )." words and test has ".(scalar @parents )." words";
    next;
  } else {
    $usedSentences++;
  }

  my $thisSentTrueParents = 0;
  my $thisSentTrueUndirectedParents = 0;
  my $thisSentTrueNedParents = 0;
  for( my $i = 0; $i <= $#parents; $i++ ) {
    unless( defined $ignore{ $id }{ $i } or defined $ignore{ $id }{ $parents[$i] }) {
      $totalParents++;

      $thisSentTrueParents++ if $parents[ $i ] == $thisGold[ $i ];

      my $undir = $i > $parents[$i] ? $i.".".$parents[$i] : $parents[$i].".".$i;
      $thisSentTrueUndirectedParents++ if defined $undirectedGold{ $id }{ $undir };

      my $nedDep = $parents[$i]."-->".$i;
      $thisSentTrueNedParents ++ if defined $nedGold{ $id }{ $nedDep };
    }
  }

  $trueParents += $thisSentTrueParents;
  $undirectedHits += $thisSentTrueUndirectedParents;
  $nedHits += $thisSentTrueNedParents;

  if( $opts{d} ) {
    #print join "	", "label", "matches", "true", "acc";
    print join "	",
      $id,
      $thisSentTrueParents,
      $thisSentTrueUndirectedParents,
      $thisSentTrueNedParents,
      scalar @parents,
      sprintf( "%2.1f", 100*($thisSentTrueParents / ( scalar @parents)) ),
      sprintf( "%2.1f", 100*($thisSentTrueUndirectedParents / ( scalar @parents)) ),
      sprintf( "%2.1f", 100*($thisSentTrueNedParents / ( scalar @parents)) );
  }

}

if( $concise ) {
  print "Directed ".( $trueParents / $totalParents );
  print "Undirected ".( $undirectedHits / $totalParents );
  print "Ned ".( $nedHits / $totalParents );
} else {
  print "Skipped $skippedSentences sentences.";
  print "Used $usedSentences sentences.";
  print "Longest skipped sentence was $skippedMaxLength words long";
  print "Directed accuracy: $trueParents/$totalParents =	".( $trueParents / $totalParents );
  print "Undirected accuracy: $undirectedHits/$totalParents =	".( $undirectedHits / $totalParents );
  print "Ned accuracy: $nedHits/$totalParents =	".( $nedHits / $totalParents );
}

