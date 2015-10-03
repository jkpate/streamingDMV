#!/usr/bin/perl -wl

use strict;

use File::Basename qw/ dirname basename /;
use Getopt::Long;
use File::Temp qw/ tempdir /;

my $binDir = dirname( __FILE__ );




unless( scalar @ARGV > 0 ) {
  print STDERR "Usage: iterationsPerSentence.pl outputfiles";
  exit;
}

my @fields = (
  "trainStrings",
  "testStrings",
  "parserType",
  "alpha",
  "rootAlpha",
  "stopAlpha",
  "chooseAlpha",
  "backoffAlpha",
  "notBackoffAlpha",
  "squarelyNormalized",
  "uposCount",
  "batchVB",
  "incIters",
  "incConvergence",
  "reservoirSize",
  "particleFilter",
  "noResampling",
  "numParticles",
  "miniBatchSize",
  "initialMiniBatchSize",
  "convergeInitialMiniBatch",
  # "possiblyResampleEvery",
  "randomSeed"
);


print join ",",
  @fields,
  "trainStringsCount",
  "testStringsCount",
  "trainWordsCount",
  "testWordsCount",
  "vocabSize",
  "totalIters",
  "avIters";

foreach my $fPath (@ARGV) {
  print STDERR $fPath;

  my %fields = ();
  my $trainingStrings = 0;
  my $testingStrings = 0;
  my $trainingWords = 0;
  my $testingWords = 0;
  my $vocabSize = 0;

  open RESULT, "<", $fPath or die "Error opening $fPath for reading: $!";
    while( <RESULT> ) {
      chomp;
      if( /^(\d+) training strings$/ ) {
        $trainingStrings = $1;
      } elsif( /^(\d+) testing strings$/ ) {
        $testingStrings = $1;
      } elsif( /^(\d+) training words$/ ) {
        $trainingWords = $1;
      } elsif( /^(\d+) testing words$/ ) {
        $testingWords = $1;
      } elsif( /^(\d+) unique words/ ) {
        $vocabSize = $1;
      } elsif( /^([^:]+):\s+([^\s]+)$/ ) {
        $fields{$1} = $2;
      } elsif( /^Job started/ || /^[Jj]ava/ || /^OpenJDK/) {
      } else {
        die "didn't find end of header\n$_\n" unless /^Using/;
        last;
      }
    }
  close RESULT;

  $fields{backoffAlpha} = "NA" if not defined $fields{backoffAlpha};
  $fields{notBackoffAlpha} = "NA" if not defined $fields{notBackoffAlpha};
  $fields{uposCount} = "1" if not defined $fields{uposCount};
  $fields{particleFilter} = "false" if not defined $fields{particleFilter};
  $fields{numParticles} = 0 if not defined $fields{numParticles};
  $fields{squarelyNormalized} = 'false' if not defined $fields{squarelyNormalized};
  $fields{noResampling} = 'false' if not defined $fields{noResampling};
  $fields{reservoirSize} = 0 if not defined $fields{reservoirSize};
  $fields{rootAlpha} = $fields{alpha} if not defined $fields{rootAlpha};
  $fields{stopAlpha} = $fields{alpha} if not defined $fields{stopAlpha};
  $fields{chooseAlpha} = $fields{alpha} if not defined $fields{chooseAlpha};
  $fields{convergeInitialMiniBatch} = "false" if not defined $fields{convergeInitialMiniBatch};

  my $tail = `tail $fPath -n 30`;

  $tail =~ /([0-9.]+) total iters/;
  my $totalIters = $1;

  $tail =~ /it(\d+):logProb:/;
  my $sentencesSeen = $1;

  print join ",",
    ( map { $fields{$_ } } @fields ),
    $trainingStrings,
    $testingStrings,
    $trainingWords,
    $testingWords,
    $vocabSize,
    $totalIters,
    ($totalIters/$sentencesSeen);

    # print $totalIters;
    # print $sentencesSeen;



}




