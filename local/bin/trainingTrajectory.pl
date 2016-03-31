#!/usr/bin/perl -wl

use strict;

use File::Basename qw/ dirname basename /;
use Getopt::Long;
use File::Temp qw/ tempdir /;

my $binDir = dirname( __FILE__ );

my $gold;
my $minLength = 4;
my @maxLengths = ( 10, 15, 20, 40 );
my $noCache = 0;
my $cacheDir = "cachedLines";

GetOptions(
  "gold=s" => \$gold,
  "minLength=i" => \$minLength,
  "noCache" => \$noCache,
  "cacheDir=s" => \$cacheDir,
);


unless( defined $gold and $gold ne "" ) {
  print STDERR "Usage: trainingTrajectory.pl [-minLength minLength] [-maxLength maxLength] [-cacheDir cacheDir] -gold gold outputfiles";
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
  "scaleInitMiniBatchCounts",
  "harmonicMiniBatchInit",
  "harmonicCorpusInit",
  "uposCount",
  "batchVB",
  "stochasticVB",
  "collapsedVB",
  "tau",
  "kappa",
  "incIters",
  "incConvergence",
  "reservoirSize",
  "particleFilter",
  "noResampling",
  "numParticles",
  "miniBatchSize",
  "initialMiniBatchSize",
  "largeMiniBatchEvery",
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
  "sentencesSeen",
  "logProb",
  ( map { "directed.$_" } @maxLengths ),
  ( map { "undirected.$_" } @maxLengths ),
  ( map { "ned.$_" } @maxLengths );

foreach my $fPath (@ARGV) {
  print STDERR $fPath;

  # my $cacheName = "cachedLines/".(basename( $fPath ))."-".(basename($gold)).".lines";
  my $cacheName = "$cacheDir/".(basename( $fPath ))."-".(basename($gold)).".lines";

  my %fields = ();
  my $trainingStrings = 0;
  my $testingStrings = 0;
  my $trainingWords = 0;
  my $testingWords = 0;
  my $vocabSize = 0;
  if( ( not $noCache ) && ( -s $cacheName ) ) {
    print STDERR "printing from cache...";
    system( "cat $cacheName" );
  } else {
    print STDERR "no cache -- evaluating...";
    if( ( not $noCache ) && ( -s $cacheName ) ) {
      unlink( $cacheName );
    }
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
    $fields{largeMiniBatchEvery} = "0" if not defined $fields{largeMiniBatchEvery};
    $fields{harmonicMiniBatchInit} = "false" if not defined $fields{harmonicMiniBatchInit};
    $fields{harmonicCorpusInit} = "false" if not defined $fields{harmonicCorpusInit};
    $fields{scaleInitMiniBatchCounts} = "false" if not defined $fields{scaleInitMiniBatchCounts};
    $fields{stochasticVB} = "false" if not defined $fields{stochasticVB};
    $fields{collapsedVB} = "false" if not defined $fields{collapsedVB};
    $fields{tau} = 1 if not defined $fields{tau};
    $fields{kappa} = 0.9 if not defined $fields{kappa};
    # $fields{possiblyResampleEvery} = $fields{miniBatchSize} if not defined $fields{possiblyResampleEvery};

    die "$trainingStrings $testingStrings $trainingWords $testingWords $vocabSize" unless 
      $trainingStrings && $testingStrings && $trainingWords && $testingWords && $vocabSize;

    my @logProbLines = split "\n", `grep "^it.*:logProb:" $fPath`;

    foreach my $logProbLine (@logProbLines) {
      my ($itSpec, $type, $logProb) = split ':', $logProbLine;

      # my $depDir = tempdir( CLEANUP => 1 );
      my $depDir = File::Temp->newdir();

      system( "grep $itSpec:dependency $fPath > $depDir/$itSpec.dep" );
      my %eval = ();
      foreach my $maxLength ( @maxLengths ) {
        my $eval = `$binDir/depEval.v2.pl -c -g $gold -m $minLength -l $maxLength $depDir/$itSpec.dep`;
        $eval =~ /Directed ([0-9.]+)/;
        $eval{"directed.$maxLength"} = $1;
        $eval =~ /Undirected ([0-9.]+)/;
        $eval{"undirected.$maxLength"} = $1;
        $eval =~ /Ned ([0-9.]+)/;
        $eval{"ned.$maxLength"} = $1;
      }


      $itSpec =~ /(\d+)/;
      my $sentencesSeen = $1;

      my $csvLine = join ",",
        ( map { $fields{$_ } } @fields ),
        $trainingStrings,
        $testingStrings,
        $trainingWords,
        $testingWords,
        $vocabSize,
        $sentencesSeen,
        $logProb,
        ( map { $eval{ "directed.$_" } } @maxLengths ),
        ( map { $eval{ "undirected.$_" } } @maxLengths ),
        ( map { $eval{ "ned.$_" } } @maxLengths );

      print $csvLine;

      if( not $noCache ) {
        open CACHE, ">>", $cacheName or
          die "Error opening $cacheName for appending $!";
          print CACHE $csvLine;
        close CACHE;
      }
    }

  }

}



