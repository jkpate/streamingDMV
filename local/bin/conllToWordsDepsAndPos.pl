#!/usr/bin/perl -wl

use strict;

use Lingua::Treebank;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

$Lingua::Treebank::Const::CHILD_PROLOG = " ";
$Lingua::Treebank::Const::CHILD_EPILOG = " ";
$Lingua::Treebank::Const::INDENT_CHAR = "";

my $pruneNonwords = sub {
  my ( $self, $state ) = @_;

  my $annot = $self -> annot();


  if(
      $annot=~/punct$/ ||
      $annot=~/^[^a-zA-Z#\$]+\.\d+-[^a-zA-Z#\$]+/
  ) {
      push @{$state}, $self;
  }
};

my $stopCondition = sub { return 0 };


foreach my $f ( @ARGV ) {
  open F, '<', $f or die "Error opening file: $!";
    my $fName = $f;
    $fName =~ s/^.*\///;

    print STDERR "processing $fName";

    my $sentNum = 0;

    my $rootIdx = -1;
    my @words = ();
    my @heads = ();
    my @children = ();
    my @pos = ();
    while(<F>) {
      if( /^$/ ) {
        my $sentID = "$fName.s$sentNum:";

        die unless $rootIdx >= 0;
        $heads[$rootIdx] = scalar @words;

        unless( $#words == $#heads && $#heads == $#pos ) {
          print join " ", @words;
          print join " ", @heads;
          print join " ", @pos;
        }
        die unless( $#words == $#heads && $#heads == $#pos );


        print join " ", "origWords:".$sentID, @words;
        print join " ", "origHeads:".$sentID, "[", ( join ", ", @heads ) , "]";
        print join " ", "origWordsAndPos:".$sentID, map { $words[$_]."#".$pos[$_] } 0..$#words;

        for( my $i = 0; $i <= $#words; $i++ ) {
          $children[$i] = [] unless defined $children[$i]; 
        }

        my $originalPTB = toPenn( \@children, $rootIdx, 'ROOT', \@words, \@pos );

        print join " ", "origPTB:".$sentID, "(", $originalPTB, ")";

        my @origRecovHeads = recover_heads( $originalPTB );
        foreach( my $i = 0; $i <= $#words; $i++ ) {
          die ( join "; ", $i, @origRecovHeads, @heads ) if $origRecovHeads[$i] != $heads[$i];
        }

        my @nonsenseNodes = ();
        $originalPTB -> walk( $pruneNonwords, $stopCondition, \@nonsenseNodes );

        foreach my $n ( @nonsenseNodes ) {
          #print STDERR "$uttID removing $n";
          if(
            defined $n->parent() and
            not $n->is_terminal() and
            scalar @{ $n->children } > 1
          ) {
            $n->parent()->retract($n);
          } else {
            $n->wither();
          }
        }

        print join " ", "prunedPTB:$sentID", "( $originalPTB )";

        my @prunedHeads = recover_heads( $originalPTB );
        my @prunedWords = map { $a = $_->word(); $a =~ s/\.\d+$//g; $a } $originalPTB->get_all_terminals ;
        my @prunedPos = map { $a = $_->annot();  $a } $originalPTB->get_all_terminals ;
        my @prunedWordsAndPos = ();
        for( my $i = 0; $i <= $#prunedWords; $i++ ) {
          my $pos = $prunedPos[$i];
          $pos =~s/#//g;
          $prunedWordsAndPos[$i] = "$prunedWords[$i]#$pos";
        }
        print join " ", "prunedWords:$sentID", @prunedWords;
        print join " ", "prunedPos:$sentID", @prunedPos;
        print join " ", "prunedWordsAndPos:$sentID", @prunedWordsAndPos;
        print join " ", "prunedHeads:$sentID", "[",  ( join ", ", @prunedHeads ), "]";




        $sentNum++;

        $rootIdx=-1;
        @words = ();
        @heads = ();
        @children = ();
        @pos = ();
      } else {
        my @fields = split;
        my $idx = $fields[0] - 1;
        my $hIdx = $fields[6] - 1;
        my $word = $fields[1];

        $word =~s/-/._./g;
        $word =~s/=/.:equalsign:./g;

        $words[$idx] = $word.".".$idx;
        $pos[$idx] = $fields[3];
        if( $hIdx >= 0 ) {
          $heads[$idx] = $hIdx;
          push @{ $children[$hIdx] }, [ $idx, $fields[7] ];
        } else {
          $rootIdx = $idx;
        }
      }
    }

  close F;
}

sub recover_heads {
  my $ptb = shift;

  my @ptbTerms = $ptb->get_all_terminals();
  my @recoveredHeads = ();
  # Ok, this is quadratic but whatevs
  for( my $i = 0; $i < $#ptbTerms; $i++ ) {
    my $leftWord = $ptbTerms[$i]->word();
    for( my $j = $i+1; $j <= $#ptbTerms; $j++ ) {
      my $rightWord = $ptbTerms[$j]->word();

      my $ancestor = $ptbTerms[$i]->find_common_ancestor( $ptbTerms[$j] );

      if( defined $ancestor ) {
        my @children = @{ $ancestor->children()};
        my $leftMatch = 0;
        my $rightMatch = 0;

        # print "leftWord: $leftWord";
        # print "rightWord: $rightWord";

        for( my $k = 0; $k <= $#children; $k++ ) {
          my $tag = $children[$k]->tag();
          # special handling for punctuation-initial words
          # $tag =~ s/^[-,:=?][^.]*\.\d+\K-(punct|discourse)$//g;
          $tag =~ s/^[-,:=?][^.]*\.\d+\K-.*$//g;

          # print "\tkth word: ".($tag);
          $leftMatch = $leftMatch ||  $tag eq $leftWord;
          $rightMatch = $rightMatch || $tag eq $rightWord;
        }

        # print "leftMatch: $leftMatch";
        # print "rightMatch: $rightMatch";

        my $ancestLabel = $ancestor->tag();
        # print "$ancestLabel eq ".($origPTBTerms[$i]->word())." ? ".($ancestLabel eq $origPTBTerms[$i]->word());
        if( $leftMatch && $rightMatch ) {
          if( $ancestLabel eq $rightWord ) {
            $recoveredHeads[$i] = $j;
          } elsif( $ancestLabel eq $leftWord ) {
            $recoveredHeads[$j] = $i;
          }
        }
      }
    }
  }
  my $rootCount = 0;
  # print join " ", "preRootRecoDeps$uttID", "[",  ( join ", ", @recoveredHeads ), "]";
  foreach( my $i = 0; $i <= $#ptbTerms; $i++ ) {
    if( not defined $recoveredHeads[$i] ) {
      # print "$i not defined";
      $recoveredHeads[$i] = scalar @ptbTerms;
      $rootCount++;
    }
  }

  return @recoveredHeads;
}

sub toPenn {
  my $childrenRef = shift;
  my @children = @$childrenRef;

  my $myIndex = shift;

  my $relationType = shift;

  my $terminalsRef = shift;
  my @terminals = @$terminalsRef;

  my $preTerminalsRef = shift;
  my @preTerminals = @$preTerminalsRef;

  # print $myIndex;



  if( scalar @children == 0 ) { # base case

    my $terminalChild = 
      Lingua::Treebank::Const -> new -> from_penn_string( "($preTerminals[$myIndex]-$relationType $terminals[$myIndex])" );
    return $terminalChild;

  } else { # recursion
    # print "trying to access $myIndex: $terminals[ $myIndex ]";

    my $nonTerminal = 
      Lingua::Treebank::Const -> new -> from_penn_string(
        # "($terminals[$myIndex]-$relationType ($preTerminals[$myIndex] $terminals[$myIndex]) )"
        "($terminals[$myIndex]-$relationType ($terminals[$myIndex]-$preTerminals[$myIndex] $terminals[$myIndex]) )"
      );

    my @leftwardDependencies = ();
    my @rightwardDependencies = ();

    foreach my $childAndRelation ( sort { $a->[0] <=> $b->[0] } @{ $children[$myIndex] } ) {
      my $childIndex = $childAndRelation->[0];
      my $childRelation = $childAndRelation->[1];
      my $thisChild = toPenn( $childrenRef, $childIndex, $childRelation, $terminalsRef, $preTerminalsRef );

      if( $childIndex < $myIndex ) {
        #$nonTerminal -> prepend( $thisChild );
        push @leftwardDependencies, $thisChild;
      } else {
        #$nonTerminal -> append( $thisChild );
        push @rightwardDependencies, $thisChild;
      }

    }

    $nonTerminal -> prepend( @leftwardDependencies );
    $nonTerminal -> append( @rightwardDependencies );

    return $nonTerminal;

  }


}

