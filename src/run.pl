#!/usr/bin/perl
use strict;
use warnings;

sub check_data {
    my $data = $_[0];
    if ( $data =~ /-[0-9]+/ ) {
        print "Every element must belong natural numbers\n";
        exit(1);
    }
    if ( not $data =~ /[0-9]+/ ) {
        print "Every element in the row must be a number\n";
        exit(1);
    }
}

sub main {
    my $filename = $ARGV[0];

    if ( not $filename ) {
        print "ups...!\n";
        print "Filename expected as first parameter\n";
        exit(1);
    }
    if ( not -e $filename ) {
        print "ups...!\n";
        print "'$filename' does not exist\n";
        print "Your file can have any extension, only make sure that satisfies all conditions\n";
        exit(1);
    }

    my $uniq_values_count = 0;
    my @data = {};

    open(FH, '<', $filename);
    while ( <FH> ) {
        if ( not "$_" ~~ @data ) {
            $uniq_values_count++;
        }
        check_data($_);
        push(@data, $_)
    }
    my $data_len = $#data;

    if ( $data_len == 0 ) {
        print "The file could not be empty...!\n";
        exit(1);
    }


    print "Good file!\n";
    system("gfortran -g -fcheck=all -Wall main.f95");
    system("./a.out $filename $data_len $uniq_values_count");
    system("rm a.out");
    close(FH);
}

main();
