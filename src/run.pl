#!/usr/bin/perl
use strict;
use warnings;

sub check_data {
    my @data = @{$_[0]};
    for (@data) {
        if ( not $_ =~ /[0-9]+/ ) {
            print "Every element in the row must be a number\n";
            exit(1);
        }
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

    open(FH, '<', $filename);

    # What is 'n_y' and 'n_x' variables
    # The file given must be something like one table to fortran able read it and make every math formula (FORTRAN can read any file)
    # so 'n_x' varibale will be useful to know how many rows has the table and 'n_y' to know hoy many columns has the table.
    my $n_x = 0;
    my $n_y = -1;

    while ( <FH> ) {
        my @thisrow = split(' ', $_);
        my $thisY   = scalar @thisrow;
        if ( $n_y == -1 and not $n_x ) {
            # The first line will work as reference to anothers rows, every next row must have the same 'n_y' columns
            $n_y = $thisY;
        }
        if ( $n_y != $thisY ) {
            print "All rows must have the same number of elements!\n";
            exit(1);
        }
        check_data(\@thisrow);
        $n_x++;
    }

    system("gfortran main.f90");
    system("./a.out " . "$n_x" . " " . "$n_y");
    close(FH);
}

main();
