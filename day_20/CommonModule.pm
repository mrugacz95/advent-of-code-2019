package CommonModule;
use strict;
use warnings FATAL => 'all';
use Exporter;

use Exporter qw(import);
our @EXPORT = qw(readGrid grid_at coord_2_int int_2_coord find_neighbours build_graph);

sub readGrid() {
    open(MAP, "<day_20.in") or die "Couldn't open input";
    my @map = <MAP>;
    my @grid = ();
    foreach my $line (@map) {
        my @row;
        foreach my $c (split("", $line)) {
            if ($c ne "\n") {
                push(@row, $c);
            }
        }
        push(@grid, [ @row ]);
    }
    close(MAP);
    return(@grid);
}

sub grid_at {
    my ($y, $x, @grid) = @_;
    return($grid[$y][$x])
}
sub coord_2_int {
    my ($y, $x, $height, $width) = @_;
    return $y * ($width - 1) + $x;
}
sub int_2_coord {
    my ($number, $height, $width) = @_;
    my $x = $number % ($width - 1);
    my $y = ($number - $x) / ($width - 1);
    return($y, $x);
}


sub find_neighbours {
    my ($y, $x, $height, $width, @grid) = @_;
    my @directions = ([ -1, 0 ], [ 1, 0 ], [ 0, 1 ], [ 0, -1 ]);
    my @neighbours;
    foreach my $dir (@directions) {
        my $dy = $dir->[0];
        my $dx = $dir->[1];
        if (grid_at($y + $dy, $x + $dx, @grid) eq '.') {
            push(@neighbours, coord_2_int($y + $dy, $x + $dx, $height, $width));
        }
    }
    return @neighbours;
}


sub build_graph {
    sub add_portal {
        my $code = $_[0];
        my $at = $_[1];
        my $portals_ref = $_[2];
        if (exists($portals_ref->{$code})) {
            push(@{$portals_ref->{$code}}, $at);
        }
        else {
            $portals_ref->{$code} = [ $at ];
        }
    }
    sub check_match {
        my $pattern = "[A-Z]";
        my ($first, $second, $field, $y, $x, $p_ref, $height, $width) = @_;
        if ($first =~ /$pattern/ and
            $second =~ /$pattern/ and
            $field eq '.') {
            my $code = $first . $second;
            my $coords = coord_2_int($y, $x, $height, $width);
            add_portal($code, $coords, $p_ref);
        }
    }
    my ($height, $width, @grid) = @_;
    my %graph;
    for (my $y = 1; $y < $height - 1; $y++) {
        for (my $x = 1; $x < $width - 1; $x++) {
            my @neigh = find_neighbours($y, $x, $height, $width, @grid);
            if (scalar(@neigh) != 0) {
                $graph{coord_2_int($y, $x, $height, $width)} = \@neigh;
            }
        }
    }
    my %portals;
    # find portals
    for (my $y = 0; $y < $height - 2; $y++) {
        for (my $x = 0; $x < $width - 2; $x++) {
            my $top = grid_at($y, $x + 1, @grid);
            my $mid = grid_at($y + 1, $x + 1, @grid);
            my $bot = grid_at($y + 2, $x + 1, @grid);
            my $left = grid_at($y + 1, $x, @grid);
            my $right = grid_at($y + 1, $x + 2, @grid);
            check_match($top, $mid, $bot, $y + 2, $x + 1, \%portals, $height, $width);
            check_match($mid, $bot, $top, $y, $x + 1, \%portals, $height, $width);
            check_match($left, $mid, $right, $y + 1, $x + 2, \%portals, $height, $width);
            check_match($mid, $right, $left, $y + 1, $x, \%portals, $height, $width);
        }
    }
    # insert portals to graph 
    my $entrance;
    my $exit;
    foreach my $key (keys %portals) {
        my @positions = @{$portals{$key}};
        if (scalar(@positions) == 2) {
            my $from = $positions[0];
            my $to = $positions[1];
            push(@{$graph{$from}}, $to);
            push(@{$graph{$to}}, $from);
        }
        elsif ($key eq 'AA') {
            $entrance = @{$portals{$key}}[0];
        }
        elsif ($key eq 'ZZ') {
            $exit = @{$portals{$key}}[0];
        }
    }
    return($entrance, $exit, \%graph, \%portals);
}