use strict;
use warnings FATAL => 'all';

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
my @grid = readGrid();
my $width = scalar(@{$grid[0]});
my $height = scalar(@grid);
sub grid_at {
    my $y = $_[0];
    my $x = $_[1];
    return($grid[$y][$x])
}
sub coord_2_int {
    my $y = $_[0];
    my $x = $_[1];
    return $y * ($width - 1) + $x;
}
sub int_2_coord {
    my $number = $_[0];
    my $x = $number % ($width - 1);
    my $y = ($number - $x) / ($width - 1);
    return($y, $x);
}
sub find_neighbours {
    my $y = $_[0];
    my $x = $_[1];
    my @directions = ([ -1, 0 ], [ 1, 0 ], [ 0, 1 ], [ 0, -1 ]);
    my @neighbours;
    foreach my $dir (@directions) {
        my $dy = $dir->[0];
        my $dx = $dir->[1];
        if (grid_at($y + $dy, $x + $dx) eq '.') {
            push(@neighbours, coord_2_int($y + $dy, $x + $dx));
        }
    }
    return @neighbours;
}
sub build_graph {
    my %graph;
    for (my $y = 1; $y < $height - 1; $y++) {
        for (my $x = 1; $x < $width - 1; $x++) {
            my @neigh = find_neighbours($y, $x);
            if (scalar(@neigh) != 0) {
                $graph{coord_2_int($y, $x)} = \@neigh;
            }
        }
    }
    my %portals;
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
        my ($first, $second, $field, $y, $x, $p_ref) = @_;
        if ($first =~ /$pattern/ and
            $second =~ /$pattern/ and
            $field eq '.') {
                my $code = $first . $second;
                my $coords = coord_2_int($y, $x);
                add_portal($code, $coords, $p_ref);
        }
    }
    my $pattern = "[A-Z]";
    # find portals
    for (my $y = 0; $y < $height - 2; $y++) {
        for (my $x = 0; $x < $width - 2; $x++) {
            my $top = grid_at($y, $x + 1);
            my $mid = grid_at($y + 1, $x + 1);
            my $bot = grid_at($y + 2, $x + 1);
            my $left = grid_at($y + 1, $x);
            my $right = grid_at($y + 1, $x + 2);
            check_match($top, $mid, $bot, $y + 2, $x + 1, \%portals);
            check_match($mid, $bot, $top, $y, $x + 1, \%portals);
            check_match($left, $mid, $right, $y + 1, $x + 2, \%portals);
            check_match($mid, $right, $left, $y + 1, $x, \%portals);
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
    return($entrance, $exit, %graph);
}

sub dfs {
    my ($from, $to, %graph) = @_;
    my @queue;
    my @distance;
    my %visited;
    push(@queue, $from);
    push(@distance, 0);
    while (scalar(@queue) > 0) {
        my $current = shift(@queue);
        $visited{$current} = 1;
        my $dist = shift(@distance);
        if ($current == $to) {
            return($dist);
        }
        my @neighbours = @{$graph{$current}};
        foreach my $neighbour (@neighbours) {
            if (!exists($visited{$neighbour})) {
                push(@queue, $neighbour);
                push(@distance, $dist + 1);
            }
        }
    }
    print("Not found\n");
}
my ($entrance, $exit, %graph) = &build_graph();
my $dist = dfs($entrance, $exit, %graph);
print("Steps: $dist");