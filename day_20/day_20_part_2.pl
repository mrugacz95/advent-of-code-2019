package day_20_part_2;
use strict;
use warnings FATAL => 'all';
use FindBin;
use lib "$FindBin::RealBin/.";
use CommonModule;

my @grid = readGrid();
my $width = scalar(@{$grid[0]});
my $height = scalar(@grid);
my ($entrance, $exit, $graph, $portals) = &build_graph($height, $width, 0, @grid);

sub dfs_to_all {
    my ($from, %graph) = @_;
    my @queue;
    my @queue_distance;
    my %all_distances;
    my %visited;
    push(@queue, $from);
    push(@queue_distance, 0);
    while (scalar(@queue) > 0) {
        my $current = shift(@queue);
        $visited{$current} = 1;
        my $dist = shift(@queue_distance);
        $all_distances{$current} = $dist;
        my @neighbours = @{$graph{$current}};
        foreach my $neighbour (@neighbours) {
            if (!exists($visited{$neighbour})) {
                push(@queue, $neighbour);
                push(@queue_distance, $dist + 1);
            }
        }
    }
    return(%all_distances)
}
sub isOuter {
    my ($y, $x) = int_2_coord($_[0], $height, $width);
    if ($y == 2 or $y == $height - 3 or $x == 2 or $x == $width - 3) {
        return(1);
    }
    return(0);
}

my @all_portal_positions; # list of portal positions
map {push(@all_portal_positions, @{$portals->{$_}})} keys %$portals;

my %portals_reverse; # contains position to name
foreach my $portal_name (keys(%$portals)) {
    foreach my $portal_pos (@{$portals->{$portal_name}}) {
        $portals_reverse{$portal_pos} = $portal_name;
    }
}

# Build distance matrix between all portals
my %dist_mat;
foreach my $portal_name (keys(%$portals)) {
    print("$portal_name: ");
    foreach my $portal_position (@{$portals->{$portal_name}}) {
        print("$portal_position, ");
        my %to_all = dfs_to_all($portal_position, %$graph);
        $dist_mat{$portal_position} = { $portal_position => 0 };
        foreach my $portal_pos (@all_portal_positions) {
            if (exists($to_all{$portal_pos})) {
                # print("{$portal_position}->{$portal_pos} = $to_all{$portal_pos} \n");
                $dist_mat{$portal_position}->{$portal_pos} = $to_all{$portal_pos};
            }
            else {
                # print("{$portal_position}->{$portal_pos} = inf \n");
                $dist_mat{$portal_position}->{$portal_pos} = "inf";
            }
        }
    }
    print("\n")
}
sub print_dist_mat() {
    # print dist matrix
    foreach my $row_portal (keys(%$portals)) {
        foreach my $row_pos (@{$portals->{$row_portal}}) {
            print("$row_portal");
            if (isOuter($row_pos)) {
                print("(O)\n")
            }
            else {
                print("(I)\n");
            }
            foreach my $col_portal (keys(%$portals)) {
                foreach my $col_pos (@{$portals->{$col_portal}}) {
                    # print("$row_portal AA dist " . $dist_mat{1511}->{1511});
                    print("\t$col_portal");
                    if (isOuter($col_pos)) {
                        print("(O)")
                    }
                    else {
                        print("(I)");
                    }
                    print(":$dist_mat{$row_pos}->{$col_pos}\n")
                }
            }
        }
    }
}
# fix portals list to have outer in first position. AA and ZZ entries will not be changed.
sub fix_portals {
    my $portals_ref = $_[0];
    print("portals ref $portals_ref\n");
    foreach my $key (keys(%$portals_ref)) {
        if (!isOuter(@{$portals_ref->{$key}}[0])) {
            my $list = $portals_ref->{$key};
            # print("$key @$list\n");
            $portals_ref->{$key} = [ $list->[1], $list->[0] ];
        }
    }
}


# region checks
# print("$dist_mat{263}->{3171}\n"); # should be 503
# print("is outer ". isOuter(9156) ."\n");
# foreach my $portal_pos (@all_portal_positions){
#     print("$portals_reverse{$portal_pos} is $portal_pos\n")
# }
# end checks
# print("dupa\n");
# foreach my $key (keys(%$portals)){
#     print("k:$key ");
#     print("v:$portals->{$key}->[0]\n");
#     if(!isOuter($portals->{$key}->[0])){
#         print("Bad outer order\n");
#     }
# }
# sub multi_dimensinal_dfs{
#     my ($graph_ref, $portals_ref, $h, $w, $dist_mat) = @_;
#     my $max_level = 0;
#     my $level_offset = $height * $width;
#     my %graph = {}
#     
# }
fix_portals($portals);

sub formatOuter {
    my $pos = $_[0];
    if (isOuter($pos)) {
        return("(O)")
    }
    return("(I)")
}

sub multi_dimensional_dfs {
    my ($graph_ref, $portals_ref, $h, $w, $dist_mat) = @_;
    my $max_level = 0;
    my $level_offset = $height * $width;
    my %visited;
    # distance, level, name
    my @nodes = ([ 0, 0, @{$portals_ref->{"AA"}}[0], "AA" ],
        [ "inf", 0, @{$portals_ref->{"ZZ"}}[0], "ZZ" ]);
    # add all inner
    foreach my $name (keys(%$portals_ref)) {
        my @list = @{$portals_ref->{$name}};
        if (scalar(@list) > 1) { # add only inner
            push(@nodes, [ "inf", 0, @{$portals_ref->{$name}}[1], $name ])
        }
    }
    while (scalar(@nodes) > 0) {
        # find nearest, priority queue should be used 
        # print("nodes len " . scalar(@nodes) . "\n");
        my $nearest = 0;
        for my $i (1 .. scalar(@nodes) - 1) {
            # print("$i $nodes[$i][0]\n");
            if ($nodes[$i][0] < $nodes[$nearest][0]) {
                $nearest = $i;
                # print("nearest $i\n");
            }
        }
        my ($distance, $level, $position, $name) = @{$nodes[$nearest]};
        print("Current: $name " . formatOuter($position) . " lvl: $level dist: $distance\n");
        # print("nearest $nearest ");
        # print("$distance, $level, $position $name " . isOuter($position) . "\n");
        if ($name eq "ZZ" and $level == 0) {
            print("Distance $distance");
            last;
        }
        if ($name ne 'AA' and $name ne "ZZ") {
            if (!isOuter($position)) {
                if ($level == $max_level) {
                    # is inner
                    # add next level nodes
                    print("Need to add next dimension, node: $name (" . isOuter($position) . "), current max: $max_level\n");
                    $max_level += 1;
                    foreach my $portal_name (keys(%$portals_ref)) {
                        if ($portal_name eq "AA" or $portal_name eq "ZZ") {
                            next;
                        }
                        my $outer_dist = "inf";
                        if ($portal_name eq $name) {
                            $outer_dist = $distance + 1;
                        }
                        push(@nodes, [ $outer_dist, $level + 1, @{$portals_ref->{$portal_name}}[0], $portal_name ]); # add outer 
                        push(@nodes, [ "inf", $level + 1, @{$portals_ref->{$portal_name}}[1], $portal_name ]);       # add inner
                    }
                }
                else {
                    # update only outer node
                    print("Updating next dimension outer node distance\n");
                    foreach my $inner_ref (@nodes) {
                        # find corresponding inner and update dist 
                        my ($inner_dist, $inner_lvl, $inner_pos, $inner_name) = @$inner_ref;
                        if ($name eq $inner_name
                            and ($inner_lvl == $level + 1)
                            and ($inner_dist > $distance + 1)
                            and isOuter($inner_pos)) {
                            print("Update needed\n");
                            $inner_ref->[0] = $distance + 1;
                            last;
                        }
                    }
                }
            }
            elsif (isOuter($position)) {
                # is outer and not ZZ
                print("Updating previous dimension inner node distance\n");
                foreach my $inner_ref (@nodes) {
                    # find corresponding inner and update dist 
                    my ($inner_dist, $inner_lvl, $inner_pos, $inner_name) = @$inner_ref;
                    if ($name eq $inner_name
                        and ($inner_lvl == $level - 1)
                        and ($inner_dist > $distance + 1)
                        and !isOuter($inner_pos)) {
                        print("Update needed\n");
                        $inner_ref->[0] = $distance + 1;
                        last;
                    }
                }
            }
        }
        splice(@nodes, $nearest, 1);      # remove current node from list
        $visited{"$position $level"} = 1; # mark visited

        # mark distances to neighbours
        for my $node_ref (@nodes) {
            my ($neigh_dist, $neigh_lvl, $neigh_pos, $neigh_name) = @$node_ref;
            # print("lvl $level $neigh_lvl\n");
            # print("dist $distance $neigh_dist\n");
            if ((exists($dist_mat->{$position}->{$neigh_pos}) and $neigh_lvl == $level)
                or ($neigh_name eq $name and !isOuter($neigh_pos) and isOuter($position) and $neigh_lvl = $level - 1)
                or ($neigh_name eq $name and isOuter($neigh_pos) and !isOuter($position) and $neigh_lvl - 1 == $level)) {
                # same lvl, same name +=1 lvl, dist exists!
                # print("dist $distance $position $neigh_pos\n");
                # print("dist_mat $dist_mat->{$position}");
                my $new_dist = $distance + $dist_mat->{$position}->{$neigh_pos};
                if ($new_dist < $neigh_dist) {
                    $node_ref->[0] = $new_dist;
                    # print("dist changed")
                }
            }
        }
    }
}

multi_dimensional_dfs($graph, $portals, $height, $width, \%dist_mat)