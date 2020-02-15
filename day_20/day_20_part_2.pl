package day_20_part_2;
use strict;
use warnings FATAL => 'all';
use FindBin;
use lib "$FindBin::RealBin/.";
use CommonModule;

my @grid = readGrid();
my $width = scalar(@{$grid[0]});
my $height = scalar(@grid);
my ($entrance, $exit, $graph, $portals) = &build_graph($height, $width, @grid);


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
        my @neighbours = @{$graph{$current}};
        foreach my $neighbour (@neighbours) {
            if (!exists($visited{$neighbour})) {
                push(@queue, $neighbour);
                push(@queue_distance, $dist + 1);
                $all_distances{$current} = $dist + 1;
            }
        }
    }
    return(%all_distances)
}
sub isOuter {
    my ($y,$x) = int_2_coord($_[0], $height, $width);
    if($y == 2 or $y == $height - 3 or $x == 2 or $x == $width - 3){
        return(1);
    }
    return(0);
}

my @all_portal_positions; # list of portal positions
map { push(@all_portal_positions, @{$portals->{$_}}) } keys %$portals;

my %portals_reverse; # contains position to name
foreach my $portal_name (keys(%$portals)){
    foreach my $portal_pos (@{$portals->{$portal_name}}){
        $portals_reverse{$portal_pos} = $portal_name;
    }
}

# Build distance matrix between all portals
my %dist_mat;
foreach my $portal_name (keys(%$portals)){
    print("$portal_name: ");
    foreach my $portal_position (@{$portals->{$portal_name}}) {
        print("$portal_position, ");
        my %to_all = dfs_to_all($portal_position, %$graph);
        $dist_mat{$portal_position} = {};
        foreach my $portal_pos (@all_portal_positions){
            $dist_mat{$portal_position}->{$portal_pos} = $to_all{$portal_pos};
        } 
    }
    print("\n")
}
# fix portals list to have outer in first position. AA and ZZ entries will not be changed.
sub fix_portals {
    my $portals_ref = $_[0];
    print("portals ref $portals_ref\n");
    foreach my $key (keys(%$portals_ref)){
        if(!isOuter(@{$portals_ref->{$key}}[0])){
            my $list = $portals_ref->{$key};
            print("$key @$list\n");
            $portals_ref->{$key} = [$list->[1], $list->[0]];
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
sub multi_dimensinal_dfs{
    my ($graph_ref, $portals_ref, $h, $w, $dist_mat) = @_;
    my $max_level = 0;
    my $level_offset = $height * $width;
    
    
}
fix_portals($portals);
multi_dimensinal_dfs($graph, $portals, $height, $width, \%dist_mat)