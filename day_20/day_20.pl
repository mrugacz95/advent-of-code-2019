use strict;
use warnings FATAL => 'all';
use FindBin;
use lib "$FindBin::RealBin/.";
use CommonModule;

my @grid = readGrid();
my $width = scalar(@{$grid[0]});
my $height = scalar(@grid);

my ($entrance, $exit, $graph, $portals) = &build_graph($height, $width, 1, @grid);


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

my $dist = dfs($entrance, $exit, %$graph);
print("Steps: $dist");