use strict;
use warnings FATAL => 'all';
use FindBin;   
use lib "$FindBin::RealBin/."; 
use CommonModule;

my @grid = readGrid();
my $width = scalar(@{$grid[0]});
my $height = scalar(@grid);

my ($entrance, $exit, %graph) = &build_graph($height, $width, @grid);
my $dist = dfs($entrance, $exit, %graph);
print("Steps: $dist");