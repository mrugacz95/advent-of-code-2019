<?php
class Moon{
    public $pos;
    public $v;
    public function __construct($x, $y, $z){
        $this->pos = array("x"=>$x, "y"=>$y, "z"=>$z);
        $this->v = array("x"=>0, "y"=>0, "z"=>0);
    }
}
$file_name = "day12.in";
$datafile = fopen($file_name, "r") or die("Unable to open file!");
$data = fread($datafile, filesize($file_name));
fclose($datafile);
echo '<pre>', var_dump(htmlspecialchars($data)), '</pre>';
$data = explode("\n", $data);
$moons = array();
foreach ($data as $element) {
    preg_match('<x=(-?\d+), y=(-?\d+), z=(-?\d+)>', $element, $matches, PREG_OFFSET_CAPTURE);
    $x = (int) $matches[1][0];
    $y = (int) $matches[2][0];
    $z = (int) $matches[3][0];
    $moon = new Moon($x, $y, $z);
    array_push($moons, $moon);
}
$axes = array("x", "y", "z");
function updateVelocities($first, $second, $axes) {
    foreach ($axes as $axis) {
        $firstPos  = $first->pos[$axis];
        $secondPos = $second->pos[$axis];
        if ($firstPos > $secondPos) {
            $first->v[$axis] -= 1;
            $second->v[$axis] += 1;
        } elseif ($first->pos[$axis] < $second->pos[$axis]) {
            $first->v[$axis] += 1;
            $second->v[$axis] -= 1;
        }
    }
}
function updateMoons($moons, $axes) {
    $size = count($moons);
    for ($i = 0; $i < $size; $i++) {
        for ($j = 0; $j < $size; $j++) {
            if ($i >= $j) { // Select every pair
                continue;
            }
            $first  = $moons[$i];
            $second = $moons[$j];
            updateVelocities($first, $second, $axes);
        }
    }
    foreach ($moons as $moon) { // Update positions
        foreach ($axes as $axis) {
            $moon->pos[$axis] += $moon->v[$axis];
        }
    }
}
function printMoons($moons, $axes) {
    foreach ($moons as $moon) {
        echo 'pos=(';
        foreach ($axes as $axis) {
            echo $moon->pos[$axis], ', ';
        }
        echo ') v=(';
        foreach ($axes as $axis) {
            echo $moon->v[$axis], ', ';
        }
        echo ')<br>';
    }
}
$steps = 1000;
echo "<br>Step 0:<br>";
printMoons($moons, $axes);
for ($i = 1; $i <= $steps; $i++) {
    updateMoons($moons, $axes);
    //     echo "Step $i:<br>";
    //     printMoons($moons, $axes);
}
function calculateEnergy($moons, $axes) {
    $sum = 0;
    foreach ($moons as $moon) {
        $potential = 0;
        foreach ($axes as $axis) {
            $potential += abs($moon->pos[$axis]);
        }
        $kinetic = 0;
        foreach ($axes as $axis) {
            $kinetic += abs($moon->v[$axis]);
        }
        $sum += $potential * $kinetic;
    }
    return $sum;
}
echo 'total energy: ', calculateEnergy($moons, $axes);
