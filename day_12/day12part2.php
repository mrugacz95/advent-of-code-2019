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
$initialState = array();
foreach ($data as $element) {
    preg_match('<x=(-?\d+), y=(-?\d+), z=(-?\d+)>', $element, $matches, PREG_OFFSET_CAPTURE);
    $x = (int) $matches[1][0];
    $y = (int) $matches[2][0];
    $z = (int) $matches[3][0];
    array_push($moons, new Moon($x, $y, $z));
    array_push($initialState, new Moon($x, $y, $z));
}
$axes = array("x", "y", "z");
function updateVelocities($first, $second, $axis) {
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
function updateMoons($moons, $axis) {
    $size = count($moons);
    for ($i = 0; $i < $size; $i++) {
        for ($j = 0; $j < $size; $j++) {
            if ($i >= $j) { // Select every pair
                continue;
            }
            $first  = $moons[$i];
            $second = $moons[$j];
            updateVelocities($first, $second, $axis);
        }
    }
    foreach ($moons as $moon) { // Update positions
        $moon->pos[$axis] += $moon->v[$axis];
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

echo "<br>Step 0:<br>";
printMoons($moons, $axes);
function checkFinished($moons, $initial, $axis){
    $size = count($moons);
    for($i = 0; $i< $size; $i++) {
        if($moons[$i]->v[$axis] != $initial[$i]->v[$axis]){
            return False;
        }
        if($moons[$i]->pos[$axis] != $initial[$i]->pos[$axis]){
            return False;
        }
    }
    return True;
}
function calculatePeriod($moons, $initialState, $axis){
    $steps = 0;
    while (1) {
        $steps++;
        updateMoons($moons, $axis);
        if(checkFinished($moons, $initialState, $axis)){
            break;
        }
    }
    return $steps;
}
$periods = array();
foreach ($axes as $axis) {
    $period = calculatePeriod($moons, $initialState, $axis);
    array_push($periods, $period);
}
echo 'periods: x=', $periods[0], ' y=', $periods[1], ' z=', $periods[2], '<br>';

function gcd($a, $b){
    while($b != 0){
        $c = $a % $b;
        $a = $b;
        $b = $c;
    }
    return $a;
}
function lcm($a, $b){
    return $a * $b / gcd($a, $b);
}
$steps = lcm(lcm($periods[0], $periods[1]), $periods[2]);
echo "Steps done $steps<br>";