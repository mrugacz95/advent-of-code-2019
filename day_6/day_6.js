let fs = require('fs');

let data = fs.readFileSync('day_6.in', 'utf8')
    .split("\n")
    .map((it) => {
        return it.split(")")
    });

let tree = {};


// load tree
for (let orbit of data) {
    let planet = orbit[0];
    let satellite = orbit[1];
    if (tree.hasOwnProperty(planet)) {
        tree[planet].satellites.push(satellite)
    } else {
        tree[planet] = {};
        tree[planet].satellites = [satellite];
    }
}

console.log(tree);

// count distances
function dfs(node, dist) {
    if (tree.hasOwnProperty(node)) {
        tree[node].dist = dist;
    } else {
        tree[node] = {};
        tree[node].dist = dist;
    }
    console.log(node, dist);
    if (tree.hasOwnProperty(node) && tree[node].hasOwnProperty("satellites")) {
        let children = tree[node].satellites;
        for (let child of children) {
            dfs(child, dist + 1)
        }
    }
}

dfs("COM", 0);

// count orbits
let count = 0;
for (let key in tree) {
    if (tree.hasOwnProperty(key)) {
        count += tree[key].dist;
    }
}
console.log(count);