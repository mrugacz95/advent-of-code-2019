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
    if (!tree.hasOwnProperty(satellite)) {
        tree[satellite] = {"satellites": []};
    }
    tree[satellite].parent = planet;
}

// count distances
function dfs(node, dist) {
    if (tree.hasOwnProperty(node)) {
        tree[node].dist = dist;
    } else {
        tree[node] = {};
        tree[node].dist = dist;
    }
    // console.log(node, dist);
    if (tree.hasOwnProperty(node) && tree[node].hasOwnProperty("satellites")) {
        let children = tree[node].satellites;
        for (let child of children) {
            dfs(child, dist + 1)
        }
    }
}

dfs("COM", 0);

console.log(tree);

function find_LCA(node1, node2) {
    let dist1 = tree[node1].dist;
    let dist2 = tree[node2].dist;
    while (dist2 > dist1) {
        node2 = tree[node2].parent;
        dist2 = tree[node2].dist;
    }
    while (dist1 > dist2) {
        node1 = tree[node1].parent;
        dist1 = tree[node1].dist;
    }
    while (node1 !== node2) {
        node2 = tree[node2].parent;
        node1 = tree[node1].parent;
    }
    return node1;
}


let lca = find_LCA('YOU', 'SAN');

console.log(lca);

let dist = tree['YOU'].dist + tree['SAN'].dist - tree[lca].dist * 2 - 2;
console.log(dist);