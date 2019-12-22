using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace day_14_part_2
{
    class Day14Part2
    {
        static void Main(string[] args)
        {
            byte[] inputBytes = Properties.Resources.day_14;
            var input = System.Text.Encoding.UTF8.GetString(inputBytes);
            new Solution(input);
        }

    }
    class Edge
    {
        public uint quantity { get; }
        public string target { get; }

        public Edge(uint quantity, string target)
        {
            this.quantity = quantity;
            this.target = target;
        }
    }
    enum Status { NOT_VISITED, TEMPORARY, VISITED };

    class Vertex
    {
        public Status status = Status.NOT_VISITED;
        public List<Edge> edges { get; } = new List<Edge>();
        public ulong quantityObtained;
        public ulong quantityReuqired = 0;
    }

    class Solution
    {
        Dictionary<string, Vertex> graph = new Dictionary<string, Vertex>();
        List<string> sorted = new List<string>();
        void dfs(string node)
        {
            if (graph[node].status == Status.VISITED)
            {
                return;
            }
            if (graph[node].status == Status.TEMPORARY)
            {
                Console.WriteLine("Cycle detected. Wrong graph.");
                throw new Exception("Cycle detected. Wrong graph.");
            }
            foreach (var edge in graph[node].edges)
            {
                dfs(edge.target);
            }
            graph[node].status = Status.VISITED;
            sorted.Add(node);
        }

        Tuple<uint, string> getQuantityAndName(string substance)
        {
            var parts = substance.Split(' ');
            return new Tuple<uint, string>(Convert.ToUInt32(parts[0]), parts[1]);
        }

        ulong calculateOreRequired(ulong fuel) { 
        
            foreach(var vertex in graph){
                vertex.Value.quantityReuqired = 0;
            }
            graph["FUEL"].quantityReuqired = fuel;
            foreach (var vertex in sorted)
            {
                foreach (var edge in graph[vertex].edges)
                {
                    ulong numberOfReactions = (ulong)Math.Ceiling(((double)graph[vertex].quantityReuqired) / ((double)graph[vertex].quantityObtained));
                    graph[edge.target].quantityReuqired += edge.quantity * numberOfReactions;
                }
            }
            return graph["ORE"].quantityReuqired;
        }

        ulong binarySearch()
        {
            ulong minFuel = 0;
            ulong maxFuel = 1000000000; // Increase if needed
            ulong maxCapacity = 1000000000000;
            ulong answer = 0;
            while (minFuel <= maxFuel)
            {
                var mid = (minFuel + maxFuel) / 2;

                var oreRequired = calculateOreRequired(mid);
    
                if (oreRequired >= maxCapacity)
                {
                    maxFuel = mid - 1;
                }
                else
                {
                    answer = mid;
                    minFuel = mid + 1;
                }
            }
            return answer;
        }

        public Solution(string input)
        {
            foreach (string line in input.Split('\n'))
            {
                var parts = line.Trim().Split(new[] { " => " }, StringSplitOptions.RemoveEmptyEntries);
                var product = getQuantityAndName(parts[1]);
                if (!graph.ContainsKey(product.Item2))
                {
                    graph.Add(product.Item2, new Vertex());
                }
                graph[product.Item2].quantityObtained = product.Item1;

                var reactants = parts[0].Split(new[] { ", " }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var reactant in reactants)
                {
                    var quantityAndName = getQuantityAndName(reactant);
                    if (!graph.ContainsKey(quantityAndName.Item2))
                    {
                        graph.Add(quantityAndName.Item2, new Vertex());
                    }
                    graph[product.Item2].edges.Add(new Edge(quantityAndName.Item1, quantityAndName.Item2));
                }
            }
            foreach (var vertex in graph)
            {
                if (vertex.Value.status == Status.NOT_VISITED)
                {
                    dfs(vertex.Key);
                }
            }
            sorted.Reverse();
            Console.WriteLine();
            Console.WriteLine("FUEL able to produce: {0}", binarySearch());
        }
    }
}
