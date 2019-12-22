using System;
using System.Collections.Generic;

namespace day_14
{
    class Day14
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
        public int quantity { get; }
        public string target { get; }

        public Edge(int quantity, string target)
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
        public int quantityObtained;
        public int quantityReuqired = 0;
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

        Tuple<Int32, string> getQuantityAndName(string substance)
        {
            var parts = substance.Split(' ');
            return new Tuple<Int32, string>(Convert.ToInt32(parts[0]), parts[1]);
        }

        public Solution(string input)
        {
            foreach (string line in input.Split('\n'))
            {
                var parts = line.Trim().Split(new[] { " => " }, StringSplitOptions.RemoveEmptyEntries);
                var product = getQuantityAndName(parts[1]);
                if (!graph.ContainsKey(product.Item2)){
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
            Console.WriteLine("Sorted:");
            foreach (var vertex in sorted)
            {
                Console.Write("{0}, ", vertex);
            }
            Console.WriteLine();
            graph["FUEL"].quantityReuqired = 1;
            foreach (var vertex in sorted)
            {
                foreach (var edge in graph[vertex].edges)
                {
                    int numberOfReactions = (int)Math.Ceiling(((float)graph[vertex].quantityReuqired) / ((float)graph[vertex].quantityObtained));
                    graph[edge.target].quantityReuqired += edge.quantity * numberOfReactions;
                }
            }
            Console.WriteLine("ORE required: {0}", graph["ORE"].quantityReuqired);
        }
    }
}
