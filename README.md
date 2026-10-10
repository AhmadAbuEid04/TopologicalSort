Topological Sort with Cycle Detection in Standard ML
A purely functional implementation of topological sorting in Standard ML (SML). It uses depth-first search with three-color vertex marking, and when the graph contains a cycle it reports the cycle instead of an ordering.
How it works
Each vertex is colored during the DFS:

Color
Value
Meaning
White
0
Not yet visited
Gray
1
Currently being explored
Black
2
Finished


Visiting a white neighbor recurses into it.
Reaching a gray neighbor means a back edge, so a cycle exists. The cycle is extracted from the current DFS path.
Reaching a black neighbor is skipped, since it is already finished.
When a vertex has no more neighbors to explore, it is marked black and prepended to the result. After all vertices are processed, the list is a valid topological order.

The function returns a Result:

datatype Result =

    TopologicalOrder of string list

  | CycleDetected of string list

No mutable state or library helpers like List.find are used. Colors are kept in an association list and passed through the recursion.
Usage
topoSort vertices edges

vertices: a string list of all vertex names
edges: a (string * string) list, where ("A","B") means A must come before B

Example:

val result = topoSort ["A","B","C","D"]

                      [("A","B"), ("A","C"), ("B","D"), ("C","D")]

val _ = printResult result
Running
Requires an SML implementation such as SML/NJ.

sml topological.sml

The file runs five built-in tests when loaded:

Acyclic diamond graph
Graph with a cycle (A → B → C → A)
Single vertex, no edges
Disconnected graph with two components
Goalie equipment dressing order
Sample output
--- Test 1: Acyclic Graph ---

Topological Order:

A -> C -> B -> D

--- Test 2: Graph with Cycle ---

Cycle Detected:

A -> B -> C -> A

--- Test 3: Single Vertex ---

Topological Order:

X

--- Test 4: Disconnected Graph ---

Topological Order:

C -> D -> A -> B

--- Test 5: Goalie Equipment (from slides) ---

Topological Order:

battingglove -> catchglove -> blocker -> tshirt -> chestpad -> sweater -> mask -> skates -> shorts -> pants -> socks -> hose -> legpads

Any valid ordering is correct for a topological sort, so the order of independent vertices may differ between implementations.
Complexity
The neighbor lookup scans the full edge list, and colors are stored in a list, so this version favors clarity over speed. It runs in roughly O(V · E) time. Swapping in a map or array for adjacency and colors would bring it down to O(V + E).
Files
topological.sml: implementation and test cases

