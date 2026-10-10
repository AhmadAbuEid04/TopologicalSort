

(* =========================================================
   Topological Sort with Cycle Detection in SML
   CS 31600 - Final Project
   Compatible with SML '97
   ========================================================= *)


(* ---------------------------------------------------------
   RESULT TYPE
   Represents either a valid topological order
   or a detected cycle path
   --------------------------------------------------------- *)

datatype Result =
    TopologicalOrder of string list
  | CycleDetected of string list


(* ---------------------------------------------------------
   UTILITY HELPERS
   (SML '97 safe - no String.concatWith or List.find)
   --------------------------------------------------------- *)

(* Join a string list with a separator -- replaces String.concatWith *)
fun joinWith (sep : string) (lst : string list) : string =
    case lst of
        []      => ""
      | [x]     => x
      | x :: rest => x ^ sep ^ joinWith sep rest

(* Find first element matching predicate -- replaces List.find *)
fun findInList (pred : 'a -> bool) (lst : 'a list) : 'a option =
    case lst of
        []      => NONE
      | x :: rest =>
            if pred x
            then SOME x
            else findInList pred rest


(* ---------------------------------------------------------
   COLOR HELPERS
   Colors are represented as integers:
     0 = White (unvisited)
     1 = Gray  (in progress)
     2 = Black (finished)
   Stored as an association list: (string * int) list
   --------------------------------------------------------- *)

(* Get the color of a vertex; default White (0) if not found *)
fun getColor (v : string) (colorList : (string * int) list) : int =
    case findInList (fn (name, _) => name = v) colorList of
        SOME (_, c) => c
      | NONE        => 0

(* Return a new color list with vertex v set to color c *)
fun setColor (v : string) (c : int) (colorList : (string * int) list)
    : (string * int) list =
    case colorList of
        [] => [(v, c)]
      | (name, old) :: rest =>
            if name = v
            then (name, c) :: rest
            else (name, old) :: setColor v c rest


(* ---------------------------------------------------------
   GRAPH HELPERS
   --------------------------------------------------------- *)

(* Return all vertices that u points to *)
fun neighbors (u : string) (edges : (string * string) list) : string list =
    case edges of
        [] => []
      | (src, dst) :: rest =>
            if src = u
            then dst :: neighbors u rest
            else neighbors u rest

(* Extract the cycle subpath from path starting at vertex v *)
fun extractCycle (v : string) (path : string list) : string list =
    case path of
        [] => [v]
      | x :: rest =>
            if x = v
            then [v]
            else extractCycle v rest @ [x]


(* ---------------------------------------------------------
   DFS-VISIT (recursive core)
   Returns: (updated colorList, Result)
   --------------------------------------------------------- *)

fun dfsVisit
    (u      : string)
    (edges  : (string * string) list)
    (colors : (string * int) list)
    (order  : string list)
    (path   : string list)
    : (string * int) list * Result =

    let val colors1 = setColor u 1 colors
        val path1   = path @ [u]
    in
        processNeighbors u (neighbors u edges) edges colors1 order path1
    end

(* Process each neighbor of u one at a time *)
and processNeighbors
    (u      : string)
    (nbrs   : string list)
    (edges  : (string * string) list)
    (colors : (string * int) list)
    (order  : string list)
    (path   : string list)
    : (string * int) list * Result =

    case nbrs of
        (* No more neighbors -- u is finished, prepend to order *)
        [] =>
            let val colors2 = setColor u 2 colors
            in
                (colors2, TopologicalOrder (u :: order))
            end

      | (v :: rest) =>
            let val vColor = getColor v colors
            in
                if vColor = 0 then
                    (* White: recurse into v *)
                    let val (colors2, result) =
                            dfsVisit v edges colors order path
                    in
                        case result of
                            CycleDetected c =>
                                (colors2, CycleDetected c)
                          | TopologicalOrder newOrder =>
                                processNeighbors u rest edges colors2 newOrder path
                    end

                else if vColor = 1 then
                    (* Gray: back edge found -- cycle detected *)
                    let val cycle = extractCycle v path
                    in
                        (colors, CycleDetected cycle)
                    end

                else
                    (* Black: already finished, skip *)
                    processNeighbors u rest edges colors order path
            end


(* ---------------------------------------------------------
   OUTER DFS LOOP
   Iterates over all vertices; calls dfsVisit on unvisited ones
   --------------------------------------------------------- *)

fun dfsAll
    (vertices : string list)
    (edges    : (string * string) list)
    (colors   : (string * int) list)
    (order    : string list)
    : Result =

    case vertices of
        [] => TopologicalOrder order

      | (u :: rest) =>
            if getColor u colors = 0 then
                let val (colors2, result) =
                        dfsVisit u edges colors order []
                in
                    case result of
                        CycleDetected c => CycleDetected c
                      | TopologicalOrder newOrder =>
                            dfsAll rest edges colors2 newOrder
                end
            else
                dfsAll rest edges colors order


(* ---------------------------------------------------------
   TOP-LEVEL ENTRY
   --------------------------------------------------------- *)

fun topoSort
    (vertices : string list)
    (edges    : (string * string) list)
    : Result =

    let val initColors = map (fn v => (v, 0)) vertices
    in
        dfsAll vertices edges initColors []
    end


(* ---------------------------------------------------------
   OUTPUT
   --------------------------------------------------------- *)

fun printResult (result : Result) : unit =
    case result of
        TopologicalOrder order =>
            ( print "Topological Order:\n"
            ; print (joinWith " -> " order)
            ; print "\n"
            )
      | CycleDetected cycle =>
            ( print "Cycle Detected:\n"
            ; print (joinWith " -> " cycle)
            ; print "\n"
            )


(* ---------------------------------------------------------
   TEST CASES
   --------------------------------------------------------- *)

(* Test 1: Acyclic graph
   Graph:  A -> B -> D
           A -> C -> D
   Expected: valid topological order ending with D
*)
val _ = print "\n--- Test 1: Acyclic Graph ---\n"
val vertices1 = ["A", "B", "C", "D"]
val edges1    = [("A","B"), ("A","C"), ("B","D"), ("C","D")]
val _         = printResult (topoSort vertices1 edges1)


(* Test 2: Graph with a cycle
   Graph:  A -> B -> C -> A
   Expected: cycle detected
*)
val _ = print "\n--- Test 2: Graph with Cycle ---\n"
val vertices2 = ["A", "B", "C"]
val edges2    = [("A","B"), ("B","C"), ("C","A")]
val _         = printResult (topoSort vertices2 edges2)


(* Test 3: Single vertex, no edges
   Expected: topological order with just X
*)
val _ = print "\n--- Test 3: Single Vertex ---\n"
val vertices3 = ["X"]
val edges3    = [] : (string * string) list
val _         = printResult (topoSort vertices3 edges3)


(* Test 4: Disconnected graph -- two separate components
   Graph:  A -> B    C -> D
   Expected: valid topological order covering all 4 vertices
*)
val _ = print "\n--- Test 4: Disconnected Graph ---\n"
val vertices4 = ["A", "B", "C", "D"]
val edges4    = [("A","B"), ("C","D")]
val _         = printResult (topoSort vertices4 edges4)


(* Test 5: Goalie equipment example from the slides *)
val _ = print "\n--- Test 5: Goalie Equipment (from slides) ---\n"
val vertices5 = ["socks","hose","shorts","pants","skates","legpads",
                 "tshirt","chestpad","sweater","mask","catchglove",
                 "blocker","battingglove"]
val edges5    = [ ("socks","hose"),       ("hose","legpads"),
                  ("shorts","pants"),     ("pants","legpads"),
                  ("skates","legpads"),   ("tshirt","chestpad"),
                  ("chestpad","sweater"), ("sweater","mask"),
                  ("catchglove","blocker"),
                  ("battingglove","catchglove"),
                  ("battingglove","blocker") ]
val _         = printResult (topoSort vertices5 edges5)