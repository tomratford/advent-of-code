
var LEFT = image.Point{1, 0}
var RIGHT = image.Point{-1, 0}
var DOWN = image.Point{0, 1}
var UP = image.Point{0, -1}

var BOUNDS image.Rectangle

func Dijkstra(graph map[image.Point]int, source image.Point, target image.Point) (map[image.Point]int, map[image.Point]image.Point) {

	dist := make(map[image.Point]int, len(graph))
	prev := make(map[image.Point]image.Point, len(graph))

	Q := make([]image.Point, 0, len(graph))

	for v := range graph {
		dist[v] = 999_999_999
		Q = append(Q, v)
	}
	dist[source] = 0

	for len(Q) > 0 {

		// u ← vertex in Q with minimum dist[u]
		var u image.Point
		mindist := -1
		for _, k := range Q {
			v, ok := dist[k]
			if ok && (v < mindist || mindist == -1) {
				mindist = v
				u = k
			}
		}

		i := slices.Index(Q, u)
		if i == -1 {
			fmt.Println(Q, u)
		}
		if i+1 >= len(Q) {
			Q = Q[:i]
		} else {
			Q = append(Q[:i], Q[i+1:]...)
		}

		neighbours := getNeighbours(u, Q)
		// for each neighbor v of u still in Q
		for _, v := range neighbours {
			if alt := dist[u] + graph[v]; alt < dist[v] {
				dist[v] = alt
				prev[v] = u
			}
		}
	}

	return dist, prev
}

func getNeighbours(u image.Point, Q []image.Point) {
	neighbours := []image.Point{
		u.Add(LEFT),
		u.Add(RIGHT),
		u.Add(DOWN),
		u.Add(UP),
	}
	neighbours = slices.DeleteFunc(neighbours, func(n image.Point) bool {
		return slices.Contains(Q, n) && !n.In(BOUNDS)
	})
	return neighbours
}
