# (node, weight)

adjList = {
    1: [(2, 2), (3, 4)],
    2: [(4, 7), (3, 1)],
    3: [(5, 3)],
    4: [(6, 1)],
    5: [(2, 4), (6, 5)],
    6: [],
}

import heapq

distances = [float('inf')] * (len(adjList.keys()) + 1)
traveledFrom = [0] * (len(adjList.keys()) + 1)

emptyPQ = []

distances[1] = 0


heapq.heappush(emptyPQ, (0, 1))

while len(emptyPQ) != 0:

    minDistanceVertex = heapq.heappop(emptyPQ)

    vertexCost, vertex = minDistanceVertex

    # for all adjacencies 
    for neighborV, pathWeight in adjList[vertex]:

        distanceCost = vertexCost + pathWeight

        if distanceCost < distances[neighborV]:
            distances[neighborV] = distanceCost
            traveledFrom[neighborV] = vertex
            heapq.heappush(emptyPQ, (distanceCost, neighborV))

print(distances)
print(traveledFrom)
