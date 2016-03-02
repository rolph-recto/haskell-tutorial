import Data.List (sort)

type Graph = ([String], [(String,String)]) -- (nodes, edges)

toposort :: ([String], [(String,String)]) -> Maybe [String]
toposort ([], edges) = Just []
toposort g@(nodes, edges) =
  let candidates = zeroIndegreeNodes g in
  if length candidates == 0
  then Nothing -- cycle!
  else do
    let chosen = chooseNode candidates
    let g' = removeNode chosen g
    tl <- toposort g'
    return (chosen : tl)

zeroIndegreeNodes :: Graph -> [String]
zeroIndegreeNodes (nodes,edges) = filter noIncoming nodes
  where noIncoming node = not (any (\(src,dest) -> dest == node) edges)

chooseNode :: [String] -> String
chooseNode nodes = head (sort nodes)

removeNode :: String -> Graph -> Graph
removeNode node (nodes,edges) = (nodes', edges')
  where nodes' = filter (not . (node ==)) nodes
        edges' = filter (not . (node ==) . fst) edges
