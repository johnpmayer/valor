
module Modules where

import Graphics.Collage as C
import Graphics.Matrix as M

data Tree leaf node tag
  = Leaf leaf
  | Node node [(tag, Tree leaf node tag)]

foldTree : 
  (leaf -> a) -> 
  (node -> [a] -> a) -> 
  (tag -> a -> a) ->
  Tree leaf node ->
  a
foldTree fLeaf fNode fTag tree =
  case tree of
    Leaf leaf -> fLeaf leaf
    Node node roses -> 
      fNode node $ map (\(tag,sub) -> fTag tag $ foldTree fLeaf fNode fTag sub) roses

data Part 
  = Engine { radius : Float }
  | FuelTank { width : Float, length : Float }
  | Habitat { radius : Float }

type Beam = { length : Float }

type Attach = { offset : Float, angle : Float }

type Structure = Tree Part Beam Attach

fuelPod : Structure
fuelPod = Node { length = 24 } $
  [
    (
      { offset = 12, angle = 0 },
      Leaf $ FuelTank { length = 16, width = 5 }
    )
  ]

cockpit : Structure
cockpit = Leaf $ Habitat { radius = 10 }

mainEngine : Structure
mainEngine = Leaf $ Engine { radius = 16 }

basicStructure : Structure
basicStructure = Node { length = 60 } 
  [
    ({offset=0,angle=0}, cockpit),
    ({offset=10,angle=(pi/2)},fuelPod),
    ({offset=20,angle=(pi/2)},fuelPod),
    ({offset=30,angle=(pi/2)},fuelPod),
    ({offset=40,angle=(pi/2)},fuelPod),
    ({offset=10,angle=(0-pi/2)},fuelPod),
    ({offset=20,angle=(0-pi/2)},fuelPod),
    ({offset=30,angle=(0-pi/2)},fuelPod),
    ({offset=40,angle=(0-pi/2)},fuelPod),
    ({offset=55,angle=0},mainEngine)
  ]

type Ship = { 
  x : Float, 
  y : Float, 
  t : Float,
  s : Structure }

drawShip : Ship -> Form
drawShip ship = 
  groupTransform (M.move ship.x ship.y $ M.rotate ship.t M.identity) [drawStructure ship.s]

drawStructure : Structure -> Form
drawStructure = foldTree drawPart drawBeam posAttachment 

drawPart : Part -> Form
drawPart part = case part of
  Engine meta -> 
    C.rotate (0-pi/2) $ filled red $ ngon 3 meta.radius
  FuelTank meta -> 
    filled green $ rect meta.width meta.length
  Habitat meta ->
    filled blue $ circle meta.radius

posAttachment : Attach -> Form -> Form
posAttachment attach f =
  groupTransform (M.rotate attach.angle $ M.move 0 attach.offset M.identity) [f]

drawBeam : Beam -> [Form] -> Form
drawBeam beam fSubs = 
  let wBeam = 0.1 * beam.length
      fBeam = C.move 0 (0 - (beam.length / 2.0)) $
        filled gray $ rect wBeam beam.length
  in group $ fBeam :: fSubs

