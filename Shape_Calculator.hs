constPi :: Double
constPi = 3.141592

data Point = Point {x:: Double, y:: Double, z:: Double} deriving Show

--data Ball = Ball {center:: Point, radius:: Double }
--data Cube = Cube {anchor:: Point, side_length:: Double}
--data Cuboid = Cuboid {anchor_:: Point, length:: Double, width:: Double, height:: Double }

data Shape =    Ball {center:: Point, radius:: Double } 
                        | Cylinder {center:: Point, radius:: Double, height:: Double}
                        | Cube {anchor:: Point, length:: Double} 
                        | Cuboid {anchor:: Point, length:: Double, width:: Double, height:: Double }

anchorPoint:: Shape -> Point
anchorPoint a = case a of
    (Ball c r) -> c 
    (Cylinder c r h) -> c
    (Cube a l) -> a 
    (Cuboid a l w h) -> a 

volume :: Shape -> Double
volume (Ball c r) = (4/3)*constPi*r*r*r
volume (Cylinder c r h) = constPi*h*r*r
volume (Cube a l) = l*l*l 
volume (Cuboid a l w h) = l*w*h 

area:: Shape -> Double
area (Ball _ r) = 4*constPi *r*r
area (Cylinder _ r h) = 2*constPi*r*(r+h)
area (Cube _ l) = 6*l*l
area (Cuboid _ l w h) = 2*(l*w + l*h + w*h)

addCuboidCorner:: Point -> Double -> Double -> Double -> Double -> [Point]
addCuboidCorner point 0 _ _ _ = [point]
addCuboidCorner (Point x y z) 1 l w h = [Point (x + l) y z, Point x (y+w) z, Point x y (z+h)]
addCuboidCorner (Point x y z) 2 l w h = [Point (x + l) (y+w) z, Point x (y+w) (z+h), Point (x+l) y (z+h)]
addCuboidCorner (Point x y z) 3 l w h= [Point (x + l) (y+w) (z+h)]

corners:: Shape -> [Point]
corners (Ball c r) = []
corners (Cylinder c r h) = []
corners (Cube point l) = concatMap helper [0,1,2,3] where
                                    helper x = addCuboidCorner point x l l l
corners (Cuboid point l w h) = concatMap helper [0,1,2,3] where
                                    helper x = addCuboidCorner point x l w h

cornerTuple:: [Point] -> [(Double,Double,Double)]
cornerTuple [] = []
cornerTuple (point : xs) = foldr (\ point -> (++) [(x point, y point, z point)]) [] xs


main:: IO()
main = do  
    let test_point = Point 0 0 0
    let test_ball = Ball test_point 1
    let test_cylinder = Cylinder test_point 4 13
    let test_cube = Cube test_point 2
    let test_cuboid = Cuboid test_point 2 4 6

    print(anchorPoint test_ball)

    print "Cube"
    print(volume test_cube)
    print(area test_cube)
    print(corners test_cube)

    print "Cylinder"
    print(volume test_cylinder)
    print(area test_cylinder)
    print(corners test_cylinder)
    print(cornerTuple(corners test_cylinder))

    print "Ball"
    print(volume test_ball)
    print(area test_ball)
    print(corners test_ball)

    print "Cuboid"
    print(volume test_cuboid)
    print(area test_cuboid)
    print(corners test_cuboid)
    print(cornerTuple(corners test_cuboid))
