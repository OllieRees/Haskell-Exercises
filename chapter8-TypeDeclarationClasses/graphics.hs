-- From Haskell for Mac's tutorial page (not Graham Hutton's book).

type PointType = (Float, Float)
type VectorType = (Float, Float)

-- We can represent a Vector by (Float, Float), despite it being nameable as Vector.
-- Move a point by a vector.
movePointN' :: Float -> VectorType -> PointType -> PointType
movePointN' scalar (vx, vy) (x, y) = (scalar * vx + x, scalar * vy + y)

-- To stop CONFUSION BETWEEN POINT AND VECTOR - both are (Float, Float) - we can define them as DATA TYPES with Parameters (PRODUCT TYPES)
data Point = Point Float Float deriving (Show, Eq)
data Vector = Vector Float Float deriving (Show, Eq)
data Line = Line Point Point deriving (Show, Eq)
data Colour = Colour Int Int Int Int deriving (Show, Eq) -- RGB + Opacity
data LineStyle = Solid | Dotted | Dashed deriving (Show, Eq) 
data FillStyle = NoFill | SolidFill deriving (Show, Eq) 

-- As a result of having separate constructors for Point and Vector our method won't get confused
movePointN :: Float -> Vector -> Point -> Point
movePointN scalar (Vector vx vy) (Point x y) = Point (scalar * vx + x) (scalar * vy + y)

-- But, as a result, we can't simply turn a point to a vector: we need a method...
pointToVector :: Point -> Vector
pointToVector (Point x y) = Vector x y

-- Define some colours
red, green, blue :: Colour
red = Colour 255 0 0 255
green = Colour 0 255 0 255
blue = Colour 0 0 255 255

-- Picture Object Type : SUM TYPE since at least 1 Data Constructor is parameterised. 
data PictureObject = 
    Path {points :: [Point], colour :: Colour, lineStyle :: LineStyle} 
    | Circle {centre :: Point, radius :: Float, colour :: Colour, lineStyle :: LineStyle, fillStyle :: FillStyle} 
    | Ellipse {centre :: Point, length :: Float, width :: Float, xAxisRotation :: Float, colour :: Colour, lineStyle :: LineStyle, fillStyle :: FillStyle}
    | Polygon {vertices :: [Point], colour :: Colour, lineStyle :: LineStyle, fillStyle :: FillStyle}
    deriving (Show, Eq) 

-- Data Constructors can be curried 
-- Example Path (produces red lambda sign)
myPath :: PictureObject
myPath = Path [Point 210 200, Point 270 200, Point 545 600, Point 525 600, Point 380 390, Point 250 600, Point 230 600, Point 370 380, Point 260 215, Point 210 215] red Solid

-- Example Circles
dashedCircle, dottedCircle, solidCircle :: PictureObject
dashedCircle = Circle (Point 400 400) 180 blue Dashed NoFill 
dottedCircle = Circle (Point 400 400) 90 green Dotted NoFill
solidCircle = Circle (Point 400 400) 20 red Solid SolidFill 

-- Example Ellipses
redEllipse, greenEllipse, blueEllipse :: PictureObject
redEllipse = Ellipse (Point 400 400) 300 100 0 red Solid SolidFill
greenEllipse = Ellipse (Point 400 400) 300 100 (pi/4) green Solid SolidFill
blueEllipse = Ellipse (Point 400 400) 300 100 (pi/2) blue Solid SolidFill

-- Since PictureObject defines a geometric shape we can define a picture being a list of PictureObjects
type Picture = [PictureObject]

simpleEllipsePic :: Float -> Picture
simpleEllipsePic n = map (\angle -> Ellipse (Point 400 400) 250 70 angle green Dashed SolidFill) [0, pi/n .. (n - 1) * pi/n]

movePoint :: Vector -> Point -> Point
movePoint (Vector vx vy) (Point x y) = movePointN 1 (Vector vx vy) (Point x y)

-- Transformations using pattern matching since Path and Polygons have multiple points to move
-- Note that Path, Polygon and Circle/Ellipse have different implementations since their point parameter is named differently : partial projection.
movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vector picObj@Path{} = picObj{points = map (movePoint vector) $ points picObj} 
movePictureObject vector picObj@Polygon{} = picObj{vertices = map (movePoint vector) $ vertices picObj}
movePictureObject vector picObj = picObj{centre = movePoint vector $ centre picObj}

rotatePoint :: Float -> Point -> Point -> Point
rotatePoint alpha (Point x0 y0) (Point x y) 
    = Point ( cos alpha * nx - sin alpha * ny + x0) (sin alpha * nx + cos alpha * ny + y0)
    where 
        nx = x - x0
        ny = y - y0
        
-- More examples of using pattern matching and referencing the TYPE RECORDS so that we only deal with the parameters that change
rotatePictureObject :: Float -> Point -> PictureObject -> PictureObject
rotatePictureObject alpha pnt picObj@Path{} = picObj{ points = map (rotatePoint alpha pnt) $ points picObj }
rotatePictureObject alpha pnt picObj@Polygon{} = picObj{ vertices = map (rotatePoint alpha pnt) $ vertices picObj }
rotatePictureObject alpha pnt picObj@Circle{} = picObj{ centre = rotatePoint alpha pnt $ centre picObj }
rotatePictureObject angle point picObj@Ellipse{} 
    = picObj {centre = rotatePoint angle point $ centre picObj, 
    xAxisRotation = xAxisRotation picObj + angle }

-- Let's define a method which we can rotate a Picture Object to create a Picture
magnetaPoly :: PictureObject
magnetaPoly = Polygon [Point 430 400, Point 500 420, Point 680 400, Point 500 380] magneta Solid SolidFill
        where
            magneta = Colour 153 153 0 130

rotatePic :: Int -> PictureObject -> Picture
rotatePic n obj = map (\n -> rotatePictureObject (0.2 * fromIntegral n) (Point 400 400) obj) [0..n]

-- Rotates a shape by a given angle around a given point, with the point moving by the given vector with each picture. Note that the Int param. is number of iterations.
dynamicRotate :: PictureObject -> Point -> Vector -> Float -> Int -> Picture
dynamicRotate picObj _ _ _ 0 = [picObj]
dynamicRotate picObj initPnt dirVec angle n = rotatePicObj : dynamicRotate (movePictureObject dirVec rotatePicObj) (movePoint dirVec initPnt) dirVec angle (n - 1)
    where
        rotatePicObj = rotatePictureObject angle initPnt picObj

-- Data Constructors differ from functions since we can see (unwrap) the arguments/properties of a data constructor...
showProperties :: PictureObject -> String
showProperties pic 
        = case pic of 
                Path points colour lineStyle 
                    -> "The vertices of the path are " ++ show points ++ 
                    "; the colour of the path is " ++ show colour ++
                    "; the line style of the path is " ++ show lineStyle ++ "."
                Circle centre radius colour lineStyle fillStyle
                    -> "The centre of the circle is " ++ show centre ++
                    "; the radius of the circle is " ++ show radius ++
                    "; the colour of the circle is " ++ show colour ++ 
                    "; the line style and the fill style of the circle is " ++ show lineStyle ++ show fillStyle ++ "."
                Ellipse centre length height xAxisRotation colour lineStyle fillStyle
                    -> "The centre of the ellipse is " ++ show centre ++ 
                    "; the length and height (respectively) of the ellipse is " ++ show length ++ show height ++ 
                    "; the rotation of the ellipse around its centre is " ++ show xAxisRotation ++
                    "; the colour of the ellipse is " ++ show colour ++
                    "; the line style and fill style of the ellipse is " ++ show lineStyle ++ show fillStyle ++ "."
                Polygon vertices colour lineStyle fillStyle 
                    -> "The vertices of the polygon are " ++ show vertices ++ 
                    "; the colour of the polygon is " ++ show colour ++
                    "; the line and fill style of the polygon is " ++ show lineStyle ++ show fillStyle ++ "." 
