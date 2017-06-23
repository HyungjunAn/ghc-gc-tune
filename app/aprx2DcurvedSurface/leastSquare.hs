import Data.Matrix
import Data.Either

minA = 16384
minH = 1048576

arr1 = [(minA, minH, 26.176),
        (minA, minH * 2^10, 16.497),
        (minA * 2^15, minH, 15.922),
        (minA * 2^14, minH * 2^10, 18.019),
        (minA * 2^7, minH * 2^5, 13.049)] :: [(Rational, Rational, Rational)]

mm = foldr f ([],[]) arr1
    where
        f = \(a, h, t) (xs, ys) -> ([a^2, a, h^2, h, 1] : xs , [t] : ys)

ma = fromLists (fst mm)
me = fromLists (snd mm)

ms = concat (toLists (leastSquare ma me))

findMin :: [Rational] -> (Rational, Rational)
findMin [a, b, c, d, e] = (-b/(2*a), -d/(2*c))
findMin _ = (0,0)






m = fromLists ([[1,1,1,1,1],
                [1,1,25,5,1],
                [25,5,1,1,1],
                [9, 3, 16, 4,1], 
                [1,-1,1,-1,1]] :: [[Rational]])
e = fromLists ([[2],
                [26],
                [26],
                [25],
                [2]] :: [[Rational]])

m1 = fromLists ([[4.0, 2.0, 1.0], [4.0, -2.0, 1.0], [9.0, 3.0, 1.0]] :: [[Rational]])
m2 = fromLists ([[4.0], [4.0], [9.0]] :: [[Rational]])

leastSquare :: Matrix Rational -> Matrix Rational -> Matrix Rational
leastSquare a b = i_ata * at * b
            where
                at = transpose a
                Right i_ata = inverse (at * a)






