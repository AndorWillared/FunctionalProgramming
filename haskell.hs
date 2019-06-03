data Matrix a = Matrix [[a]] deriving (Eq, Show)

addMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
addMatrix = zipWith (zipWith (+))

mulMatrixHelp m1 m2 zw = 
mulMatrix m1 m2 = []

instance Num a => Num (Matrix a) where 
    (Matrix m1) + (Matrix m2) = Matrix (addMatrix m1 m2)
    (Matrix m1) * (Matrix m2) = Matrix (mulMatrix m1 m2)
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    
sigmoid:: Floating a => a -> a
sigmoid x = 1.0/(1.0 + exp(-x))

iterateLists :: Floating a => [a] -> [a] -> [(a,a)]
iterateLists (a:ax) (b:bx)= (a,b) : iterateLists ax bx
iterateLists _ _ = []

neuron_out :: Floating a => [a] -> [a] -> a -> a -- -b or +b?
neuron_out input weights bias = sum [ sigmoid (i*w-bias) | (i,w) <- (iterateLists input weights)]

--do_forward_pass :: Floating a => 
do_forward_pass input weights biases = []

forward_pass :: Floating a => [a] -> [a] -> [a] -> [[a]] -- w = weights of one layer, ws = weights of other layers
forward_pass input (w:ws) (b:bs) = do_forward_pass input w b ++ forward_pass (do_forward_pass input w b) ws bs
forward_pass input _ _ = []