sigmoid:: Floating a => a -> a
sigmoid x = 1.0/(1.0 + exp(-x))
neuron_out:: Floating a => (a -> a) -> [a] -> [a] -> a -> Int -> a
neuron_out function weights inputs bias num_ins = function ( sum [weights!!i * inputs!!i + bias | i <- [0..(num_ins-1)]])
