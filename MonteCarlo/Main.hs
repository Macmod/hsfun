import MonteCarlo

main = do
    area <- areaMCIO (\x -> x^2 - sin(10*x)) (-2, 2) (0, 4) 10000
    putStrLn $ "Área (x^2, x=(-2, 2), y=(0, 4), t=10000): " ++ show area

    area <- extremeMCIO (\x -> x^2 + 3*sin(10*x)) (-2, 2) Maximum 10000
    putStrLn $ "Máximo (x^2 + 3*sin(10*x), x=(-2, 2), t=10000): " ++ show area
