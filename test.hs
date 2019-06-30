import System.Random

randomChar :: Int->Char
randomChar x =  ['a'..'z'] !! (mod x 26) 


main :: IO ()
main = do
    num1 <- randomIO :: IO Int
    num2 <- randomIO :: IO Int
    num3 <- randomIO :: IO Int
    num4 <- randomIO :: IO Int
    num5 <- randomIO :: IO Int
    num6 <- randomIO :: IO Int
    num7 <- randomIO :: IO Int
    print $ [randomChar num1,randomChar num2,randomChar num3,randomChar num4,randomChar num5,randomChar num6,randomChar num7]
