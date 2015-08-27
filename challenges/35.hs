primeFactors :: Int -> [Int]
primeFactors num = filter test [2..num]
	where test a = if ((isPrime a) && (num `mod` a) == 0)
						then True
						else False

isPrime :: Int -> Bool
isPrime 0 = True
isPrime 1 = True
isPrime num = isPrime' num 2
	where
		isPrime' num divisor 
			| divisor < num = if (num `mod` divisor == 0)
								then False
								else isPrime' num (divisor+1)
			| otherwise 	= True
