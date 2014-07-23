isPrime :: Integer -> Bool
isPrime 0 = True
isPrime 1 = True
isPrime num = isPrime' num 2
	where
		isPrime' num divisor 
			| divisor < num = if (num `mod` divisor == 0)
								then False
								else isPrime' num (divisor+1)
			| otherwise 	= True
