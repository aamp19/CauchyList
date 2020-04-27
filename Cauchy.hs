module Cauchy where
    import Control.Applicative
    data CauchyList = CauchyList Int [Int] 
    
    instance Eq CauchyList where
        -- Implement Eq type class here
        (CauchyList p a) == (CauchyList p2 b) = (a==b) && (p==p2)

    instance Num CauchyList where
        -- Implement Num type class here
        (CauchyList p a) + (CauchyList p2 b) = 
            if (length a == length b) then CauchyList p (map (`mod` p)(zipWith (+) a b)) --checks if list a and list b are equal in length, if they are, add both lists and mod the result with the p value
            else if (length a > length b) then (CauchyList p a) + (CauchyList p2 (b++[0])) --checks if list a is larger that list b in terms of length, if this is true, add 0's to list b
            else (CauchyList p (a++[0])) + (CauchyList p2 b) --otherwise add 0's to list a
       
        (CauchyList p a) - (CauchyList p2 b) = 
            if (length a == length b) then CauchyList p (map (`mod` p)(zipWith (-) a b))--checks if list a and list b are equal in length, if they are, subtract both lists and mod the result with the p value
            else if (length a > length b) then (CauchyList p a) - (CauchyList p2 (b++[0])) --checks if list a is larger that list b in terms of length, if this is true, add 0's to list b
            else (CauchyList p (a++[0])) - (CauchyList p2 b)--otherwise add 0's to list a
        
        (CauchyList p a) * (CauchyList p2 b) =
            if (length a == length b) then CauchyList p (map (`mod` p)(zipWith (+) a (zipWith (*) a (reverse b)))) --checks if list a and list b are qual in length, if they are, mutlipy list a with the reverse of list b, and then add it with the rest of the elements of list a
            else if (length a > length b) then (CauchyList p a) * (CauchyList p2 (b++[0]))--checks if list a is larger that list b in terms of length, if this is true, add 0's to list b
            else (CauchyList p (a++[0])) * (CauchyList p2 b)--otherwise add 0's to list a
    instance Show CauchyList where
    --     -- Implement Show type class here
        show (CauchyList p a) = 
            "p: " ++ (show p) ++ "\nlength: " ++ show((length a)) ++ "\ncontent: " ++ (show a) --this shows the values for p, the length of the list and the result after the operation
