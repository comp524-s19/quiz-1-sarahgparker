finalGrade :: (Integral a) => [a] -> [a] -> a

finalGrade grades weights = gradesum `div` weightsum
  where gradesum = sum [fst(x) * snd(x) | x <- (zip grades weights)]
        weightsum = sum weights    
