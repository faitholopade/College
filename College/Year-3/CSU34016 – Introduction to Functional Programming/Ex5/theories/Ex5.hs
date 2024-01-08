module Ex5 where
-- no code for Q1


-- for Q2:
frec x  =  if x <= 5 then 9 else x * frec (x `div` 10)



-- for Q3:
bonus []      =  16
bonus (x:xs)  =  x + 15 + bonus xs




--for Q4:
casef x
  | x < 5   =  2*x
  | x >= 5  = 2*x-1


