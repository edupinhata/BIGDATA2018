
delta a b c = sqrt( b^2 - (4*a*c) )

f a b c = ((((-b) + delta a b c)/(2*a)), (((-b) - delta a b c)/(2*a)))

main = do
  print( f 2.0 1.0 4.0)
