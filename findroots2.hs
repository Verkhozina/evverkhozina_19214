 findroots 0 0 0 = error "Infinity"
 findroots 0 0 с = error "No roots"
 findroots 0 b c = (-c/b, -c/b) 
 findroots a b c = if d < 0 
                     then error "No roots"
                     else (x1, x2)
                     where x1 = (-b + sqrt d) / (2 * a)
                           x2 = (-b - sqrt d) / (2 * a)
                           d = b * b - 4 * a * c