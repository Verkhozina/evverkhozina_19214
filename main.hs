function :: (Float, Float, Float) -> (Float, Float)

function (a,b,c) = if d < 0 then error "No roots" else (x1, x2)
  where x1 = (-b + sqrt d) / (2 * a)
	x2 = (-b - sqrt d) / (2 * a)
	d  = b * b - 4 * a * c