funcao a b c =
      if (((b ^ 2) - 4 * a * c) > 0)
      then let x = ((b ^ 2) - 4 * a * c) ::Double
               y = (-b + sqrt(x)) / (2 * a) ::Double
               z = (-b - sqrt(x)) / (2 * a) ::Double
           in (y,z)
      else (0,0)

