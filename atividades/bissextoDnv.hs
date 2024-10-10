bissexto :: Int -> Bool
bissexto ano = if mod ano 4 /= 0 then False
               else if mod ano 100 == 0 then False
                    else if mod ano 400 /= 0 then True
                         else False