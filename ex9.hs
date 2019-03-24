classifica :: Int -> String
classifica n | n < 10 = "reprovado"
             | n < 13 = "suficiente"
             | n < 16 = "bom"
             | n < 19 = "muito bom"
             | n < 21 = "excelente"
