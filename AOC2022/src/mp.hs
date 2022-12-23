packet = (Lit <$> decimal) <|> (List <$> between "[" "]" (packet `sepBy` ","))
pPacket (El <$> pNum) <++ (List <$> between "[" "]" (pPacket `sepBy` ","))
