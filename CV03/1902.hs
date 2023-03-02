{-
Keďže Monty nemá rád ľudí, vo februári zakázal v Springfielde oslavy sviatku Sv. Valentína. V meste sa začala šíriť vlna nepokojov a potreboval si vylepšiť reputáciu. Prikázal svojmu asistentovi Smithersovi, aby do konca mesiaca vymyslel nejaký valentínsky event.
Smithers navrhol pre všetkých záujemcov objímaciu hru, aby každý dostal presne 1 objatie. Pripravil 1902 lístkov s číslom, pre každého účastníka jeden. Aj Monty chcel byť súčasťou tejto hry a chcel ju vyhrať, tak si vybral jedno číslo ako prvý. Následne si každý účastník zobral jedno číslo zo zvyšných.
Všetci sa postavili do kruhu podľa poradia čísel od 1 po 1902. Postupovalo sa tak, že osoba s číslom 1 objala tretiu osobu v poradí (t.j. osobu s číslom 4) a obaja opustili kruh.
Takto to pokračovalo ďalej, nasledujúca osoba objala znova tretiu osobu v poradí. T.j. osoba s číslom 5 objala osobu s číslom 8 a opustili kruh. Kruh sa stále zmenšoval.
Postupne sa prvé kolo končilo, 1897 objala 1900 a 1901 objala 3. Čísla 4, 5, 8 a 9 v kruhu už neboli, preto potom 6 objala 11, ...
Hra sa skončila, keď ostali už len dvaja.
Zistíte, ktoré číslo si vybral Monty, aby skončil ako víťaz?
A ktoré číslo si vybral Smithers, aby práve on objal Montyho vo finále?
-}

l = [1..1902] 

    
monty = (iterate (\(x:y:z:w:xs) -> xs ++ [y,z]) [1..1902] ) !! 950 


