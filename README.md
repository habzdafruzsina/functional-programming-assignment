# Funkcionális programozás

ELTE IK
_ beadandó 2023-24-1_
_tárgyfelelős: Horpácsi Dániel_

## Beadandó

### “Shunting yard” algoritmus
### A feladathoz kapcsolódó tudnivalók
Feladat beadási módja:
- Csak olyan megoldást fogadunk el, ami lefordul! A fordítási hibás kódrészletek, félmegoldások kommentben szerepeljenek!
A megoldást egy .hs kiterjesztésű fájlba írjátok!
- Ezt a .hs fájlt csomagoljátok be egy .zip állományba!
- Az így kapott .zip állományt töltsétek fel Canvasba, a feladat megoldásaként!
- A tesztelés eredményét a TMS rendszerben is meg lehet tekinteni!
- A megadott függvények típusát másold be a megoldásodba!
- A megoldás során lehet használni bármilyen függvényt a szabványkönyvtárból (Hoogle (Linkek egy külső oldalra) ajánlott a könyvtárbeli függvények keresésére).
- Tanács: a megoldás megkezdése előtt importáld a Data.Char és Data.List modulokat a szabványkönyvtárból!
- A feladatok megoldását mindenki önállóan készítse el!
- A feladatokhoz sokszor több tippet is megadtam. Ha szeretnéd próbára tenni a programozási tudásod, próbáld meg a feladatokat a tippek elolvasása nélkül megoldani!
- A feladatok pontszámának összege több, mint 20. A maximum 20 ponton felül megszerzett pontokat a plusz-mínuszok pontszámába fogom elszámolni.
- A megoldásban többször is szükség lesz hibák jelzésére, amit pl. az error függvénnyel tehetsz meg! Érdemes minden esetben a hibákra specifikus hibaüzeneteket kiírni, hogy a kiértékelés során könnyebben tudd a kódolási hibákat megtalálni.
    - Amennyiben paramétereket is szeretnél szerepeltetni a hibaüzenetekben, használd a show függvényt, amely tetszőleges Show típusosztály példánnyal rendelkező típus értékének megadja a szöveges reprezentációját!
### Feladatok
#### unsafeLookup (1 pont)
Definiálj egy függvényt, amely egy asszociatív listából (kulcs-érték párok listájából) megadja az adott kulcshoz tartozó értéket! Ha a keresett kulcs nem szerepel a listában, a kiértékelés álljon meg egy hibaüzenettel!
```
{
unsafeLookup :: Eq a =>  a -> [(a,b)]-> b
}
```
Tesztek:
```
{
unsafeLookup 1 [(1,1),(2,2)] == 1
unsafeLookup 2 [(1,1),(2,2)] == 2
unsafeLookup 25 [(x, ['a'..'z'] !! x) | x <- [0..25]] == 'z'
unsafeLookup 0 [(x, ['a'..'z'] !! x) | x <- [0..25]] == 'a'
}
```
#### parseCharOfBase (2 pont)
Definiálj egy függvényt amely egész számként megadja az adott számrendszerbeli karakter értékét! Ha az adott karakter nem az adott számrendszer számjegye, a kiértékelés álljon le hibával! A feladatsor szempontjából elég, hogyha a bináris, oktális, decimális és hexadecimális számrendszereket veszed figyelembe, de megvalósíthatod általánosabban is a függvényt! A következő karakterek számítanak validnak ezekben a számrendszerekben:
- bináris számrendszer: '0','1'
- oktális számrendszer: '0','1','2','3','4','5','6','7'
- decimális számrendszer: '0','1','2','3','4','5','6','7','8','9'
- hexadecimális számrendszer: '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','A','B','C','D','E','F' (ebben az esetben a nagy- és kisbetűs karakterek ugyanazokat a számjegyeket jelölik)
```
{
parseCharOfBase :: Integer -> Char -> Integer
}
```
Tesztek:
```
{
parseCharOfBase 2 '0' == 0
parseCharOfBase 2 '1' == 1
all (\(x, y) -> parseCharOfBase 8 x == y) (zip ['0'..'7'] [0..])
all (\(x, y) -> parseCharOfBase 10 x == y) (zip ['0'..'9'] [0..])
all (\(x, y) -> parseCharOfBase 16 x == y) (zip (['0'..'9'] ++ ['a'..'f']) [0..])
all (\(x, y) -> parseCharOfBase 16 x == y) (zip (['0'..'9'] ++ ['A'..'F']) [0..])
}
```
#### parseInteger (2 pont)
Definiálj egy olyan függvényt, amely adott bázis szerint feldolgoz egy szöveget és előállítja a neki megfelelő nemnegatív egész számot! Ha a feldolgozandó szöveg üres, vagy a bázisnak nem megfelelő karaktereket tartalmaz, akkor a kiértékelés álljon meg egy hibaüzenettel!
```
{
parseInteger :: Int -> String -> Integer
}
```
Tesztek:
```
{
parseInteger 16 "CafeBabe" == 3405691582
parseInteger 10 "123456789" == 123456789
parseInteger 8  "576" == 382
parseInteger 2  "100010" == 34
}
```
#### parseLiteral (3 pont)
Készíts egy függvényt, amely kiegészíti a nemnegatív számok szöveges beolvasását. A függvény egy prefix alapján eldönti, hogy milyen számrendszerben kell értelmezni a prefix utáni számjegyeket!
A lehetséges prefixek: 0b, 0B = bináris szám, 0o, 0O = oktális szám, 0x, 0X = hexadecimális szám
Ha a szöveg nem tartalmaz semmilyen prefixet, akkor tízes számrendszerbeli értéknek tekintsd a teljes szöveget!
Ha a paraméterül kapott szöveg üres, akkor a kiértékelés álljon meg a hibaüzenettel!
```
{
    parseLiteral :: String -> Integer
}
```
Tesztek:
```
{
parseLiteral "0x100beef" == 16826095
parseLiteral "0X100beef" == 16826095
parseLiteral "0o723" == 467
parseLiteral "0O32132" == 13402
parseLiteral "727282353" == 727282353
parseLiteral "0b100010" == 34
parseLiteral "0B100010" == 34
}
```
Tipp: Definiálj egy segédfüggvényt, amely eldönti egy szövegről, hogy milyen számrendszernek megfelelő prefixet jelöl!
A kód olvashatósága érdekében vezesd be a következő típusszinonimát!
```
{
type Token = String
}
```
#### isOperator (1 pont)
Add meg azt a függvényt, amely egy karakterről eldönti, hogy egy műveleti jel-e! A következő műveleteket támogasd a megoldásban:
- ( és ): zárójelek,
- +: összeadás,
- -: kivonás,
- *: szorzás,
- /: (egész) osztás,
- %: maradékképzés,
- ~: aritmetikai negáció, vagyis egy szám (-1)-gyel való szorzása
```
{
isOperator :: Char -> Bool
}
```
Tesztek:
```
{
isOperator '('
isOperator '+'
all isOperator ['(', ')', '+', '-', '~', '/', '%', '*']
not (isOperator 'x')
not (isOperator '1')
}
```
#### tokenize (4 pont)
Készíts egy olyan függvényt, amely egy teljes kifejezést tartalmazó szöveget felbont tokenekre! A tokenek kétfélék lehetnek:
- az előző feladatban szereplő műveleti jelek,
- számliterálok.
Figyelem! Egyelőre csak Token-ekre bontás a feladat, még nem kell a számliteráloknak megfelelő Integer értékeket előállítani az eredményben!
```
{
    tokenize :: String -> [Token]
    }
```
Tesztek:
```
{
tokenize "" == []
tokenize "0x12AA" == ["0x12AA"]
tokenize "1 + 2" == ["1", "+", "2"]
tokenize "(1 +    2)" == ["(", "1", "+", "2", ")"]
tokenize "(1   + 2) *   3" == ["(", "1", "+", "2", ")", "*", "3"]
tokenize "(1+2)*3" == ["(", "1", "+", "2", ")", "*", "3"]
tokenize "(b1+b0010)*3" == ["(", "b1", "+", "b0010", ")", "*", "3"]
tokenize "3*(1 +  2)" == ["3", "*", "(", "1", "+", "2", ")"] 
tokenize "0x4D5A*2+0o32-~111" == ["0x4D5A", "*", "2", "+", "0o32", "-", "~", "111"]
tokenize "( (~9 )  +  ~0o711 )   / ( 0Xcafe%  8 )" == ["(", "(", "~", "9", ")", "+", "~", "0o711", ")", "/", "(","0Xcafe", "%", "8", ")"]
}
```
Tipp: A szóközök nélküli szöveget tulajdonképpen az operátorok mentén kell feldarabolni, és minden darab (az operátorok is) egy-egy tokennek felel meg.
#### precedence (2 pont)
Definiálj egy függvény, amely meghatározza az operátorok precedenciáját (egy nemnegatív egész számként)! A függvénynek úgy kell kiosztania ezeket az értékeket, hogy a következő legyen a relatív precedencia a műveletek között:
- negáció >
- szorzás = osztás = maradékképzés >
- összeadás = kivonás.
Az egy szinten levő műveletek azonos precedenciájúak lesznek. Ha a függvény paramétere nem operátor, akkor a kiértékelés álljon le egy hibával! A zárójelet itt most ne tekintsd műveletnek, ezért nem tartozik hozzá kötési erősség!
```
{
precedence :: Token -> Int
}
```
Tesztek:
```
{
precedence "+" == precedence "-"
(precedence "/" == precedence "*") && (precedence "%" == precedence "/")
precedence "+" < precedence "*"
(precedence "~" > precedence "+") && (precedence "~" > precedence "*")
}
```
#### shunt (5 pont)
Valósítsd meg Dijkstra “shunting yard” algoritmusát (Linkek egy külső oldalra) a shunt függvénnyel! Az algoritmus működésének szemléltetése ittLinkek egy külső oldalra található. A feladatsorban használt operátorok mindegyike balasszociatív, így nem kell a megoldásban az asszociativitást külön vizsgálni! Ha szereted a kihívást, próbáld meg kizárólag ezen források és a tesztek alapján megoldani a feladatot!
```
{
    shunt :: [Token] -> [Token] -> [Token]
}
```
Tesztek:
```
{
shunt [] ["1"] == ["1"]
shunt [] ["(","999",")"] == ["999"]
shunt [] ["(","(","(","22",")",")",")"] == ["22"]
shunt [] ["~","42"] == ["42", "~"]
shunt [] ["~","42","*","0x29"] == ["42", "~", "0x29", "*"]
shunt [] ["3","+","17","*","89"] == ["3", "17", "89", "*", "+"]
shunt [] ["(","777","+","0o22",")","%","16"] == ["777", "0o22", "+", "16", "%"]
shunt [] ["(","(","2","*","238",")","/","(","1956","+","77",")",")"] == ["2", "238", "*", "1956", "77", "+", "/"]
shunt [] ["~","(","19","%","2",")","*","~","(","472","/","3",")"] == ["19", "2", "%", "~", "472", "3", "/", "~", "*"]
shunt [] ["3","+","4","*","2","/","(","1","-","5",")"] == ["3", "4", "2", "*", "1", "5", "-", "/", "+"]
["3", "4"] ++ shunt ["*", "+"] ["2", "/", "(", "1", "-", "5"] == ["3", "4", "2", "*", "1", "5", "-", "(", "/", "+"]
}
```
Tipp: Az algoritmus egy veremben tárolja az operátorokat a kiértékelés során. Ezt a vermet egy listával valósítsd meg (ez lesz a függvény első paramétere)! A függvény második paramétere a feldolgozandó infix tokensorozat. Az algoritmus a következő módon működik:
- Ha nincs több feldolgozandó token, akkor az eredmény a verem aktuális tartalma!
- Ha van egy első token, akkor a verem tartalma a következő lehetőségek alapján módosul:
    - Ha az első token egy nyitó zárójel "(", akkor tedd ezt a zárójelet a verem tetejére!
    - Ha az első token egy záró zárójel ")", akkor vedd ki az összes operátort a verem tetejéről az első nyitó zárójelig "(", és fűzd ezeket az épülő postfix tokensorozat elejére! Ezután a veremből vedd ki a nyitó zárójelet is!
        - Tipp: Ha nem megfelelően zárójelezett a formula, ezt egy hibával lehet jelezni (pl. ebben a lépésben ez felismerhető úgy, hogy nincs a veremben nyitó zárójel)!
    - Ha az első token egy operátor (ami nem zárójel), akkor vizsgáld meg a vermet:
        - Ha a verem üres, vagy az első eleme nyitó zárójel "(", vagy a feldolgozandó operátor precedenciája nagyobb, mint a verem tetején lévő operátoré, akkor tedd a feldolgozandó operátort a verem tetejére!
        - Egyébként (azaz, ha a feldolgozandó operátor precedenciája kisebb, vagy egyenlő, mint a verem tetején lévőé) vedd ki a verem tetejéről az operátorokat addig, amíg el nem jutsz egy nyitó zárójelig "(", vagy egy olyan operátorig, amelynek a precedenciája már kisebb, mint a feldolgozandó operátoré! A kivett operátorokat fűzd az épülő postfix tokensorozat elejére!
    - Egyébként feltehető, hogy az első token egy számliterál, fűzd ezt az épülő postfix tokensorozat elejére, és hagyd a vermet változatlanul!
- Az algoritmus az előző pontokban leírtak alapján működik tovább a fennmaradó tokensorozatra, a módosított veremmel, miközben a postfix tokensorozatot építi.
További tippek:
- A feladatot célszerű két részfeladatra bontani:
1.	Add meg az algoritmus egy lépését az aktuális verem állapota, és egy token esetén (a fenti algoritmus 2. pontja, amely az első token feldolgozását írja le)! A részfeladatot megoldó függvény eredménye legyen a módosított veremállapot, illetve a postfix alak elejére fűzendő tokenek listája!
2.	Írj egy rekurzív függvényt, amely alkalmazza az előző részfeladatban definiált függvényt a tokenlista aktuális elemére! Ne felejtsd el a segédfüggvény által eredményezett vermet és postfix tokensorozatot megfelelően felhasználni!
- Hasznos standard függvények ehhez a feladathoz:
    - take/drop: egy lista első n elemét adja eredményül/dobja el.
    - takeWhile: egy lista olyan prefixét adja eredményül, amelyben minden elem teljesíti a megadott feltételt. Pl. takeWhile even [2,4,5,6,7,8] == [2,4].
    - dropWhile: egy lista elejéről eldobja azokat az elemeket, amelyek teljesítik a megadott feltételt. Pl. dropWhile even [2,4,5,6,7,8] == [5,6,7,8].
- Érdemes a precedenciák előtt azt ellenőrizni, hogy az adott operátor nyitó zárójel-e, ugyanis a precedence függvény nincs értelmezve a zárójeleken.
#### calculate (5 pont)
Valósítsd meg a lengyel forma kiértékelésének algoritmusát! Ez az algoritmus szintén egy vermet használ (a függvény első paramétere), de ebben az esetben a műveletek operandusai kerülnek bele. A függvény második paramétere egy postfix kifejezést reprezentáló tokensorozat. Az algoritmus a következő módon működik:
- Ha nincs több feldolgozandó token, akkor az eredmény a verem tetején lévő szám!
    - Tipp: Ha a verem nem egyelemű ebben az esetben, az azt jelenti, hogy a kezdeti tokensorozat nem megfelelően volt formázva, ezt lehet hibával jelezni!
- Ha van egy első feldolgozandó token, akkor az algoritmus következő lehetőségek alapján jár el:
    - Ha az első token egy operátor, akkor a veremből kivesz az operátor paraméterszámának megfelelő számú elemet (ez a negáció "~" esetén 1, minden más operátor esetében 2). A kivett elemekre alkalmazza az operátor által jelzett függvényt, majd a kapott eredményt visszateszi a verem tetejére.
    - Ha az első token nem operátor, akkor feltételezhető, hogy egy számliterál. Ebben az esetben a számliterálnak megfelelő Integer típusú érték kerül a verem tetejére.
```
{
calculate :: [Integer] -> [Token] -> Integer
}
```
Tesztek:
```
{
calculate [] ["3"] == 3
calculate [11] ["~"] == -11
calculate [2,1] ["/"] == 0
calculate [2,1] ["-"] == -1
calculate [] ["3","2","+"] == 5
calculate [] ["3","2","+","4","*"] == 20
calculate [] ["3","4","2","*","1","5","-","/","+"] == 1
}
```
Tippek:
- Érdemes definiálni egy függvényt amely az operátorok tokenjeit leképezi a nekik megfelelő standard függvényre.
- Az esetszétválasztás során érdemes a verem méretét is ellenőrizni a feltételekben. Akármelyik lépésben a nem megfelelő veremméret azt jelzi, hogy a kezdeti kifejezés nem volt megfelelően formálva (ezt szintén lehet jelezni futásidejű hibával).
- Amikor kétparaméteres operátort alkalmazol a veremből kivett két operandusra, figyelj az operandusok sorrendjére!
- Ezt a feladatot is érdemes két lépésben megoldani:
1.	Adj meg egy függvényt, amely az algoritmus egy lépését valósítja meg! Ez a függvény adott verem és egy feldolgozandó token alapján eredményezze egy új vermet a fent leírtak alapján!
2.	Adj meg egy rekurzív függvényt, amely az előző segédfüggvényt alkalmazza a tokensorozat elemeire!
#### evaluateExp (2 pont)
Az előbbi függvények segítségével definiáld azt a függvényt, amely szöveges formában megadott infix kifejezések eredményét számolja ki!
```
{
evaluateExp :: String -> Integer
}
```
Tesztek:
```
{
evaluateExp "42" == 42
evaluateExp "11 + 22" == 33
evaluateExp "11 + ~22" == -11
evaluateExp "3 + 4 * 2 / ( 1 - 5 )" == 1
}
```

