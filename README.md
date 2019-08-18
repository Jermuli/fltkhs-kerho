# fltkhs-kerho  
Ohjelmointityö kurssille TIEA306 tehty [kerho-ohjelma](http://users.jyu.fi/~vesal/ohj2/) toteutus käyttäen haskell:ia. Ohjelma sisältää
malli kerhon, joka antaa hieman mallia, minkälaisia kerhoja kyseisellä ohjelmalla pystytään tekemään.  

## Kääntäminen  

#### Yleisiä huomioita kääntämisestä
Jotkin virustorjuntaohjelma (todistetusti avast) laittavat jonkun MSYS2 paketeista karanteeniin, jonka jälkeen kääntäminen ei toimi. Ainoa keino korjata tämä on asentaa stack kokonaan uudelleen, joten suosittelen että jos koneessasi on viruksentorjuntaohjelmana avast, laitat sen pois päältä kääntämisprosessin ajaksi. Stackin setup voi kaatua, jos internetyhteydessä on häikkää setup:in aikana, joten suosittelen vakaata nettiyhteyttä. Ensimmäinen kääntämiskerta vie huomattavasti aikaa n. 10 min, toistuvat kääntämiskerrat ovat nopeampia n. 20 sekunttia. Tuetut käyttöjärjestelemät ovat fltkhs tekijän mukaan Linux, BSD, OSX ja Windows (henkilökohtaisesti olen onnistunut kääntämään ohjelman muutamalla windows 7 ja 10). Myös erikoismerkit projektikansion pathnamessa on todettu aiheuttavan ongelmia, joten kääntäessä välttäkää esim. ääkkösiä pathnamessa. Jos jokin menee pieleen, ja ongelman korjauksen jälkeenkin kääntämisessä ilmenee ongelmia, niin yleensä `.stack-work` kansion poistaminen auttaa ongelmaan. Alkuperäinen kääntämisohje fltkhs:n hello world demolle, josta nämä ohjeet ovat peräisin, löytyy [täältä](http://hackage.haskell.org/package/fltkhs-0.8.0.2/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html#g:5).
#### Kääntämisprosessi
Aluksi asenna [stack](https://docs.haskellstack.org/en/stable/README/)  
Tämän jälkeen mene komentorivillä tämän projektin kansioon ja suorita komento:  
`stack setup`  
Seuraavaksi käyttäjän on asennettava kääntäjää varten muutama paketti stackin mukana tulevaan MSYS2:seen (tämän pitäisi koskea vain windows käyttäjiä, koska muissa tuetuissa käyttöliittymissä kyseiset paketit kuulema löytyvät jo valmiina, joten muut käyttöliittymät voivat hypätä seuraavien vaiheiden yli suoraan kääntämään)  
Suorita komentorivillä komento:  
`stack exec mintty`  
ja avaa stackin mukana tuleva MSYS2  
MSYS2 komentorivillä päivitä MSYS2 seuraavilla komennoilla:  
`pacman -Syy`  
`pacman -Syu` (Tämän jälkeen komentorivi on yleensä kaatunut. Sulje komentorivin prosessi ja jatka vain muitten pakettien asentamista)  
`pacman -S wget`  
`pacman -S tar`  
`pacman -S unzip`  
`pacman -S zip`  
`pacman -S man`  
`pacman -S autoconf`  
`pacman -S make`  
`pacman -S automake`  
Tämän jälkeen voit mennä takaisin käyttöjärjestelmäsi natiiviin komentoriviin ja nyt ollessasi tämän projektin kansiossa voit kääntää ohjelman yksinkertaisesti komennolla:  
`stack build --flag fltkhs:bundled`  
Tämän jälkeen voit käynnistää ohjelman komennolla:  
`stack exec fltkhs-fluid-kerho`  
