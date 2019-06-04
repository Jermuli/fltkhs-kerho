# fltkhs-kerho
Ohjelmointityö kurssille tehty [kerho-ohjelma](http://users.jyu.fi/~vesal/ohj2/) käyttäen haskell:ia

## Suunnitelma
### Ohjelman oletettu rakenne: 
Ohjelman toiminta "apinoidaan" [kerho-ohjelmasta](http://users.jyu.fi/~vesal/ohj2/), joten ohjelma koostuu käyttöliittymästä, jonka kautta ohjelman käyttäjä pystyy käsittelemään kerhoista, jäsenistä ja harrastuksista muodostuvaa tietorakennetta.
### Työhön vaaditut työkalut: 
Ohjelman kääntäminen tullaan suorittamaan stackillä/ghci:llä. Kirjoittamisessa tulen kokeilemaan Vim:in käyttöä yleisestä painostuksesta johtuen, mutta jos kyseinen ei rupea ottamaan tuulta alleen, vaihdan takaisin tuttuun ja turvalliseen notepad++:ssaan. Haskelin peruskirjastot tulevat luultavasti riittämään tietorakenteiden sekä I/O hoitamiseen. Käyttöliittymä tullaan tekemään suosittelun takia fltkhs kirjastolla ja fltk mukana tulevalla gui editorilla fluidilla.
### Mahdolliset ongelmakohdat: 
Ongelmakohtana on ollut ja tulee varmaan myös jatkossakin olemaan fltkhs. Kyseinen kirjasto on aiheuttanut ongelmia perusjuttujen kuten kääntämisen kanssa, en ole vielä tähänkään päivään mennessä saanut käännettyä luurankoprojektia fluidia käyttäville projekteille, joten tämän hetkinen versio omasta työstäni onkin rakennettu fluidia käyttävien demojen päälle. Myös kirjastolla tehtyjen ohjelmien vakaus on hieman arveluttavaa. Esimerkiksi fltk:sta fltkhs:lle portatut demot ovat itsellänni onnistuneet kaatumaan ihan peruskokeilun aikana. Myös tietorakenteiden relaatiot voivat aiheuttaa hieman ongelmia, koska itse en ole kyseisiä haskelilla ennen tehnyt.
### Aikataulu: 
Valitettavasti alkuperäinen aikataulu työn tekemisestä vuoden ensimmäisen neljänneksen aikana on jo historiaa (tämä johtuu pääosin omasta aikaansaamattomuudesta, mutta taustalla on ollut myös vähän ihan kunnollisia syitä kuten uuden kämpän etsintä ja muutto). Olen suunnitellut tekeväni työn valmiiksi kesäkuussa. Näillä näkymin optimistinen aikatauluni on saada työ valmiiksi ennen juhannusta (21. kesäkuuta), tämä päivä on myös tasan puolivuotta alkuperäisen suunnitelma sähköpostin lähettämisestä. Kirjoittamishetkellä 4. kesäkuuta (23vk) minulla on karkea hahmoitelma käyttöliittymästä ja projektirakenne joka suostuu kääntymään. Tämän viikon (viikko 23) aikana minun olisi tarkoitus viilailla käyttöliittymää ja aloittaa tietorakenteiden tekeminen. Viikolla 24 tarkoitukseni olisi saada tietorakenteita käsittelevät funktiot sekä tiedostojen luku/kirjoittaminen valmiiksi ja aloittaa callback:kien kirjoittamaan käyttöliittymään. Tämän jälkeen meillä pitäisi olla valmis ohjelma. Olettaen että optimistinen aikatauluni ei pidä, otan parin viikon päästä yhteyttä sähköpostitse jos aiheen voimassaoloa olisi mahdollista pidentää.

## Työn kuvaus
Työn tarkoituksena on siis täyttää Ohjelmointityö kurssin vaatimukset. Aiheeksi on valittu kurssin tim sivulta valmiiden aiheiden kohdalta ["kerho-ohjelman"](http://users.jyu.fi/~vesal/ohj2/) tekeminen haskell kielellä.

## Kääntäminen
Ohjelman kääntämiseen tarvitaan [stack](https://docs.haskellstack.org/en/stable/README/).
Stackin asentamisen jälkeen käyttäjän tarvii asentaa [fltkhs](http://hackage.haskell.org/package/fltkhs-0.8.0.2/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html).
Tämän jälkeen kääntäminen onnistuu komentorivillä menemällä kohde kansioon ja kääntämällä lähdekoodin komennolla `stack install --flag fltkhs:bundled`. 
Lopuksi ohjelman voi käynnistää komennolla `stack exec fltkhs-fluid-kerho` 
(ohjelman käynnistäminen suoraan ei toimi, koska oletuksena vaadittavat linkitetyt tiedostot eivät löydy kansiosta johon ohjelma on käännetty)
