# fltkhs-kerho


- [fltkhs-kerho](#fltkhs-kerho)
  * [Itsearviointi ja jälkiselvitys](#itsearviointi-ja-jälkiselvitys)
    + [Tiivistelmä ohjelmointityöstä](#tiivistelmä-ohjelmointityöstä)
    + [Tehtävän kuvaus, tausta ja tavoitteet](#tehtävän-kuvaus-tausta-ja-tavoitteet)
    + [Käytännön toteutus](#käytännön-toteutus)
    + [Oman työn arviointi](#oman-työn-arviointi)
    + [Työssä käytetyt lähteet](#työssä-käytetyt-lähteet)
    + [Ylläpitäjän tarvitsemat tiedot](#ylläpitäjän-tarvitsemat-tiedot)
        * [Kääntäminen](#kääntäminen)
        * [Tiedettyjä oikkuja](#tiedettyjä-oikkuja)
        * [Ohjelman käyttäminen](#ohjelman-käyttäminen)
    + [Mahdollinen jakokehitys](#mahdollinen-jakokehitys)
    + [Yhteenveto](#yhteenveto)
  * [Suunnitelma](#suunnitelma)
      - [Ohjelman oletettu rakenne:](#ohjelman-oletettu-rakenne)
      - [Työhön vaaditut työkalut:](#työhön-vaaditut-työkalut)
      - [Mahdolliset ongelmakohdat:](#mahdolliset-ongelmakohdat)
      - [Aikataulu:](#aikataulu)


## Itsearviointi ja jälkiselvitys


### Tiivistelmä ohjelmointityöstä
Ohjelmointityö kurssille TIEA306 tehty [kerho-ohjelma](http://users.jyu.fi/~vesal/ohj2/) toteutus käyttäen haskell:ia


### Tehtävän kuvaus, tausta ja tavoitteet
Tehtävänä oli luoda kopio vesan tekemästä [kerho-ohjelmasta](http://users.jyu.fi/~vesal/ohj2/) käyttäen kielenä haskell:ia. Valitsin aiheen, koska mielestäni haskell on mielenkiintoinen kieli ja halusin selvittää miten osaisin toteuttaa sillä ihan omin neuvoin interaktiivisen ohjelman. Tavoitteeni oli saada aikaiseksi ohjelma, joka täyttäisi vesan vastaavan ohjelman perusominaisuudet


### Käytännön toteutus
Ohjelman lähdekoodi muodostuu 4 komponentista: fltkhs-kerho.hs (eli main), joka kutsuu fluid tiedostossa määriteltyä käyttöliittymää. Fluid tiedosto Kayttoliittyma.fl joka määrittelee käyttöliittymän ulkonäön (tämän tiedoston editoinnista/tarkastelusta lisää myöhemmin). Callbacks.hs joka sisältää kaikki eventtien eli callbackkien kutsumat toiminnot, sekä pitää yllä tietorakennetta, jota se muokkaa käyttöliittymästä saatujen inputtien mukaan, sekä hoitaa tiedostoihin tallentamisen ja tiedostojen avaamisen, eli lyhyesti, hoitaa reagoinnin ohjelman käyttäjän toimintaan. Ja lopuksi Tietorakenteet.hs, joka määrittelee tietorakenteet ja sisältää tietorakenteiden muokkaamiseen ja tarkistamiseen käytettäviä funktioita. Kaikki neljä tiedostoa ovat minun itseni kirjoittamia, mutta Callback.hs sisältää harrastusten infotaulukon, joka on otettu fltkhs tekijän demosta pienin muokkauksin (kyseinen on selkeästi eroteltu ja merkitty lähdekoodiin, sekä julkaistu MIT-lisenssin alla, joka käytännössä antaa minulle vapaat kädet sen käyttöön).


### Oman työn arviointi
Ihan alkuperäinen suunnitelmani olisi ollut että olisin saanut tehtyä hieman nätimmän ja paremman ohjelman (jouduin karsimaan alkuperäisestä ohjelmasta löytyvän tulostamisen ja harrastusten muokkaamisen. Myös harrastustaulukon päivittyminen heti harrastuksen poiston/lisäämisen jälkeen jäi uupumaan. Tietorakenne kyllä päivittyy heti, mutta jäsen joudutaan valitsemaan uudestaan tai poistamaan/lisäämään toinen harrastus, jotta itse lista päivittyisi, syytä sille miksei redraw komenno toimi en tosin tiedä) Myös Jäsenten harrastuksia listaavan taulukon sort ei toimi kunnolla muussa kun lajin kohdassa. Aloitusvuoden ja h/vko kohtien osalla numeroita verrataan myös teksteinä, joten esimerkiksi 11 tulee ennen 9, vaikka järjestys olisi nouseva. Myös ulkonäöllisesti ohjelma ei ole mitään silmäkarkkia, mutta jos fltkhs perustuu n. 20 vuotta vanhaan fltk, en tiedä voinko parempaan odottaakkaan. Eniten vaikeuksia tuotti lähdemateriaalin puute. FLTKHS dokumentointi on ainakin minusta hieman nihkeää. Funktioista annetaan vaan nimet ja syötteiden tyyppi, eikä mitään esimerkkiä tai selitystä miten ne toimivat. Joissain tapauksissa tämä riittää, mutta itse huomasin että jouduin kokeilemaan tekikö funktio sitä mitä oletin sen nimen perusteella tekevän. Mielestäni tämä ei ole hyvää ajankäyttöä. Myös haskellin tyyppisysteemi tuotti välillä ongelmia, mutta suurimmasta osasta ajasta se oli tielläni ihan syystä, ja myönnän että kyseinen auttaa karsimaan virheitä huomattavasti. Suurin ongelma tyyppien kanssa on erityisesti String ja Text tyypin kanssa. Puolet kääntämisessä saamista virheilmoituksista tulivat näistä, ja pyrin vielä pysymään vain Text:issä, koska käyttöliittymä tuki sitä. Hyvänä esimerkkinä tästä on se että TEXT paketin tiedostosta luku tahtoo tiedoston sijainnin String:inä eikä tekstinä, enkä voi vain käsittää miksi. Yksi tyyppi teksteille on tarpeeksi. Odotin että fltkhs tulisi olemaan suurin ongelmienlähde, mutta yllätykseni paketti itsessään on aivan mainio väline, jahka alkukankeudesta päästiin eroon. Olen etsinyt ohjelmastani tapoja joilla sen saisi kaatumaan toistettavasti ja olen pyrkinyt paikkaamaan niitä. Ainoat kaatumiset joista tiedän, johtuvat siitä jos kerhojen tallennustiedostoja käydään manuaalisesti muuttamassa ja yritetään sen jälkeen ladata niitä. Voi olla että harrastuksien poistoon on jäänyt vieläkin jokin kaatumisen aiheuttava bugi, mutta mielestäni kyseiset olen jo metsästänyt läpi, tosin en ole varma, koska suurinosa kyseisestä koodista ei ole minun kirjoittamaa, ja sen toiminta on osin minulle vieläkin mysteeri. Työkalut eivät nyt mitenkään erityisesti haitanneet tai auttaneet minua työssä, paitsi fluid. Ajattelin aluksi että GUI designer ohjelma olisi todella kätevä työtä tehdessä, mutta se aika mitä säästin komponenttien asettelussa, jouduin käyttämään moninkertaisesti saadakseni fluidissa laitetut komponentit toimimaan halutusti. Ja koska Käyttöliittymä.fl määrittelee käyttöliittymän, jouduin ahtamaan pienen osan koodista .fl tiedostoon, joka tarkoittaa että kyseisen koodin lukemiseen ja muokkaamiseen joudutaan käyttämään fluidin omaa käyttöliittymää, joka ei ole ihan optimaalinen. Aikataulu oli ISOIN akilleenkantapääni ylivoimaisesti. Pyysin aihetta joulukuussa, enkä ollut käytännössä saanut mitään aikaiseksi vielä huhtikuussa, ja huhti-toukokuu väli oli myös hyvin hiljaista työn teon osalta. Kesäkuussa sain hyvin tuulta työni purjeisiin, mutta silloinkin kun jäin pahaan paikkaan työni edistyminen tyssäsi taas. Vasta heinäkuussa sain todella työtä edistettyä, ja sen jälkeen työtahtini onkin mielestäni ollut ihan hyvää, aloitukseni oli vaan suoraan sanottuna aivan kamala. Olen koko yliopistoajan ollut todella huono tekemään asioita ajallaan ja jätänkin ne yleensä viime tinkaan. Pitäisi petrata paljon tältä osastolta ja vaan yrittää pakottaa itseni tekemään asioita, kun saan edes vähän edistyttyä niin jatkaminen on helppoa. Verrattuna kursseilla opetettuun ja kirjallisuuteen mielestäni työn käytännössä tekeminen (ainakin näin sooloprojektina) on huomattavasti hektisempää mitä on annettu odottaa. Kursseilla on mielestäni annettu ohjelmoinnista sellainen kuva että sinulla on suunnitelma jonka mukaan teet ja ohjelma rakentuu pikkuhiljaa pala palaltaan kokoon. Omassa työskentelyssäni huomasin että olen koodannut itseni umpikujaan ja jouduin purkamaan aijempia rakennelmia ja tekemään tilalle uusia, tai yrittää saada vanhat viritelmät toimimaan jollain uudella tavalla, joka silloin tällöin johti siihen että jouduin myöhemmin tekemään vielä ihmeellisempiä viritelmiä, jotta saisin työni toimimaan.


### Työssä käytetyt lähteet
Työn lähteinä on käytetty [fltkhs](http://hackage.haskell.org/package/fltkhs) ja [fltk](https://www.fltk.org/documentation.php) dokumentointia, sekä otettu mallia ja apinoitu fltkhs:lle [tehdyistä](https://github.com/deech/fltkhs-fluid-demos) [demoista](https://github.com/deech/fltkhs-demos). Materiaalia on netissä yllättävän vähän dokumentointien lisäksi, joka olikin mielestäni haastavin tekijä koko työn teossa.


### Ylläpitäjän tarvitsemat tiedot
##### Kääntäminen
Ohjelman kääntämiseen tarvitaan [stack](https://docs.haskellstack.org/en/stable/README/).
Stackin asentamisen jälkeen käyttäjän tarvitsee onnistua kääntämään fltkhs-hello-world demo [fltkhs](http://hackage.haskell.org/package/fltkhs-0.8.0.2/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html) (ohjeet löytyvät linkistä). Olen henkilökohtaisesti onnistunut kääntämään ohjelman Windows 7/10 muutamalla eri koneella, ja paketin tekijän mukaan kääntäminen pitäisi olla helpompaa muilla tuetuilla alustoilla, koska toisin kuin Windowsin tapauksessa, käyttäjän ei erikseen tarvitse asentaa esim.`autotool` tai `tar` paketteja. Windows käyttäjien tarvitsee kuitenkin ladata kyseiset paketit stackin mukana tulevalla msys2 shellillä, jonka jälkeen käyttäjät voivat vasta kääntää ohjelman (ohjeet tähän löydät [fltkhs](http://hackage.haskell.org/package/fltkhs-0.8.0.2/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html) linkistä). Huomioitavaa: jotkut antivirukset (kuten avast) merkkaavat jonkun msys2:lla ladattavista paketeista karanteeniin. Tämän tapahduttua stack ei voi enää kääntää ohjelmaa, ja ainoa korjaustapa jonka olen todennut toimivaksi on poistaa stack kokonaan, laittaa antivirus pois päältä ja kokeilemalla koko touhua alusta pakettien lataus mukaanlukien. Omassa tapauksessani msys2 kansio jäi kummittelemaan stackin poiston jälkeenkin asennuskohteeseen, eikä suostunut deletoitumaan ennen kun käynnistin koneen uudelleen. Onnistuin myös toistamaan kyseisen kaverini koneella, joten suosittelen ainakin avastin kohdalla laittamaan kyseisen pois päältä kääntämisen ajaksi, jos aijot kääntää tätä ohjelmaa. Suosittelen myös suht varmaa nettiyhteyttä, koska pätkivä netti on toinen asia jonka on onnistunut itsellä pilaamaan stackin toimimisen.
Jos olet onnistunut kääntämään fltkhs-hello-world ohjelman, pitäisi kerho-ohjelman kääntäminen onnistua myös komentorivillä yksinkertaisesti menemällä kohdekansioon ja kääntämällä lähdekoodi ohjelmaksi komennolla `stack install --flag fltkhs:bundled`. Ensimmäisellä kääntökerralla ohjelma tekee paljon asioita .stack-work kansioon, johon ohjelma asennetaan. Kun tämän lisää stackin hitaisii latauksiin ensimmäinen kääntäminen tulee kestämään useita minuutteja (9 min kun kerran jaksoin mitata), joten maltti on valttia. Tulevat kääntökerrat tosin ovat huomattavasti nopeampia (n. 20 sekunttia).
Lopuksi ohjelman voi käynnistää komennolla `stack exec fltkhs-fluid-kerho` 
(ohjelman .exe tiedoston käynnistäminen suoraan ei toimi, koska oletuksena vaadittavat linkitetyt tiedostot eivät löydy kansiosta johon ohjelma on käännetty, jos haluat jakaa ohjelmaa ilman lähdekoodia, fltkhs linkissä on siihenkin ohjeet)
Src kansiosta löytyvää .fl tiedostoa, joka kääntämisen yhteydessä käännetään haskelliksi fltkhs avulla, pääsee tutkimaan [fltk](https://www.fltk.org/) mukana tulevalla gui editorilla, fluidilla. Fltk paketin kääntäminen onnistuu esim. visual studiolla. 
##### Tiedettyjä oikkuja
Välillä käynnistäessä ohjelma antaa virheilmoituksen `freeHaskellFunctionPtr: not for me, guv!` ja kaatuu. Syytä tähän en tiedä, mutta ohjelman uudelleenkäynnistäminen toimii. Kääntäjä ei huomaa fluid (.fl) tiedostoon tehtyjä muokkauksia automaattisesti, joten jos muutat jotain siellä joudut myös muuttamaan jotain toista tiedostoa, jotta kääntäjä suostuu kääntämään ohjelman uudestaan.
##### Ohjelman käyttäminen
Ohjelman menubaarista löytyy "Apua" painike, joka antaa tiiviin infopaketin siitä minkälaisia syötteitä tietyt kentät hyväksyvät tallentamaan, jos kyseisellä syötteellä on olemassa jonkinlainen tarkistus olemassa. Paketin mukana tulee myös "malli" kerho, jonka käyttäjä voi ladata kirjoittamalla "Avaa" menupalkista tulevaan ikkunaan "malli" ja painamalla nappia.


### Mahdollinen jakokehitys
Jos joku tahtoo jatkaa kyseistä projektia, seuraava luonnollinen vaihe olisi tietenkin korjata pari ohjelmasta löytyvää vikaa ja implementoida alkuperäisestä kerho-ohjelmasta lötyvät, mutta tästä puuttuvat toiminnot työhön. Myös fltkhs themeihin tutustumalla, voi käyttöliittymästä saada vähän parempaa silmäkarkkia, mutta mielestäni kirjoitushetkellä kyseisiä ei olla vielä kauheasti implementoitu paketin tekijän osalta


### Yhteenveto
Tekemällä oppii parhaiten, vaikka se vaatiikin myös eniten työtä. Mielestäni tällaisillä tekemiseen painoitetuilla kursseilla oppii paljon paremmin, vaikka ne ovatkin myös huomattavasti luentokursseja työläämpiä tehdä. Ja verrattuna aijempiin isompii ohjelmointiprojekteihin tämä tuli tehtyä yksin, joka vielä edistää oppimista lisää. Loppuenlopuksi olen ihan tyytyväinen aikaan saamaani työhön, ja vaikka aloittaminen tuntui aivan kamalalta, loppua kohden työn tekeminen alkoikin parhaimmillaan olla ihan mukavaa.


## Suunnitelma
#### Ohjelman oletettu rakenne:
Ohjelman toiminta "apinoidaan" [kerho-ohjelmasta](http://users.jyu.fi/~vesal/ohj2/), joten ohjelma koostuu käyttöliittymästä, jonka kautta ohjelman käyttäjä pystyy käsittelemään kerhoista, jäsenistä ja harrastuksista muodostuvaa tietorakennetta.
#### Työhön vaaditut työkalut:
Ohjelman kääntäminen tullaan suorittamaan stackillä/ghci:llä. Kirjoittamisessa tulen kokeilemaan Vim:in käyttöä yleisestä painostuksesta johtuen, mutta jos kyseinen ei rupea ottamaan tuulta alleen, vaihdan takaisin tuttuun ja turvalliseen notepad++:ssaan. Haskelin peruskirjastot tulevat luultavasti riittämään tietorakenteiden sekä I/O hoitamiseen. Käyttöliittymä tullaan tekemään suosittelujen takia fltkhs kirjastolla ja fltk mukana tulevalla gui editorilla fluidilla, jonka tuottamia .fl tiedostoja myös fltkhs tukee.
#### Mahdolliset ongelmakohdat:
Ongelmakohtana on ollut ja tulee varmaan myös jatkossakin olemaan fltkhs. Kyseinen kirjasto on aiheuttanut ongelmia perusjuttujen kuten kääntämisen kanssa. Esim. en ole vielä tähänkään päivään mennessä saanut käännettyä [luurankoprojektia](https://github.com/deech/fltkhs-fluid-hello-world) fluidia käyttäville projekteille, joten tämänhetkinen versio omasta työstäni onkin rakennettu fluidia käyttävien [demojen](https://github.com/deech/fltkhs-fluid-demos) luurangon päälle. Myös kirjastolla tehtyjen ohjelmien vakaus on hieman arveluttavaa. Esimerkiksi fltk:sta fltkhs:lle portatut [demot](https://github.com/deech/fltkhs-fluid-demos) ovat itsellänni onnistuneet kaatumaan ihan peruskokeilun aikana. Myös tietorakenteiden relaatiot voivat aiheuttaa hieman ongelmia, koska itse en ole kyseisiä haskelilla ennen tehnyt.
#### Aikataulu:
Valitettavasti alkuperäinen aikataulu työn tekemisestä vuoden ensimmäisen neljänneksen aikana on jo historiaa (tämä johtuu pääosin omasta aikaansaamattomuudesta, mutta taustalla on ollut myös vähän ihan kunnollisia syitä kuten uuden kämpän etsintä ja muutto). Olen suunnitellut tekeväni työn valmiiksi kesäkuussa. Näillä näkymin optimistinen aikatauluni on saada työ valmiiksi ennen juhannusta (21. kesäkuuta), tämä päivä on myös tasan puolivuotta alkuperäisen suunnitelman lähettämisestä sähköpostilla. Kirjoittamishetkellä 4. kesäkuuta (23vk) minulla on karkea hahmoitelma käyttöliittymästä tehtynä fluidilla ja projektirakenne joka suostuu kääntymään. Tämän viikon (viikko 23) aikana minun olisi tarkoitus viilailla käyttöliittymää ja aloittaa tietorakenteiden tekeminen. Viikolla 24 tarkoitukseni olisi saada tietorakenteita käsittelevät funktiot sekä tiedostojen luku/kirjoittaminen valmiiksi ja aloittaa callback:kien kirjoittamaan käyttöliittymään. Tämän jälkeen meillä pitäisi olla valmis ohjelma. Olettaen että optimistinen aikatauluni ei pidä, otan parin viikon päästä yhteyttä sähköpostitse jos aiheen voimassaoloa olisi mahdollista pidentää.
