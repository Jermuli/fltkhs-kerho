# fltkhs-kerho
Ohjelmointityö kurssille tehty kerho-ohjelma käyttäen haskellia

## Kääntäminen
Ohjelman kääntämiseen tarvitaan [stack](https://docs.haskellstack.org/en/stable/README/).
Stackin asentamisen jälkeen käyttäjän tarvii asentaa [fltkhs](http://hackage.haskell.org/package/fltkhs-0.8.0.2/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html).
Tämän jälkeen kääntäminen onnistuu komentorivillä menemällä kohde kansioon ja kääntämällä lähdekoodin komennolla `stack install --flag fltkhs:bundled`. 
Lopuksi ohjelman voi käynnistää komennolla `stack exec fltkhs-fluid-kerho` 
(ohjelman käynnistäminen suoraan ei toimi, koska oletuksena vaadittavat linkitetyt tiedostot eivät löydy kansiosta johon ohjelma on käännetty)
