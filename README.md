# Latte

Kompilator języka Latte do x86.

## Kompilacja
Program kompilujemy przy użyciu polecenia `make`.

## Użyte biblioteki
- base
- containers
- array
- mtl
- lens
- extra
- transformers
- filepath
- process
- optparse-applicative


## Struktura
- src/main - pliki main
- src/latte/Frontend - implementacja frontendu
- src/latte/Backend - implementacja backendu
- src/parser - pliki parsera

## Składnia
Składnia rzutowania ma postać `"null" "as" Typ`, przykładowo `null as list`, gdzie `list` jest obiektem.

## Zaimplementowane rozszerzenia
- struktury
- obiekty
- metody wirtualne
- tablice
