Task Format {
    Exec { stack exec -- stylish-haskell -c .\stylish-haskell.yaml -i .\app\Main.hs .\src\PhotographPixela.hs }
}

Task Lint {
    Exec { stack exec -- hlint app src }
}
