
## sym-to-cnf
Преобразование ограничений в CNF

    $ random-sym --total-vars=64 --vars-in-sym=32 --sym-size=1000000 \
      | sort -ru \
      | sym-to-cnf --total-vars=64 --method=tseitin +RTS -K20M

TODO
  * нужны тесты
  * избавиться от +RTS -K20M (why state monad is not tail recursive?)
  * списки поменять на векторы
