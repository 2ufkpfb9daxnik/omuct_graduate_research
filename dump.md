# eioについて練習

## 環境構築

```bash
opan update
opam install dune eio_main
```

## ビルドと実行

トップレベルから実行

```bash
dune exec eio_practice
```

そうじゃなくて、個別のファイルを実行

```bash
dune exec bin/concurrent.exe
```

このconcurrent.exeは、bin/concurrent.mlをビルドして実行することになる。

このときbin/duneには以下の追記が必要:

```lisp
(executable
 (name concurrent)
 (libraries eio_main))
```
