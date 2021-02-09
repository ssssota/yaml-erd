## yaml-erd

yamlファイルを入力として対応するER図を出力するツール

### ビルド

```
$ dotnet publish -c release
```
とすると `path/to/yaml-erd/bin/release/netcoreapp3.0` 以下にバイナリ `yaml-erd` が生成される．  

例えば64bit Linux向けにビルドしたい場合は
```
$ dotnet publish -c release -r linux-x64
```
とすればよい．この場合は `path/to/yaml-erd/bin/release/netcoreapp3.0/linux-x64` 以下に64bit Linux向けのバイナリが生成される．

### 使い方

`./sample.yaml` のようなYamlファイルを用意して
```
$ yaml-erd ./sample.yaml
```
と実行すると `./sample.yaml` に対応するER図が `./output.png` に出力される．  


出力されるファイル名は `--output` もしくは `-o` オプションで変えることができる．例えば
```
$ yaml-erd ./sample.yaml -o hoge.png
```
とすると `hoge.png` ファイルに出力される．


出力される形式は `--format` もしくは `-f` オプションで指定できる．
対応しているフォーマットはPNG, PDF, SVGであり, それぞれ `png`, `pdf`, `svg` で指定できる.  
例えば
```
$ yaml-erd ./sample.yaml -o hoge.pdf -f pdf
```
とすると `hoge.pdf` にPDF形式で出力される.  


画像の出力はdotファイルを経由して生成しており，中間のdotファイルの中身を見たい場合は `--temp` もしくは `-t` オプションで出力先を指定できる．
例えば
```
$ yaml-erd ./sample.yaml -o hoge.png -t temp.dot
```
をすると，`hoge.png` を生成するための中間ファイルが `temp.dot` に出力される．


`yaml-erd` がどのような引数で中間のdotファイルを画像ファイルにコンパイルしているのかを知りたい場合は `--verbose` オプションをつければよい．
```
$ yaml-erd ./yagisan.yml -o ./hoge.png -t ./temp.dot --verbose
[RUN] dot -Tpng ./temp.dot -o ./hoge.png
```

生成されるdotファイルに問題がある場合は
```
$ yaml-erd ./yagisan.yml -o ./hoge.png -t ./temp.dot --verbose
[RUN] dot -Tpng ./temp.dot -o ./hoge.png
[STDERR]
Error: ./temp.dot: syntax error in line 115 near '-'

[EXIT CODE] 1
```
などと出力される．


dotコマンドの引数を追加したい場合は `--additional-dot-args` オプションで指定できる.  
例えば, ノードのフォントにCalibriを使うためにdotコマンドに `-Nfontname="Calibri"` を追加したい場合は
```
$ yaml-erd ./yagisan.yml -o ./hoge.png -f png --verbose --additional-dot-args "-Nfontname=\"Calibri\""
[RUN] dot -Tpng /tmp/tmp70aTiC.tmp -o ./hoge.png  -Nfontname="Calibri"
```
となる.

### 入力フォーマット

入力されるYamlファイルは以下のような形式になっている必要がある
```
schema:
  <エンティティ名1>:
    struct:
      <エンティティの構造>      
    relations:
      - <リレーション1>
      - <リレーション2>
      - ..
  <エンティティ名2>:
    struct:
      <エンティティの構造>
    relations:
      - <リレーション1>
      - <リレーション2>
      - ..
  ..
```

つまり，toplevelには `schema` キーのみがあり，その下にエンティティの定義が並ぶ．

### エンティティの構造

Yaml上でエンティティの構造は葉が型名であるレコードとして表現される．
例えば, 葉が `"int"` の場合それに対応するフィールドが整数であることを表す．

つまり
```
  id: int
  name: string
```
や
```
  id: int
  foo:
    bar: int
    baz: string
```
はエンティティの構造として合法である．

### リレーション

リレーションが0個の場合，エンティティの `relations` は省略できる．  
リレーションのフォーマットは `"(参照元フィールド名) <リレーションの種類> <参照先エンティティ名>(<参照先フィールド名>)"` である．  

例えば `Hoge` エンティティがリレーション `(fuga) -- Foo(bar)` を持つ時，これは `Hoge.fuga` から `Foo.bar` へのリレーションを表す．  

リレーションの種類によってそのリレーションの多重度を表すことができる．  
リレーションの種類の両端は左右それぞれ以下のものを指定できる．  

| 左端 | 右端 | 意味 |
| ---- | ---- | ---- |
|` || `|` || `| 1つ  |
|` |o `|` o| `| 0か1 |
|` }o `|` o{ `| 0以上|
|` }| `|` |{ `| 1以上|

例えば `||--||` は一対一の関係を表すし，`||--o{` は一対多の関係を表す

