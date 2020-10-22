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
$ dotnet run -- ./sample.yaml
```
と実行すると `./sample.yaml` に対応するER図が `./output.dot` として出力される．  
出力されたdotファイルは `dot` コマンドでPDFやPNGに変換できる．`output.dot` をPNGに変換したい場合は例えば次のコマンドを実行する．
```
$ dot -Tpng -o output.png output.dot
```


出力されるファイル名は `--output` もしくは `-o` オプションで変えることができる．例えば
```
$ dotnet run -- ./sample.yaml -o hoge.dot
```
とすると `hoge.dot` ファイルに出力される．


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

Yaml上でエンティティの構造は葉が `"int"` もしくは `"string"` であるレコードとして表現される．
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
はエンティティの構造として合法だが，
```
  id: itn
  name: string
```
などは合法ではない．

葉が `"int"` の場合それに対応するフィールドが整数であることを表し，一方で `"string"` の場合それに対応するフィールドが文字列であることを表す．

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

