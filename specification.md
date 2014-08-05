# ICFP Programming Contest 2014

# 仕様

# イントロダクション

今年のICFPプログラミングコンテストでは，80年代のソフトウェアとハードウェアの考古学を楽しんでもらいたいと考えた．
たまたま，(友達の友達や破綻資産売却を通じて)LamCo社の内部資料を山を手にいれた．
80年代に遡ると，LamCo社はアーケードゲームの製造に失敗した．
製品を商品にすることができず，有名なライバル社に叩きのめされたために彼らのことに聞き及ぶことはなかったと思う．

隠匿書類から満つかったのは，彼等の少々興味深い技術であった．
また，彼等は開発過程でひどい内部抗争から派手に機能不全に陥いった．
結局それが原因だろうとにらんでいる．

そこでコンテストの課題は，LamCo社の妙な古いソフトウェアとハードウェア技術を精査し，シミュレーションでアーケードハードウェアを再構築することであり，そのアーケードプラットフォームのソフトウェアを書いて，ゲームを遊べるようにし，君のライバルよりも上手くプレイすることである．

## あかされた歴史

LamCo社が構築しようとしていたアーケードゲームではλマンという小さなキャラを制御するようになっていた．
このキャラは迷路の中を錠剤を食いながら幽霊から逃げまわるというものである．
このゲームは有名なアーケードゲームに酷似している．
なんでそんなに似たものを作ろうとしたのかは知るよしもないが，LamCoは自分達よりも成功しているライバルからアイデアを盗んだのではなかと思う．
彼等自身がいちばんよく承知している理由により，LamCoは2人であそぶゲームにしようと決めたのだ．
2つのλマンキャラが幽霊の鼻先で殆どの錠剤を根そぎにする戦いを繰り広げるというものである．
彼等には一人であそぶためには仇のλマン用AIが必要なことが判っていた．
このことときつい締切がライバル関係にあった2つの技術チームを一緒に仕事させることを強要するという経営陣の破局的決定を招いた．
経営陣は一方のチームに幽霊のAIとゲームの機構を担当させ，もう一方のチームにはλマンのAIを担当させた．
経営陣が言うには，このミッションクリティカルなプロジェクトに対する能天気な考えは先見の明のあるものだったのである．
彼等はウィンウィンになると信じて疑わず，クライアントに簡単に届けられる成果があがるものと信じていたのである．
時間に追われ，経営陣に攻められ，どちらのチームも自分達の好む技術を使うことを許されて，計画ではそれらを最後に全部統合する予定であった．
幽霊-ゲーム機構チームは8ビットマイクロプロセッサがすべてであると信じるようなエンジニアが率いていた．
λマンAIチームはLISPにとりつかれたエンジニアに率いられていた．

もちろん，最終成果物はそれを組んだチームを反映したものになった．
マザーボードは数台の8ビットマイクロコントローラを積んでおり，これを用いて幽霊とゲーム機構を実装していた．
またλマンAIを走らせる専用のコプロセッサも積んでいた．
ソフトウェアもそれぞれのチームの哲学を反映したものであった．
幽霊AIとゲーム機構はアセンブリ言語で直接書かれており，λマンAIはLISPのとある方言で書かれていた．

このプロジェクトが失敗した本当の原因はよくわからない．
スケジュール遅延の原因は，異なる技術を統合することによるもの，2つのチームのいがみあい，材料のすざまじい高騰，あるいは，その全部かもしれない．
経営陣のせいではないだろう．
確かなことはプロジェクトは完全は消滅し，LamCo社は経営破綻したということである．

## 課題の概要

見つかった技術者のノートによれば，プロジェクトが消滅するまでには，ハードウェアもソフトウェアもなんとか動くところまで来ていた．
残念なことに元々のソフトウェアを回復することはできないが，ハードウェアのデザインノート詳細にわかっている．

これまでに我々（つまり審査員）はこのアーケードハードウェアのソフトウェアでのシミュレーションを終えている．
君の仕事はこのハードウェア上で走るソフトウェアを書くことである．

## ライトニングラウンド概要

ライトニングラウンドでは「クラシック」版でプレイする．1人のλマンと幽霊たちが対戦する．

λマンAIを書いてもらいたい．
λマンは審査員が用意した迷図で審査員が用意した幽霊AIと戦う．

いくつかの迷図とあまり賢くない幽霊AIのアセンブリコードをいくつかを君に提供する．

ライトニングラウンドは最初の24時間である．
したがって，提出期限は12:00 UTC 26/07/2014である．

## 本番ラウンド概要

本番ラウンドもライトニングラウンドとほぼ同じだが，ちょっとひねってある．詳細についてはライトニングラウンドが終了したときにあかされる．
ライトニングラウンド終了したらすぐにトップページのリンクをチェックすること．

本番ラウンドは72時間であり，提出期限は12:00 UTC 28/07/2014である．

## 残りの仕様

残りの仕様は以下を参照のこと．

- 完全な仕様は[λマンゲームのルール](#the-lambda-man-game-rules)にある．
- 得点方法の詳細は[得点方法](#scoring)にある．
- 幽霊AIのマイクロコントローラについては[マイクロコントローラ](#ghost-cpu-ghc)にある．
- λマンAIにつかわれているプロセッサについては[プロセッサ](#lambda-man-cpu)にある．
- 解答の提出方法の詳細は[提出方法](#submission-procedure)にある．

## 参考資料

ゲームルールとλマンプロセッサの参考実装を用意してある．

- [http://icfpcontest.org/reference.html](http://icfpcontest.org/reference.html)

ここには他にも参考資料があり，地図の例など役にたつ資料がある．

この仕様と参考実装(その他)との間に食い違いがある場合はすぐに審査員に知らせてください！

# <a name="the-lambda-man-game-rules">λマンゲームのルール</a>

## 目的

λマンは壁にかこまれた2次元迷図上で活動し，追い掛けてくる幽霊を回避しつつ，食えるだけの錠剤を食わなければならない．λマンには命が3つあって，幽霊に掴まると命を1つは失なう．λマンの命がすべてなくなった時点でゲームオーバーである．錠剤をすべて食べればλマンはそのレベルを完遂したことになる．

錠剤以外に，λマンは強力錠剤も食う．強力錠剤を食うとλマンは短時間だけ幽霊を食う能力を得る．

フルーツを食えばボーナスポイントが付く．フルーツは定まった時間ごと？に地図上の特定の位置に現れる．

規約として以下の記号を用いてゲームの各種要素を表現する．

|   記号    |     要素    |
|:---------:|:-----------:|
|  <space>  |      空     |
|    \#	    |      壁     |
|    \.	    |     錠剤    |
|    o      |  強力錠剤 |
|    %      |     フルーツ    |
|    \\	    |   λマン    |
|    =      |     幽霊    |

## 機構

世界は完全に決定的であり，クロックの刻みごとに走る．

時の刻みごとに，

1. すべてのλマンと幽霊はこの刻みに併せて移動する．（λマンと幽霊はすべての刻みで動くわけではなく，何刻みかごとに動くことに注意せよ．）
2. つぎに，すべてのアクション（イジケモードの解除，フルーツの出現/消滅）がここで起こる．
3. つぎに，λマンが錠剤，スーパー錠剤，フルーツと同じセルにあるかをチェックする．<br>
    1. λマンが錠剤と同じセルにあれば，λマンは錠剤を食べ，錠剤はゲームからなくなる．
    2. λマンが強力錠剤と同じセルにあれば，λマンは錠剤を食べ，強力錠剤はゲームからなくなる．
    3. λマンがフルーツと同じセルにあれば，λマンは錠剤を食べ，フルーツはゲームからなくなる．
4. つぎに，1つあるいは複数の幽霊がλマンと同じセルにあれば，幽霊モードが有効かどうかによって，λマンが命を1つ失うか，幽霊を食うかのどちらかになる．詳細は後述．
5. つぎに，通常の錠剤（すなわち強力錠剤ではない）がすべて食べつくされたら，λマンの勝利で，ゲームは終了．
6. つぎに，λマンの命の数が0なら，λマンの負けでゲームは終了．
7. 最後に時刻刻みが1つ進む．

## 命を1つ失うとき

チクの最後でλマンが見えている幽霊と同じセルにいて，イジケモードが有効になっていなければ，λマンは命を1つ失う．この場合，λマンおよびすべての幽霊はただちにスタート地点に戻りその向きもスタート時点と同じ向きになる（したがって，次のチクの開始点ではλマンおよび幽霊はスタート地点にいる）．

## 強力錠剤

強力錠剤を食うと，すべての幽霊は反転して反対方向に向き直前の位置に移動し，イジケモードが有効になる．イジケモードが有効な間に幽霊と同じセルをλマンが占めれば，幽霊は食われる．幽霊は食われたら，スタート地点に戻り，スタート時点での方向を向く．そうしてイジケモードが切れるまで見えなくなる．見えないあいだは幽霊は，食いも食われもしない．

イジケモードのときに強力錠剤を食うと，イジケモードの時刻カウントはリセットされる．

イジケモードが切れたら，すべての幽霊は見えるようになる．

## 得点

このゲームの目的は最高点を獲得することである．すべてのレベルでのすべての得点の和を最高にすることである．
得点は以下のようにして獲得する．

錠剤1個が10点．

強力錠剤が50点．

フルーツはその風味によって得点がことなる．フルーツはレベルで風味がちがい，それは以下のようになっている．

|  レベル |   風味     |  得点    |
|:--------|:-----------|---------:|
|  1	  |   Cherry   |  100     |
|  2      | Strawberry |  300     |
|  3      |  Peach     |  500     |
|  4      |  Peach     |  500     |
|  5      |  Apple     |  700     |
|  6      |  Apple     |  700     |
|  7      |  Grapes    | 1000     |
|  8      |  Grapes    | 1000     |
|  9      |  Galaxian  | 2000     |
| 10      |  Galaxian  | 2000     |
| 11      |  Bell      | 3000     |
| 12      |  Bell      | 3000     |
| > 12    |  Key       | 5000     |

地図のレベルはその面積できまる．mapWidth * mapHeightの大きさの地図だとすると，そのレベルは以下を満す．

```
100 * (level - 1) < mapWidth * mapHeight <= 100 * level
```

たとえば，15 * 18 = 270のサイズだとすると，これはレベル3の地図である．200 < 270 <= 300 だからである．

イジケモードの間に最初に幽霊を食うと200点である．
ひきつづき次の強力錠剤を食うまでに幽霊を食うごとに得点は前の得点の2倍になり，上限は1600点である．

| 食った幽霊         | 得点 |
|:-------------------|-----:|
| 1匹目              |   200|
| 2匹目              |   400|
| 3匹目              |   800|
| 4匹目およびそれ以降|  1600|

λマンが1つのレベルで錠剤をすべて食えば，ボーナスで，得点が残りの命の数に1を加えたものかけたものになる．
たとえば，1つしか命がのこっていなければ，得点が倍になるということである．

## 時の刻み

世界は時の刻みで走る．
Ultimate Tick Clock (UTC)は現在のチク時刻である．
各ゲームの開始時に1から始まる．
ゲームはEnd Of Lives (EOL)までで，EOLはλマンが命を使い切ったとき，もしくは，ある定まったUTCが来て命が0にセットされたときである．

| イベント | UTC                             |
|:---------|:--------------------------------|
|  EOL     | 127 * mapWidth * mapHeight * 16 |

各レベルでフルーツは2つ現れる．特定のUTCで，1つ目のフルーツが出現する．
2つめのフルーツが現れかどうかは1つ目のフルーツが食われたかどうかによる．

| イベント | UTC                             |
|:---------|:--------------------------------|
| フルーツ1| 127 * 200 に出現                |
| フルーツ2| 127 * 400 に出現                |

それぞれのフルーツは食われてしまうか，特定の時刻が来れば期限切れになって消える．

| イベント | UTC                             |
|:---------|:--------------------------------|
| フルーツ1| 期限切れ 127 * 280              |
| フルーツ2| 期限切れ 127 * 480              |

それぞれの強力錠剤を食うとイジケモードに以降する．
イジケモードは，最近に食った強力錠剤を食った時点から一定の期間がたてば効力が消える．

| イベント         | チク期間                        |
|:-----------------|:--------------------------------|
| イジケモード期間 | 127 * 20                        |

λマンと幽霊の移動速度は異なる．λマンは食っているときは遅く，幽霊はイジケモードのときに移動が遅くなる．
すべての移動は一定の間隔でおこり，移動のチクは以下のとおり．
たとえば，λマンは最初移動は127チクに起こり，次は154チクに移動する．

| イベント               | UTC    |
|:-----------------------|:-------|
| λマン                 | 127    |
| λマン(食ってるとき)   | 137    |
| 幽霊 0                 | 130    |
| 幽霊 1                 | 132    |
| 幽霊 2                 | 134    |
| 幽霊 3                 | 136    |
| 幽霊 0 (イジケモード)  | 195    |
| 幽霊 1 (イジケモード)  | 198    |
| 幽霊 2 (イジケモード)  | 201    |
| 幽霊 3 (イジケモード)  | 204    |

λマンあるいは幽霊が移動するチクカウントにおいて次の移動のチクカウントが設定される．
これはそのときの状態に依存する．
たとえば，λマンが錠剤のあるセルにいたら次の移動はその直前の移動に設定されたチクカウントに137を加えた時刻である．

幽霊が5体以上いるときには，チクカウントは循環的に割り当てられる．つまり幽霊4は幽霊0と同じ設定になる．
λマンや幽霊のチク設定が再設定されるイベントや条件は他にはない（フライトモードに入るとき，幽霊が食われるとき，λマンが食われるときでも再設定されることはない）．

## 移動

λマンは壁がない隣のセルに移動できる．隣のセルとは1つ上，1つ下，1つ左，1つ右のセルのことである．

λマンが不正な移動選ぶと移動しません．

幽霊は壁のない隣のセルにしか移動できません．
ゴーストが移動する時刻にはゴーストは必ず移動しなければなりません．4方が壁に囲まれていないかぎり．さらに，ゴーストは今向いている方向と反対方向には移動できません．ただし，三方を壁でかこまれていて，そちらにしか移動できない場合はその限りではありません．

結果として，ゴーストはジャンクションでは後戻り方向以外の方向しか選べません．
ジャンクションは少くとも3方向に壁のないセルがある場所をいいます．
例えば以下はすべてジャンクションです．

```
 # #     # #             # #     # # 
## ##    # ##   #####   ## #    ## ##
  =      #=       =       =#      =  
#####    # ##   ## ##   ## #    ## ##
         # #     # #     # #     # # 
```

## ゴースト

ゴーストが不正な移動を選択すると（あるいは全く動かないことを選択すると）上右下左の順に見て動ける方向に動きます．

ゲーム開始時にはゴースト，λマンはすべて下を向いています．

## トーナメント得点

ライトニングラウンドでは君のλマンと審査員の用意した幽霊と審査員が用意した迷図で対決します．

ライトニングラウンドでの得点は，一連（別別の地図で）の君とのゲームで君が獲得した得点の総和になる．
ここで使われた地図と幽霊は見えないのであるが，簡単なものから難しいものへの順にならんでいる．
その地図の性質については[後述する](#map-properties)．

## <a name="map-properties">地図の性質</a>

地図は長方形で，x 方向，y 方向ともに 0 から番号が付く．
左上の角が(0,0)である．したがって，x 方向は右へいくほど番号が多きくなり，y方向は下えいくほど番号が大きくなる．

地図は迷図のようになっており，1つの正方形のセル幅の通路で構成されており，それより広い場所はない．
より形式的にいうと，壁を含まないセルの2×2の空間はない．

地図の周辺は壁がぐるりと回っている．

地図にあるすべての錠剤は到達可能である．

地図のサイズ，幽霊の数，強力錠剤の数は地図ごとに変る．
簡単な地図ほど，小さく，幽霊の数は少く，強力錠剤の密度は高い．
難しい地図ほど，大きく，幽霊の数は多く，強力錠剤の密度は低い．
最大の地図サイズは 256 × 256 である．

## 幽霊と幽霊のプログラム

幽霊AIプログラムはそれぞれの地図ごとに幽霊に割り当てられている．
AIプログラムの数以上の幽霊がいる地図ではAIプログラムを循環的に割り当てる．

たとえば，幽霊が4体，AIが2つの場合の割あては以下のとおり

| 幽霊     | AI プログラム |
|:---------|:--------------|
| ghost 1  | program 1     |
| ghost 2  | program 2     |
| ghost 3  | program 1     |
| ghost 4  | program 2     |

幽霊の順番は出発の座標の大小できまります．

このシステムでは高々4つの幽霊プログラムしか使いませんが幽霊は5体以上です．

# GHost CPU (GHC)

GHost CPU (GHC)は旧型の8bitマイクロコントローラである．
それぞれの幽霊が別々のGHCで動く．
CPUマニュアルの完全なコピーを見つけたが，簡単なパーツで，当時の慣習にしたがって書かれている．
君らは若くて1980年代のことなどわからないだろうから，すこし説明しておいた．

## GHC State

各レジスタは8bit符号なし整数を保持（0から255まで）．
メモリはデータメモリとコードメモリの2本あり，それぞれ格納場所が0番から255番まである．
したがって，レジスタの内容は直接データメモリアドレス，データメモリ場所の内容，コードメモリアドレスと解釈できる．
GHCはすべてを256のモデューロで実行する．したがって，255 + 1 = 0 である．

## 初期化とプログラム実行

ゲーム開始時にGHCのコードメモリはプログラムで初期化する．コードの形式は[コード形式](#code-format)の節で説明する．
コードメモリの内容はゲーム中は変更されない．
すべてのデータメモリ格納位置とすべてのレジスタは0で初期化される．

それぞれのゲームサイクルにおいて，GHCはプログラムを走らせ，1024個(まで)の命令を実行する．
すなわち，そのゲームサイクルは1024の実行サイクルに分割される．
1つの実行サイクルはPCがよって参照されているアドレスの命令をコードメモリからGHCが読むところから始まる．
GHCは命令を実行する．命令については[命令リファレンス](#instructionReference)を参照のこと．
命令によってデータメモリ，レジスタの内容が変更されることがある．
実行サイクルの最後にPCの値がその実行サイクル開始時の値と同じであれば，GHCはそれを1インクリメントする．
実行は以下は，いかのような実行サイクルの最後に終了するものとする．
実行された命令がHLTである場合，そのゲームサイクルで1024番目の実行サイクルであった場合，実行した命令がエラーを起こした場合．
データメモリの内容とレジスタの内容はゲーム中は永続的に保持される．

## <a name="#code-format">Code Format</a>

規約により，GHCプログラムは.ghcという拡張子のファイルに格納．

プログラムは複数の行で構成され，行は改行文字で終端する．
行の内容は，空白はいくつつづいても1つの空白と見做す．
行は，1つの白空白のみを含む空行か，命令を1つ含む．

プログラムには;を使ってコメントを入れられる．;から行末までが無視される．
したがって，コメントのみの行は空行とみなされる．

命令は0個以上の引数をもつニーモニックである．
ニーモニックはアルファベットの大文字小文字を区別しない．

引数は以下のどれかである．

1. レジスタ引数（ A から H あるいは PC で示す）
2. 間接汎用レジスタ引数（角括弧でかこって，[A] .. [H] として示す [PC]はないことに注意）．
3. 定数引数．（10進で 0 から 255 まで）
4. データメモリ格納場所の定数（アドレスを示す10進を角括弧で囲う[0] から [255]）．

必要な引数の数は命令によって違う．[命令リファレンスを参照](#instruction-reference)

ニーモニックと最初の引数との間には1つの空白がなければならない．
引数と引数の間には , がなければならない．

命令は前後に空白がつづいてもよい．
引数および引数区切り子のコンマの前後に空白があってもよい．

GHCが初期化されたとき，各行に含まれる1命令がメモリの対応する位置に格納される．
たとえば，最初の空でない行の命令はアドレス0に格納される．空でない2つ目の行の命令はアドレス1に格納される．
256個しか格納場所がないので，プログラムは高々256命令である．

# <a name="instructionReference">命令リファレンス</a>

GHCは以下の命令を実行できる

```
MOV dest,src
```

``src``引数の値を``dest``引数にコピー．``dest``は定数であってはならない．

```
INC dest
```

``dest``の値を1増やし，結果を``dest``に格納する．``dest``は定数あるいはPCレジスタであってはならない．

```
DEC dest
```

``dest``の値を1減らし，結果を``dest``に格納する．``dest``は定数あるいはPCレジスタであってはならない．

```
ADD dest,src
```

``src``の値を``dest``の値に加え，結果を``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
SUB dest,src
```

``src``の値を``dest``の値から引き，結果を``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
MUL dest,src
```

``src``の値を``dest``の値倍し，結果を``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
DIV dest,src
```

整数の除算を計算する``dest``の値を``src``の値で除算した商を``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．``src``が0のときエラーになる．

```
AND dest,src
```

``dest``の値と``src``の値のビット毎のANDを取る．結果は``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
OR dest,src
```

``dest``の値と``src``の値のビット毎のORを取る．結果は``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
XOR dest,src
```

``dest``の値と``src``の値のビット毎のXORを取る．結果は``dest``に格納する．``dest``は定数またはPCレジスタであってはならない．

```
JLT targ,x,y
```

``x``の値と``y``の値を比較し，``x``の値が``y``の値未満であれば，PCを定数値``targ``に設定する．

```
JEQ targ,x,y
```

``x``の値と``y``の値を比較し，``x``の値が``y``の値と等しければ，PCを定数値``targ``に設定する．

```
JGT targ,x,y
```

``x``の値と``y``の値を比較し，``x``の値が``y``の値より大きければ，PCを定数値``targ``に設定する．

```
INT i
```

割り込みサービス``i``番を起動する．[割り込みリファレンス](#interrupt-reference)参照．

```
HLT
```

GHCの実行を停止する．

# <a name="#interrupt-reference">割り込みリファレンス</a>

割り込みサービスの起動効果はアーキテクチャ依存である．LamCoのアーキテクチャでは以下のような標準にしたがっている．

```
INT 0
```

- 入力:
    - レジスタA: 幽霊の新しい方向<br>
      幽霊の無機を設定する．0 は上，1 は右，2 は下，3 は左

ゲームサイクルの最後に幽霊の方向が設定される．
この割り込みは単一のゲームサイクルの中で複数回呼ばれた場合には最後の割り込みでの設定がすべての設定を上書きする．
レジスタAに不正な方向が使われた場合にはそのゲームサイクルの開始時点での幽霊の元々の方向が保たれる．

```
INT 1
```

- 出力:
    - レジスタ A: 1つめのλマンの x 座標
    - レジスタ B: 1つめのλマンの y 座標

1つめのλマンの位置をレジスタAとレジスタBに格納する．
単一λマン版では，1つめのλマンが唯一のλマン．

```
INT 2
```

- 出力:
    - レジスタ A:  2つめのλマンの x 座標
    - レジスタ B:  2つめのλマンの y 座標

2つめのλマンの位置をレジスタAとレジスタBに格納する．
単一λマン版では，の割り込みの動作は不明．

````
INT 3
```

- 出力:
    - レジスタ A: この幽霊の番号

この幽霊の番号をレジスタAに格納する．

```
INT 4
```

- 入力:
    - A: 幽霊の番号
- 出力
    - レジスタ A: 番号で指定した幽霊の開始時の x 座標
    - レジスタ B: 番号で指定した幽霊の開始時の y 座標

レジスタAから読んだ番号の幽霊の開始時の位置をレジスタAとレジスタBに格納する．

```
INT 5
```

- 入力:
    - A: 幽霊の番号
- 出力
    - レジスタ A: 番号で指定した幽霊の現在の x 座標
    - レジスタ B: 番号で指定した幽霊の現在の y 座標

レジスタAから読んだ番号の幽霊の現在の位置をレジスタAとレジスタBに格納する．

```
INT 6
```

- 入力:
    - A: 幽霊の番号
- 出力
    - レジスタ A: 番号で指定した幽霊の現在のバイタリティ
    - レジスタ B: 番号で指定した幽霊の現在の向き

レジスタAから読んだ番号の幽霊の現在のバイタリティをレジスタAに現在の向きをレジスタBに格納する．

- バイタリティ:
    - 0: 標準
    - 1: イジケモード
    - 2: 不可視モード

```
INT 7
```

- 入力:
    - レジスタ A: セルの x 座標
    - レジスタ B: セルの y 座標
- 出力:
    - レジスタ A: 指定したセルの内容

レジスタ A とレジスタ Bで指定した位置のセルの内容をレジスタ A に格納．指定した座標が地図外なら0が入る．

- 内容
    - 0: 壁 (#)
    - 1: 空 (<space>)
    - 2: 錠剤
    - 3: 強力錠剤
    - 4: フルーツ
    - 5: λマンのスタート地点
    - 6: 幽霊のスタート地点

```
INT 8
```

- 入力:
    - レジスタ PC
    - レジスタ A..H

PCの現在の値とすべてのレジスタの値を外部のデバッグ/トレースエージェントに送る．

## 例

以下のGHCプログラムは命令列のふるまいを説明するためのものである．

- miner.ghc

```
; Always try to go down.
mov a,2
int 0
hlt
```

- flipper.ghc

```
; Go up if our x-ordinate is even, or down if it is odd.
int 3          ; Get our ghost index in A.
int 5          ; Get our x-ordinate in A.
and a,1        ; Zero all but least significant bit of A.
               ; Now A is 0 if x-ordinate is even, or 1 if it is odd.
mov b,a        ; Save A in B because we need to use A to set direction.
mov a,2        ; Move down by default.
jeq 7,b,1      ; Don't change anything if x-ordinate is odd.
mov a,0        ; We only get here if x-ordinate was even, so move up.
int 0          ; This is line 7, the target of the above jump. Now actually set the direction.
hlt            ; Stop.
```

- fickle.ghc

```
; Keep track of how long we have spent travelling in each direction.
; Try to go in the direction we've travelled in least.

               ; Count of time spent going in direction 0 is in memory address 0, and so on.
mov a,255      ; A is the min value.
mov b,0        ; B is the corresponding direction.
mov c,255      ; C is the candidate direction for the new min.
               ; Start of loop.
inc c          ; Pick new direction.
jgt 7,[c],a    ; Jump if count of direction C is above best so far.
               ; We have a new min.
mov a,[c]      ; Save new min.
mov b,c        ; Save direction.
jlt 3,c,3      ; Jump target. Loop back if we have not tried all 4 directions.

mov a,b        ; Actually set desired direction.
int 0

int 3          ; Get our ghost index in A.
int 6          ; Get out current direction in B.
inc [b]        ; Increment corresponding count.
hlt            ; Stop.
Errors
```

As mentioned above, if an instruction causes an error then execution halts. If prior to this the new direction of the Ghost was set (with INT 0) then this will be the requested move of the ghost and the game will continue. If the direction is not set then the ghost's requested move will be the same as its previous move.

# Lambda-Man CPU

我々はλマンAIチームが使っていたプログラミング環境についてのいくつかの文書を復元することができた.
それはプロセッサISAと少しばかりのアセンブリコードとを含んでいた．
λマンAIチームがLISPにとりつかれていて,あるLISP形式を使っていたことは知っていたが，
残念なことに彼等のLISPコードやコンパイラについてはいかなるものもついに発見することはできなかった．

LamCo "General Compute Coprocessor" (GCC) は当時としてはかなり斬新で洗練されたコプロセッサだった．
それはLISPコンパイラをターゲットとして設計されているように思われた．
直交する命令セットではなく,コンパイラにとって有用であったに違いないと(思われる)
かなりの数のいくぶん特殊化された命令を持っていた.
我々はそれでも一人のエンジニアによる手書きのノートを見つけた.
そこには誰かがPascalの変種でコンパイラを書いたことが示されたいた.
いくつか制限付きではあったが．

幸運にも我々はプロセッサの原文を持っており,そこには命令と操作が詳細に記されている.
だが実に悲しいことにコンパイラからどのように使うつもりであったかについては十分ではなかった.

以降の節は原文からの抜粋に我々がコメントを添えたものだ．

## 全体構成

このマシンはスタックマシンで,3つのスタックをそれぞれ異なる目的のために使う.
こいつは相対的に(必要に応じて)大きなメモリを持つ.
メモリへのアクセス方法や構成方法はまったく普通とは違っている.
メモリで生きているスタックは別として,残りのメモリはゴミ集めされたヒープのために使われる.
このゴミ集め機構はハードウェアによって実装されている.
このため汎用のメモリアクセス命令は存在しない.
全てのメモリアクセスはこれらスタックかゴミ集めされたヒープかになる.

## CPU Registers

There are 4 programmer visible machine registers, all of which are for special purposes:

- %c: control register (program counter / instruction pointer)
- %s: data stack register
- %d: control stack register
- %e: environment frame register

## Memory stacks

Three of the registers point into special data structures in memory:

- Data stack
- Control stack
- Environment frame chain
- The remainder of the memory is dedicated to the data heap.

## Control register and program code layout

The machine has logically separate address spaces for code versus data.
The %c register is an instruction pointer, pointing to the next instruction to be executed. 
Programs are laid out from low addresses to high. 
The effect of most instructions on the instruction pointer is simply to increment its value by one.

## Data stack and register

The data stack is used to save intermediate data values during calculations, and to return results from function calls.

It is a logically contiguous stack. The %s register points to the top of the stack.

Many of the instructions simply pop and push values on the data stack. For example the ADD instruction pops two integer values off the stack and pushes back their sum.

# Control stack and register

The control stack is used to save return information in function calls. It saves return address and environment frame pointers.

It is a logically contiguous stack.

Only the complex control flow instructions affect the control stack and register. See SEL/JOIN and AP/RAP/RTN for details.

## Environment frames and register

The environment is used for storing local variables, including function parameters. There is an instruction for loading values from the environment onto the top of the data stack. The environment consists of a chain of frames, which is used to implement nested variable scopes within higher level languages, such as local blocks with extra local variables and functions.

The environment is more complex than the two stack structures. Rather than a contiguous stack, it consists of a chain of environment frames. Each frame contains a pointer to its parent frame. In fact the chain of frames is not strictly a stack: the lifetime of the frames is not always a simple LIFO stack order. Because of this the frames are managed by the hardware GC.

Each frame consists of a pointer to its parent frame and zero or more data values. The %e register points to the local frame. The machine has direct support for loading a value from the local frame or any of its parent frames, which in general requires following the chain of environment frames.

(We believe in the real hardware this feature was implemented in microcode, with internal registers to cache the outermost frame and a fixed number of the inner most frames for quick access).

## Data heap and data values

The machine makes extensive use of dynamic allocations and has built-in support for a garbage collected data heap. Values in the data stack and in environment frames can be pointers into the heap.

There are three types of data value: integers, pairs and closures. Integers are represented in the usual way as binary signed integers. Pairs and closures are represented by pointers to objects in the data heap. The three types of values are distinguished by tag bits. The tag bits are not software visible (except for the ATOM instruction) but are used by the hardware for error checking.

There are three instructions for manipulating pairs: allocating a pair, accessing the first and accessing the second component of a pair.

All program data structures have to be represented using combinations of pairs and integers (and sometimes closures).

# Instruction reference

-----

- LDC - load constant

- Synopsis: load an immediate literal;
    - push it onto the data stack
- Syntax:  LDC $n
- Example: LDC 3
- Effect:

```
  %s := PUSH(SET_TAG(TAG_INT,$n),%s)
  %c := %c+1
```

-----

- LD - load from environment

- Synopsis: load a value from the environment;
    - push it onto the data stack
- Syntax:  LD $n $i
- Example: LD 0 1
- Effect:

```
  $fp := %e
  while $n > 0 do            ; follow chain of frames to get n'th frame
  begin
    $fp := FRAME_PARENT($fp)
    $n := $n-1
  end
  $v := FRAME_VALUE($fp, $i) ; i'th element of frame
  %s := PUSH($v,%s)          ; push onto the data stack
  %c := %c+1
```

- Notes: Values within a frame are indexed from 0.

-----

- ADD - integer addition

- Synopsis: pop two integers off the data stack;
    - push their sum
- Syntax: ADD
- Effect:

```
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x + $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1
```

-----

- SUB - integer subtraction

- Synopsis: pop two integers off the data stack;
    - push the result of subtracting one from the other
- Syntax: SUB
- Effect:

```
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x - $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1
```

-----

- MUL - integer multiplication

- Synopsis: pop two integers off the data stack;
    - push their product
- Syntax: MUL
- Effect:

```
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x * $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1
```

-----

- DIV - integer division

- Synopsis: pop two integers off the data stack;
    - push the result of the integer division of one of the other
- Syntax: DIV
- Effect:

```
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x / $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1
```

-----

- CEQ - compare equal

- Synopsis: pop two integers off the data stack;
          test if they are equal;
          push the result of the test
Syntax: CEQ
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x == $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CGT - compare greater than

Synopsis: pop two integers off the data stack;
          test if the first is strictly greater than the second;
          push the result of the test
Syntax: CGT
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x > $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CGTE - compare greater than or equal

Synopsis: pop two integers off the data stack;
          test if the first is greater than or equal to the second;
          push the result of the test
Syntax: CGTE
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x >= $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


ATOM - test if value is an integer

Synopsis: pop a value off the data stack;
          test the value tag to see if it is an int;
          push the result of the test
Syntax: ATOM
Effect:
  $x,%s := POP(%s)
  if TAG($x) == TAG_INT then
    $y := 1
  else
    $y := 0
  %s := PUSH(SET_TAG(TAG_INT,$y),%s)
  %c := %c+1


CONS - allocate a CONS cell

Synopsis: pop two values off the data stack;
          allocate a fresh CONS cell;
          fill it with the two values;
          push the pointer to the CONS cell
Syntax: CONS
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  $z := ALLOC_CONS($x,$y)
  %s := PUSH(SET_TAG(TAG_CONS,$z),%s)
  %c := %c+1


CAR - extract first element from CONS cell

Synopsis: pop a pointer to a CONS cell off the data stack;
          extract the first element of the CONS;
          push it onto the data stack
Syntax: CAR
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_CONS then FAULT(TAG_MISMATCH)
  $y := CAR($x)
  %s := PUSH($y,%s)
  %c := %c+1


CDR - extract second element from CONS cell

Synopsis: pop a pointer to a CONS cell off the data stack;
          extract the second element of the CONS;
          push it onto the data stack
Syntax: CDR
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_CONS then FAULT(TAG_MISMATCH)
  $y := CDR($x)
  %s := PUSH($y,%s)
  %c := %c+1


SEL - conditional branch

Synopsis: pop an integer off the data stack;
          test if it is non-zero;
          push the return address to the control stack;
          jump to the true address or to the false address
Syntax:  SEL $t $f
Example: SEL 335 346  ; absolute instruction addresses
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  %d := PUSH(SET_TAG(TAG_JOIN,%c+1),%d)   ; save the return address
  if $x == 0 then
    %c := $f
  else
    %c := $t


JOIN - return from branch

Synopsis: pop a return address off the control stack, branch to that address
Syntax:  JOIN
Effect:
  $x,%d := POP(%d)
  if TAG($x) != TAG_JOIN then FAULT(CONTROL_MISMATCH)
  %c := $x


LDF - load function

Synopsis: allocate a fresh CLOSURE cell;
          fill it with the literal code address and the current
            environment frame pointer;
          push the pointer to the CLOSURE cell onto the data stack
Syntax:  LDF $f
Example: LDF 634      ; absolute instruction addresses
Effect:
  $x := ALLOC_CLOSURE($f,%e)
  %s := PUSH(SET_TAG(TAG_CLOSURE,$x),%s)
  %c := %c+1


AP - call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          allocate an environment frame of size $n;
          set the frame's parent to be the environment frame pointer
            from the CLOSURE cell;
          fill the frame's body with $n values from the data stack;
          save the stack pointer, environment pointer and return address
            to the control stack;
          set the current environment frame pointer to the new frame;
          jump to the code address from the CLOSURE cell;
Syntax:  AP $n
Example: AP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $e := CDR_CLOSURE($x)
  $fp := ALLOC_FRAME($n)      ; create a new frame for the call
  FRAME_PARENT($fp) := $e
  $i := $n-1
  while $i != -1 do           ; copy n values from the stack into the frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  %d := PUSH(%e,%d)                     ; save frame pointer
  %d := PUSH(SET_TAG(TAG_RET,%c+1),%d)  ; save return address
  %e := $fp                             ; establish new environment
  %c := $f                              ; jump to function


RTN - return from function call

Synopsis: pop a stack pointer, return address and environment frame
            pointer off of the control stack;
          restore the stack and environment;
          jump to the return address
Syntax:  RTN
Effect:
  $x,%d := POP(%d)            ; pop return address
  if TAG($x) == TAG_STOP then MACHINE_STOP
  if TAG($x) != TAG_RET then FAULT(CONTROL_MISMATCH)
  $y,%d := POP(%d)            ; pop frame pointer
  %e := $y                    ; restore environment
  %c := $x                    ; jump to return address
Notes:
  Standard ABI convention is to leave the function return value on the
  top of the data stack. Multiple return values on the stack is possible,
  but not used in the standard ABI.

  The latest hardware revision optimizes the deallocation of the 
  environment frame. If the environment has not been captured by LDF
  (directly or indirectly) then it can be immediately deallocated.
  Otherwise it is left for GC.


DUM - create empty environment frame

Synopsis: Prepare an empty frame;
          push it onto the environment chain;
Syntax:  DUM $n
Example: DUM 3      ; size of frame to allocate
Effect:
  $fp := ALLOC_FRAME($n)       ; create a new empty frame of size $n
  FRAME_PARENT($fp) := %e      ; set its parent frame
  %e := SET_TAG(TAG_DUM,$fp)   ; set it as the new environment frame
  %c := %c+1
Notes:
  To be used with RAP to fill in the frame body.


RAP - recursive environment call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          the current environment frame pointer must point to an empty
            frame of size $n;
          fill the empty frame's body with $n values from the data stack;
          save the stack pointer, parent pointer of the current environment
             frame and return address to the control stack;
          set the current environment frame pointer to the environment
            frame pointer from the CLOSURE cell;
          jump to the code address from the CLOSURE cell;
Syntax:  RAP $n
Example: RAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $fp := CDR_CLOSURE($x)
  if TAG($fp) != TAG_DUM then FAULT(FRAME_MISMATCH)
  if FRAME_SIZE($fp) != $n then FAULT(FRAME_MISMATCH)
  $i := $n-1
  while $i != -1 do           ; copy n values from the stack into the empty frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  $fpp := FRAME_PARENT($fp)
  %d := PUSH($fpp,%d)                   ; save frame pointer
  %d := PUSH(SET_TAG(TAG_RET,%c+1),%d)  ; save return address
  %e := $fp                             ; establish new environment
  %c := $f                              ; jump to function


STOP - terminate co-processor execution

Synopsis: terminate co-processor execution and signal the main proessor.
Syntax:  STOP
Effect:
  MACHINE_STOP
Notes:
  This instruction is no longer part of the standard ABI. The standard ABI
  calling convention is to use a TAG_STOP control stack entry. See RTN.
Tail call extensions

TSEL - tail-call conditional branch

Synopsis: pop an integer off the data stack;
          test if it is non-zero;
          jump to the true address or to the false address
Syntax:  TSEL $t $f
Example: TSEL 335 346  ; absolute instruction addresses
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x == 0 then
    %c := $f
  else
    %c := $t
Notes:
  This instruction is the same as SEL but it does not push a return address


TAP - tail-call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          allocate an environment frame of size $n;
          set the frame's parent to be the environment frame pointer
            from the CLOSURE cell;
          fill the frame's body with $n values from the data stack;
          set the current environment frame pointer to the new frame;
          jump to the code address from the CLOSURE cell;
Syntax:  TAP $n
Example: TAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $e := CDR_CLOSURE($x)
  $fp := ALLOC_FRAME($n)      ; create a new frame for the call
  FRAME_PARENT($fp) := $e
  $i := $n
  while $i != 0 do            ; copy n values from the stack into the frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  %e := $fp                   ; establish new environment
  %c := $f                    ; jump to function
Notes:
  This instruction is the same as AP but it does not push a return address

  The latest hardware revision optimizes the case where the environment
  frame has not been captured by LDF and the number of args $n in the
  call fit within the current frame. In this case it will overwrite the
  frame rather than allocating a fresh one.

TRAP - recursive environment tail-call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          the current environment frame pointer must point to an empty
            frame of size $n;
          fill the empty frame's body with $n values from the data stack;
          set the current environment frame pointer to the environment
            frame pointer from the CLOSURE cell;
          jump to the code address from the CLOSURE cell;
Syntax:  TRAP $n
Example: TRAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $fp := CDR_CLOSURE($x)
  if TAG($fp) != TAG_DUM then FAULT(FRAME_MISMATCH)
  if FRAME_SIZE($fp) != $n then FAULT(FRAME_MISMATCH)
  $i := $n
  while $i != 0 do            ; copy n values from the stack into the empty frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  $fpp := FRAME_PARENT($fp)
  %e := $fp                   ; establish new environment
  %c := $f                    ; jump to function
Notes:
  This instruction is the same as RAP but it does not push a return address
Pascal extensions

ST - store to environment

Synopsis: pop a value from the data stack and store to the environment
Syntax:  ST $n $i
Example: ST 0 1
Effect:
  $fp := %e
  while $n > 0 do            ; follow chain of frames to get n'th frame
  begin
    $fp := FRAME_PARENT($fp)
    $n := $n-1
  end
  $v,%s := POP(%s)           ; pop value from the data stack
  FRAME_VALUE($fp, $i) := $v ; modify i'th element of frame
  %c := %c+1
Debug extensions

DBUG - printf debugging

Synopsis: If tracing is enabled, suspend execution and raise a trace
          interrupt on the main processor. The main processor will read
          the value and resume co-processor execution. On resumption
          the value will be popped from the data stack. If tracing is not
          enabled the value is popped from the data stack and discarded.
Syntax:  DBUG
Effect:
  $x,%s := POP(%s)
  %c := %c+1
Notes:
  This is the formal effect on the state of the machine. It does
  also raise an interrupt but this has no effect on the machine state.

BRK - breakpoint debugging

Synopsis: If breakpoint debugging is enabled, suspend execution and raise
          a breakpoint interrupt on the main processor. The main processor
          may inspect the state of the co-processor and can resume
          execution. If breakpoint debugging is not enabled it has no
          effect.
Syntax:  BRK
Effect:
  %c := %c+1
Examples

Although we did not find any code from their higher level language, the instruction reference does include a couple examples. Note that it uses an assembly syntax with symbolic labels rather than absolute instruction addresses, so these programs cannot be executed directly.

The following GCC programs illustrate the behaviour of some of the
instructions:
local.gcc

Minimal example of creating and using a local variable.

  LDC  21
  LDF  body     ; load body
  AP   1        ; call body with 1 variable in a new frame
  RTN
body:
  LD   0 0      ; var x
  LD   0 0      ; var x
  ADD
  RTN
goto.gcc

Minimal example of mutual recursion. Note that the recursion in this
example does not terminate. It will fail with an out of memory error
due to the stack use.

  DUM  2        ; 2 top-level declarations
  LDF  go       ; declare function go
  LDF  to       ; declare function to
  LDF  main     ; main function
  RAP  2        ; load declarations into environment and run main
  RTN           ; final return
main:
  LDC  1
  LD   0 0      ; var go
  AP   1        ; call go(1)
  RTN
to:
  LD   0 0      ; var n
  LDC  1
  SUB
  LD   1 0      ; var go
  AP   1        ; call go(n-1)
  RTN
go:
  LD   0 0      ; var n
  LDC  1
  ADD
  LD   1 1      ; var to
  AP   1        ; call to(n+1)
  RTN
The processor/co-processor interface

In addition to the basic processor description, we found some documentation on the interface between the main processor and the co-processor.

On co-processor power up, it is in the halt state with heap and stacks
initialised to empty.

The primary processor uses the processor/co-processor interface (see
appendix 3.II) to initialise the code, heap, stacks and registers and
initiate execution.
(We don't actually have appendix 3.II, but fortunately we don't need it for our software simulation.)

When execution halts, the co-processor will raise an interrupt
(implementation defined) in the primary processor. The primary processor
may use the processor/co-processor interface to inspect the registers,
stack and heap. It may inspect special status registers to determine the
reason for execution halting: stop, fault, trace or breakpoint.
Essentially the main processor sets things up to run, being able to control the full state of the co-processor, then it tells the co-processor to run. When the co-processor halts then the main processor can look at the end state to get the program result. The state of the co-processor is preserved to use in a subsequent run.

When using the standard ABI, the primary processor may ensure execution
halts on the final return by installing an entry in the control stack (%d
register) with the TAG_STOP set.
The standard calling convention for functions/procedures apparently uses the RTN instruction. So for the main processor to call a function on the co-processor it needs a way to get it to halt after that function returns. The way it does that is by putting a special "stop" entry on the control stack so that when the function returns, instead of jumping to a return address the co-processor simply halts.

In the standard ABI, the initial entry point is at address 0. The arguments
and return are implementation defined.
This is just telling us that by convention the "main" function lives at the beginning of the program code. Whether the main function takes any arguments and what the return type is depend on the application. Fortunately we have the document that explains what these are for the Lambda-Man AI.

Lambda-Man AI interface

For LAMBDAMAN, main is a function with two arguments:

 1. the initial state of the world, encoded as below
 2. undocumented

It returns a pair containing:

 1. the initial AI state
 2. the AI step function (closure)

The AI step function has two arguments:

 1. the current AI state
 2. the current state of the world, encoded as below

It returns a pair containing:

 1. the current AI state
 2. the move, encoded as below

The encoding of the AI state is private. The host should pass the current
AI state on each step but not otherwise inspect it.
So what this is telling us is that the AI is implemented as a function that looks at the current state of the game and returns the move it wants Lambda-Man to make. The AI does not have to be completely stateless however, it is able to make use of a private state that it can use and update on each step. It works by returning that new state, and the host (i.e. the game mechanics) is expected to pass that new state to the AI function in the next step. The initial AI state comes from the initial entry point (the 'main' function).

A slightly peculiar thing is that 'main' returns the AI step function, rather than that being another "well known" address. Apparently this is the convention that only 'main' has a well known address and any other entry points that the host needs must be returned by main. Note that when it says that it returns a function it doesn't mean a code address but a "CLOSURE cell". See the LDF and AP instructions for what we know about that.

The state of the world is encoded as follows:

A 4-tuple consisting of

1. The map;
2. the status of Lambda-Man;
3. the status of all the ghosts;
4. the status of fruit at the fruit location.

The map is encoded as a list of lists (row-major) representing the 2-d
grid. An enumeration represents the contents of each grid cell:

  * 0: Wall (`#`)
  * 1: Empty (`<space>`)
  * 2: Pill 
  * 3: Power pill
  * 4: Fruit location
  * 5: Lambda-Man starting position
  * 6: Ghost starting position
For example, this map

###
#..
is encoded as

[ [ 0, 0, 0 ]
, [ 0, 2, 2 ]
]

Note that the map does reflect the current status of all the pills and
power pills. The map does not however reflect the current location of
Lambda-Man or the ghosts, nor the presence of fruit. These items are
represented separately from the map.

The Lambda-Man status is a 5-tuple consisting of:
  1. Lambda-Man's vitality;
  2. Lambda-Man's current location, as an (x,y) pair;
  3. Lambda-Man's current direction;
  4. Lambda-Man's remaining number of lives;
  5. Lambda-Man's current score.

Lambda-Man's vitality is a number which is a countdown to the expiry of
the active power pill, if any. It is 0 when no power pill is active.
  * 0: standard mode;
  * n > 0: power pill mode: the number of game ticks remaining while the
           power pill will will be active

The status of all the ghosts is a list with the status for each ghost.
The list is in the order of the ghost number, so each ghost always appears
in the same location in the list.

The status for each ghost is a 3-tuple consisting of
  1. the ghost's vitality
  2. the ghost's current location, as an (x,y) pair
  3. the ghost's current direction

The Ghosts' vitality is an enumeration:
  * 0: standard;
  * 1: fright mode;
  * 2: invisible.

The Ghosts' and Lambda-Man's direction is an enumeration:
  * 0: up;
  * 1: right;
  * 2: down;
  * 3: left.

The status of the fruit is a number which is a countdown to the expiry of
the current fruit, if any.
  * 0: no fruit present;
  * n > 0: fruit present: the number of game ticks remaining while the
           fruit will will be present.

Lambda-Man's move is a direction as encoded above.
This document refers to tuples and lists, but of course the only primitive data structure is a CONS cell—a pair. Though it is not clearly specified anywhere, we believe it uses the following encoding for tuples and lists.

Tuples are encoded as right nested pairs, four example a 4-tuple:

(a,b,c,d)  is encoded as     (a, (b, (c, d)))
           which is really   CONS a (CONS b (CONS c d))
The encoding for a list is also right nested CONS cells, but slightly different in the base case. The empty list is encoded as the integer 0.

     []  encoded as                          0
    [c]  encoded as                   CONS c 0
  [b,c]  encoded as           CONS b (CONS c 0)
[a,b,c]  encoded as   CONS a (CONS b (CONS c 0))
Example

To illustrate the interface, here is an AI that always goes down.

  DUM  2        ; 2 top-level declarations
  LDC  2        ; declare constant down
  LDF  step     ; declare function step 
  LDF  init     ; init function
  RAP  2        ; load declarations into environment and run init
  RTN           ; final return
init:
  LDC  42
  LD   0 1      ; var step
  CONS
  RTN           ; return (42, step)
step:
  LD   0 0      ; var s
  LDC  1
  ADD
  LD   1 0      ; var down
  CONS
  RTN           ; return (s+1, down)
We can see from the code that it also (pointlessly) maintains an integer state (starting from 42) and increments it on every step.

Resource constraints

The Lambda-Man CPU had a clock speed of 3.072 MHz. Each processor cycle executed exactly one instruction, and each invocation of the Lambda-Man AI was allowed to run for one second before a move was made. In addition, the Lambda-Man AI was given one minute of time for initialization, before the game began.

From this it follows that each invocation of the AI before a move was allowed to run up to 3072 * 10^3 instructions. We assume that taking any more instructions than this would result in catastrophic failure.

The main RAM was—for its time—impressively large, and was able to hold 10 million CONS cells to be used by the Lambda-Man AI in the heap. Again, trying to allocate memory over this limit resulted in an error, probably results in catastrophic failure.

Errors

During the game, if an instruction causes an error then execution halts and no result is returned. In this case Lambda-Man's requested move will be the same as its previous move.

Submission procedure

You do not need to register to enter the competition, but you do need to fill in some details to submit a solution.

Submission is by filling in a web form with your team details, and a URL and SHA1 checksum of a .zip or .tar.gz file containing your solution. The judges will download the file containing your solution after the closing deadline and compare its SHA1 checksum with the one you provided.

Submission form
We request (but do not strictly require) that you include in your .tar.gz/.zip various additional material:

All the source code you wrote to help you prepare your solution.
Any build scripts or other build files needed to run your code.
Some directions for the judges to tell us how to run your code, including any necessary details of the environment.
Any documentation/description of your solution that you wish to share with the judges, e.g. the approach you took, why your solution is awesome/funny/quirky/lame, any feedback about the problem or competition.
It is not essential that the judges be able to run your code, however they will try their best to run it for the top few winning entries and any entries under consideration for the judges' prize.

Privacy

Some people will want to show off their participation in the contest while some people will want to take part anonymously. We want to allow both.

The submission form therefore includes a number of optional fields for extra information about your team that we will publish if you choose to supply it; in particular the names of the members of your team and a URL for a team home page.

We do ask for contact details but we will not share these with anyone and only use them if we need to get in contact with you.

We will publish team names and scores/ranks.

We will not publish your full solution but we encourage you to do so yourself via your team home page after the competition ends. Please do not publish solutions to the lightning round until the full competition ends.

We may describe details of your solution or additional material during the final contest presentation at ICFP (if it is one of the winning entries or otherwise notable). If there is anything in your solution or additional material that you would like to remain confidential then please make it clear in the notes included in your submission.

Note that there are some practical constraints on anonymity and confidentially in the case that you happen to win: we need to say something about the winning teams and entries at the contest presentation and it is impossible to pay prize money anonymously. So while you are most welcome to participate while insisting on a high level of confidentiality, it may be impractical to award a prize in that case and the judges may decide to make the award to the next best entry.

Solution format

Your submission file (.zip or .tar.gz) should have the following format:

Subdirectory solution that contains your solution file: lambdaman.gcc.

Subdirectory code with the source code you wrote to help you prepare your solution and any auxiliary material that can be helpful to the judges to build your code. You should include a README file here with any documentation/description of your solution that you wish to share with the judges.
