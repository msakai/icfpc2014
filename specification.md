# ICFP Programming Contest 2014

# 仕様

# イントロダクション

For this year's ICFP programming contest we thought it would be fun to do a bit of '80s software and hardware archaeology. 
We just so happen (via friends of friends and bankruptcy asset sales) to have got our hands on a bunch of internal documents from LamCo. 
Back in the '80s, LamCo were a failed manufacturer of arcade games. 
You've probably never heard of them because they never managed to get a product out and were beaten by their better known rivals.

今年のICFPプログラミングコンテストでは，80年代のソフトウェアとハードウェアの考古学を楽しんでもらいたいと考えた．
たまたま，(友達の友達や破綻資産売却を通じて)LamCo社の内部資料を山を手にいれた．
80年代に遡ると，LamCo社はアーケードゲームの製造に失敗した．
製品を商品にすることができず，有名なライバル社に叩きのめされたために彼らのことに聞き及ぶことはなかったと思う．

What we've found from the cache of documents is that they had some rather interesting technology.
We also found that they had a spectacularly dysfunctional development process marked by bitter internal rivalries, which we suspect goes a long way to explain their failed projects.

隠匿書類から満つかったのは，彼等の少々興味深い技術であった．
また，彼等は開発過程でひどい内部抗争から派手に機能不全に陥いった．
結局それが原因だろうとにらんでいる．

So the contest task involves delving into LamCo's quirky old software and hardware technology, reconstructing the arcade hardware in simulation, and writing software for the arcade platform to play the game—and to play it better than your rivals!

そこでコンテストの課題は，LamCo社の妙な古いソフトウェアとハードウェア技術を精査し，シミュレーションでアーケードハードウェアを再構築することであり，そのアーケードプラットフォームのソフトウェアを書いて，ゲームを遊べるようにし，君のライバルよりも上手くプレイすることである．

## あかされない歴史

LamCo were building an arcade game where you control a little character called "Lambda-Man" who runs around in a maze eating pills and evading ghosts. 
The game bears a striking resemblance to another well known arcade game.
We don't know exactly why they ended up being so similar, though we rather suspect LamCo of stealing ideas from their more successful rival. 
We don't know exactly why they ended up being so similar, though we rather suspect LamCo of stealing ideas from their more successful rival.
For reasons best known to themselves, LamCo decided to make a two player version of their game where two Lambda-Man characters battle it out to swipe the most pills from under the noses of the ghosts. 
Because they also wanted to have a one player mode they found that they needed an AI to play the opposing Lambda-Man. 
This and the usual pressure to meet tight deadlines led to a catastrophic decision by The Management to force two rival engineering teams to work together on the project.
The Management got one team to work on the ghost AIs and game mechanics and another team to work on the Lambda-Man AI.
The Management claimed this was because they wanted to be proactive not reactive with their blue-sky thinking on this mission critical project, and believed this to be a win-win situation, harvesting low-hanging fruit with client focused deliverables.
Because of time pressure and The Management, each team was allowed to use their favourite technology stack and the plan was to integrate it all together at the end.
The ghost and game mechanics team were led by engineers who seemed to believe that 8-bit microprocessors are the be all and end all of computing. 
The Lambda-Man AI team were led by engineers obsessed with LISP and all its arcanery.

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

Of course the end product ended up reflecting the teams that had built it.
The main motherboard had several 8-bit microcontrollers to implement the ghost AIs and the game mechanics. 
It also had a customised coprocessor for running the Lambda-Man AI.
The software also reflected the different teams' philosophies: the ghost AIs and game mechanics were written directly in assembly while the Lambda-Man AI was written in some dialect of LISP.

もちろん，最終成果物はそれを組んだチームを反映したものになった．
マザーボードは数台の8ビットマイクロコントローラを積んでおり，これを用いて幽霊とゲーム機構を実装していた．
またλマンAIを走らせる専用のコプロセッサも積んでいた．
ソフトウェアもそれぞれのチームの哲学を反映したものであった．
幽霊AIとゲーム機構はアセンブリ言語で直接書かれており，λマンAIはLISPのとある方言で書かれていた．

We don't know exactly why the project failed.
It could have been schedule slippage from integrating the different technologies, infighting between the two teams, the horrendous bill of materials or all of the above.
But surely not The Management. 
All we know for sure is that the project was ultimately cancelled and LamCo filed for bankruptcy.

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

For the lightning round we will play the "classic" version of the game where it is just a single Lambda-Man against the ghosts.

ライトニングラウンドでは「クラシック」版でプレイする．1人のλマンと幽霊たちが対戦する．

Your challenge is is to write an AI for the Lambda-Man. 
It will play against ghost AIs supplied by the judges and in mazes supplied by the judges.

λマンAIを書いてもらいたい．
λマンは審査員が用意した迷図で審査員が用意した幽霊AIと戦う．

You will be supplied with several of the mazes and with the assembly code for a few (not very smart) ghost AIs.

いくつかの迷図とあまり賢くない幽霊AIのアセンブリコードをいくつかを君に提供する．

The lightning round runs for the first 24 hours of the competition, so submissions must be in before 12:00 UTC 26/07/2014.

ライトニングラウンドは最初の24時間である．
したがって，提出期限は12:00 UTC 26/07/2014である．

## 本番ラウンド概要

The full round will be broadly the same as the lightning round, but with a twist.
The details will be made available at the end of the lightning round. 
Check the front page for the link once the lightning round has closed.

本番ラウンドもライトニングラウンドとほぼ同じだが，ちょっとひねってある．詳細についてはライトニングラウンドが終了したときにあかされる．
ライトニングラウンド終了したらすぐにトップページのリンクをチェックすること．

The full round runs for the full 72 hours of the competition, so submissions must be in before 12:00 UTC 28/07/2014.

本番ラウンドは72時間であり，提出期限は12:00 UTC 28/07/2014である．

## 残りの仕様

残りの仕様は以下を参照のこと．

- 完全な仕様は[λマンゲームのルール](#the-lambda-man-game-rules)にある．
- 得点方法の詳細は[得点方法](#scoring)にある．
- 幽霊AIのマイクロコントローラについては[マイクロコントローラ](#ghost-cpu-ghc)にある．
- λマンAIにつかわれているプロセッサについては[プロセッサ](#lambda-man-cpu)にある．
- 解答の提出方法の詳細は[提出方法](#submission-procedure)にある．

## 参考資料

To help you get started we are providing a reference implementation of the game rules and the Lambda-Man processor.

ゲームルールとλマンプロセッサの参考実装を用意してある．

- [http://icfpcontest.org/reference.html](http://icfpcontest.org/reference.html)

This site also contains other refence material that you might find useful, such as sample maps.

ここには他にも参考資料があり，地図の例など役にたつ資料がある．

If you find any discrepancies between this specification document and the reference implementation (or other material), then please let the judges know as soon as possible!

この仕様と参考実装(その他)との間に食い違いがある場合はすぐに審査員に知らせてください！

# <a name="the-lambda-man-game-rules">λマンゲームのルール</a>

## 目的

A Lambda-Man lives in a two-dimensional maze made up of walls, and must eat as many pills as he can, while avoiding the ghosts who chase him. 
Lambda-Man has three lives, and if a ghost catches Lambda-Man, then he loses a life.
When there are no more Lambda-Man lives, the game is over. When all the pills are eaten, Lambda-Man has completed the level.

λマンは壁にかこまれた2次元迷図上で活動し，追い掛けてくる幽霊を回避しつつ，食えるだけの錠剤を食わなければならない．λマンには命が3つあって，幽霊に掴まると命を1つは失なう．λマンの命がすべてなくなった時点でゲームオーバーである．錠剤をすべて食べればλマンはそのレベルを完遂したことになる．

In addition to pills, a Lambda-Man may also eat power pills. These gives every Lambda-Man the ability to eat ghosts for a short period of time.

錠剤以外に，λマンはパワー錠剤も食う．パワー錠剤を食うとλマンは短時間だけ幽霊を食う能力を得る．

Bonus points are awarded for eating the fruit, which appears after a specific period of time at a specific location on the map before disappearing.

フルーツを食えばボーナスポイントが付く．フルーツは定まった時間ごと？に地図上の特定の位置に現れる．

By convention, the following symbols are used to represent the various elements of the game:

規約として以下の記号を用いてゲームの各種要素を表現する．

|   記号    |     要素    |
|:---------:|:-----------:|
|  <space>  |      空     |
|    \#	    |      壁     |
|    \.	    |     錠剤    |
|    o      |  パワー錠剤 |
|    %      |     フルーツ    |
|    \\	    |   λマン    |
|    =      |     幽霊    |

## 機構

The world is entirely deterministic, and runs on a tick-by-tick basis.

世界は完全に決定的であり，クロックの刻みごとに走る．

時の刻みごとに，

1. All Lambda-Man and ghost moves scheduled for this tick take place. (Note that Lambda-Man and the ghosts do not move every tick, only every few ticks; see the ticks section below.)<br>
すべてのλマンと幽霊はこの刻みに併せて移動する．（λマンと幽霊はすべての刻みで動くわけではなく，何刻みかごとに動くことに注意せよ．）
2. Next, any actions (fright mode deactivating, fruit appearing/disappearing) take place.<br>
つぎに，すべてのアクション（イジケモードの解除，フルーツの出現/消滅）がここで起こる．

3. Next, we check if Lambda-Man is occupying the same square as pills, power pills, or fruit:<br>
つぎに，λマンが錠剤，スーパー錠剤，フルーツと同じセルにあるかをチェックする．<br>
    1. If Lambda-Man occupies a square with a pill, the pill is eaten by Lambda-Man and removed from the game.<br>
    λマンが錠剤と同じセルにあれば，λマンは錠剤を食べ，錠剤はゲームからなくなる．
    2. If Lambda-Man occupies a square with a power pill, the power pill is eaten by Lambda-Man, removed from the game, and fright mode is immediately activated, allowing Lambda-Man to eat ghosts.<br>
    λマンがパワー錠剤と同じセルにあれば，λマンは錠剤を食べ，パワー錠剤はゲームからなくなる．
    3. If Lambda-Man occupies a square with a fruit, the fruit is eaten by Lambda-Man, and removed from the game.<br>
    λマンがフルーツと同じセルにあれば，λマンは錠剤を食べ，フルーツはゲームからなくなる．
4. Next, if one or more visible ghosts are on the same square as Lambda-Man, then depending on whether or not fright mode is active, Lambda-Man either loses a life or eats the ghost(s). See below for details.<br>
つぎに，1つあるいは複数の幽霊がλマンと同じセルにあれば，幽霊モードが有効かどうかによって，λマンが命を1つ失うか，幽霊を食うかのどちらかになる．詳細は後述．
5. Next, if all the ordinary pills (ie not power pills) have been eaten, then Lambda-Man wins and the game is over.<br>
つぎに，通常の錠剤（すなわちパワー錠剤ではない）がすべて食べつくされたら，λマンの勝利で，ゲームは終了．
6. Next, if the number of Lambda-Man lives is 0, then Lambda-Man loses and the game is over.<br>
つぎに，λマンの命の数が0なら，λマンの負けでゲームは終了．
7. Finally, the tick counter is incremented.<br>
最後に時刻刻みが1つ進む．

## 命を1つ失うとき

If at the end of a tick Lambda-Man is in the same square as a visible ghost and fright mode is not active then Lambda-Man loses a life. In this case, Lambda-Man and all the ghosts are immediately returned to their starting positions and starting directions (so that at the beginning of the next tick, Lambda-Man and the ghosts are in their starting positions).

チクの最後でλマンが見えている幽霊と同じセルにいて，イジケモードが有効になっていなければ，λマンは命を1つ失う．この場合，λマンおよびすべての幽霊はただちにスタート地点に戻りその向きもスタート時点と同じ向きになる（したがって，次のチクの開始点ではλマンおよび幽霊はスタート地点にいる）．

## パワー錠剤

When a power pill is eaten, all ghosts turn around and move in the opposite direction to their previous move, and fright mode is enabled. While in fright mode, if a ghost occupies the same square as a Lambda-Man, the ghost is eaten. When a ghost is eaten, it is returned to its starting position and starting direction, and is invisible until fright mode expires. While invisible, the ghost can neither eat nor be eaten.

パワー錠剤を食うと，すべての幽霊は反転して反対方向に向き直前の位置に移動し，イジケモードが有効になる．イジケモードが有効な間に幽霊と同じセルをλマンが占めれば，幽霊は食われる．幽霊は食われたら，スタート地点に戻り，スタート時点での方向を向く．そうしてイジケモードが切れるまで見えなくなる．見えないあいだは幽霊は，食いも食われもしない．

If a power pill is eaten during fright mode, the fright mode tick count is reset.

イジケモードのときにパワー錠剤を食うと，イジケモードの時刻カウントはリセットされる．

When fright mode expires, all ghosts become visible.

イジケモードが切れたら，すべての幽霊は見えるようになる．

## 得点

The aim of the game is to achieve the highest score, which is the sum of all the scores achieved on all levels. Points are awarded as follows.

このゲームの目的は最高点を獲得することである．すべてのレベルでのすべての得点の和を最高にすることである．
得点は以下のようにして獲得する．

Each pill eaten is worth 10 points

錠剤1個が10点．

Each power pill eaten is worth 50 points.

パワー錠剤が50点．

Each fruit eaten is worth points depending on its flavour. The flavour of a fruit is determined by the level as described below:

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

The level of a map is determined by its area. Given a map of size mapWidth * mapHeight, the level is the number which satisfies:

地図のレベルはその面積できまる．mapWidth * mapHeightの大きさの地図だとすると，そのレベルは以下を満す．

```
100 * (level - 1) < mapWidth * mapHeight <= 100 * level
```

For example, a map of size 15 * 18 = 270 is a level 3 map, since 200 < 270 <= 300.

たとえば，15 * 18 = 270のサイズだとすると，これはレベル3の地図である．200 < 270 <= 300 だからである．

While in fright mode, the first ghost eaten is worth 200 points. 
Each subsequent ghost eaten until another power-pill is eaten is worth double the previous one, up to a limit of 1600 points:

イジケモードの間に最初に幽霊を食うと200点である．
ひきつづき次のパワー錠剤を食うまでに幽霊を食うごとに得点は前の得点の2倍になり，上限は1600点である．

| 食った幽霊         | 得点 |
|:-------------------|-----:|
| 1匹目              |   200|
| 2匹目              |   400|
| 3匹目              |   800|
| 4匹目およびそれ以降|  1600|

If Lambda-Man manages to eat all of the pills on a level, he is awarded with a bonus: his score is multiplied by the remaining lives plus one. For example, if the Lambda-Man is on his last life, the score is doubled.

λマンが1つのレベルで錠剤をすべて食えば，ボーナスで，得点が残りの命の数に1を加えたものかけたものになる．
たとえば，1つしか命がのこっていなければ，得点が倍になるということである．

## 時の刻み

The world runs tick-by-tick. The Ultimate Tick Clock (UTC) gives the current tick time, and is counted from 1 at the beginning of each game. A game runs until the End Of Lives (EOL), which happens either when Lambda-Man runs out of lives, or if a particular UTC is reached, whereupon lives are set to 0:

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

Each fruit remains in the game until either it is eaten, or it expires at a fixed UTC.

それぞれのフルーツは食われてしまうか，特定の時刻が来れば期限切れになって消える．

| イベント | UTC                             |
|:---------|:--------------------------------|
| フルーツ1| 期限切れ 127 * 280              |
| フルーツ2| 期限切れ 127 * 480              |

Each power pill eaten triggers fright mode, which expires after a fixed duration counted from when the last power pill is eaten.

それぞれのパワー錠剤を食うとイジケモードに以降する．
イジケモードは，最近に食ったパワー錠剤を食った時点から一定の期間がたてば効力が消える．

| イベント         | チク期間                        |
|:-----------------|:--------------------------------|
| イジケモード期間 | 127 * 20                        |

The Lambda-Man and ghosts move at different speeds. Lambda-Man moves slower when he is eating, and the ghosts move slower when they are in fright mode. All moves are at regular intervals, based on their ticks per move value which is described below. For example, the first Lambda-Man move occurs at tick 127, the second at tick 254, and so on.

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

On a tick when Lambda-Man or a ghost is scheduled to move, their next move is scheduled for the appropriate number of ticks in the future, depending on their current state. For example if Lambda-Man moves into a square with a pill then the next tick on which he will move will be his previous scheduled tick number plus 137.

λマンあるいは幽霊が移動するチクカウントにおいて次の移動のチクカウントが設定される．
これはそのときの状態に依存する．
たとえば，λマンが錠剤のあるセルにいたら次の移動はその直前の移動に設定されたチクカウントに137を加えた時刻である．

幽霊が5体以上いるときには，チクカウントは循環的に割り当てられる．つまり幽霊4は幽霊0と同じ設定になる．
λマンや幽霊のチク設定が再設定されるイベントや条件は他にはない（フライトモードに入るとき，幽霊が食われるとき，λマンが食われるときでも再設定されることはない）．

## 移動

The Lambda-Man can move into any adjacent square that is not occupied by a wall. An adjacent square is one that is up, down, left, or right of another.

λマンは壁がない隣のセルに移動できる．隣のセルとは1つ上，1つ下，1つ左，1つ右のセルのことである．

When Lambda-Man tries to choose an illegal move, he stops moving.

λマンが不正な移動選ぶと移動しません．

Ghosts can only move into an adjacent square that is not occupied by a wall. At a tick when a ghost may move, it must move (unless it is surrounded on all four sides by walls). Furthermore, a ghost cannot move in the opposite direction to its current direction, unless that is the only direction available (because it is surrounded on three sides by walls).

幽霊は壁のない隣のセルにしか移動できません．
ゴーストが移動する時刻にはゴーストは必ず移動しなければなりません．4方が壁に囲まれていないかぎり．さらに，ゴーストは今向いている方向と反対方向には移動できません．ただし，三方を壁でかこまれていて，そちらにしか移動できない場合はその限りではありません．

Consequently, a ghost can only choose its direction at a junction and cannot choose to turn back on itself. A junction is a square which has at least three adjacent squares with no walls. For example, the following are all junctions.

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

When a ghost encounters a bend, it is forced to continue around the bend. When a ghost encounters a dead end, it is forced to turn around.

ゴースト

When a ghost chooses an illegal move (or no move at all) at a junction, it is forced to continue in its previous direction if this is legal, and if not, then the first legal direction out of up, right, down, and left, in that order.

ゴーストが不正な移動を選択すると（あるいは全く動かないことを選択すると）上右下左の順に見て動ける方向に動きます．

At the start of the game, all ghosts and Lambdaman face down.

ゲーム開始時にはゴースト，λマンはすべて下を向いています．

## トーナメント得点

For the lightning round it is your Lambda-Man versus ghosts supplied by the judges and in mazes supplied by the judges.

ライトニングラウンドでは君のλマンと審査員の用意した幽霊と審査員が用意した迷図で対決します．

Your overall score in the lightning round is the sum of your individual scores on a series of games, played in different maps. The actual maps and ghosts used will not be revealed but they will range from easy to hard. The map properties are described below.

ライトニングラウンドでの得点は，一連（別別の地図で）の君とのゲームで君が獲得した得点の総和になる．
ここで使われた地図と幽霊は見えないのであるが，簡単なものから難しいものへの順にならんでいる．
その地図の性質については[後述する](#map-properties)．

## <a name="map-properties">地図の性質</a>

Maps are rectangular. Map x and y coordinates are indexed from 0. The top left corner is (0,0), so increasing x-coordinates are to the right, and increasing y-coordinates are down.

地図は長方形で，x 方向，y 方向ともに 0 から番号が付く．
左上の角が(0,0)である．したがって，x 方向は右へいくほど番号が多きくなり，y方向は下えいくほど番号が大きくなる．

Maps are like mazes: they consist of corridors 1-square wide, without open spaces. More formally: there are no 2x2 areas consisting only of non-wall squares.

地図は迷図のようになっており，1つの正方形のセル幅の通路で構成されており，それより広い場所はない．
より形式的にいうと，壁を含まないセルの2×2の空間はない．

Maps have walls around the edges.

地図の周辺は壁がぐるりと回っている．

Every pill in a map is accessible.

地図にあるすべての錠剤は到達可能である．

The maps vary in size and in the number of ghosts and power pills. Easy maps will be smaller, with fewer ghosts and a higher density of power pills. Harder maps will be larger, with more ghosts and a lower density of power pills. The maximum map size is 256 by 256.

地図のサイズ，幽霊の数，パワー錠剤の数は地図ごとに変る．
簡単な地図ほど，小さく，幽霊の数は少く，パワー錠剤の密度は高い．
難しい地図ほど，大きく，幽霊の数は多く，パワー錠剤の密度は低い．
最大の地図サイズは 256 × 256 である．

## 幽霊と幽霊のプログラム

Ghost AI programs are assigned to ghosts in each map. If there are more ghosts in a map than then are ghost AI programs in use then the AI programs are assigned to the ghosts cyclically.

幽霊AIプログラムはそれぞれの地図ごとに幽霊に割り当てられている．
AIプログラムの数以上の幽霊がいる地図ではAIプログラムを循環的に割り当てる．

For example, if a map has 4 ghosts and there are 2 AI programs then they are assigned as follows:

たとえば，幽霊が4体，AIが2つの場合の割あては以下のとおり

| 幽霊     | AI プログラム |
|:---------|:--------------|
| ghost 1  | program 1     |
| ghost 2  | program 2     |
| ghost 3  | program 1     |
| ghost 4  | program 2     |

The order of ghosts is in increasing order of their starting coordinates, where (x1, y1) is considered smaller than (x2, y2) if y1 < y2' or if y1 = y2' and x1 < x2'.
幽霊の順番は出発の座標の大小できまります．

The system allows at most 4 ghost programs (but more than 4 ghosts).
このシステムでは高々4つの幽霊プログラムしか使いませんが幽霊は5体以上です．

# GHost CPU (GHC)

The GHost CPU (GHC) is a conventional 8-bit microcontroller. Each ghost is run by a separate GHC. Although we found a complete copy of the CPU manual, it is quite terse in parts, as it assumes the conventions of the day. For those of you who (because of age or youth) do not remember the 1980s, we have tried to explain some of these conventions.
GHost 

## GHC State

Each register holds an 8-bit unsigned integer (between 0 and 255 inclusive). There are 2 separate memories: a data memory and a code memory, each with 256 locations (numbered from 0 to 255 inclusive). Hence the contents of a register can be interpreted directly as a data memory address, the contents of a data memory location, or a code memory address. The GHC performs all arithmetic modulo 256, so 255 + 1 = 0.

Initialisation and Program Execution

At the start of a game, the GHC's code memory is initialised with a program, as described in the section Code Format. The contents of the code memory does not change during a game. All data memory locations and all registers are initialised to 0.

During each game cycle, the GHC runs the program, executing up to 1024 instructions: the game cycle is divided into 1024 execution cycles. At the start of each game cycle, the PC is initialised to 0. An execution cycle begins with the GHC reading the instruction at the address referenced by the PC from code memory. It executes the instruction, as described in the section Instruction Reference, possibly changing the contents of the data memory and registers. At the end of the execution cycle, if the value of the PC is the same as it was at the start of the execution cycle, the GHC increments it. Execution terminates at the end of an execution cycle if: the instruction executed was HLT; it was the 1024th execution cycle of the game cycle; or execution of the instruction caused an error. The contents of the data memory and registers persist between game cycles.

Code Format

By convention, a GHC program is stored in a file with the extension .ghc (GHost Code).

A program consists of several lines, terminated by newline characters. The contents of a line are whitespace insensitive: multiple consecutive whitespace characters are treated identically to one. A line is either empty (containing only whitespace) or contains an instruction.

A program may contain comments, which are introduced using a semicolon (;). Anything from a semicolon until the end of a line (including the semicolon) is ignored. Hence a line containing only a comment is regarded as empty.

An instruction consists of a mnemonic and zero or more arguments. The mnemonic is a case-insensitive sequence of alphabet characters.

An argument is either:

a register argument (indicated by its name (A to H or PC));
an indirect general-purpose register argument (indicated by its name enclosed in square brackets ([A] to [H] but not [PC]));
a constant argument (indicated by its encoding in decimal (0 to 255));
or the contents of a data memory location (indicated by its address in decimal, enclosed in square brackets ([0] to [255])).
The number of arguments required depends on the instruction (see Instruction Reference).

There must be a whitespace character between a mnemonic and the first argument (if any). There must be a comma (,) between consecutive arguments.

Instructions may optionally be preceded or followed by whitespace. Arguments and argument-separating commas may optionally be preceded or followed by whitespace.

When the GHC is initialised, each line containing an instruction is stored in the corresponding code memory location. For example, the instruction on the first non-empty line is stored at address 0 and the instruction on the second non-empty line is stored at address 1. As there are only 256 code memory locations available, a program may contain at most 256 instructions.

Instruction Reference

The GHC is able to execute the following instructions:

MOV dest,src

Copy the value of the src argument into the dest argument. dest may not be a constant.

INC dest

Add 1 to the value of dest and store the result in dest. dest may not be a constant or the register PC.

DEC dest

Subtract 1 from the value of dest and store the result in dest. dest may not be a constant or the register PC.

ADD dest,src

Add the value of src to the value of dest and store the result in dest. dest may not be a constant or the register PC.

SUB dest,src

Subtract the value of src from the value of dest and store the result in dest. dest may not be a constant or the register PC.

MUL dest,src

Multiply the value of src by the value of dest and store the result in dest. dest may not be a constant or the register PC.

DIV dest,src

Compute the integer quotient of dest by the value of src, and store the result in dest. dest may not be a constant or the register PC. Results in an error if the value of src is 0.

AND dest,src

Bitwise AND the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.

OR dest,src

Bitwise OR the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.

XOR dest,src

Bitwise XOR the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.

JLT targ,x,y

Compare the value of x with the value of y. If the value of x is less than the value of y, set the PC to the constant value targ.

JEQ targ,x,y

Compare the value of x with the value of y. If the value of x is equal to the value of y, set the PC to the constant value targ.

JGT targ,x,y

Compare the value of x with the value of y. If the value of x is greater than the value of y, set the PC to the constant value targ.

INT i

Invoke the interrupt service i (see Interrupt Reference).

HLT

Halt execution of the GHC.

Interrupt Reference

The effect of invoking an interrupt service is architecture-dependent. In the LamCo architecture, the following interrupts are standard:

INT 0

In:
Register A: ghost's new direction
Set the direction of the ghost. 0 is up; 1 is right; 2 is down; 3 is left.

The direction of the ghost is set at the end of the game cycle. If the interrupt is called multiple times in a single game cycle, the last interrupt overrides any earlier ones. Using an invalid direction in register A is equivalent to retaining the ghost's original direction at the beginning of the game cycle.

INT 1

Out:
Register A: First Lambda-Man's x-ordinate
Register B: First Lambda-Man's y-ordinate
Stores the first Lambda-Man's position in registers A (x-ordinate) and B (y-ordinate). In the single Lambda-Man version of the game, the first Lambda-Man is the only Lambda-Man.

INT 2

Out:
Register A: Second Lambda-Man's x-ordinate
Register B: Second Lambda-Man's y-ordinate
Stores the second Lambda-Man's position in registers A (x-ordinate) and B (y-ordinate). In the single Lambda-Man version of the game, the behaviour of this interrupt is unknown.

INT 3

Out:
Register A: this ghost's index
Stores the ghost's index in register A.

INT 4

In:
Register A: ghost index
Out:
Register A: indexed ghost's starting x-ordinate
Register B: indexed ghost's starting y-ordinate
For the ghost with index read from register A, stores its starting position in registers A (x-ordinate) and B (y-ordinate).

INT 5

In:
Register A: ghost index
Out:
Register A: indexed ghost's current x-ordinate
Register B: indexed ghost's current y-ordinate
For the ghost with index read from register A, stores its current position in registers A (x-ordinate) and B (y-ordinate).

INT 6

In:
Register A: ghost index
Out:
Register A: indexed ghost's current vitality
Register B: indexed ghost's current direction
For the ghost with index read from register A, stores its vitality in register A, and its direction in register B.

Vitality:
0: standard;
1: fright mode;
2: invisible.
INT 7

In:
Register A: map square x-ordinate
Register B: map square y-ordinate
Out:
Register A: contents of map square
Stores the contents of map square with index read from registers A (x-ordinate) and B (y-ordinate) in register A. If the co-ordinates lie outside the defined bounds of the map, stores 0.

Contents:
0: Wall (#)
1: Empty (<space>)
2: Pill
3: Power pill
4: Fruit
5: Lambda-Man starting position
6: Ghost starting position
INT 8

In:
Register PC
Register A..H
Sends the current value of the PC and all registers to an external debug/trace agent.

Examples

The following GHC programs illustrate the behaviour of some of the instructions:

miner.ghc

; Always try to go down.
mov a,2
int 0
hlt
flipper.ghc

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
fickle.ghc

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

As mentioned above, if an instruction causes an error then execution halts. If prior to this the new direction of the Ghost was set (with INT 0) then this will be the requested move of the ghost and the game will continue. If the direction is not set then the ghost's requested move will be the same as its previous move.

Lambda-Man CPU

We have been able to recover some documentation about the programming environment used by the Lambda-Man AI team, including their processor ISA and some bits of assembly code. We also know that the Lambda-Man AI team, being LISP fanatics, used some form of LISP but unfortunately we have not been able to find any of their LISP code, nor their compiler.

The LamCo "General Compute Coprocessor" (GCC) is a rather unconventional and—for its time—sophisticated coprocessor. It appears to have been designed as a target for a LISP compiler. Rather than a set of orthogonal instructions, it has a fair number of somewhat specialised instructions that (we presume) must have been useful for a compiler. We did however find a handwritten note by one of the engineers indicating that someone had written a compiler from a variant of Pascal, albeit with some limitations.

Fortunately we do have the original documentation of the processor which describes the instructions and operation in detail, though sadly not very much on how it was intended to be used by a compiler.

The sections below include excerpts from the original documentation along with our own comments.

General architecture

The machine is stack based, with three different stacks used for different purposes. It has—for its time—a relatively large memory. The way the memory is accessed and organised is quite unusual: apart from the stacks that live in memory, the rest of the memory is used for a garbage collected heap, with the GC implemented by the hardware. Because of this there are no general purpose memory access instructions: all memory access is in one of these stacks or in the GC'd heap.

CPU Registers

There are 4 programmer visible machine registers, all of which are for special purposes:

%c: control register (program counter / instruction pointer)
%s: data stack register
%d: control stack register
%e: environment frame register
Memory stacks

Three of the registers point into special data structures in memory:

Data stack
Control stack
Environment frame chain
The remainder of the memory is dedicated to the data heap.

Control register and program code layout

The machine has logically separate address spaces for code versus data. The %c register is an instruction pointer, pointing to the next instruction to be executed. Programs are laid out from low addresses to high. The effect of most instructions on the instruction pointer is simply to increment its value by one.

Data stack and register

The data stack is used to save intermediate data values during calculations, and to return results from function calls.

It is a logically contiguous stack. The %s register points to the top of the stack.

Many of the instructions simply pop and push values on the data stack. For example the ADD instruction pops two integer values off the stack and pushes back their sum.

Control stack and register

The control stack is used to save return information in function calls. It saves return address and environment frame pointers.

It is a logically contiguous stack.

Only the complex control flow instructions affect the control stack and register. See SEL/JOIN and AP/RAP/RTN for details.

Environment frames and register

The environment is used for storing local variables, including function parameters. There is an instruction for loading values from the environment onto the top of the data stack. The environment consists of a chain of frames, which is used to implement nested variable scopes within higher level languages, such as local blocks with extra local variables and functions.

The environment is more complex than the two stack structures. Rather than a contiguous stack, it consists of a chain of environment frames. Each frame contains a pointer to its parent frame. In fact the chain of frames is not strictly a stack: the lifetime of the frames is not always a simple LIFO stack order. Because of this the frames are managed by the hardware GC.

Each frame consists of a pointer to its parent frame and zero or more data values. The %e register points to the local frame. The machine has direct support for loading a value from the local frame or any of its parent frames, which in general requires following the chain of environment frames.

(We believe in the real hardware this feature was implemented in microcode, with internal registers to cache the outermost frame and a fixed number of the inner most frames for quick access).

Data heap and data values

The machine makes extensive use of dynamic allocations and has built-in support for a garbage collected data heap. Values in the data stack and in environment frames can be pointers into the heap.

There are three types of data value: integers, pairs and closures. Integers are represented in the usual way as binary signed integers. Pairs and closures are represented by pointers to objects in the data heap. The three types of values are distinguished by tag bits. The tag bits are not software visible (except for the ATOM instruction) but are used by the hardware for error checking.

There are three instructions for manipulating pairs: allocating a pair, accessing the first and accessing the second component of a pair.

All program data structures have to be represented using combinations of pairs and integers (and sometimes closures).

Instruction reference

LDC - load constant

Synopsis: load an immediate literal;
          push it onto the data stack
Syntax:  LDC $n
Example: LDC 3
Effect:
  %s := PUSH(SET_TAG(TAG_INT,$n),%s)
  %c := %c+1


LD - load from environment

Synopsis: load a value from the environment;
          push it onto the data stack
Syntax:  LD $n $i
Example: LD 0 1
Effect:
  $fp := %e
  while $n > 0 do            ; follow chain of frames to get n'th frame
  begin
    $fp := FRAME_PARENT($fp)
    $n := $n-1
  end
  $v := FRAME_VALUE($fp, $i) ; i'th element of frame
  %s := PUSH($v,%s)          ; push onto the data stack
  %c := %c+1
Notes:
  Values within a frame are indexed from 0.


ADD - integer addition

Synopsis: pop two integers off the data stack;
          push their sum
Syntax: ADD
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x + $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


SUB - integer subtraction

Synopsis: pop two integers off the data stack;
          push the result of subtracting one from the other
Syntax: SUB
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x - $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


MUL - integer multiplication

Synopsis: pop two integers off the data stack;
          push their product
Syntax: MUL
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x * $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


DIV - integer division

Synopsis: pop two integers off the data stack;
          push the result of the integer division of one of the other
Syntax: DIV
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x / $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CEQ - compare equal

Synopsis: pop two integers off the data stack;
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
