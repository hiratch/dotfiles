---
name: prepare-pr-topdown
description: 開発担当が使う。自分の変更を PR にして reviewer に渡すまでを受け持つ。branch の push と draft PR の作成、self-review への引き渡し、reviewer が判断に使う問いにコードから意図を組み立て直さずに答えられる Description の作成（日本語で起こし、校正の後に訳す）、reviewer の指定、ready 化（stacked PR は base のマージの後）を行う。PR を作るとき、作った PR を reviewer に渡すとき、PR の内容が変わって Description を書き直すときに使う。
---

# prepare-pr-topdown

## 目的と終わりの状態

自分の変更を、reviewer が少ない読む量で必要性・方針・根拠・実現方法を判断でき、後の読み手がなぜこの変更をしたかを辿れる PR にして、reviewer に渡す。その間、ユーザーの校正は、reviewer に渡す内容が固まった後の 1 回で済ませる。

終わったと言えるのは、reviewer に依頼し、PR を ready にしたとき。stacked PR で base がまだマージされていなければ、reviewer に依頼して draft のまま待つところまでで一区切りとし、base のマージの後に ready にする。ready にするとき、次が成り立っている。

- タイトルと本文が、その時点の head に対して書かれ、ユーザーが校正した原文（最終言語が日本語でなければ、その訳）である
- その head で CI が終わり、失敗があればその扱いをユーザーと決めている
- self-review の記録がその head の変更内容と合っているか、合わない差分をユーザーが承知している（照合は hook が行う）
- stacked PR なら、base の PR がマージされ、この PR の base がその行き先に変わり、diff がこの PR の変更だけになっている

## 進め方の前提

- reviewer は、まず課題と理由を掴み、それに照らしてコードを読む。Description に課題と理由が無いと、reviewer はコードから意図を組み立て直すことになり、時間がかかるうえ、組み立て直した意図に照らしてコードを妥当と見てしまう。一方で、判断に使わない文は、reviewer の読む量とユーザーの校正の量を増やすだけになる。Description の物差しは、この 2 つから決めている
- PR はユーザーの名義で出る。draft でもリポジトリの人に見えるので、本文は最初から最終言語で置く。reviewer が判断に使うのは、reviewer を指定して ready にした後なので、校正はそれまでに済ませる。draft の本文は、手元に起こした日本語の原文の仮の訳とし、self-review でその主張もコードと照らす
- self-review は、修正と「Description で説明する」という対応によって、Description に書くことを変える。文面を self-review の前に確定させると、校正が二度になる

## 入口

依頼の言い方ではなく、今の状態から始める箇所を決める。見るのは、ブランチがリモートにあるか、PR があるか（draft か、本文があるか、reviewer がいるか）、base がどこか（ほかの PR のブランチなら stacked）、self-review の記録があって今の変更内容と合うか、CI の状態。済んだはずの工程も、PR が作り直されたり内容が変わったりしていれば、その前提が今も成り立つかを確かめる。言い方で入口を決めると、内容の変わった PR を前の状態のまま扱ってしまうため。

最初の応答では、この状態、これから行うこと、ユーザーに決めてもらうこと（draft PR の作成、使う self-review のスキル、Description の下読みをするサブエージェントの承認、状態や会話から分からなければ関連する Issue と前提の PR）をまとめて示す。往復を減らすため。

## 流れ

次の順序には理由があり、崩すとそれぞれに書いた失敗が起きる。

### 1. push と draft PR の作成（self-review の前）

self-review の前に作る。self-review の中で CI の結果を扱えるため（self-review の後に CI の失敗で直すと、記録の後の変更になり、self-review をやり直すかを問われる）。また、stacked PR の base が GitHub 上で決まり、self-review の記録がその base に対して取られるため（PR が無いと記録は既定ブランチに対して取られて親の PR の変更まで含み、base がマージされた後の照合が食い違う）。

- タイトルと本文には、Description の日本語の原文を手元に起こし、その仮の訳を置く。原文はセッションの scratchpad に置き、セッションをまたぐなら案件メモリの引き継ぎに書く。書き方は [references/description.md](references/description.md) にあり、書き始める前に読む。開発担当が実装の入口で合わせた課題の木（根、方針と前提、契約、止まる条件に当たった観測）があれば、それを下書きにする。Description の主張を、コードから組み立て直さずに書けるため。根拠は確かめる予定のまま置き、3 で確かめた結果に置き換える。校正は 3 で行う
- base は、stacked PR なら親の PR のブランチにする
- PR の作成で起動するワークフローに CI 以外の副作用（PR ごとの環境のデプロイなど）があれば、作る時期をユーザーに諮る

### 2. self-review

self-review は self-review のスキルが受け持ち、どれを使うかはユーザーが決める。

self-review で「Description で説明する」と合意した点は、その場で文面にせず、Description を書くときにまとめて入れる。校正を 1 回にするため。self-review が控えた結果のうち、確かめた課題の木、Description で説明する点、reviewer に判断を委ねると決めた点、確かめられなかった範囲、Issue にしたものが、Description の材料になる。

### 3. Description の確定（self-review の記録の後）

手元の原文に self-review の結果を入れて直し、下読み・校正・訳を経て確定する。下読みと校正への出し方は [references/description.md](references/description.md) にある。

確定したら、タイトルと本文を更新する。

### 4. reviewer の指定（Description の確定の直後）

stacked PR でも、ready を待たずに指定する。reviewer を指定してもマージはできるようにならないので、base のマージを待つ理由が当てはまらず、待つとレビュー待ちを直列に積むことになるため。

- 指定の前に、CI の状態と Description が今の head と合っているかを確かめる。reviewer は最初に CI の状態と Description を見るので、合っていないと、reviewer の注意を無駄にするか、古い説明に照らしてコードを読ませるため
- reviewer は、プロジェクトのメモリや慣行に既定の reviewer があればそれを案にし、ユーザーが決める。CODEOWNERS があれば、ready 化で自動で依頼されることを添える
- reviewer の指定と ready 化には hook が確認ダイアログを出すので、チャットで改めて承認を求めない。本文の更新の承認を求めるときに、続けて誰を指定し、ready にするか（stacked PR なら待つか）を示しておく

### 5. ready 化

reviewer の指定の後に行う。reviewer に Slack の通知が届くのは、この順序のときだけのため。stacked PR は、base の PR がマージされるまで draft のまま保つ。reviewer を指定する前の確認も、同じファイルにある。draft はマージできないので、base より先に、あるいは base のブランチへマージされることを防げるため。

stacked PR の base がマージされたら、ready にする前に [references/stacked-pr.md](references/stacked-pr.md) を読み、そこにある確認をする。

## 約束

- draft PR の作成は、日本語の原文を示し、その仮の訳を置くことへのユーザーの承認を得て行う。仮の訳は 3 辺の検査の前のもので、ready の前に確定する訳とは別に扱う。確定した本文への更新は、書き込む内容そのものをユーザーが見て承認してから行う。承認がどの内容までを含むかが曖昧なとき（日本語の原文への承認が訳文を含むかなど）は、含まないものとして、書き込む内容を示して確かめる
- 本文の更新、reviewer の指定、ready 化の直前に、head が、Description を現物と突き合わせたときの SHA から変わっていないかを確かめる。変わっていれば進めず、差分を見て Description が合うかを確かめ直す
