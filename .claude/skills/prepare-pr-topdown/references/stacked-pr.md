# stacked PR

stacked PR（ほかの PR のブランチを base にする PR）のときだけ要る確認。

## reviewer を指定する前

base で絞ったワークフロー（`branches: [develop, main]` など）は、base が親の PR のブランチである間は走らない。まだ走っていなければ、そのことをユーザーに示す。reviewer は最初に CI の状態を見るため。

## base がマージされた後、ready にする前

- base が行き先（多くは既定ブランチ）に変わったか。GitHub は、マージされた PR の head ブランチが消されたときに付け替える。消されていなければ付け替わらず、マージ済みのブランチに向いたままになる
- diff にマージ済みの親の変更が含まれていないか。親が squash や rebase でマージされると含まれるので、行き先に rebase する。force push が要るので、ユーザーの合意を得て行う
- 行き先に対する CI が、この head で走って終わったか。base で絞ったワークフロー（`branches: [develop, main]` など）は、付け替えの前には走っていない。既定の起動条件（opened・synchronize・reopened）のワークフローは、base の付け替えだけでは走らないと GitHub のドキュメントから読める（実測していない）ので、走っていなければ走らせ方をユーザーに諮る
- Description が今の head と合うか。合わない部分（親の PR への依存の記述など）を直し、変わった部分だけを校正に出す。既にレビューが始まっていれば、変わったことを reviewer にどう伝えるかもユーザーに諮る
