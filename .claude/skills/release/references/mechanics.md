# GitHub release の仕組みと落とし穴

根拠の種類を各項目に付ける。「実測」は実際に動かして確かめたもの、「文書」は GitHub の公式文書と gh のヘルプの直接の記載、「ソース」は gh CLI のソースコードの読解、「報告」は第三者の報告、「読み取り」はそれらからの推論で実測していないもの。

## tag と release

- release は tag に基づく。tag が無ければ、`target_commitish`（既定は既定ブランチ）から作られ、既にあれば `target_commitish` は無視される（文書）
- draft は write 権限のある人にだけ見える（旧版の文書。現行の文書には記載が見つからなかった）
- draft の間は tag は作られず、publish で作られる（読み取り。gh のソースに「REST には無いので GraphQL で pending tag name から draft を探す」というコメントがあり、draft の URL が `untagged-` で始まるという報告があり、`gh release edit --tag` で draft の tag 名を変えられる。公式文書に明示は見つからなかった）
- target にブランチ名を渡すと、tag はその時点でなく publish の時点の head から作られると読める（読み取り）。draft の間に main が進むとずれるので、SHA を渡す
- REST の PATCH で本文だけを送ると、draft の `tag_name` が placeholder（`untagged-…`）に書き換わる（実測・2026-09。gh のソースにも「tag 名を送らないと API が release から tag を外す」とあり、gh の `release edit` は既存の tag 名を常に送り直している）。draft の更新は `gh release edit` で行う。`gh api -X PATCH` で更新するなら、`tag_name` と `target_commitish` を毎回含める。どちらでも、更新の後に `tag_name` と `target_commitish` が変わっていないことを確かめる。`gh release edit` が tag を保つのはソースの読解で、実測していないため
- immutable releases が有効なら、publish の後は tag を動かせず消せず、asset を追加・変更・削除できない。タイトル、本文、prerelease と latest の印は変えられる。release を消しても同じ tag 名は再利用できない。draft の間は変えられる（文書）。設定を読む API は見つからなかったので、過去の release の `isImmutable`（`gh release list --json`）から推し量る
- 対象 commit が `.github/workflows/` を変えていると、token に `workflow` の権限が要り、無いと 404（ときに 403）になる（文書）

## publish が起こすこと

- 通知: release を watch する人（Custom の Releases を含む）に、publish で通知が届く（文書。REST の文書に「release の作成は通知を起こす」とあり、旧版の文書に「release が published されたとき」とある）。本文の編集で通知が再送されるかは、記載が見つからなかった
- `release` イベント: draft の保存・編集・削除では起動しない（文書）。draft を publish すると `published` が起動し、prerelease でなければ `released` も起動する。`created` は「draft が保存された、または draft を経ずに公開された」ときで、draft からの publish では起動しない。`prereleased` も draft からの publish では起動しない（文書）。したがって、`types` に `published` を含まないワークフローは draft からの publish では走らない。Y1 で、公開で走るワークフローと走らないワークフローを示す
- tag の作成で `push`（`tags` のフィルタ）と `create` のワークフローが起動する（文書）。release の publish で作られた tag でも起動するかは、記載が見つからなかった（読み取り: 起動すると見込む）
- latest: `/releases/latest` の URL と、引数なしの `gh release view` が指す。REST の `make_latest` は既定 `true` で、`legacy` なら作成日と semver で決まる。draft と prerelease は latest にできない（文書）。gh の `--latest` のヘルプは未指定なら「日付と版で自動」と言い、REST の既定の記載と合わないので、latest にするかは常に明示する。旧系列の hotfix（2.0.0 の後の 1.2.1 など）は `--latest=false`
- prerelease: semver の pre-release の識別子（`-rc.1` など）があれば prerelease にする。latest にならない（文書）
- discussion: `--discussion-category` で publish のときに作られる。慣行があれば従う（文書）
- 本文の `@user` は Contributors の節に載る（文書）。mention の通知が届きうるので、人を指す書き方は慣行に合わせる

## 書き込みのコマンド

Y1 の承認の後に draft、Y2 の承認の後に publish。

```
gh release create <tag> --draft --target <SHA> --title '<title>' --notes-file <訳文のファイル> [--generate-notes] [--prerelease]
gh release view <tag> --json body -q .body | diff - <訳文のファイル>   # 承認した文面との一致。自動生成の節を付けたなら、その前までを比べる。改行の正規化で差が出たら、その差だけかを見る
gh release edit <tag> --draft=false --latest        # または --latest=false
```

`--notes-file` と `--generate-notes` を併せると、渡した本文が自動生成の節（What's Changed と Full Changelog）の前に置かれる（文書。gh のヘルプ）。付けるかは慣行に従う。

asset は draft に付ける（`gh release upload <tag> <file>`）。

## 公開後の確認

```
gh release view <tag> --json isDraft,publishedAt,url,tagName,targetCommitish
gh release list --json tagName,isLatest --limit 5     # latest の印は list にだけある
git ls-remote --tags origin '<tag>*'    # annotated なら <tag>^{} の SHA が対象 commit
gh run list --event release --limit 5
gh run list --event push --branch <tag> --limit 5   # 未実測
```

外部の文書の release へのリンク（`/releases/tag/<tag>`）が辿れるかも確かめる。draft の間、この URL は無い。