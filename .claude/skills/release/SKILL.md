---
name: release
description: GitHub release を作成する手順。release PR の確認、Notable changes のドラフト（日本語→校正→英訳）、外部ドキュメントの注記確定、gh/REST での下書き作成と publish、その落とし穴を扱う。リリース PR が用意できた段階から publish 完了までに使う。
---

GitHub release を作成してください。repo 固有の慣行（bump ブランチ名・release PR の書式・文書チェックの仕組み等）は、リポジトリに `RELEASING.md` があればそちらを正本とし、この skill は横断的な手順と gh / REST の落とし穴を担います。

## 手順

1. **repo の `RELEASING.md` を読む**（あれば）。以下の手順と食い違う点は repo 側に従う
2. **release PR（develop → main 等）を確認する**: title `vX.Y.Z`、本文（repo 慣行に従う。例: マージ済み PR 番号の列挙）、CI の結果、文書チェック系 Action のコメント（確定すべき文書注記を列挙している場合がある）
3. **Notable changes をドラフトする**: `gh release view <前回タグ>` で前回の書式を確認して踏襲する（例: `## Notable changes` の下に `### Interface changes` / `### Behavior changes`、内部整備は非掲載）。日本語で起こしてユーザーの校正を受け、確定後に英訳する。外部ドキュメントを更新した場合はその参照を 1 文添える（release note を読む関係者に文書が現行であることを伝えるため）
4. **外部ドキュメント（Notion 等）の更新が必要な場合は、「release PR の merge → 文書更新 → publish」の順で行う**（必要かどうかは文書チェック系 Action のコメントや PR の宣言で判断する。多くのリリースでは該当なし）。merge 前に文書を更新すると、PR レビューが長引いた場合に文書と実装が食い違う時間が延びるため、文書更新は merge 後に行い、publish は文書が現行になってから
5. **下書きを作成する**:
   ```
   gh release create vX.Y.Z --target <release branch> --title "vX.Y.Z" --notes-file <notable.md> --generate-notes --draft
   ```
   `--notes-file` と `--generate-notes` を併用すると、指定本文の後ろに自動生成ノート（What's Changed・Full Changelog）が追記される（UI の「自動生成＋冒頭に追記」と同じ並び）。作成後に REST で body・`tag_name`・`target_commitish` を確認する
6. **下書きの本文を修正するときは REST の PATCH で、`tag_name` と `target_commitish` を毎回明示する**。body だけを送ると draft の `tag_name` が placeholder（`untagged-…`）に書き換わる（実測・2026-09）。応答でその 2 値を確認する
7. **publish する**: ユーザーの確認を得てから PATCH `draft=false`（同じく tag / target を明示）。`git fetch --tags` で tag が release branch の先端 commit を指すことを確認する
8. **事後**: 作業メモリの入口を更新する。下流の消費者（API を呼ぶ側等）への通知は既存の取り決めに従う
