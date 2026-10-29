# 会話の全文の記録の読み方

圧縮の後も、ユーザーの言葉と順序をそのまま根拠にするための、記録の場所・構造・抜粋のしかた。

## 場所

- `~/.claude/projects/<作業ディレクトリのパスの英数字以外を - に置き換えたもの>/<セッション ID>.jsonl`。公式の文書（Claude Code の「Explore the .claude directory」の Application data）は、ここに「messages, tool calls, and tool results, plus the system prompt, tool definitions, and parameters」が書かれるとする
- セッション ID の探し方。SKILL.md の `${CLAUDE_SESSION_ID}` が置き換わっていればそれ（公式の skills の文書に置換の記載がある。未実測）。圧縮の要約に記録のパスが添えられていることがある（実例あり）。どちらも無ければ、そのディレクトリで最も新しい `.jsonl` のうち、直近のやり取りを含むものを選ぶ
- サブエージェントの記録は `<セッション ID>/subagents/` の下に同じ形式で残る。大きいツールの出力は `<セッション ID>/tool-results/` に分けて置かれる
- `cleanupPeriodDays`（既定 30 日）を過ぎると消える

## 構造

2026-10 にこのマシンの記録で、キーの名前と値の語彙だけを数えて確かめた（発言の内容は読んでいない）。Claude Code の版で変わりうるので、抽出の前にキーの名前だけを数えて合うかを見る。

- 1 行 1 JSON。`type` が `user`・`assistant` の行が会話。ほかに `system`、`attachment`（注入された文脈）、`file-history-snapshot`、`api-request` などがある
- `message.role` と `message.content`。content は文字列か、ブロックの配列（`text`・`thinking`・`tool_use`・`tool_result`）
- `isCompactSummary: true` の行が圧縮の要約。ここより前が要約された範囲
- `isSidechain: true` の行はサブエージェントの枝
- `isMeta: true` の user 行は、ユーザーが打った文ではなく注入されたもの。ユーザーの言葉は、`type: user` で `isMeta` でなく、content が文字列か `text` ブロックの行。`tool_result` だけの user 行はツールの結果
- `isMeta` の付かない注入もある。2026-10 にこのマシンの 61 の記録で、user 行の文の先頭のタグの名前だけを数えた（中身は読んでいない）。通知は `<task-notification>`（背景のタスクの終了）、`<bash-stdout>`（`!` で打ったコマンドの出力）、`<local-command-stdout>`（ローカルのコマンドの出力）。ユーザーの行為は `<command-name>`・`<command-message>`（スラッシュコマンド）、`<bash-input>`（`!` で打ったコマンド）、`<pasted_content>`（貼り付け）、`[Request interrupted`（中断）。通知をユーザーの発言として引用しないよう、抽出で分ける
- `timestamp`、`uuid`、`parentUuid`、`cwd`、`gitBranch`、`agentSetting`（担当）、`attributionSkill`（スキルから出たツールの呼び出し）
- ツールの結果の行の `permissionDecision` は辞書で、`decision`（`accept`・`reject`）、`source`（`config`・`user_temporary`）、`reasonType`（`classifier`・`mode`・`safetyCheck`・`other`）、hook の承認なら `reasonCode: hook:PreToolUse` を持つ。拒否された行には `toolDenialKind`（`automode-blocked`・`permission-rule` など）が付く

## 抽出

記録は数十 MB になるので、全文を読まずに抜粋を作る。scratchpad に script を置き、順に、ユーザーの発言（全文）、通知の印、Claude の text（全文）、tool_use の名前と入力の要点、拒否、圧縮の印を、行番号と時刻つきで書き出す。thinking と tool_result の本文は含めない（見立てには使わず、量が大きい）。事象を特定した後、その周辺の tool_result だけを読む。

抽出が正しいかは、文脈に残っている直近のやり取りと抜粋の末尾を突き合わせて確かめる。ユーザーの打った文がユーザーの発言として出ているか、注入された文が混ざっていないか。USER の文の先頭のタグも数え、通知のタグや見たことのないタグが無いかを見る。見たことのないタグは、ユーザーの行為か通知かで振り分け、通知なら `NOTICE` に足す。

骨子（`python3 -I 抽出.py 記録のパス > 抜粋.txt`。2026-10 に約 9,500 行の記録で、通知を分ける前の版で USER だった 159 件が USER 154 件と NOTICE 5 件に分かれ、USER に通知のタグが残らず、末尾の USER がユーザーの直近の発言と一致することを確かめた）:

```python
import json, sys

# isMeta の付かない注入のうち、ユーザーの行為でないもの（stderr は未観測だが同じ形と見て含める）
NOTICE = ('<task-notification', '<bash-std', '<local-command-std')

for n, line in enumerate(open(sys.argv[1]), 1):
    try:
        o = json.loads(line)
    except json.JSONDecodeError:
        continue
    if o.get('isSidechain'):
        continue
    t = o.get('type')
    m = o.get('message') or {}
    c = m.get('content')
    ts = (o.get('timestamp') or '')[:19]
    if o.get('isCompactSummary'):
        print(f'--- {n} {ts} COMPACT SUMMARY ---')
        continue
    if t == 'user' and not o.get('isMeta'):
        texts = [c] if isinstance(c, str) else [b.get('text', '') for b in (c or []) if b.get('type') == 'text']
        for text in filter(None, texts):
            if text.lstrip().startswith(NOTICE):
                print(f'[{n} {ts} NOTICE] {text.strip()[:120]}')
            else:
                print(f'[{n} {ts} USER]\n{text}')
        decision = (o.get('permissionDecision') or {}).get('decision')
        if o.get('toolDenialKind') or decision not in (None, 'accept'):
            print(f'[{n} {ts} TOOL DENIED] decision={decision} kind={o.get("toolDenialKind")}')
    elif t == 'assistant':
        for b in (c or []):
            if b.get('type') == 'text':
                print(f'[{n} {ts} CLAUDE]\n{b.get("text", "")}')
            elif b.get('type') == 'tool_use':
                inp = b.get('input') or {}
                brief = inp.get('command') or inp.get('file_path') or inp.get('description') or inp.get('skill') or ''
                print(f'[{n} {ts} TOOL {b.get("name")}] {str(brief)[:120]}')
```
