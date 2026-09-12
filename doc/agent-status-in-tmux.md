# Agent Status in tmux

Every session in the status line carries a dot showing what its coding agent is
doing. A glance tells you which session is working and which one wants you.

| Dot | State | Meaning |
|---|---|---|
| `○` dim | `idle` | Nothing outstanding |
| `○ ● ○ ●` bright | `busy` | The agent is working. The dot pulses once a second |
| `●` brightest | `attention` | The agent has something for you |

A solid dot means the agent finished a turn, or asked permission, while you
were somewhere else. Switching to that session clears it. A turn that ends in
front of you never raises one, so the dot only ever marks what you missed.

The dots stay in the bar's own green. Brightness carries the urgency, and the
shape separates busy from attention. The scale inverts wherever the background
is lighter than the text — in light mode, and on the current session's
highlight block — so there idle is the dot closest to the background and
attention is the darkest.

---

## The pieces

`bin/agent-status <state>` writes the state to the tmux session option
`@agent_state`. The state belongs to the session, so it survives detaching, and
a session with no option reads as idle.

```
agent-status busy | attention | idle | seen | focus in|out | get
```

It resolves the session from `TMUX_PANE`, which a hook process inherits from
the CLI. Called from inside a tmux hook there is no `TMUX_PANE`, so it falls
back to the hook's own target. It drains stdin, because both CLIs feed the hook
their event as JSON and a hook that never reads the pipe can block the agent.
It exits 0 whatever happens, so a failure never fails the hook. Setting `idle`
unsets the option rather than storing a value, which keeps the state clean.

`attention` downgrades to `idle` when you are already watching that session —
a client shows it, and the terminal has focus. `home/tmux.conf` keeps the
focus flag current and clears the dot when you arrive:

```
set-hook -g client-session-changed 'run-shell -b "~/bin/agent-status seen"'
set-hook -g client-focus-in  'run-shell -b "~/bin/agent-status focus in"'
set-hook -g client-focus-out 'run-shell -b "~/bin/agent-status focus out"'
```

`run-shell -b` runs them in the background, so a hook never delays a session
switch. An unset focus flag counts as focused, so a missing hook cannot leave
stale dots.

Each write calls `tmux refresh-client -S`, so the dot changes at once rather
than waiting for the next tick.

`bin/tmux-session-list` draws the dots. It reads every session's state in one
call:

```
tmux ls -F '#{session_created}|#{@agent_state}|#{session_name}'
```

The pulse comes from the clock, not from a timer: the script takes
`date +%s % 2` and picks a filled or a hollow glyph. Every pulsing dot shares
that phase, so they beat together.

`home/tmux.conf` sets `status-interval 1`. Without it tmux redraws every 15
seconds and the pulse does not move.

The dots cost about three columns per session, so `status-left-length` is 300.
At the old 100 the last session names were cut off.

---

## Wiring

### Claude Code

`home/claude/settings.json`:

| Event | State |
|---|---|
| `UserPromptSubmit` | `busy` |
| `PreToolUse` (`*`) | `busy` |
| `Stop` | `attention` |
| `SessionEnd` | `idle` |

`Notification` is not used. It fires both for a permission request and after
sixty seconds of waiting on you, and the second one turns every parked session
solid.

`UserPromptSubmit` already carries `canary-inject.sh`, so the state hook is
appended to that array rather than replacing it.

### Copilot CLI

`home/copilot/hooks/agent-status-hooks.json`, linked to
`~/.copilot/hooks/` by `do.sh link-copilot`:

| Event | State |
|---|---|
| `userPromptSubmitted` | `busy` |
| `preToolUse` | `busy` |
| `agentStop` | `attention` |
| `sessionEnd` | `idle` |
| `permissionRequest` | `attention` |
| `errorOccurred` | `attention` |

Copilot's documented events are `sessionStart`, `sessionEnd`,
`userPromptSubmitted`, `preToolUse`, `postToolUse`, `agentStop`, `subagentStop`
and `errorOccurred`. `permissionRequest` is in the CLI's hook enum but not in
the published list, so treat it as untested.

Both CLIs load hooks at start, so a change needs a restart.

`PreToolUse` is what takes the dot off `attention` when work restarts.
Approving a permission prompt is not a prompt submission, so
`UserPromptSubmit` does not fire — the next tool call is the first sign the
agent resumed.

Hooks run synchronously and block the agent, and `PreToolUse` runs on every
tool call. `agent-status` reads the state first and returns without writing or
redrawing when nothing changed, which costs about 50 ms.

---

## Limits

A CLI killed mid-turn leaves the dot pulsing, because no `agentStop` or `Stop`
fires. Clear it by hand:

```
agent-status idle
```

Two agents in one session share one dot. The state is per session, not per
pane.

---

## Also here, not wired up

`bin/agent-notify` sends a macOS notification naming the tmux session that
wants you. It was tried and found intrusive. To use it, call it from the same
hooks that call `agent-status attention`.

---

## See also

- [bin-scripts.md](bin-scripts.md) — the scripts in `bin/`
- [tmux-config.md](tmux-config.md) — the rest of the tmux configuration
- [Copilot CLI hooks](https://docs.github.com/en/copilot/how-tos/copilot-cli/customize-copilot/use-hooks)
- [Claude Code hooks](https://docs.claude.com/en/docs/claude-code/hooks)
