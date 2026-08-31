#!/usr/bin/env bash
# The patchboard, daemonless: start/stop/status/restart/log.
#
# Runs detached in its own session (setsid + nohup) so it survives the
# shell that launched it; state lives in ~/.local/share/otb (perf IRs,
# w3.scl, patchboard.log). Regenerate IRs with:
#   otb album corpus/bach-wtc/kern ~/.local/share/otb/perf
#
# License: GPL-2.0-or-later.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
DATA=$HOME/.local/share/otb
# surgepy resolution: explicit env, the stable copy under DATA, or a
# surge build tree
if [ -n "${SURGEPY_DIR:-}" ]; then
  SURGEPY=$SURGEPY_DIR
elif [ -d "$HOME/.local/share/otb/surgepy" ]; then
  SURGEPY=$HOME/.local/share/otb/surgepy
else
  SURGEPY=$(dirname "$(find $HOME/code/surge -name 'surgepy*.so' 2>/dev/null | head -1)" 2>/dev/null || true)
fi
LOG=$DATA/patchboard.log
# the venv may live in the repo or in $HOME (the README's location)
if [ -x "$ROOT/.venv-audition/bin/python" ]; then
  PY=$ROOT/.venv-audition/bin/python
elif [ -x "$HOME/.venv-audition/bin/python" ]; then
  PY=$HOME/.venv-audition/bin/python
else
  echo "no .venv-audition found in $ROOT or \$HOME (see README)"; exit 1
fi

pid() {
  # match the python itself, not shells whose cmdline quotes the same text
  for p in $(pgrep -f "tools/patchboard.py" 2>/dev/null); do
    if [ "$(ps -o comm= -p "$p" 2>/dev/null)" = python ]; then
      echo "$p"; return
    fi
  done
  true
}

case "${1:-status}" in
  start)
    if [ -n "$(pid)" ]; then echo "already running (pid $(pid))"; exit 0; fi
    cd "$ROOT"
    setsid nohup env PYTHONPATH="$SURGEPY" \
      "$PY" tools/patchboard.py "$DATA/perf" \
      --scl "$DATA/w3.scl" --port 8766 \
      --calibration config/calibration.json \
      > "$LOG" 2>&1 < /dev/null &
    sleep 2
    [ -n "$(pid)" ] && echo "up (pid $(pid)) — /bach on the tailnet" \
      || { echo "failed to start:"; tail -5 "$LOG"; exit 1; }
    ;;
  stop)
    p=$(pid)
    [ -n "$p" ] && kill "$p" && echo "stopped (pid $p)" || echo "not running"
    ;;
  restart)
    "$0" stop; sleep 1; "$0" start
    ;;
  status)
    p=$(pid)
    if [ -n "$p" ]; then
      echo "running (pid $p)"
      curl -s http://127.0.0.1:8766/state | head -c 200; echo
    else
      echo "not running"
    fi
    ;;
  log)
    tail -20 "$LOG"
    ;;
  *)
    echo "usage: board.sh [start|stop|restart|status|log]"; exit 1
    ;;
esac
