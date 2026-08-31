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
SURGEPY=${SURGEPY_DIR:-$HOME/code/surge-src/ignore/bpy/src/surge-python}
LOG=$DATA/patchboard.log

pid() { pgrep -f "venv-audition/bin/python tools/patchboard.py" | head -1 || true; }

case "${1:-status}" in
  start)
    if [ -n "$(pid)" ]; then echo "already running (pid $(pid))"; exit 0; fi
    cd "$ROOT"
    setsid nohup env PYTHONPATH="$SURGEPY" \
      .venv-audition/bin/python tools/patchboard.py "$DATA/perf" \
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
